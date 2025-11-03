using Ledger.Web.Data;
using Microsoft.EntityFrameworkCore;
using Ledger.Web.Components;
using Ledger.Web.Identity;              
using Microsoft.AspNetCore.Identity; 
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Http; 



var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services.AddDbContext<LedgerContext>(options =>
{
    options.UseNpgsql(builder.Configuration.GetConnectionString("Default"));
});

builder.Services.AddDbContext<AuthDbContext>(o =>           
    o.UseNpgsql(builder.Configuration.GetConnectionString("Default")));


builder.Services
    .AddDefaultIdentity<ApplicationUser>(opts =>                 
    {
        opts.SignIn.RequireConfirmedAccount = false;
        opts.Password.RequiredLength = 6;
        opts.Password.RequireDigit = false;
        opts.Password.RequireUppercase = false;
        opts.Password.RequireNonAlphanumeric = false;
    })
    .AddEntityFrameworkStores<AuthDbContext>();


    builder.Services.ConfigureApplicationCookie(o =>
{
    o.Cookie.SecurePolicy = CookieSecurePolicy.SameAsRequest;
    o.SlidingExpiration = true;
    o.ExpireTimeSpan = TimeSpan.FromDays(14);
});

     
builder.Services.AddAuthorization();  

builder.Services.AddHttpClient();    
builder.Services.AddRazorPages(); 
builder.Services.AddRazorComponents()
    .AddInteractiveServerComponents();



var app = builder.Build();

// Configure the HTTP request pipeline.
if (!app.Environment.IsDevelopment())
{
    app.UseExceptionHandler("/Error", createScopeForErrors: true);
    // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
    app.UseHsts();
}

app.MapPost("/auth/login", async (
    [FromBody] LoginDto dto,
    SignInManager<ApplicationUser> signInManager,
    UserManager<ApplicationUser> userManager) =>
{
    // Find user by EMAIL only (since users register by email)
    var user = await userManager.FindByEmailAsync(dto.Email);
    if (user is null)
        return Results.BadRequest(new { error = "Invalid email or password" });

    var result = await signInManager.PasswordSignInAsync(
        user, dto.Password, dto.RememberMe, lockoutOnFailure: false);

    return result.Succeeded
        ? Results.Ok(new { ok = true })
        : Results.BadRequest(new { error = result.IsLockedOut ? "Locked out" : "Invalid credentials" });
})
.DisableAntiforgery();


app.UseHttpsRedirection();
app.UseStaticFiles();                                            

app.UseAuthentication();                                         
app.UseAuthorization();                                          


app.UseAntiforgery();

app.MapRazorPages();    
app.MapRazorComponents<App>()
    .AddInteractiveServerRenderMode();

    app.MapGet("/reports/statement", async (
    HttpContext http,
    LedgerContext db,
    AuthenticationStateProvider auth) =>
{
    var state = await auth.GetAuthenticationStateAsync();
    var user = state.User;
    if (!(user.Identity?.IsAuthenticated ?? false))
        return Results.Unauthorized();

    var uid = user.FindFirst(System.Security.Claims.ClaimTypes.NameIdentifier)!.Value;

        // 1) Write CSV for this user
    var txns = await db.Transactions
        .Where(t => t.UserId == uid)
        .OrderBy(t => t.Date)
        .ToListAsync();

    var tmp = Path.Combine(Path.GetTempPath(), $"ledger_{uid}_{Guid.NewGuid():N}");
    Directory.CreateDirectory(tmp);
    var csvPath = Path.Combine(tmp, "transactions.csv");
    var outPath = Path.Combine(tmp, "statement.txt");

    await using (var sw = new StreamWriter(csvPath))
    {
        await sw.WriteLineAsync("Date,Type,Amount,Memo,UserId");
        foreach (var t in txns)
        {
            var type = t.Type == TxnType.Credit ? "Credit" : "Debit";
            var memo = (t.Memo ?? "").Replace("\"", "'");
            await sw.WriteLineAsync($"{t.Date:yyyy-MM-dd},{type},{t.Amount},{memo},{t.UserId}");
        }
    }

        // 2) Invoke COBOL program
    var exe = Path.Combine(AppContext.BaseDirectory, "cobol/bin/ledger_report");
    if (!System.IO.File.Exists(exe))
        return Results.Problem("COBOL binary not found. Build it with cobol/build.sh", statusCode: 500);

    var psi = new System.Diagnostics.ProcessStartInfo
    {
        FileName = exe,
        ArgumentList = { csvPath, outPath },
        RedirectStandardOutput = true,
        RedirectStandardError = true
    };
    using var proc = System.Diagnostics.Process.Start(psi)!;
    var stderr = await proc.StandardError.ReadToEndAsync();
    await proc.WaitForExitAsync();
    if (proc.ExitCode != 0)
        return Results.Problem($"COBOL error:\n{stderr}", statusCode: 500);

})
.RequireAuthorization();

app.Run();

public record LoginDto(string Email, string Password, bool RememberMe);

