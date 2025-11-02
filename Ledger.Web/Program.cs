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

app.Run();

public record LoginDto(string Email, string Password, bool RememberMe);

