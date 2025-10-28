using Ledger.Web.Data;
using Microsoft.EntityFrameworkCore;
using Ledger.Web.Components;
using Ledger.Web.Identity;              
using Microsoft.AspNetCore.Identity; 
using Microsoft.AspNetCore.Mvc; 

public record LoginDto(string Email, string Password, bool RememberMe);

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
    SignInManager<ApplicationUser> signInManager) =>
{
    var result = await signInManager.PasswordSignInAsync(
        dto.Email, dto.Password, dto.RememberMe, lockoutOnFailure: false);

    if (!result.Succeeded)
        return Results.BadRequest(new { error = result.IsLockedOut ? "Locked out" : "Invalid credentials" });

    return Results.Ok(new { ok = true });
})
.DisableAntiforgery(); // JSON post from Blazor component


app.UseHttpsRedirection();
app.UseStaticFiles();                                            

app.UseAuthentication();                                         
app.UseAuthorization();                                          


app.UseAntiforgery();

app.MapRazorPages();    
app.MapRazorComponents<App>()
    .AddInteractiveServerRenderMode();

app.Run();
