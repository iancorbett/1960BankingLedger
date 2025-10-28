using Ledger.Web.Data;
using Microsoft.EntityFrameworkCore;
using Ledger.Web.Components;
using Ledger.Web.Identity;              
using Microsoft.AspNetCore.Identity; 

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
        opts.SignIn.RequireConfirmedAccount = false;
        opts.Password.RequiredLength = 6;
        opts.Password.RequireDigit = false;
        opts.Password.RequireUppercase = false;
        opts.Password.RequireNonAlphanumeric = false;
    })
    .AddEntityFrameworkStores<AuthDbContext>();

builder.Services.AddAuthentication().AddIdentityCookies();       
builder.Services.AddAuthorization();  


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

app.UseHttpsRedirection();


app.UseAntiforgery();

app.MapStaticAssets();
app.MapRazorComponents<App>()
    .AddInteractiveServerRenderMode();

app.Run();
