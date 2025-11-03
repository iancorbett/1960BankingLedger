## 1960 Banking Ledger

A retro-themed personal finance ledger built with modern tech.

Ledger lets users securely log in, track transactions, and visualize their balances — styled like an old-school checkbook but powered by ASP.NET Core, EF Core, and PostgreSQL.

---

## Tech Stack
Layer	Technology
Frontend	Blazor Server (Razor Components, Interactive Mode)
Backend	ASP.NET Core 8
Database	PostgreSQL + EF Core
Auth	ASP.NET Identity (Core)
Language	C#
Hosting	Local / Render / Azure / Fly.io
Styling	Bootstrap 5 (default Blazor template)

---

## Setup

**1. Clone & restore**
git clone https://github.com/korbskilabs/1960Ledger.git
cd 1960Ledger/Ledger.Web
dotnet restore

**2. Configure database**

Create a PostgreSQL database and add its connection string in
appsettings.Development.json:

{
  "ConnectionStrings": {
    "Default": "Host=localhost;Port=5432;Database=ledgerdb;Username=postgres;Password=yourpassword"
  }
}

**3. Apply migrations**
dotnet ef database update --project Ledger.Web

**4. Run the app**
dotnet run


Visit https://localhost:5001
 (or whatever port it logs).

 ---