## 1960 Banking Ledger

A retro-themed personal finance ledger built with modern tech.

Ledger lets users securely log in, track transactions, and visualize their balances — styled like an old-school checkbook but powered by ASP.NET Core, EF Core, and PostgreSQL.

---

<img width="1683" height="920" alt="Screenshot 2025-11-02 at 10 33 11 PM" src="https://github.com/user-attachments/assets/57169bbd-b625-4737-b19a-949277a55433" />

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
git clone https://github.com/iancorbett/1960Ledger.git
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

 ## Authentication

Identity-based login (/auth/login endpoint)

Password policy: 6 chars minimum, no uppercase/digit/symbol requirements (easy local dev)

Cookie-based session; persists 14 days
