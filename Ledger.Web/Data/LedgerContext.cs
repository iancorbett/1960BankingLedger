using Microsoft.EntityFrameworkCore;

namespace Ledger.Web.Data;

public class LedgerContext : DbContext
{
    public LedgerContext(DbContextOptions<LedgerContext> options) : base(options) { }

    public DbSet<User> Users => Set<User>();
    public DbSet<Transaction> Transactions => Set<Transaction>();
}
