using Microsoft.EntityFrameworkCore;

namespace Ledger.Web.Data;

public class LedgerContext : DbContext
{
    public LedgerContext(DbContextOptions<LedgerContext> options) : base(options) { }

    public DbSet<Transaction> Transactions => Set<Transaction>();

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        // Don't let EF create its own ApplicationUser table
        modelBuilder.Entity<Transaction>().Ignore(t => t.User);
    }
}
