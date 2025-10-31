using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Ledger.Web.Identity;

namespace Ledger.Web.Data;

// Explicit type for debit/credit selection in the UI
public enum TxnType
{
    Debit = -1,
    Credit = 1
}

public class Transaction
{
    public int Id { get; set; }

    // IdentityUser.Id is a string (GUID), so keep this as string
    [Required]
    public string UserId { get; set; } = "";

    // Navigation to your Identity user
    [ForeignKey(nameof(UserId))]
    public ApplicationUser? User { get; set; }

    [Required]
    public DateTime Date { get; set; } = DateTime.UtcNow.Date;

    [NotMapped]
        public DateTime CreatedAt
    {
        get => Date;
        set => Date = value;
    }
     [Required, MaxLength(100)] public string Payee { get; set; } = "";

    [MaxLength(200)]
    [MaxLength(200)] public string? Memo { get; set; } = "";

    // Store money with fixed precision/scale in Postgres
    [Required, Column(TypeName = "numeric(12,2)")]
    public decimal Amount { get; set; } // +credit, -debit

    // Convenience: derive the type from the sign of Amount (not stored in DB)
    [NotMapped]
    public TxnType Type => Amount >= 0 ? TxnType.Credit : TxnType.Debit;
}
