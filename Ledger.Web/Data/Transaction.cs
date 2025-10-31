using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Ledger.Web.Identity;

namespace Ledger.Web.Data;

public class Transaction
{
    public int Id { get; set; }

    //  Use string because IdentityUser.Id is string (GUID)
    [Required]
    public string UserId { get; set; } = "";

    //  Navigation property to ApplicationUser
    [ForeignKey(nameof(UserId))]
    public ApplicationUser? User { get; set; }

    [Required]
    public DateTime Date { get; set; } = DateTime.UtcNow.Date;

    [Required, MaxLength(100)]
    public string Payee { get; set; } = "";

    [MaxLength(200)]
    public string? Memo { get; set; } = "";

    [Required, Column(TypeName = "numeric(12,2)")]
    public decimal Amount { get; set; } // +credit, -debit
}

namespace Ledger.Web.Data
{
    public enum TxnType
    {
        Debit = -1,
        Credit = 1
    }

    public class Transaction
    {
        public int Id { get; set; }

        [Required] public int UserId { get; set; }
        public User? User { get; set; }

        [Required] public DateTime Date { get; set; } = DateTime.UtcNow.Date;
        [Required] public string Payee { get; set; } = "";
        public string Memo { get; set; } = "";

        // +credit, -debit
        [Required] public decimal Amount { get; set; }

        // Optional: store explicit type; you can also derive this from Amount
        public TxnType Type => Amount >= 0 ? TxnType.Credit : TxnType.Debit;
    }
}

