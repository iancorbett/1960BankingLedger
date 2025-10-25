using System.ComponentModel.DataAnnotations;

namespace Ledger.Web.Data;

public class Transaction
{
    public int Id { get; set; }

    [Required] public int UserId { get; set; }
    public User? User { get; set; }

    [Required] public DateTime Date { get; set; } = DateTime.UtcNow.Date;
    [Required] public string Payee { get; set; } = "";
    public string Memo { get; set; } = "";
    [Required] public decimal Amount { get; set; } // +credit, -debit
}
