using System.ComponentModel.DataAnnotations;

namespace Ledger.Web.Data;

public class User
{
    public int Id { get; set; }

    [Required, EmailAddress]
    public string Email { get; set; } = "";

    [Required]
    public string PasswordHash { get; set; } = "";

    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
}
