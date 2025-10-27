using System.Net.Http.Headers;
using System.Net.Http.Json;
using Microsoft.JSInterop;

namespace Client.Services
{
    public class AuthService
    {
        private readonly HttpClient _http; //lets Blazor call backend (/auth/login, /api/..., etc.)
        private readonly IJSRuntime _js; //allows C# to call browser JavaScript functions — like localStorage

        public AuthService(HttpClient http, IJSRuntime js) //Blazor automatically injects the main HttpClient and IJSRuntime when this service starts up.
        {
            _http = http;
            _js = js;
        }

        public async Task<bool> Login(string email, string password)
        {
            var res = await _http.PostAsJsonAsync("/auth/login", new { email, password }); //Sends a POST request to backend /auth/login route with the email and password.
            if (!res.IsSuccessStatusCode) return false;

            var payload = await res.Content.ReadFromJsonAsync<LoginReply>(); //reads the response and turns it into a small C# object
            if (string.IsNullOrWhiteSpace(payload?.token)) return false;

            await _js.InvokeVoidAsync("localStorage.setItem", "auth_token", payload!.token); //Saves the JWT token in localStorage
            _http.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", payload.token);
            return true;
        }

        public async Task Logout() //removes token from browser
        {
            await _js.InvokeVoidAsync("localStorage.removeItem", "auth_token");
            _http.DefaultRequestHeaders.Authorization = null;
        }

        public async Task TryAttachToken() //checks if token already is attached, re applies same one if it exists
        {
            var token = await _js.InvokeAsync<string>("localStorage.getItem", "auth_token");
            if (!string.IsNullOrWhiteSpace(token))
            {
                _http.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", token);
            }
        }

        private record LoginReply(string token);
    }
}
