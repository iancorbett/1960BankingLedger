using System.Net.Http.Headers;
using System.Net.Http.Json;
using Microsoft.JSInterop;

namespace Client.Services
{
    public class AuthService
    {
        private readonly HttpClient _http;
        private readonly IJSRuntime _js;

        public AuthService(HttpClient http, IJSRuntime js)
        {
            _http = http;
            _js = js;
        }

        public async Task<bool> Login(string email, string password)
        {
            var res = await _http.PostAsJsonAsync("/auth/login", new { email, password });
            if (!res.IsSuccessStatusCode) return false;

            var payload = await res.Content.ReadFromJsonAsync<LoginReply>();
            if (string.IsNullOrWhiteSpace(payload?.token)) return false;

            await _js.InvokeVoidAsync("localStorage.setItem", "auth_token", payload!.token);
            _http.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", payload.token);
            return true;
        }

        public async Task Logout()
        {
            await _js.InvokeVoidAsync("localStorage.removeItem", "auth_token");
            _http.DefaultRequestHeaders.Authorization = null;
        }

        public async Task TryAttachToken()
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
