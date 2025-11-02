window.auth = {
    login: async function (dto) {
      const res = await fetch("/auth/login", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        credentials: "include", // crucial for cookies
        body: JSON.stringify(dto)
      });
      return res.ok;
    }
  };
  