(function() {
  var STORAGE_KEY = "libeRaryDarkTheme";

  function isDarkPreferred() {
    try {
      var shared = localStorage.getItem("liber.theme");
      if (shared === "dark" || shared === "light") return shared === "dark";
      var legacy = localStorage.getItem(STORAGE_KEY);
      if (legacy === "1" || legacy === "0") return legacy === "1";
    } catch (e) {
      return !!(window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches);
    }
    return !!(window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches);
  }

  function applyTheme(dark) {
    if (dark) {
      document.body.classList.add("theme-dark");
    } else {
      document.body.classList.remove("theme-dark");
    }
    var label = document.getElementById("theme_label");
    var toggle = document.getElementById("theme_toggle");
    if (label) {
      label.textContent = dark ? "Dark" : "Light";
    }
    if (toggle) {
      toggle.checked = !!dark;
    }
    try {
      localStorage.setItem("liber.theme", dark ? "dark" : "light");
      localStorage.setItem(STORAGE_KEY, dark ? "1" : "0");
      document.documentElement.setAttribute("data-liber-theme", dark ? "dark" : "light");
    } catch (e) {}
  }

  function initThemeToggle() {
    var toggle = document.getElementById("theme_toggle");
    if (!toggle || toggle.dataset.bound === "1") {
      return;
    }
    toggle.dataset.bound = "1";
    applyTheme(isDarkPreferred());
    toggle.addEventListener("change", function() {
      applyTheme(toggle.checked);
    });
  }

  $(document).on("shiny:connected", initThemeToggle);
  $(function() {
    applyTheme(isDarkPreferred());
    initThemeToggle();
  });
})();
