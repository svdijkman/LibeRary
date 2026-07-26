(function() {
  var STORAGE_KEY = "libeRaryDarkTheme";

  function isDarkPreferred() {
    return window.LibeRDesign.theme.initialDark(STORAGE_KEY);
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
    window.LibeRDesign.theme.store(dark, STORAGE_KEY, true);
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
