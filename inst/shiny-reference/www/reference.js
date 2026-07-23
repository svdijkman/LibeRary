(function () {
  function applyTheme(dark) {
    document.body.classList.toggle('theme-dark', dark);
    document.documentElement.setAttribute('data-liber-theme', dark ? 'dark' : 'light');
    var toggle = document.getElementById('theme_toggle');
    if (toggle) toggle.checked = dark;
    var label = document.getElementById('theme_label');
    if (label) label.textContent = dark ? 'Dark' : 'Light';
  }

  function boot() {
    var dark = !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
    try {
      var shared = localStorage.getItem('liber.theme');
      if (shared === 'dark' || shared === 'light') dark = shared === 'dark';
      else {
        var legacy = localStorage.getItem('libeRaryDarkTheme');
        if (legacy === '1' || legacy === '0') dark = legacy === '1';
      }
    } catch (e) {}
    applyTheme(dark);
    document.addEventListener('change', function (event) {
      if (event.target && event.target.id === 'theme_toggle') {
        applyTheme(event.target.checked);
        try {
          localStorage.setItem('liber.theme', event.target.checked ? 'dark' : 'light');
          localStorage.setItem('libeRaryDarkTheme', event.target.checked ? '1' : '0');
          document.documentElement.setAttribute('data-liber-theme', event.target.checked ? 'dark' : 'light');
        } catch (e) {}
      }
    });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot);
  else boot();

  function registerShinyHandler() {
    if (!window.Shiny || registerShinyHandler.done) return;
    registerShinyHandler.done = true;
    Shiny.addCustomMessageHandler('referenceToggleTraining', function (message) {
      var input = document.getElementById('training_eligible');
      if (!input) return;
      input.disabled = !!message.disabled;
      if (message.disabled) input.checked = false;
      var label = input.closest('label');
      if (label) label.style.opacity = message.disabled ? '0.55' : '1';
    });
  }
  registerShinyHandler();
  document.addEventListener('shiny:connected', registerShinyHandler);
})();
