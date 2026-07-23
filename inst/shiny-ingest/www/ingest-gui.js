// Live log, progress, and theme toggle for the LibeRary literature GUI
var INGEST_THEME_KEY = "liberaryIngestDarkTheme";

function ingestDarkPreferred() {
  try {
    var shared = localStorage.getItem("liber.theme");
    if (shared === "dark" || shared === "light") return shared === "dark";
    var legacy = localStorage.getItem(INGEST_THEME_KEY);
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
  if (label) label.textContent = dark ? "Dark" : "Light";
  if (toggle) toggle.checked = !!dark;
  try {
    localStorage.setItem("liber.theme", dark ? "dark" : "light");
    localStorage.setItem(INGEST_THEME_KEY, dark ? "1" : "0");
    document.documentElement.setAttribute("data-liber-theme", dark ? "dark" : "light");
  } catch (e) {}
}

function initThemeToggle() {
  var toggle = document.getElementById("theme_toggle");
  if (!toggle || toggle.dataset.bound === "1") return;
  toggle.dataset.bound = "1";

  applyTheme(ingestDarkPreferred());

  toggle.addEventListener("change", function() {
    applyTheme(toggle.checked);
  });
}

$(document).on("shiny:connected", function() {
  initThemeToggle();

  Shiny.addCustomMessageHandler("appendLog", function(message) {
    var box = document.getElementById("log_box");
    if (!box) return;
    if (box.innerText === "Ready.") box.innerText = "";
    box.innerText += (box.innerText.length ? "\n" : "") + message;
    box.scrollTop = box.scrollHeight;
  });

  Shiny.addCustomMessageHandler("resetLog", function(x) {
    var box = document.getElementById("log_box");
    if (!box) return;
    box.innerText = "";
  });

  Shiny.addCustomMessageHandler("setProgress", function(x) {
    var bar = document.getElementById(x.scope === "current" ? "current_prog_bar" : "prog_bar");
    if (!bar) return;
    var value = Math.max(0, Math.min(100, Number(x.value) || 0));
    bar.style.width = value + "%";
    bar.innerText = value + "%";
    bar.setAttribute("aria-valuenow", value);
    bar.classList.remove("progress-bar-success", "progress-bar-danger", "progress-bar-warning");
    if (x.status === "done") {
      bar.classList.remove("active");
      bar.classList.add("progress-bar-success");
    } else if (x.status === "error") {
      bar.classList.remove("active");
      bar.classList.add("progress-bar-danger");
    } else if (x.status === "cancelled") {
      bar.classList.remove("active");
      bar.classList.add("progress-bar-warning");
    } else if (x.status === "idle") {
      bar.classList.remove("active");
    } else {
      bar.classList.add("active");
    }
  });
});

// Ensure toggle works even if DOM ready before shiny:connected
$(function() {
  initThemeToggle();
});
