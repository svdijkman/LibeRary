# LibeRary literature pipeline - run via ingest_shiny()

library(shiny)
if (requireNamespace("DT", quietly = TRUE)) library(DT)

`%||%` <- function(x, y) {
  if (is.null(x) || (is.atomic(x) && length(x) == 1L && is.na(x))) y else x
}

pkg_root <- Sys.getenv("LIBERARY_PKG_ROOT", "")
if (!nzchar(pkg_root)) {
  sf <- system.file("", package = "LibeRary")
  if (nzchar(sf)) {
    pkg_root <- normalizePath(sf, winslash = "/", mustWork = FALSE)
  }
}
if (!nzchar(pkg_root)) {
  cand <- normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "DESCRIPTION"))) {
    desc <- tryCatch(read.dcf(file.path(cand, "DESCRIPTION")), error = function(e) NULL)
    if (!is.null(desc) && desc[1, "Package"] == "LibeRary") pkg_root <- cand
  }
}
is_source_root <- nzchar(pkg_root) && length(list.files(file.path(pkg_root, "R"), pattern = "[.]R$")) > 0L
if (is_source_root && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg_root, quiet = TRUE)
} else {
  library(LibeRary)
}

default_config_path <- library_config_path()
default_cfg <- tryCatch(ingest_load_config(), error = function(e) getFromNamespace("DEFAULT_CONFIG", "LibeRary"))
default_entrez_email <- default_cfg$entrez$email %||% ""
default_entrez_api_key <- default_cfg$entrez$api_key %||% ""
default_data_dir <- default_cfg$data_dir %||% library_home()
provider_choices <- stats::setNames(library_llm_providers()$id, library_llm_providers()$name)

llm_role_control <- function(role, title, choices, selected) {
  tags$div(
    class = "llm-role-block",
    tags$div(class = "llm-role-title", title),
    tags$div(
      class = "llm-role-controls",
      tags$div(
        class = "llm-provider-control",
        selectInput(paste0(role, "_provider"), NULL, choices = choices,
                    selected = selected, width = "100%")
      ),
      tags$div(class = "llm-model-control", uiOutput(paste0(role, "_model_ui"))),
      actionButton(
        paste0("edit_instruction_", role), NULL, icon = icon("edit"),
        class = "btn-default btn-sm llm-instruction-button",
        title = paste("Edit", title, "model instruction"),
        `aria-label` = paste("Edit", title, "model instruction")
      )
    )
  )
}

package_version <- as.character(utils::packageVersion("LibeRary"))

ui <- fluidPage(
  tags$head(
    tags$title("LibeRary"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$script(HTML("
      (function() {
        function boot() {
          try {
            var shared = localStorage.getItem('liber.theme');
            var legacy = localStorage.getItem('liberaryIngestDarkTheme');
            var dark = shared === 'dark' || (shared !== 'light' && legacy === '1');
            if (shared !== 'dark' && shared !== 'light' && legacy !== '1' && legacy !== '0') {
              dark = matchMedia('(prefers-color-scheme: dark)').matches;
            }
            document.documentElement.setAttribute('data-liber-theme', dark ? 'dark' : 'light');
            if (dark) document.body.classList.add('theme-dark');
          } catch (e) {
            if (matchMedia('(prefers-color-scheme: dark)').matches) document.body.classList.add('theme-dark');
          }
        }
        if (document.body) boot();
        else document.addEventListener('DOMContentLoaded', boot);
      })();
    ")),
    tags$style(HTML("
    :root {
      --ingest-bg: #f4f8f5;
      --ingest-surface: #ffffff;
      --ingest-surface-2: #f8fbf9;
      --ingest-text: #1e2b23;
      --ingest-muted: #63746a;
      --ingest-border: #d5e3da;
      --ingest-input-bg: #ffffff;
      --ingest-log-bg: #12231a;
      --ingest-log-fg: #dcebe2;
      --ingest-tab-active: #ffffff;
      --ingest-accent: #236a45;
      --ingest-accent-hover: #184e34;
      --ingest-accent-soft: #e6f1ea;
      --ingest-accent-text: #184e34;
      --ingest-shadow: rgba(20,70,43,.10);
    }
    body.theme-dark {
      --ingest-bg: #10271e;
      --ingest-surface: #18382b;
      --ingest-surface-2: #214936;
      --ingest-text: #f1faf5;
      --ingest-muted: #b7d0c3;
      --ingest-border: #3b6f58;
      --ingest-input-bg: #24513f;
      --ingest-log-bg: #0c2018;
      --ingest-log-fg: #e4f3eb;
      --ingest-tab-active: #24513f;
      --ingest-accent: #65d39b;
      --ingest-accent-hover: #86e2b0;
      --ingest-accent-soft: #2b5e48;
      --ingest-accent-text: #d0f5e0;
      --ingest-shadow: rgba(0,0,0,.28);
    }
    body {
      background-color: var(--ingest-bg);
      color: var(--ingest-text);
      font: 12px/1.45 'Segoe UI', Arial, sans-serif;
      transition: background-color 0.2s, color 0.2s;
    }
    .container-fluid { padding: 0 15px 18px; }
    .well, .tab-content {
      border: 1px solid var(--ingest-border);
      border-radius: 10px;
      box-shadow: 0 2px 7px var(--ingest-shadow);
    }
    .form-control, .selectize-input { min-height: 34px; border-radius: 7px; }
    .btn { min-height: 32px; border-radius: 7px; font-size: 12px; font-weight: 650; }
    .well, .tab-content, .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
      background-color: var(--ingest-surface) !important;
      color: var(--ingest-text) !important;
      border-color: var(--ingest-border) !important;
    }
    .nav-tabs { min-height: 42px; padding: 0 13px; background: var(--ingest-surface-2); border: 0; border-bottom: 1px solid var(--ingest-border); border-radius: 10px 10px 0 0; }
    .nav-tabs > li > a { min-height: 42px; margin: 0; padding: 11px 15px 8px; color: var(--ingest-muted); border: 0 !important; border-bottom: 3px solid transparent !important; background: transparent !important; font-weight: 650; }
    .nav-tabs > li.active > a { color: var(--ingest-accent); border-bottom-color: var(--ingest-accent) !important; }
    .form-control, textarea.form-control, .selectize-input {
      background-color: var(--ingest-input-bg);
      color: var(--ingest-text);
      border-color: var(--ingest-border);
    }
    .selectize-input input { color: var(--ingest-text); }
    .selectize-input {
      background: var(--ingest-input-bg) !important;
      color: var(--ingest-text) !important;
    }
    .selectize-dropdown, .selectize-dropdown-content {
      background: var(--ingest-input-bg) !important;
      color: var(--ingest-text) !important;
      border-color: var(--ingest-border);
    }
    .selectize-dropdown .active { background: var(--ingest-accent-soft); color: var(--ingest-text); }
    .form-control:focus {
      border-color: var(--ingest-accent);
      box-shadow: 0 0 0 2px rgba(101, 211, 155, 0.25);
    }
    .help-block, .text-muted { color: var(--ingest-muted) !important; }
    hr { border-color: var(--ingest-border); }
    #log_box {
      font-family: Consolas, monospace;
      font-size: 12px;
      background: var(--ingest-log-bg);
      color: var(--ingest-log-fg);
      height: 220px;
      overflow-y: auto;
      padding: 8px;
      border-radius: 4px;
      white-space: pre-wrap;
      border: 1px solid var(--ingest-border);
    }
    .status-badge { font-weight: bold; }
    body.theme-dark .progress { background-color: #2a4d3c; }
    .progress-bar { background-color: var(--ingest-accent); }
    .progress-bar.progress-bar-success { background-color: var(--ingest-accent); }
    .progress-bar.progress-bar-danger { background-color: #d65f70; color: #fff; }
    .progress-bar.progress-bar-warning { background-color: #e4b45f; color: #0c2018; }
    body.theme-dark .progress-bar:not(.progress-bar-danger) { color: #0c2018; }
    .progress-heading { display: flex; align-items: center; justify-content: space-between; gap: 12px; }
    .progress-heading h4 { margin: 0; }
    .progress-actions { display: flex; align-items: center; gap: 12px; min-height: 34px; }
    .progress-block {
      border: 1px solid var(--ingest-border); border-radius: 5px;
      padding: 9px 11px 7px; margin: 8px 0; background: var(--ingest-surface);
    }
    .progress-label {
      display: flex; justify-content: space-between; align-items: center;
      color: var(--ingest-muted); font-size: 12px; margin-bottom: 5px;
    }
    .progress-message { min-height: 20px; margin-top: 4px; font-size: 12px; }
    .btn.btn-primary, .btn.btn-success {
      background-color: var(--ingest-accent);
      border-color: var(--ingest-accent);
      color: #fff;
    }
    .btn.btn-primary:hover, .btn.btn-primary:focus, .btn.btn-success:hover, .btn.btn-success:focus {
      background-color: var(--ingest-accent-hover);
      border-color: var(--ingest-accent-hover);
      color: #fff;
    }
    body.theme-dark .btn.btn-primary, body.theme-dark .btn.btn-success,
    body.theme-dark .btn.btn-primary:hover, body.theme-dark .btn.btn-primary:focus,
    body.theme-dark .btn.btn-success:hover, body.theme-dark .btn.btn-success:focus {
      color: #0c2018;
    }
    .btn.btn-info {
      background-color: var(--ingest-accent-soft);
      border-color: var(--ingest-border);
      color: var(--ingest-accent-text);
    }
    .btn.btn-info:hover, .btn.btn-info:focus {
      background-color: var(--ingest-accent);
      border-color: var(--ingest-accent);
      color: #fff;
    }
    .btn.btn-warning {
      background-color: #a66a22;
      border-color: #a66a22;
      color: #fff;
    }
    .btn.btn-warning:hover, .btn.btn-warning:focus {
      background-color: #865318;
      border-color: #865318;
      color: #fff;
    }
    .btn.btn-danger {
      background-color: #d65f70;
      border-color: #d65f70;
      color: #fff;
    }
    .btn.btn-danger:hover, .btn.btn-danger:focus {
      background-color: #ed7888;
      border-color: #ed7888;
      color: #fff;
    }
    .btn.btn-danger[disabled] { background-color: #6f756f; border-color: #6f756f; opacity: .6; }
    .btn-default:not(.btn-primary):not(.btn-success):not(.btn-info):not(.btn-warning):not(.btn-danger) {
      background-color: var(--ingest-surface);
      border-color: var(--ingest-border);
      color: var(--ingest-text);
    }
    body.theme-dark .dataTables_wrapper,
    body.theme-dark table.dataTable {
      color: var(--ingest-text);
    }
    body.theme-dark table.dataTable thead th {
      background-color: var(--ingest-input-bg);
      color: var(--ingest-text);
      border-color: var(--ingest-border);
    }
    body.theme-dark table.dataTable tbody tr {
      background-color: var(--ingest-surface);
      color: var(--ingest-text);
    }
    body.theme-dark table.dataTable tbody tr:nth-child(even) {
      background-color: #1d4233;
    }
    body.theme-dark .dataTables_info,
    body.theme-dark .dataTables_length,
    body.theme-dark .dataTables_filter,
    body.theme-dark .dataTables_paginate {
      color: var(--ingest-muted) !important;
    }
    .ingest-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      min-height: 58px;
      margin: 0 -15px 13px;
      padding: 0 17px;
      color: #fff;
      background: linear-gradient(105deg, #174c32, #28744d);
      border-bottom: 1px solid rgba(255,255,255,.15);
      box-shadow: 0 2px 9px rgba(12,54,33,.24);
    }
    .ingest-header h2 {
      margin: 0;
      font-size: 19px;
      font-weight: 700;
      letter-spacing: .2px;
    }
    .ingest-brand { display: flex; align-items: center; gap: 11px; }
    .ingest-brand img { width: 42px; height: 42px; filter: drop-shadow(0 2px 3px rgba(0,0,0,.2)); }
    .ingest-brand-text { display: flex; flex-direction: column; }
    .ingest-brand-text p { margin: 1px 0 0; font-size: 10px; opacity: .82; }
    .ingest-header-actions { display: flex; align-items: center; gap: 12px; }
    .ingest-version-pill {
      padding: 3px 9px;
      border: 1px solid rgba(255,255,255,.28);
      border-radius: 999px;
      color: #fff;
      background: rgba(255,255,255,.10);
      font-size: 9px;
      font-weight: 700;
    }
    .theme-toggle-wrap {
      display: flex;
      align-items: center;
      gap: 8px;
      flex-shrink: 0;
    }
    .theme-toggle-label {
      font-size: 12px;
      color: rgba(255,255,255,.86);
      min-width: 30px;
      text-align: right;
    }
    .theme-switch {
      position: relative;
      display: inline-block;
      width: 40px;
      height: 22px;
      margin: 0;
    }
    .theme-switch input {
      opacity: 0;
      width: 0;
      height: 0;
    }
    .theme-slider {
      position: absolute;
      cursor: pointer;
      inset: 0;
      background-color: #b0b8c4;
      border-radius: 22px;
      transition: background-color 0.25s;
    }
    .theme-slider:before {
      position: absolute;
      content: '';
      height: 16px;
      width: 16px;
      left: 3px;
      bottom: 3px;
      background-color: #fff;
      border-radius: 50%;
      transition: transform 0.25s;
      box-shadow: 0 1px 2px rgba(0,0,0,0.25);
    }
    .theme-switch input:checked + .theme-slider {
      background-color: var(--ingest-accent);
    }
    .theme-switch input:checked + .theme-slider:before {
      transform: translateX(18px);
    }
    .llm-role-block {
      border: 1px solid var(--ingest-border);
      border-radius: 10px;
      padding: 7px 8px 8px;
      margin-bottom: 7px;
      background: var(--ingest-surface);
    }
    .llm-role-title {
      color: var(--ingest-muted);
      font-size: 11px;
      font-weight: 600;
      margin-bottom: 4px;
    }
    .llm-role-controls {
      display: grid;
      grid-template-columns: minmax(105px, 0.85fr) minmax(125px, 1.15fr) 34px;
      gap: 6px;
      align-items: start;
    }
    .llm-role-controls .form-group { margin-bottom: 0; }
    .llm-role-controls .selectize-control { margin-bottom: 0; }
    .llm-instruction-button {
      width: 34px;
      height: 34px;
      padding: 6px;
    }
    .modal-content {
      background: var(--ingest-surface);
      color: var(--ingest-text);
      border-color: var(--ingest-border);
    }
    .modal-header, .modal-footer { border-color: var(--ingest-border); }
    .lib-detail-panel {
      border: 1px solid var(--ingest-border);
      border-radius: 10px;
      background: var(--ingest-surface);
      padding: 12px 14px;
      min-height: 320px;
      max-height: 650px;
      overflow-y: auto;
    }
    .lib-detail-title { font-size: 18px; font-weight: 600; margin: 0 0 6px; }
    .lib-detail-id {
      font-size: 12px; color: var(--ingest-muted); font-family: Consolas, monospace;
      margin-bottom: 10px;
    }
    .lib-detail-actions { display: flex; flex-wrap: wrap; gap: 6px; margin: 9px 0 4px; }
    .lib-meta-grid {
      display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
      gap: 8px 12px; margin: 10px 0 14px; font-size: 13px;
    }
    .lib-meta-label {
      display: block; font-size: 10px; text-transform: uppercase;
      letter-spacing: 0.04em; color: var(--ingest-muted);
    }
    .lib-status-pill {
      display: inline-block; padding: 2px 8px; border-radius: 999px;
      font-size: 11px; font-weight: 600; text-transform: uppercase; color: #fff;
    }
    .lib-ctl-box {
      font-family: Consolas, Monaco, monospace; font-size: 11px; line-height: 1.35;
      background: var(--ingest-log-bg); color: var(--ingest-log-fg);
      border: 1px solid var(--ingest-border); border-radius: 4px; padding: 10px;
      max-height: 300px; overflow: auto; white-space: pre; margin: 0;
    }
    .repository-danger-zone {
      margin-top: 12px; padding: 10px; border: 1px solid rgba(214,95,112,.55);
      border-radius: 5px; background: rgba(214,95,112,.08);
    }
    .repository-danger-zone h4 { margin: 0 0 5px; color: #b94354; }
    body.theme-dark .repository-danger-zone h4 { color: #ed8b99; }
    .repository-danger-zone .help-block { margin-bottom: 8px; }
    @media (max-width: 1050px) {
      .llm-role-controls { grid-template-columns: 1fr 1fr 34px; }
    }
    .ingest-settings-group {
      margin: 0 0 10px;
      padding: 0 10px 8px;
      border: 1px solid var(--ingest-border);
      border-radius: 7px;
      background: var(--ingest-surface);
    }
    .ingest-settings-group > summary {
      margin: 0 -10px 8px;
      padding: 9px 10px;
      color: var(--ingest-text);
      background: var(--ingest-surface-2);
      border-radius: 7px;
      cursor: pointer;
      font-weight: 650;
    }
    .ingest-settings-group[open] > summary {
      border-bottom: 1px solid var(--ingest-border);
      border-radius: 7px 7px 0 0;
    }
    body button:focus-visible, body a:focus-visible, body select:focus-visible,
    body input:focus-visible, body textarea:focus-visible, body summary:focus-visible {
      outline: 2px solid var(--ingest-accent);
      outline-offset: 2px;
    }
    .ingest-workbench-row { margin: 0 -7px; }
    .ingest-workbench-row > div { padding: 0 7px; }
    @media (max-width: 900px) {
      .ingest-workbench-row > div { margin-bottom: 12px; }
      .ingest-header h2 { font-size: 17px; }
    }
  ")),
    tags$script(src = "ingest-gui.js")
  ),
  tags$div(
    class = "ingest-header",
    tags$div(
      class = "ingest-brand",
      tags$img(src = "favicon.svg", alt = "LibeRary"),
      tags$div(
        class = "ingest-brand-text",
        tags$h2("LibeRary"),
        tags$p("Literature discovery and model extraction")
      )
    ),
    tags$div(
      class = "ingest-header-actions",
      tags$span(class = "ingest-version-pill", paste0("v", package_version)),
      tags$div(
        class = "theme-toggle-wrap",
        tags$span(class = "theme-toggle-label", id = "theme_label", "Dark"),
        tags$label(
          class = "theme-switch",
          `aria-label` = "Toggle dark theme",
          tags$input(type = "checkbox", id = "theme_toggle", checked = NA),
          tags$span(class = "theme-slider")
        )
      )
    )
  ),
  fluidRow(
    column(
      4,
      wellPanel(
        h4("Settings"),
        tags$details(
          class = "ingest-settings-group", open = "open",
          tags$summary("Connection & storage"),
          textInput("entrez_email", "NCBI / Unpaywall email", value = default_entrez_email),
          passwordInput(
            "entrez_api_key",
            "NCBI API key (optional; this session only)",
            value = default_entrez_api_key,
            placeholder = "Not required at 3 requests/second or less"
          ),
          helpText("Email and non-secret settings persist automatically. The API key is read from ENTREZ_KEY or entered for this session; it is never written to the YAML file."),
          textInput("config_path", "Config YAML", value = default_config_path),
          textInput("data_dir", "Persistent data directory", value = default_data_dir)
        ),
        tags$details(
          class = "ingest-settings-group", open = "open",
          tags$summary("Language models"),
          llm_role_control(
          "triage", "Abstract triage",
          c("Same as indexing" = "same", provider_choices),
          default_cfg$llm$triage$provider %||% "same"
          ),
          llm_role_control(
          "indexing", "Investigation & synthesis", provider_choices,
          default_cfg$llm$indexing$provider %||% "ollama"
          ),
          llm_role_control(
          "vision", "PDF extraction & verification",
          c("Same as investigation" = "same", provider_choices),
          default_cfg$llm$vision$provider %||% "same"
          ),
          llm_role_control(
          "adjudication", "Discrepancy adjudication",
          c("Same as text extraction" = "same", provider_choices),
          default_cfg$llm$adjudication$provider %||% "same"
          ),
          llm_role_control(
          "assessment", "Skeptical evidence review",
          c("Same as indexing" = "same", provider_choices),
          default_cfg$llm$assessment$provider %||% "same"
          ),
          actionButton("refresh_models", "Refresh available models", class = "btn-default btn-sm"),
          actionButton("save_settings", "Save settings", class = "btn-default btn-sm"),
          span(class = "text-muted", style = "display:block;margin-top:6px;", "Settings are also saved automatically before each job.")
        ),
        tags$details(
          class = "ingest-settings-group",
          tags$summary("Runtime & extraction policy"),
          fluidRow(
          column(6, numericInput("ollama_num_ctx", "Ollama context", value = default_cfg$ollama$num_ctx,
                                 min = 4096, max = 65536, step = 4096)),
          column(6, numericInput("ollama_num_predict", "Maximum output tokens",
                                 value = default_cfg$ollama$num_predict,
                                 min = 512, max = 16384, step = 512))
        ),
        helpText("For an 8 GB GPU, 16K context is the balanced default. Larger contexts consume more VRAM and can move model layers to the CPU."),
        checkboxInput(
          "deliberative_enabled", "Deliberative evidence-led extraction",
          isTRUE(default_cfg$deliberative$enabled)
        ),
        conditionalPanel(
          "input.deliberative_enabled === true",
          checkboxInput(
            "deliberative_visual", "Verify material claims against PDF pages",
            isTRUE(default_cfg$deliberative$visual_verification)
          ),
          checkboxInput(
            "deliberative_cache", "Resume completed investigation stages",
            isTRUE(default_cfg$deliberative$cache_stages)
          ),
          fluidRow(
            column(6, numericInput(
              "deliberative_gap_rounds", "Gap-search rounds",
              value = default_cfg$deliberative$max_gap_rounds %||% 1L,
              min = 0, max = 3, step = 1
            )),
            column(6, numericInput(
              "deliberative_chunks", "Excerpts per topic",
              value = default_cfg$deliberative$max_chunks_per_stage %||% 8L,
              min = 2, max = 20, step = 1
            ))
          ),
          helpText("The investigator maps the article, searches each pharmacometric domain, challenges its own claims, searches unresolved gaps, then synthesizes the final model from an evidence ledger.")
        ),
        fluidRow(
          column(6, numericInput("triage_high", "High from", value = default_cfg$triage$high_threshold, min = 0.05, max = 1, step = 0.05)),
          column(6, numericInput("triage_intermediate", "Intermediate from", value = default_cfg$triage$intermediate_threshold, min = 0, max = 0.95, step = 0.05))
        ),
        numericInput(
          "rate_limit",
          "E-utilities requests/second",
          value = default_cfg$entrez$requests_per_second %||% 1,
          min = 0.25,
          max = 10,
          step = 0.25
        ),
        helpText("NCBI permits up to 3 requests/second without a key and up to 10 with a key. LibeRary defaults to a conservative 1 request/second."),
        checkboxInput("download_oa", "Auto-download open-access PDFs", TRUE),
        checkboxInput("independent_models", "Require different text and vision models", FALSE),
        checkboxInput("allow_remote_content", "Allow publication text to configured remote LLMs", FALSE),
        checkboxInput("use_chromote", "Chromote fallback for fetch", FALSE),
        hr(),
        p(class = "text-muted", "Provider status:"),
        textOutput("llm_status", inline = TRUE),
          tags$div(
            class = "repository-danger-zone",
            h4("Repository maintenance"),
            helpText("Permanently remove all catalog entries, PDFs, parsed documents, manifests, caches, triage records, and logs. Saved settings are retained."),
            actionButton("wipe_repository_open", "Wipe repository...", icon = icon("trash"),
                         class = "btn-danger btn-sm")
          )
        )
      )
    ),
    column(
      8,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Discover & triage",
          br(),
          helpText("PubMed query syntax. Default targets population PK/PD and NONMEM/nlmixr papers."),
          textAreaInput(
            "pubmed_query",
            "PubMed search query",
            value = ingest_default_query(),
            rows = 5,
            width = "100%"
          ),
          fluidRow(
            column(4, numericInput("discover_limit", "Max PMIDs to process", value = 20, min = 1, max = 500)),
            column(4, actionButton("count_btn", "Count PubMed hits", class = "btn-info")),
            column(4, tags$div(style = "margin-top: 25px;", textOutput("pubmed_count")))
          ),
          br(),
          actionButton("discover_btn", "Discover & triage", class = "btn-primary btn-lg"),
          hr(),
          h4("Latest triage results"),
          textOutput("triage_summary"),
          DTOutput("discover_table"),
          h4("Retained Low-probability backlog"),
          textOutput("backlog_path"),
          DTOutput("low_table")
        ),
        tabPanel(
          "Fetch PDFs",
          br(),
          helpText("Run on VPN-enabled laptop for paywalled DOIs. Uses latest discover manifest unless specified."),
          textInput("fetch_manifest", "Manifest CSV (optional)", value = ""),
          checkboxGroupInput("fetch_tiers", "Triage tiers",
                             choices = c("High" = "high", "Intermediate" = "intermediate", "Low backlog" = "low"),
                             selected = c("high", "intermediate"), inline = TRUE),
          actionButton("fetch_btn", "Run institutional fetch", class = "btn-warning btn-lg"),
          hr(),
          verbatimTextOutput("fetch_summary")
        ),
        tabPanel(
          "Process models",
          br(),
          helpText("Build a searchable Docling evidence map; investigate structure, parameters, variability, population and reproduction evidence; challenge and gap-search the findings; then compare the evidence-led synthesis with an independent PDF-vision extraction. Machine agreement is not labelled human validation."),
          textOutput("docling_status"),
          textInput("extract_manifest", "Manifest CSV (optional)", value = ""),
          numericInput("extract_limit", "Max entries to process", value = 10, min = 1, max = 200),
          checkboxGroupInput("process_tiers", "Triage tiers",
                             choices = c("High" = "high", "Intermediate" = "intermediate", "Low backlog" = "low"),
                             selected = c("high", "intermediate"), inline = TRUE),
          checkboxInput("resume_indexing", "Resume matching completed article decisions", TRUE),
          helpText("Checked: reuse only decisions made by the current extraction pipeline and prompt version. Completed investigation stages are also reusable, so interrupted articles continue from their last valid stage."),
          checkboxInput("auto_adjudicate", "Automatically adjudicate discrepancies", TRUE),
          checkboxInput("auto_reproduce", "Run executable reproduction checks when evidence is complete",
                        isTRUE(default_cfg$reproduction$auto_run)),
          conditionalPanel(
            "input.auto_reproduce === true",
            fluidRow(
              column(6, numericInput("reproduction_nsim", "Population replicates",
                                     value = default_cfg$reproduction$nsim %||% 200L,
                                     min = 1, max = 2000, step = 25)),
              column(6, numericInput("reproduction_cores", "Simulation workers",
                                     value = default_cfg$reproduction$n_cores %||% 1L,
                                     min = 1, max = 32, step = 1))
            ),
            helpText("Incomplete plans remain blocked; missing doses or generated model parameters are never silently invented.")
          ),
          actionButton("extract_btn", "Process selected tiers", class = "btn-success btn-lg"),
          hr(),
          h4("Catalog entries"),
          fluidRow(
            column(7, DTOutput("catalog_table")),
            column(5, div(class = "lib-detail-panel", uiOutput("catalog_entry_detail")))
          )
        ),
        tabPanel(
          "About",
          br(),
          h4("Default PubMed query"),
          verbatimTextOutput("default_query_display"),
          hr(),
          p("NCBI rate limit: up to 3 requests/second without an API key and up to 10 with one; LibeRary defaults to 1."),
          p("The NCBI email identifies the operator. An API key is a separate, optional My NCBI credential used for higher request rates."),
          p("Large backfills: run on weekends or weekdays 9 PM - 5 AM US Eastern."),
          p(tags$a(href = "https://www.ncbi.nlm.nih.gov/home/develop/api/", target = "_blank", "Register your tool with NCBI"))
        )
      )
    )
  ),
  hr(),
  fluidRow(
    class = "ingest-workbench-row",
    column(
      12,
      div(
        class = "progress-heading",
        h4("Job progress"),
        div(class = "progress-actions", uiOutput("job_status_ui"), uiOutput("stop_job_ui"))
      ),
      div(class = "progress-block",
        div(class = "progress-label", tags$strong("Total batch"), tags$span("Completed PMIDs")),
        div(class = "progress", style = "height: 24px; margin-bottom: 4px;",
          div(id = "prog_bar", class = "progress-bar progress-bar-striped active", role = "progressbar",
            style = "width: 0%; min-width: 2em;", "0%")
        ),
        div(class = "progress-message", textOutput("progress_message"))
      ),
      div(class = "progress-block",
        div(class = "progress-label", tags$strong("Current PMID"),
            tags$span("Stage and expected hardware")),
        div(class = "progress", style = "height: 24px; margin-bottom: 4px;",
          div(id = "current_prog_bar", class = "progress-bar progress-bar-striped", role = "progressbar",
            style = "width: 0%; min-width: 2em;", "0%")
        ),
        div(class = "progress-message", textOutput("current_progress_message"))
      ),
      h4("Live log"),
      div(id = "log_box", "Ready.")
    )
  )
)

server <- function(input, output, session) {
  job_supervisor <- reactiveVal(NULL)
  log_path <- reactiveVal("")
  progress_path <- reactiveVal("")
  last_log_lines <- reactiveVal(0L)
  discover_df <- reactiveVal(NULL)
  pubmed_count_val <- reactiveVal(NA_integer_)
  last_completion_key <- reactiveVal("")
  progress_rev <- reactiveVal(0L)
  catalog_rev <- reactiveVal(0L)
  selected_catalog_id <- reactiveVal("")
  selected_catalog_pdf_url <- reactiveVal("")
  bump_progress <- function() {
    # Keep the polling observer from becoming dependent on the value it bumps.
    # Without isolate(), every completed poll immediately invalidates itself.
    progress_rev(isolate(progress_rev()) + 1L)
  }
  triage_models <- reactiveVal(character())
  indexing_models <- reactiveVal(character())
  vision_models <- reactiveVal(character())
  assessment_models <- reactiveVal(character())
  adjudication_models <- reactiveVal(character())
  llm_status_value <- reactiveVal("Models have not been queried")
  llm_roles <- c("triage", "indexing", "vision", "adjudication", "assessment")
  llm_role_titles <- c(
    triage = "Abstract triage", indexing = "Investigation & synthesis",
    vision = "PDF extraction & verification", adjudication = "Discrepancy adjudication",
    assessment = "Skeptical evidence review"
  )
  instruction_overrides <- reactiveValues(
    triage = default_cfg$llm$triage$instruction %||% "",
    indexing = default_cfg$llm$indexing$instruction %||% "",
    vision = default_cfg$llm$vision$instruction %||% "",
    adjudication = default_cfg$llm$adjudication$instruction %||% "",
    assessment = default_cfg$llm$assessment$instruction %||% ""
  )
  active_instruction_role <- reactiveVal("")
  job_is_running <- function() {
    supervisor <- isolate(job_supervisor())
    !is.null(supervisor) && tryCatch(supervisor$is_alive(), error = function(e) FALSE)
  }

  current_cfg <- function() {
    path <- as.character(input$config_path %||% "")
    cfg <- ingest_load_config(if (nzchar(path) && file.exists(path)) path else NULL)
    cfg$entrez$email <- trimws(as.character(input$entrez_email %||% ""))
    cfg$unpaywall$email <- cfg$entrez$email
    session_key <- trimws(as.character(input$entrez_api_key %||% ""))
    if (nzchar(session_key)) cfg$entrez$api_key <- session_key
    cfg$entrez$requests_per_second <- as.numeric(input$rate_limit %||% cfg$entrez$requests_per_second)
    cfg$data_dir <- input$data_dir %||% cfg$data_dir
    cfg$inbox_dir <- file.path(cfg$data_dir, "inbox")
    cfg$cache_dir <- file.path(cfg$data_dir, "cache")
    cfg$catalog_dir <- file.path(cfg$data_dir, "catalog")
    cfg$llm$triage$provider <- input$triage_provider %||% "same"
    cfg$llm$indexing$provider <- input$indexing_provider %||% "none"
    cfg$llm$vision$provider <- input$vision_provider %||% "same"
    cfg$llm$assessment$provider <- input$assessment_provider %||% "same"
    cfg$llm$adjudication$provider <- input$adjudication_provider %||% "same"
    cfg$llm$triage$model <- input$triage_model %||% cfg$llm$triage$model
    cfg$llm$indexing$model <- input$indexing_model %||% cfg$llm$indexing$model
    cfg$llm$vision$model <- input$vision_model %||% cfg$llm$vision$model
    cfg$llm$assessment$model <- input$assessment_model %||% cfg$llm$assessment$model
    cfg$llm$adjudication$model <- input$adjudication_model %||% cfg$llm$adjudication$model
    for (role in llm_roles) cfg$llm[[role]]$instruction <- instruction_overrides[[role]] %||% ""
    cfg$triage$high_threshold <- input$triage_high %||% cfg$triage$high_threshold
    cfg$triage$intermediate_threshold <- input$triage_intermediate %||% cfg$triage$intermediate_threshold
    cfg$llm$require_independent_extraction_models <- isTRUE(input$independent_models)
    cfg$llm$allow_remote_content <- isTRUE(input$allow_remote_content)
    cfg$ollama$num_ctx <- as.integer(input$ollama_num_ctx %||% cfg$ollama$num_ctx)
    cfg$ollama$num_predict <- as.integer(input$ollama_num_predict %||% cfg$ollama$num_predict)
    cfg$deliberative$enabled <- isTRUE(input$deliberative_enabled)
    cfg$deliberative$visual_verification <- isTRUE(input$deliberative_visual)
    cfg$deliberative$cache_stages <- isTRUE(input$deliberative_cache)
    cfg$deliberative$max_gap_rounds <- as.integer(
      input$deliberative_gap_rounds %||% cfg$deliberative$max_gap_rounds
    )
    cfg$deliberative$max_chunks_per_stage <- as.integer(
      input$deliberative_chunks %||% cfg$deliberative$max_chunks_per_stage
    )
    cfg$reproduction$enabled <- TRUE
    cfg$reproduction$auto_run <- isTRUE(input$auto_reproduce)
    cfg$reproduction$nsim <- as.integer(input$reproduction_nsim %||% cfg$reproduction$nsim)
    cfg$reproduction$n_cores <- as.integer(input$reproduction_cores %||% cfg$reproduction$n_cores)
    ingest_validate_config(cfg)
  }

  persist_settings <- function(notify = FALSE) {
    cfg <- tryCatch(current_cfg(), error = identity)
    if (inherits(cfg, "error")) {
      if (notify) showNotification(conditionMessage(cfg), type = "error", duration = 10)
      return(cfg)
    }
    target <- trimws(as.character(input$config_path %||% ""))
    if (!nzchar(target)) target <- library_config_path(create = TRUE)
    saved <- tryCatch(library_save_config(cfg, target), error = identity)
    if (notify) {
      if (inherits(saved, "error")) {
        showNotification(conditionMessage(saved), type = "error", duration = 10)
      } else {
        showNotification(paste("Settings saved to", saved), type = "message")
      }
    }
    saved
  }

  refresh_llm_models <- function() {
    cfg <- current_cfg()
    model_cache <- new.env(parent = emptyenv())
    discover <- function(role) {
      endpoint <- getFromNamespace(".library_llm_role", "LibeRary")(cfg, role)
      if (identical(endpoint$provider, "none")) return(character())
      # Multimodal capability filtering differs from text-role filtering, so
      # do not reuse an indexing list for vision or adjudication dropdowns.
      key <- paste(
        endpoint$provider,
        if (role %in% c("vision", "adjudication")) "multimodal" else "text",
        sep = "|"
      )
      if (exists(key, envir = model_cache, inherits = FALSE)) return(get(key, envir = model_cache))
      value <- tryCatch({
        models <- library_llm_models(endpoint$provider, cfg, role, refresh = TRUE)
        models$id[models$usable]
      }, error = function(e) { llm_status_value(conditionMessage(e)); character() })
      assign(key, value, envir = model_cache)
      value
    }
    indexing <- discover("indexing")
    indexing_models(indexing)
    triage <- discover("triage"); triage_models(triage)
    vision <- discover("vision"); vision_models(vision)
    assessment <- discover("assessment")
    assessment_models(assessment)
    adjudication <- discover("adjudication"); adjudication_models(adjudication)
    if (length(indexing)) {
      llm_status_value(sprintf("Available models — triage %d; text %d; vision %d; adjudication %d",
                               length(triage), length(indexing), length(vision), length(adjudication)))
    } else if (identical(cfg$llm$indexing$provider, "none")) {
      llm_status_value("No parsed-text extraction model configured")
    }
  }

  observeEvent(list(input$refresh_models, input$triage_provider, input$indexing_provider,
                    input$vision_provider, input$assessment_provider, input$adjudication_provider), {
    refresh_llm_models()
  }, ignoreInit = FALSE)

  output$triage_model_ui <- renderUI({
    choices <- triage_models()
    selected <- if ((default_cfg$llm$triage$model %||% "") %in% choices) default_cfg$llm$triage$model else if (length(choices)) choices[[1L]] else ""
    selectInput("triage_model", NULL, choices = choices, selected = selected, width = "100%")
  })
  output$indexing_model_ui <- renderUI({
    choices <- indexing_models()
    selected <- if ((default_cfg$llm$indexing$model %||% "") %in% choices) default_cfg$llm$indexing$model else if (length(choices)) choices[[1L]] else ""
    selectInput("indexing_model", NULL, choices = choices, selected = selected, width = "100%")
  })
  output$vision_model_ui <- renderUI({
    choices <- vision_models()
    selected <- if ((default_cfg$llm$vision$model %||% "") %in% choices) default_cfg$llm$vision$model else if (length(choices)) choices[[1L]] else ""
    selectInput("vision_model", NULL, choices = choices, selected = selected, width = "100%")
  })
  output$assessment_model_ui <- renderUI({
    choices <- assessment_models()
    selected <- if ((default_cfg$llm$assessment$model %||% "") %in% choices) default_cfg$llm$assessment$model else if (length(choices)) choices[[1L]] else ""
    selectInput("assessment_model", NULL, choices = choices, selected = selected, width = "100%")
  })
  output$adjudication_model_ui <- renderUI({
    choices <- adjudication_models()
    selected <- if ((default_cfg$llm$adjudication$model %||% "") %in% choices) default_cfg$llm$adjudication$model else if (length(choices)) choices[[1L]] else ""
    selectInput("adjudication_model", NULL, choices = choices, selected = selected, width = "100%")
  })
  output$llm_status <- renderText(llm_status_value())

  lapply(llm_roles, function(role) {
    local({
      current_role <- role
      observeEvent(input[[paste0("edit_instruction_", current_role)]], {
        active_instruction_role(current_role)
        cfg <- current_cfg()
        effective <- getFromNamespace(".library_role_instruction", "LibeRary")(cfg, current_role)
        showModal(modalDialog(
          title = paste(llm_role_titles[[current_role]], "model instruction"),
          size = "l", easyClose = FALSE,
          textAreaInput(
            "instruction_editor", NULL, value = effective,
            rows = 18, width = "100%", resize = "vertical"
          ),
          helpText("Saving creates an upgrade-safe override in the LibeRary configuration. Restore removes the override and uses the package default."),
          footer = tagList(
            actionButton("restore_instruction", "Restore default", class = "btn-default"),
            modalButton("Cancel"),
            actionButton("save_instruction", "Save instruction", class = "btn-primary")
          )
        ))
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$save_instruction, {
    role <- active_instruction_role()
    if (!role %in% llm_roles) return()
    value <- as.character(input$instruction_editor %||% "")[[1L]]
    if (!nzchar(trimws(value))) {
      showNotification("The instruction cannot be empty. Use Restore default instead.", type = "warning")
      return()
    }
    instruction_overrides[[role]] <- value
    saved <- persist_settings(notify = FALSE)
    if (inherits(saved, "error")) {
      showNotification(conditionMessage(saved), type = "error", duration = 10)
      return()
    }
    removeModal()
    showNotification(paste(llm_role_titles[[role]], "instruction saved."), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$restore_instruction, {
    role <- active_instruction_role()
    if (!role %in% llm_roles) return()
    instruction_overrides[[role]] <- ""
    saved <- persist_settings(notify = FALSE)
    if (inherits(saved, "error")) {
      showNotification(conditionMessage(saved), type = "error", duration = 10)
      return()
    }
    default <- getFromNamespace(".library_default_instruction", "LibeRary")(role)
    updateTextAreaInput(session, "instruction_editor", value = default)
    showNotification(paste(llm_role_titles[[role]], "instruction restored to the package default."), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$save_settings, {
    persist_settings(notify = TRUE)
  })

  output$wipe_repository_footer <- renderUI({
    confirmed <- identical(as.character(input$wipe_repository_confirmation %||% ""), "YES")
    tagList(
      modalButton("Cancel"),
      actionButton(
        "wipe_repository_confirm", "Permanently wipe repository",
        class = "btn-danger", disabled = !confirmed
      )
    )
  })

  observeEvent(input$wipe_repository_open, {
    if (job_is_running()) {
      showNotification("Stop the active ingestion job before wiping the repository.",
                       type = "warning", duration = 8)
      return()
    }
    cfg <- tryCatch(current_cfg(), error = identity)
    if (inherits(cfg, "error")) {
      showNotification(conditionMessage(cfg), type = "error", duration = 10)
      return()
    }
    showModal(modalDialog(
      title = "Wipe LibeRary repository",
      size = "m", easyClose = FALSE,
      tags$div(
        class = "repository-danger-zone",
        tags$strong("This action cannot be undone."),
        tags$p("All LibeRary-managed data under the following repository will be permanently removed:"),
        tags$p(tags$code(normalizePath(cfg$data_dir, winslash = "/", mustWork = FALSE))),
        tags$p("Saved email, provider, model, and instruction settings will be retained.")
      ),
      textInput(
        "wipe_repository_confirmation", 'Type "YES" to confirm',
        value = "", placeholder = "YES", width = "100%"
      ),
      helpText("Confirmation is case-sensitive."),
      footer = uiOutput("wipe_repository_footer")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$wipe_repository_confirm, {
    confirmation <- as.character(input$wipe_repository_confirmation %||% "")
    if (!identical(confirmation, "YES")) {
      showNotification('Type "YES" exactly to wipe the repository.', type = "error")
      return()
    }
    if (job_is_running()) {
      showNotification("The repository cannot be wiped while an ingestion job is active.",
                       type = "error", duration = 8)
      return()
    }
    cfg <- tryCatch(current_cfg(), error = identity)
    result <- if (inherits(cfg, "error")) cfg else tryCatch(
      library_repository_wipe(confirmation, cfg$data_dir), error = identity
    )
    if (inherits(result, "error")) {
      showNotification(conditionMessage(result), type = "error", duration = 12)
      return()
    }
    job_supervisor(NULL)
    log_path("")
    progress_path("")
    last_log_lines(0L)
    last_completion_key("")
    discover_df(NULL)
    pubmed_count_val(NA_integer_)
    selected_catalog_id("")
    selected_catalog_pdf_url("")
    catalog_rev(isolate(catalog_rev()) + 1L)
    session$sendCustomMessage("resetLog", list())
    session$sendCustomMessage("setProgress", list(
      scope = "batch", value = 0, message = "Repository is empty", status = "idle"
    ))
    session$sendCustomMessage("setProgress", list(
      scope = "current", value = 0, message = "Waiting for a new job", status = "idle"
    ))
    removeModal()
    showNotification(
      paste("Repository wiped successfully;", result$removed, "filesystem entries removed."),
      type = "message", duration = 8
    )
  }, ignoreInit = TRUE)

  parse_pubmed_count <- function(message, log_lines = character()) {
    msg <- as.character(message %||% "")
    if (grepl("^Count:", msg)) {
      ntxt <- gsub("[^0-9]", "", sub("^Count:\\s*", "", msg))
      if (nzchar(ntxt)) {
        return(as.integer(ntxt))
      }
    }
    if (length(log_lines)) {
      hits <- grep("PubMed count:", log_lines, value = TRUE, fixed = TRUE)
      if (length(hits)) {
        ntxt <- gsub("[^0-9]", "", tail(hits, 1L))
        if (nzchar(ntxt)) {
          return(as.integer(ntxt))
        }
      }
    }
    NA_integer_
  }

  handle_job_completion <- function(prog, log_lines = character()) {
    if (is.null(prog) || !(prog$status %in% c("done", "error", "cancelled"))) {
      return(invisible(NULL))
    }
    completion_key <- paste(
      prog$status,
      prog$updated_at %||% "",
      prog$message %||% "",
      sep = "|"
    )
    if (identical(completion_key, last_completion_key())) {
      return(invisible(NULL))
    }
    last_completion_key(completion_key)

    if (prog$status %in% c("done", "cancelled")) {
      refresh_after_job()
      n <- parse_pubmed_count(prog$message, log_lines)
      if (!is.na(n)) {
        pubmed_count_val(n)
      }
    }
    invisible(NULL)
  }

  gui_paths <- reactive({
    cfg_dir <- input$data_dir
    dir.create(file.path(cfg_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
    list(
      log = file.path(cfg_dir, "logs", "gui_live.log"),
      progress = file.path(cfg_dir, "logs", "gui_progress.json")
    )
  })

  make_params <- function(job, extra = list()) {
    c(
      list(
        job = job,
        lib_path = pkg_root,
        config_path = input$config_path,
        entrez_email = input$entrez_email,
        entrez_api_key = input$entrez_api_key,
        data_dir = input$data_dir,
        source_root = is_source_root,
        triage_provider = input$triage_provider,
        triage_model = input$triage_model,
        indexing_provider = input$indexing_provider,
        indexing_model = input$indexing_model,
        vision_provider = input$vision_provider,
        vision_model = input$vision_model,
        assessment_provider = input$assessment_provider,
        assessment_model = input$assessment_model,
        adjudication_provider = input$adjudication_provider,
        adjudication_model = input$adjudication_model,
        triage_high_threshold = input$triage_high,
        triage_intermediate_threshold = input$triage_intermediate,
        require_independent_extraction_models = isTRUE(input$independent_models),
        allow_remote_content = isTRUE(input$allow_remote_content),
        deliberative_enabled = isTRUE(input$deliberative_enabled),
        deliberative_visual_verification = isTRUE(input$deliberative_visual),
        deliberative_cache_stages = isTRUE(input$deliberative_cache),
        deliberative_max_gap_rounds = input$deliberative_gap_rounds,
        deliberative_max_chunks_per_stage = input$deliberative_chunks,
        log_path = gui_paths()$log,
        progress_path = gui_paths()$progress
      ),
      extra
    )
  }

  flush_job_output <- function(paths) {
    if (file.exists(paths$log)) {
      log_lines <- readLines(paths$log, warn = FALSE)
      n <- length(log_lines)
      prev <- last_log_lines()
      if (n > prev) {
        session$sendCustomMessage("appendLog", paste(log_lines[(prev + 1L):n], collapse = "\n"))
        last_log_lines(n)
      }
      log_lines
    } else {
      character()
    }
  }

  send_progress_state <- function(prog) {
    if (is.null(prog)) return(invisible(NULL))
    batch <- prog$batch %||% prog
    session$sendCustomMessage("setProgress", list(
      scope = "batch",
      value = round(100 * as.numeric(batch$value %||% 0)),
      message = batch$message %||% "",
      status = batch$status %||% prog$status %||% "running"
    ))
    current <- prog$current %||% NULL
    if (!is.null(current)) {
      session$sendCustomMessage("setProgress", list(
        scope = "current",
        value = round(100 * as.numeric(current$value %||% 0)),
        message = current$message %||% "",
        status = current$status %||% "idle"
      ))
    }
    invisible(NULL)
  }

  sync_job_ui <- function(paths) {
    prog <- ingest_read_progress(paths$progress)
    log_lines <- flush_job_output(paths)
    send_progress_state(prog)
    bump_progress()
    handle_job_completion(prog, log_lines)
    invisible(prog)
  }

  finalize_background_worker <- function(paths, prog, log_lines = character()) {
    sup <- job_supervisor()
    if (is.null(sup) || sup$is_alive()) return(invisible(FALSE))

    child_lines <- tryCatch(
      c(sup$read_all_output_lines(), sup$read_all_error_lines()),
      error = function(e) character()
    )
    child_lines <- child_lines[nzchar(trimws(child_lines))]
    missing_pdf_font <- grepl(
      "^PDF error: No display font for '(Symbol|ArialUnicode)'$",
      trimws(child_lines)
    )
    if (any(missing_pdf_font)) {
      cat(
        sprintf(
          "[%s] WARN: PDF referenced an unavailable display font; pages were rendered, but affected symbols may require review.\n",
          format(Sys.time(), "%H:%M:%S")
        ),
        file = paths$log, append = TRUE
      )
      child_lines <- child_lines[!missing_pdf_font]
    }
    if (length(child_lines)) {
      cat(paste0("[background] ", child_lines, "\n"), file = paths$log, append = TRUE)
    }

    current <- ingest_read_progress(paths$progress)
    if (!is.null(current) && identical(current$status, "cancelled")) {
      job_supervisor(NULL)
      log_lines <- flush_job_output(paths)
      handle_job_completion(current, log_lines)
      bump_progress()
      return(invisible(TRUE))
    }
    result <- tryCatch(sup$get_result(), error = identity)
    terminal <- !is.null(current) && current$status %in% c("done", "error", "cancelled")
    if (inherits(result, "error")) {
      message <- paste("Background worker failed:", conditionMessage(result))
      if (!terminal) ingest_write_progress(paths$progress, 0, message, status = "error")
      cat(sprintf("[%s] ERROR: %s\n", format(Sys.time(), "%H:%M:%S"), message),
          file = paths$log, append = TRUE)
      showNotification(message, type = "error", duration = 12)
    } else if (is.list(result) && identical(result$ok, FALSE)) {
      message <- as.character(result$error %||% "The background job failed.")
      if (!terminal) ingest_write_progress(paths$progress, 0, message, status = "error")
      showNotification(message, type = "error", duration = 12)
    } else if (!terminal) {
      ingest_write_progress(paths$progress, 1, "Complete", status = "done")
    }

    job_supervisor(NULL)
    refreshed <- ingest_read_progress(paths$progress)
    log_lines <- flush_job_output(paths)
    handle_job_completion(refreshed, log_lines)
    bump_progress()
    invisible(TRUE)
  }

  start_job <- function(job, extra = list()) {
    if (job %in% c("count", "discover", "fetch")) {
      email <- trimws(as.character(input$entrez_email %||% ""))
      if (!nzchar(email) || !grepl("^[^[:space:]@]+@[^[:space:]@]+[.][^[:space:]@]+$", email)) {
        showNotification("Enter a valid NCBI email address in Settings.", type = "error", duration = 10)
        return(invisible(NULL))
      }
      rate <- as.numeric(input$rate_limit %||% 1)
      key <- trimws(as.character(input$entrez_api_key %||% ""))
      if (!nzchar(key) && rate > 3) {
        showNotification("An NCBI API key is required above 3 requests/second.", type = "error", duration = 10)
        return(invisible(NULL))
      }
    }
    saved <- persist_settings(notify = FALSE)
    if (inherits(saved, "error")) {
      showNotification(paste("Unable to save settings:", conditionMessage(saved)), type = "error", duration = 10)
      return(invisible(NULL))
    }
    sup <- job_supervisor()
    if (!is.null(sup) && sup$is_alive()) {
      showNotification("A job is already running.", type = "warning")
      return(invisible(NULL))
    }
    paths <- gui_paths()
    cat(
      sprintf("[%s] INFO: --- job %s started ---\n", format(Sys.time(), "%H:%M:%S"), job),
      file = paths$log
    )
    ingest_write_progress(paths$progress, 0, "Starting...", status = "running")
    session$sendCustomMessage("setProgress", list(scope = "batch", value = 2,
                                                    message = "Starting...", status = "running"))
    session$sendCustomMessage("setProgress", list(scope = "current", value = 0,
                                                    message = "Waiting for the first PMID", status = "idle"))
    bump_progress()
    log_path(paths$log)
    progress_path(paths$progress)
    last_log_lines(0L)
    last_completion_key("")
    session$sendCustomMessage("resetLog", list())
    params <- c(make_params(job, extra), list(skip_package_load = TRUE))

    # Count is one quick NCBI call — run in-process (callr subprocess often stalls on load_all).
    if (identical(job, "count")) {
      tryCatch(
        LibeRary::ingest_gui_worker(params),
        error = function(e) {
          ingest_write_progress(paths$progress, 0, conditionMessage(e), status = "error")
          cat(
            sprintf("[%s] ERROR: %s\n", format(Sys.time(), "%H:%M:%S"), conditionMessage(e)),
            file = paths$log,
            append = TRUE
          )
          bump_progress()
          showNotification(conditionMessage(e), type = "error", duration = 10)
        }
      )
      sync_job_ui(paths)
      return(invisible(NULL))
    }

    if (requireNamespace("callr", quietly = TRUE)) {
      worker <- getFromNamespace("ingest_gui_background_worker", "LibeRary")
      bg <- tryCatch(
        callr::r_bg(
          func = worker,
          args = list(params = params),
          supervise = TRUE,
          stdout = "|",
          stderr = "|"
        ),
        error = identity
      )
      if (inherits(bg, "error")) {
        message <- paste("Unable to start background worker:", conditionMessage(bg))
        ingest_write_progress(paths$progress, 0, message, status = "error")
        cat(sprintf("[%s] ERROR: %s\n", format(Sys.time(), "%H:%M:%S"), message),
            file = paths$log, append = TRUE)
        bump_progress()
        showNotification(message, type = "error", duration = 12)
        sync_job_ui(paths)
        return(invisible(NULL))
      }
      job_supervisor(bg)
    } else {
      showNotification("callr not installed — running synchronously (UI may freeze).", type = "warning", duration = 8)
      LibeRary::ingest_gui_worker(params)
      job_supervisor(NULL)
      sync_job_ui(paths)
    }
    invisible(NULL)
  }

  refresh_after_job <- function() {
    cfg <- tryCatch(ingest_load_config(input$config_path), error = function(e) NULL)
    if (!is.null(cfg)) {
      cfg$data_dir <- input$data_dir
      cfg$catalog_dir <- file.path(input$data_dir, "catalog")
      man <- ingest_latest_manifest(cfg)
      if (nzchar(man) && file.exists(man)) {
        discover_df(ingest_read_manifest(man))
      }
    }
    catalog_rev(isolate(catalog_rev()) + 1L)
    output$fetch_summary <- renderPrint({
      prog <- ingest_read_progress(gui_paths()$progress)
      if (!is.null(prog)) prog$message else "Done."
    })
  }

  observeEvent(input$count_btn, {
    start_job("count", list(query = input$pubmed_query))
  })

  observeEvent(input$discover_btn, {
    start_job("discover", list(
      query = input$pubmed_query,
      limit = input$discover_limit,
      download_oa = input$download_oa
    ))
  })

  observeEvent(input$fetch_btn, {
    if (!length(input$fetch_tiers)) {
      showNotification("Select at least one triage tier.", type = "warning")
      return()
    }
    start_job("fetch", list(
      manifest = input$fetch_manifest,
      fetch_classes = c("oa_auto", "needs_institutional", "deferred_low"),
      tiers = input$fetch_tiers,
      use_chromote_fallback = input$use_chromote
    ))
  })

  observeEvent(input$extract_btn, {
    if (!length(input$process_tiers)) {
      showNotification("Select at least one triage tier.", type = "warning")
      return()
    }
    start_job("process", list(
      manifest = input$extract_manifest,
      limit = input$extract_limit,
      tiers = input$process_tiers,
      resume = input$resume_indexing,
      adjudicate = input$auto_adjudicate
    ))
  })

  observeEvent(input$stop_job_btn, {
    sup <- isolate(job_supervisor())
    paths <- isolate(gui_paths())
    cancel <- getFromNamespace(".ingest_gui_cancel_worker", "LibeRary")(
      sup, paths$progress, paths$log, "Cancelled by user"
    )
    if (!isTRUE(cancel$cancelled)) {
      showNotification(cancel$message, type = "warning", duration = 8)
      return()
    }
    job_supervisor(NULL)
    refreshed <- ingest_read_progress(paths$progress)
    send_progress_state(refreshed)
    log_lines <- flush_job_output(paths)
    handle_job_completion(refreshed, log_lines)
    bump_progress()
    showNotification("The current ingestion job was stopped.", type = "message", duration = 5)
  }, ignoreInit = TRUE)

  poll <- reactiveTimer(400)
  observe({
    poll()
    path <- log_path()
    prog_path <- progress_path()
    if (!nzchar(prog_path) && !nzchar(path)) {
      return()
    }
    log_lines <- character()
    if (nzchar(path) && file.exists(path)) {
      log_lines <- readLines(path, warn = FALSE)
      n <- length(log_lines)
      prev <- last_log_lines()
      if (n > prev) {
        session$sendCustomMessage("appendLog", paste(log_lines[(prev + 1L):n], collapse = "\n"))
        last_log_lines(n)
      }
    }
    prog <- if (nzchar(prog_path)) ingest_read_progress(prog_path) else NULL
    if (!is.null(prog)) {
      send_progress_state(prog)
      bump_progress()
      sup <- job_supervisor()
      if (!is.null(sup) && !sup$is_alive()) {
        finalize_background_worker(list(log = path, progress = prog_path), prog, log_lines)
      } else if (prog$status %in% c("done", "error", "cancelled") && is.null(sup)) {
        handle_job_completion(prog, log_lines)
      }
    }
  })

  output$progress_message <- renderText({
    poll()
    prog <- ingest_read_progress(gui_paths()$progress)
    if (is.null(prog)) return("")
    (prog$batch %||% prog)$message %||% ""
  })

  output$current_progress_message <- renderText({
    poll()
    prog <- ingest_read_progress(gui_paths()$progress)
    if (is.null(prog$current)) return("Waiting for an article stage")
    prog$current$message %||% ""
  })

  output$pubmed_count <- renderText({
    poll()
    n <- pubmed_count_val()
    if (!is.na(n)) {
      return(paste0(format(n, big.mark = ","), " total hits"))
    }
    prog <- ingest_read_progress(gui_paths()$progress)
    if (!is.null(prog) && identical(prog$status, "running") && grepl("^Count:", prog$message %||% "")) {
      return(sub("^Count:\\s*", "", prog$message))
    }
    if (!is.null(prog) && identical(prog$status, "running") && nzchar(prog$message %||% "")) {
      return(prog$message)
    }
    "Click 'Count PubMed hits' to query NCBI"
  })

  output$default_query_display <- renderText({
    ingest_default_query()
  })

  output$job_status_ui <- renderUI({
    progress_rev()
    prog <- ingest_read_progress(gui_paths()$progress)
    st <- prog$status %||% "idle"
    col <- switch(st, running = "#65d39b", done = "#65d39b", cancelled = "#e4b45f",
                  error = "#e27483", "#8fa89b")
    tags$span(class = "status-badge", style = paste0("color:", col, ";"), toupper(st))
  })

  output$stop_job_ui <- renderUI({
    progress_rev()
    sup <- job_supervisor()
    running <- !is.null(sup) && tryCatch(sup$is_alive(), error = function(e) FALSE)
    actionButton(
      "stop_job_btn", "Stop current job", icon = icon("stop"),
      class = "btn-danger btn-sm", disabled = !running,
      title = if (running) "Terminate the active ingestion worker and its child processes" else
        "No cancellable background job is running in this GUI session"
    )
  })

  output$discover_table <- renderDT({
    df <- discover_df()
    if (is.null(df) || !nrow(df)) return(datatable(data.frame(message = "No results yet — run Discover.")))
    show_cols <- intersect(
      c("pmid", "triage_tier", "triage_probability", "recoverability_probability",
        "acquisition_class", "status", "title"),
      names(df)
    )
    datatable(df[, show_cols, drop = FALSE], options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$triage_summary <- renderText({
    df <- discover_df()
    if (is.null(df) || !nrow(df) || !"triage_tier" %in% names(df)) return("")
    counts <- table(factor(tolower(df$triage_tier), levels = c("high", "intermediate", "low")))
    sprintf("High: %d | Intermediate: %d | Low retained for later: %d",
            counts[["high"]], counts[["intermediate"]], counts[["low"]])
  })

  output$low_table <- renderDT({
    df <- discover_df()
    if (is.null(df) || !nrow(df) || !"triage_tier" %in% names(df)) {
      return(datatable(data.frame(message = "No Low-probability backlog yet.")))
    }
    low <- df[tolower(df$triage_tier) == "low", , drop = FALSE]
    if (!nrow(low)) return(datatable(data.frame(message = "No Low-probability articles in this search.")))
    columns <- intersect(c("pmid", "triage_probability", "recoverability_probability", "title"), names(low))
    datatable(low[, columns, drop = FALSE], options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })

  output$backlog_path <- renderText({
    directory <- file.path(input$data_dir, "triage")
    files <- if (dir.exists(directory)) list.files(directory, "^deferred_low_.*[.]csv$", full.names = TRUE) else character()
    if (!length(files)) return("The backlog is written after discovery.")
    paste("Saved:", normalizePath(files[[which.max(file.info(files)$mtime)]], winslash = "/", mustWork = TRUE))
  })

  output$docling_status <- renderText({
    status <- tryCatch(ingest_docling_available(current_cfg()), error = identity)
    if (inherits(status, "error")) return(paste("Docling status:", conditionMessage(status)))
    if (isTRUE(status$available)) paste("Docling standard parser:", status$version %||% status$executable) else
      "Docling is unavailable; the pdftools fallback will be recorded explicitly."
  })

  catalog_entries <- reactive({
    catalog_rev()
    root <- file.path(input$data_dir, "catalog")
    if (!dir.exists(root) || !file.exists(file.path(root, "index.json"))) {
      return(data.frame())
    }
    tryCatch(LibeRary::library_list(root = root), error = function(e) data.frame())
  })

  output$catalog_table <- renderDT({
    df <- catalog_entries()
    if (!nrow(df)) return(datatable(data.frame(message = "No catalog yet."), selection = "none"))
    show <- intersect(c("library_id", "title", "status", "compound", "advan", "confidence_overall"), names(df))
    datatable(df[, show, drop = FALSE], selection = "single",
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$catalog_table_rows_selected, {
    selected <- input$catalog_table_rows_selected
    df <- catalog_entries()
    if (length(selected) != 1L || selected > nrow(df)) {
      selected_catalog_id("")
      return()
    }
    selected_catalog_id(df$library_id[[selected]])
  })

  observeEvent(selected_catalog_id(), {
    id <- selected_catalog_id()
    if (!nzchar(id)) {
      selected_catalog_pdf_url("")
      return()
    }
    root <- file.path(input$data_dir, "catalog")
    selected_catalog_pdf_url(tryCatch(
      getFromNamespace(".library_register_pdf", "LibeRary")(session, id, root),
      error = function(e) ""
    ))
  }, ignoreInit = FALSE)

  output$catalog_entry_detail <- renderUI({
    id <- selected_catalog_id()
    if (!nzchar(id)) {
      return(tags$p(class = "text-muted", "Select a model to view its article and model details."))
    }
    root <- file.path(input$data_dir, "catalog")
    entry <- tryCatch(LibeRary::library_get(id, root), error = identity)
    if (inherits(entry, "error")) {
      return(tags$p(class = "text-muted", conditionMessage(entry)))
    }
    getFromNamespace(".library_entry_detail_ui", "LibeRary")(
      entry, root, pdf_url = selected_catalog_pdf_url()
    )
  })

  # Initial catalog/manifest load
  observe({
    refresh_after_job()
  })
}

shinyApp(ui, server)
