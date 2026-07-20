# LibeRary catalog browser — run via library_shiny()

library(shiny)
library(DT)

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

.library_valid_pkg_root <- function(path) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  desc_path <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(FALSE)
  }
  desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  !is.null(desc) &&
    "Package" %in% colnames(desc) &&
    identical(as.character(desc[1, "Package"]), "LibeRary")
}

pkg_root <- Sys.getenv("LIBERARY_PKG_ROOT", "")
if (!nzchar(pkg_root) || !.library_valid_pkg_root(pkg_root)) {
  pkg_root <- ""
  sf <- system.file("", package = "LibeRary")
  if (nzchar(sf)) {
    cand <- normalizePath(sf, winslash = "/", mustWork = FALSE)
    if (.library_valid_pkg_root(cand)) {
      pkg_root <- cand
    }
  }
}
if (!nzchar(pkg_root)) {
  cand <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (.library_valid_pkg_root(cand)) {
    pkg_root <- cand
  }
}
is_source_root <- nzchar(pkg_root) && length(list.files(file.path(pkg_root, "R"), pattern = "[.]R$")) > 0L
if (is_source_root && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg_root, quiet = TRUE)
} else {
  library(LibeRary)
}

default_catalog <- tryCatch(
  LibeRary::library_catalog_root(),
  error = function(e) ""
)

default_workspace <- if (requireNamespace("LibeRation", quietly = TRUE)) {
  tryCatch(LibeRation::nm_workspace()$path, error = function(e) "")
} else {
  ""
}

status_filter_choices <- c(
  "Any" = "",
  "discovered" = "discovered",
  "stub" = "stub",
  "draft" = "draft",
  "review" = "review",
  "validated" = "validated",
  "mbma_source" = "mbma_source",
  "deprecated" = "deprecated"
)

.library_status_color <- function(status) {
  switch(
    status,
    validated = "#198754",
    review = "#39875f",
    draft = "#fd7e14",
    stub = "#6c757d",
    discovered = "#6c757d",
    deprecated = "#dc3545",
    mbma_source = "#6610f2",
    "#495057"
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("LibeRary"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$script(HTML("
      (function() {
        function boot() {
          try {
            if (localStorage.getItem('libeRaryDarkTheme') !== '0') {
              document.body.classList.add('theme-dark');
            }
          } catch (e) {
            document.body.classList.add('theme-dark');
          }
        }
        if (document.body) boot();
        else document.addEventListener('DOMContentLoaded', boot);
      })();
    ")),
    tags$style(HTML("
    :root {
      --lib-bg: #f4f8f5;
      --lib-surface: #ffffff;
      --lib-text: #1e2b23;
      --lib-muted: #63746a;
      --lib-border: #d5e3da;
      --lib-input-bg: #ffffff;
      --lib-code-bg: #edf5f0;
      --lib-code-fg: #1e2b23;
      --lib-accent: #236a45;
      --lib-accent-hover: #184e34;
    }
    body.theme-dark {
      --lib-bg: #10271e;
      --lib-surface: #18382b;
      --lib-text: #f1faf5;
      --lib-muted: #b7d0c3;
      --lib-border: #3b6f58;
      --lib-input-bg: #24513f;
      --lib-code-bg: #0c2018;
      --lib-code-fg: #e4f3eb;
      --lib-accent: #65d39b;
      --lib-accent-hover: #86e2b0;
    }
    body {
      background-color: var(--lib-bg);
      color: var(--lib-text);
      transition: background-color 0.2s, color 0.2s;
    }
    .well, .tab-content, .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
      background-color: var(--lib-surface) !important;
      color: var(--lib-text) !important;
      border-color: var(--lib-border) !important;
    }
    .nav-tabs > li > a { color: var(--lib-muted); }
    .nav-tabs > li.active > a { border-top: 3px solid var(--lib-accent) !important; }
    .form-control, textarea.form-control, .selectize-input, pre {
      background-color: var(--lib-input-bg);
      color: var(--lib-text);
      border-color: var(--lib-border);
    }
    .selectize-input input { color: var(--lib-text); }
    .selectize-input {
      background: var(--lib-input-bg) !important;
      color: var(--lib-text) !important;
    }
    .selectize-dropdown, .selectize-dropdown-content {
      background: var(--lib-input-bg) !important;
      color: var(--lib-text) !important;
      border-color: var(--lib-border);
    }
    .selectize-dropdown .active { background: var(--lib-code-bg); color: var(--lib-text); }
    .form-control:focus {
      border-color: var(--lib-accent);
      box-shadow: 0 0 0 2px rgba(101, 211, 155, 0.25);
    }
    .btn.btn-primary, .btn.btn-success {
      background-color: var(--lib-accent);
      border-color: var(--lib-accent);
      color: #fff;
    }
    .btn.btn-primary:hover, .btn.btn-primary:focus, .btn.btn-success:hover, .btn.btn-success:focus {
      background-color: var(--lib-accent-hover);
      border-color: var(--lib-accent-hover);
      color: #fff;
    }
    body.theme-dark .btn.btn-primary, body.theme-dark .btn.btn-success,
    body.theme-dark .btn.btn-primary:hover, body.theme-dark .btn.btn-primary:focus,
    body.theme-dark .btn.btn-success:hover, body.theme-dark .btn.btn-success:focus {
      color: #0c2018;
    }
    .btn-default:not(.btn-primary):not(.btn-success) {
      background-color: var(--lib-surface);
      border-color: var(--lib-border);
      color: var(--lib-text);
    }
    .help-block, .text-muted { color: var(--lib-muted) !important; }
    hr { border-color: var(--lib-border); }
    .lib-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin: 0 0 16px 0;
      padding-bottom: 8px;
      border-bottom: 1px solid var(--lib-border);
    }
    .lib-header h2 { margin: 0; font-size: 26px; font-weight: 600; }
    .lib-brand { display: flex; align-items: center; gap: 10px; }
    .lib-brand img { width: 34px; height: 34px; }
    .lib-header-meta {
      font-size: 11px;
      color: var(--lib-muted);
      max-width: 45%;
      text-align: right;
      word-break: break-all;
    }
    .theme-toggle-wrap {
      display: flex;
      align-items: center;
      gap: 8px;
      flex-shrink: 0;
    }
    .theme-toggle-label {
      font-size: 12px;
      color: var(--lib-muted);
      min-width: 32px;
      text-align: right;
    }
    .theme-switch {
      position: relative;
      display: inline-block;
      width: 40px;
      height: 22px;
      margin: 0;
    }
    .theme-switch input { opacity: 0; width: 0; height: 0; }
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
    .theme-switch input:checked + .theme-slider { background-color: var(--lib-accent); }
    .theme-switch input:checked + .theme-slider:before { transform: translateX(18px); }
    .lib-detail-panel {
      border: 1px solid var(--lib-border);
      border-radius: 4px;
      background: var(--lib-surface);
      padding: 12px 14px;
      min-height: 320px;
      max-height: calc(100vh - 220px);
      overflow-y: auto;
    }
    .lib-detail-title {
      font-size: 18px;
      font-weight: 600;
      margin: 0 0 6px;
    }
    .lib-detail-id {
      font-size: 12px;
      color: var(--lib-muted);
      font-family: Consolas, monospace;
      margin-bottom: 10px;
    }
    .lib-detail-actions {
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
      margin: 9px 0 4px;
    }
    .lib-meta-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
      gap: 8px 12px;
      margin: 10px 0 14px;
      font-size: 13px;
    }
    .lib-meta-label {
      display: block;
      font-size: 10px;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: var(--lib-muted);
    }
    .lib-status-pill {
      display: inline-block;
      padding: 2px 8px;
      border-radius: 999px;
      font-size: 11px;
      font-weight: 600;
      text-transform: uppercase;
      color: #fff;
    }
    .lib-ctl-box {
      font-family: Consolas, Monaco, monospace;
      font-size: 11px;
      line-height: 1.35;
      background: var(--lib-code-bg);
      color: var(--lib-code-fg);
      border: 1px solid var(--lib-border);
      border-radius: 4px;
      padding: 10px;
      max-height: 360px;
      overflow: auto;
      white-space: pre;
      margin: 0;
    }
    .lib-count-badge {
      font-size: 13px;
      color: var(--lib-muted);
      margin-bottom: 8px;
    }
    body.theme-dark table.dataTable thead th {
      background-color: var(--lib-input-bg);
      color: var(--lib-text);
      border-color: var(--lib-border);
    }
    body.theme-dark table.dataTable tbody tr {
      background-color: var(--lib-surface);
      color: var(--lib-text);
    }
    body.theme-dark table.dataTable tbody tr:nth-child(even) {
      background-color: #1d4233;
    }
    body.theme-dark .dataTables_wrapper,
    body.theme-dark .dataTables_info,
    body.theme-dark .dataTables_length,
    body.theme-dark .dataTables_filter,
    body.theme-dark .dataTables_paginate {
      color: var(--lib-muted) !important;
    }
    ")),
    tags$script(src = "library-gui.js")
  ),
  tags$div(
    class = "lib-header",
    tags$div(
      class = "lib-brand",
      tags$img(src = "favicon.svg", alt = "LibeR"),
      tags$div(
        tags$h2("LibeRary"),
        tags$div(class = "lib-header-meta", textOutput("catalog_path_display", inline = TRUE))
      )
    ),
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
  ),
  fluidRow(
    column(
      3L,
      wellPanel(
        h4("Catalog"),
        textInput("catalog_root", "Catalog directory", value = default_catalog, width = "100%"),
        actionButton("reload_catalog", "Reload catalog", class = "btn-primary btn-block"),
        hr(),
        h4("Search & filter"),
        textInput("search_query", "Keywords", value = "", width = "100%"),
        selectInput(
          "filter_status",
          "Status",
          choices = status_filter_choices,
          selected = "",
          width = "100%"
        ),
        textInput("filter_compound", "Compound contains", value = "", width = "100%"),
        numericInput("filter_min_conf", "Min confidence", value = NA, min = 0, max = 1, step = 0.05, width = "100%"),
        actionButton("apply_filters", "Apply filters", class = "btn-default btn-block"),
        hr(),
        h4("LibeRation"),
        textInput("workspace_root", "Workspace root", value = default_workspace, width = "100%"),
        textInput("import_project", "Project name (optional)", value = "", width = "100%"),
        helpText("Leave blank to create lib_<entry_id>."),
        actionButton("import_workspace", "Import to workspace", class = "btn-success btn-block"),
        verbatimTextOutput("import_result")
      )
    ),
    column(
      5L,
      div(class = "lib-count-badge", textOutput("entry_count")),
      DTOutput("catalog_table")
    ),
    column(
      4L,
      div(
        class = "lib-detail-panel",
        uiOutput("entry_detail")
      )
    )
  )
)

server <- function(input, output, session) {
  catalog_root <- reactiveVal(default_catalog)
  filter_rev <- reactiveVal(0L)
  selected_id <- reactiveVal("")
  selected_pdf_url <- reactiveVal("")

  observeEvent(input$reload_catalog, {
    path <- trimws(input$catalog_root)
    if (!nzchar(path) || !dir.exists(path)) {
      showNotification("Catalog directory not found.", type = "error")
      return()
    }
    catalog_root(normalizePath(path, winslash = "/", mustWork = FALSE))
    options(LibeRary.catalog = catalog_root())
    filter_rev(filter_rev() + 1L)
    selected_id("")
    showNotification("Catalog reloaded.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_filters, {
    filter_rev(filter_rev() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$search_query, {
    filter_rev(filter_rev() + 1L)
  }, ignoreInit = FALSE)

  catalog_df <- reactive({
    filter_rev()
    root <- catalog_root()
    if (!nzchar(root) || !dir.exists(root)) {
      return(data.frame(message = "Set a valid catalog directory and click Reload catalog."))
    }
    query <- trimws(input$search_query %||% "")
    status <- input$filter_status %||% ""
    compound <- trimws(input$filter_compound %||% "")
    min_conf <- input$filter_min_conf

    df <- if (nzchar(query) || nzchar(compound) || (nzchar(status) && status != "") ||
        (length(min_conf) == 1L && !is.na(min_conf))) {
      LibeRary::library_search(
        query = query,
        compound = if (nzchar(compound)) compound else NULL,
        status = if (nzchar(status)) status else NULL,
        min_confidence = if (length(min_conf) == 1L && !is.na(min_conf)) min_conf else NULL,
        root = root
      )
    } else {
      LibeRary::library_list(root = root)
    }

    if (nrow(df) && "confidence_overall" %in% names(df)) {
      df$confidence_overall <- round(df$confidence_overall, 2)
    }
    df
  })

  output$catalog_path_display <- renderText({
    catalog_root()
  })

  output$entry_count <- renderText({
    df <- catalog_df()
    if ("message" %in% names(df)) {
      return(df$message[[1L]])
    }
    n <- nrow(df)
    paste0(format(n, big.mark = ","), " model", if (n != 1L) "s" else "", " in catalog")
  })

  output$catalog_table <- renderDT({
    df <- catalog_df()
    if (!nrow(df) || "message" %in% names(df)) {
      return(datatable(
        if ("message" %in% names(df)) df else data.frame(message = "No entries match the current filters."),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE,
        selection = "none"
      ))
    }
    show <- intersect(
      c("library_id", "title", "status", "compound", "advan", "reproduction", "confidence_overall"),
      names(df)
    )
    datatable(
      df[, show, drop = FALSE],
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(1, "asc"))
      )
    )
  })

  observeEvent(input$catalog_table_rows_selected, {
    sel <- input$catalog_table_rows_selected
    df <- catalog_df()
    if (length(sel) != 1L || !nrow(df)) {
      selected_id("")
      return()
    }
    selected_id(df$library_id[[sel]])
  })

  observeEvent(selected_id(), {
    id <- selected_id()
    if (!nzchar(id)) {
      selected_pdf_url("")
      return()
    }
    selected_pdf_url(tryCatch(
      getFromNamespace(".library_register_pdf", "LibeRary")(session, id, catalog_root()),
      error = function(e) ""
    ))
  }, ignoreInit = FALSE)

  output$entry_detail <- renderUI({
    id <- selected_id()
    if (!nzchar(id)) {
      return(tags$p(class = "text-muted", "Select a model from the table to view details."))
    }
    root <- catalog_root()
    entry <- tryCatch(
      LibeRary::library_get(id, root),
      error = function(e) {
        return(list(error = conditionMessage(e)))
      }
    )
    if (!is.null(entry$error)) {
      return(tags$p(class = "text-muted", entry$error))
    }
    return(getFromNamespace(".library_entry_detail_ui", "LibeRary")(
      entry, root, pdf_url = selected_pdf_url()
    ))
    m <- entry$manifest
    st <- m$status %||% ""
    st_col <- .library_status_color(st)
    keywords <- m$study$keywords %||% character()
    kw_txt <- if (length(keywords)) paste(keywords, collapse = ", ") else "—"
    abstract <- m$study$abstract %||% ""
    prov <- m$provenance %||% list()
    prov_lines <- if (length(prov)) {
      tags$ul(
        class = "lib-prov-list",
        lapply(names(prov), function(nm) {
          val <- prov[[nm]]
          if (is.list(val) || length(val) > 1L) {
            val <- paste(as.character(unlist(val)), collapse = "; ")
          }
          tags$li(tags$strong(paste0(nm, ": ")), as.character(val))
        })
      )
    } else {
      tags$p(class = "text-muted", "No provenance recorded.")
    }

    tagList(
      tags$div(class = "lib-detail-title", m$title %||% id),
      tags$div(class = "lib-detail-id", id),
      tags$span(
        class = "lib-status-pill",
        style = paste0("background:", st_col, ";"),
        st
      ),
      tags$div(
        class = "lib-meta-grid",
        tags$div(tags$span(class = "lib-meta-label", "Compound"), m$study$compound %||% "—"),
        tags$div(tags$span(class = "lib-meta-label", "Population"), m$study$population %||% "—"),
        tags$div(tags$span(class = "lib-meta-label", "Route"), m$study$route %||% "—"),
        tags$div(
          tags$span(class = "lib-meta-label", "ADVAN"),
          if (!is.null(m$model$advan)) paste0("ADVAN ", m$model$advan) else "—"
        ),
        tags$div(
          tags$span(class = "lib-meta-label", "Confidence"),
          if (!is.null(m$confidence$overall)) round(as.numeric(m$confidence$overall), 2) else "—"
        ),
        tags$div(tags$span(class = "lib-meta-label", "Keywords"), kw_txt)
      ),
      if (nzchar(abstract)) {
        tagList(tags$h5("Abstract"), tags$p(style = "font-size: 13px;", abstract))
      },
      tags$h5("Provenance"),
      prov_lines,
      tags$h5("Model (model.ctl)"),
      tags$pre(
        class = "lib-ctl-box",
        paste(
          tryCatch(
            LibeRary::library_model(id, root),
            error = function(e) paste("#", conditionMessage(e))
          ),
          collapse = "\n"
        )
      )
    )
  })

  observeEvent(input$import_workspace, {
    id <- selected_id()
    if (!nzchar(id)) {
      showNotification("Select a catalog entry first.", type = "warning")
      return()
    }
    if (!requireNamespace("LibeRation", quietly = TRUE)) {
      showNotification("Install LibeRation to import models.", type = "error")
      return()
    }
    ws <- trimws(input$workspace_root %||% "")
    proj <- trimws(input$import_project %||% "")
    root <- catalog_root()
    out <- tryCatch(
      LibeRary::library_use_in_workspace(
        id,
        project = if (nzchar(proj)) proj else NULL,
        workspace = if (nzchar(ws)) ws else NULL,
        root = root
      ),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      output$import_result <- renderText(conditionMessage(out))
      showNotification(conditionMessage(out), type = "error")
      return()
    }
    msg <- paste0(
      "Imported into project: ", out$project,
      "\nVersion: ", out$version_id,
              "\nWorkspace: ", out$workspace$path
    )
    output$import_result <- renderText(msg)
    showNotification(paste("Imported", id, "→", out$project), type = "message", duration = 6)
  }, ignoreInit = TRUE)

  output$import_result <- renderText("")
}

shinyApp(ui, server)
