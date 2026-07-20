# LibeRary systematic-review reference comparison - run via library_reference_shiny()

library(shiny)
library(DT)

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

pkg_root <- Sys.getenv("LIBERARY_PKG_ROOT", "")
if (!nzchar(pkg_root)) {
  sf <- system.file("", package = "LibeRary")
  if (nzchar(sf)) pkg_root <- normalizePath(sf, winslash = "/", mustWork = FALSE)
}
is_source_root <- nzchar(pkg_root) &&
  length(list.files(file.path(pkg_root, "R"), pattern = "[.]R$")) > 0L
if (is_source_root && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg_root, quiet = TRUE)
} else {
  library(LibeRary)
}

# Shiny apps are installed as ordinary files rather than evaluated inside the
# package namespace. Resolve our own helpers from that namespace so an app does
# not depend on whether a helper was exported by a previously loaded build.
.reference_call <- function(name, ...) {
  fn <- get0(name, envir = asNamespace("LibeRary"), inherits = FALSE)
  if (!is.function(fn)) {
    stop("The installed LibeRary namespace does not contain '", name,
         "'. Restart R and reinstall the current LibeRary package.")
  }
  fn(...)
}

.reference_workspace_root <- function() {
  candidates <- unique(c(
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    if (nzchar(pkg_root)) normalizePath(dirname(pkg_root), winslash = "/", mustWork = FALSE)
  ))
  candidates[dir.exists(candidates)][1L] %||% normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

.reference_latest_corpus <- function(path) {
  if (!nzchar(path) || !dir.exists(path)) return(path)
  if (file.exists(file.path(path, "manifest.json")) && dir.exists(file.path(path, "models"))) return(path)
  children <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  children <- children[file.exists(file.path(children, "manifest.json"))]
  if (!length(children)) return(path)
  versions <- lapply(basename(children), function(value) tryCatch(utils::package_version(value), error = function(e) NULL))
  valid <- !vapply(versions, is.null, logical(1))
  if (!any(valid)) return(children[[length(children)]])
  children[valid][[order(do.call(c, versions[valid]), decreasing = TRUE)[[1L]]]]
}

.reference_default <- function(environment, candidates, latest = FALSE) {
  configured <- Sys.getenv(environment, "")
  choices <- unique(c(configured, candidates))
  hit <- choices[nzchar(choices) & dir.exists(choices)][1L]
  if (is.na(hit)) return(configured %||% "")
  hit <- normalizePath(hit, winslash = "/", mustWork = FALSE)
  if (latest) .reference_latest_corpus(hit) else hit
}

workspace_root <- .reference_workspace_root()
default_corpus <- .reference_default(
  "LIBERARY_REFERENCE_ROOT",
  file.path(workspace_root, "validation", "liberary", "aed-pkpd-reference"),
  latest = TRUE
)
default_predictions <- .reference_default(
  "LIBERARY_REFERENCE_PREDICTIONS",
  c(file.path(workspace_root, "validation", "liberary", "aed-pkpd-benchmark", "text-current", "predictions"),
    file.path(workspace_root, "validation", "liberary", "aed-pkpd-benchmark", "text-current"))
)
default_source <- .reference_default(
  "LIBERARY_REFERENCE_SOURCE", file.path(workspace_root, "AED_PKPD")
)

shared_www <- system.file("shiny", "www", package = "LibeRary")
if ((!nzchar(shared_www) || !dir.exists(shared_www)) && nzchar(pkg_root)) {
  shared_www <- file.path(pkg_root, "inst", "shiny", "www")
}
if (!dir.exists(shared_www)) stop("The shared LibeRary GUI assets were not found.")
if ("liberary-reference-assets" %in% names(shiny::resourcePaths())) {
  shiny::removeResourcePath("liberary-reference-assets")
}
shiny::addResourcePath("liberary-reference-assets", shared_www)
favicon_href <- "liberary-reference-assets/favicon.svg"

.reference_empty_decisions <- function() {
  data.frame(reference_id = character(), tier = character(), training_eligible = logical(),
             review_status = character(), notes = character(), stringsAsFactors = FALSE)
}

.reference_empty_fields <- function() {
  data.frame(reference_id = character(), pointer = character(), field = character(),
             source = character(), value_json = character(), recorded_at = character(),
             stringsAsFactors = FALSE)
}

.reference_upsert <- function(value, row, keys) {
  if (!nrow(value)) return(row)
  matches <- rep(TRUE, nrow(value))
  for (key in keys) matches <- matches & as.character(value[[key]]) == as.character(row[[key]][[1L]])
  if (any(matches)) value <- value[!matches, , drop = FALSE]
  rbind(value, row)
}

.reference_json_pretty <- function(value) {
  if (is.null(value)) return("No LibeRary extraction is available for this model.")
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA,
                                pretty = TRUE, na = "null"))
}

.reference_manifest_version <- function(root) {
  path <- file.path(root, "manifest.json")
  if (!file.exists(path)) return("")
  jsonlite::read_json(path, simplifyVector = TRUE)$version %||% ""
}

.reference_next_version <- function(value) {
  parts <- suppressWarnings(as.integer(strsplit(as.character(value %||% "0.0.0"), "[.]", fixed = FALSE)[[1L]]))
  if (length(parts) != 3L || any(!is.finite(parts))) return("0.1.0")
  paste(parts[[1L]], parts[[2L]], parts[[3L]] + 1L, sep = ".")
}

.reference_register_pdf <- function(session, path, reference_id) {
  if (!nzchar(path) || !file.exists(path)) return("")
  name <- paste0("reference_pdf_", gsub("[^A-Za-z0-9_]", "_", reference_id), "_",
                 substr(digest::digest(path, algo = "sha256"), 1L, 10L))
  response <- utils::getFromNamespace("httpResponse", "shiny")
  session$registerDataObj(name, path, function(data, request) {
    if (!file.exists(data)) return(response(404L, "text/plain", "Source PDF is unavailable."))
    response(200L, "application/pdf", list(file = data, owned = FALSE),
             c(`Content-Disposition` = 'inline; filename="article.pdf"',
               `Cache-Control` = "private, no-store",
               `X-Content-Type-Options` = "nosniff"))
  })
}

ui <- fluidPage(
  tags$head(
    tags$title("LibeRary"),
    tags$link(rel = "icon", type = "image/svg+xml", href = favicon_href),
    tags$link(rel = "stylesheet", type = "text/css", href = "reference.css"),
    tags$script(src = "reference.js")
  ),
  tags$div(
    class = "lr-header",
    tags$div(class = "lr-brand", tags$img(src = favicon_href, alt = "LibeRary"),
             tags$div(tags$h2("LibeRary reference review"),
                      tags$p("Systematic-review benchmark comparison and curation"))),
    tags$div(class = "lr-header-actions",
             uiOutput("header_version"),
             tags$div(class = "lr-theme-wrap", tags$span(id = "theme_label", "Dark"),
                      tags$label(class = "lr-theme-switch", `aria-label` = "Toggle dark theme",
                                 tags$input(type = "checkbox", id = "theme_toggle", checked = NA),
                                 tags$span(class = "lr-theme-slider"))))
  ),
  fluidRow(
    column(
      width = 3L,
      tags$div(
        class = "lr-sidebar",
        tags$div(class = "lr-sidebar-section",
                 tags$h4(icon("folder-open"), " Sources"),
                 textInput("corpus_root", "Reference corpus", default_corpus),
                 textInput("predictions_root", "LibeRary predictions", default_predictions),
                 textInput("source_root", "Original review files / PDFs", default_source),
                 actionButton("load_sources", "Load comparison", icon = icon("refresh"),
                              class = "btn-primary btn-block")),
        tags$div(class = "lr-sidebar-section",
                 tags$h4(icon("filter"), " Find models"),
                 textInput("model_search", "Search", placeholder = "Drug, PMID, model id..."),
                 fluidRow(column(6, selectInput("partition_filter", "Partition",
                                                c("All" = "", "Train" = "train",
                                                  "Validation" = "validation", "Test" = "test"))),
                          column(6, selectInput("tier_filter", "Tier",
                                               c("All" = "", "A", "B", "C", "D")))),
                 selectInput("compound_filter", "Compound", choices = c("All" = ""))),
        tags$div(class = "lr-sidebar-section lr-review-form",
                 tags$h4(icon("check-square"), " Selected-model review"),
                 uiOutput("selected_model_hint"),
                 fluidRow(column(5, selectInput("review_tier", "Quality tier", c("A", "B", "C", "D"))),
                          column(7, selectInput("review_status", "Review status",
                                               c("reviewed", "needs_source_review", "unreviewed")))),
                 checkboxInput("training_eligible", "Eligible for training", FALSE),
                 helpText("Locked test models can be scored, but never used for training."),
                 textAreaInput("review_notes", "Review notes", rows = 3,
                               placeholder = "What was checked or remains uncertain?"),
                 actionButton("save_model_review", "Record model review", icon = icon("save"),
                              class = "btn-default btn-block")),
        tags$div(class = "lr-sidebar-section",
                 tags$h4(icon("code-fork"), " Successor corpus"),
                 tags$p(class = "help-block", "Create a new immutable version from recorded decisions."),
                 actionButton("open_revision", "Create successor version", icon = icon("plus-circle"),
                              class = "btn-success btn-block"),
                 uiOutput("decision_count"))
      )
    ),
    column(
      width = 9L,
      tags$div(class = "lr-model-browser",
               tags$div(class = "lr-section-title",
                        tags$div(tags$h3("Reference models"), textOutput("model_count", inline = TRUE))),
               DTOutput("models_table")),
      tags$div(
        class = "lr-workspace",
        uiOutput("selection_header"),
        conditionalPanel(
          condition = "output.hasSelection",
          tags$div(class = "lr-toolbar",
                   selectInput("prediction_variant", "Extraction", choices = "prediction",
                               width = "220px"),
                   checkboxInput("differences_only", "Differences only", TRUE),
                   uiOutput("pdf_action")),
          tags$div(
            class = "lr-three-column",
            tags$section(class = "lr-source-card lr-raw-card",
                         tags$div(class = "lr-card-header", tags$span(class = "lr-step", "1"),
                                  tags$div(tags$h4("Review appendix"),
                                           tags$p("Verbatim historical transcription"))),
                         tags$div(class = "lr-card-body", uiOutput("raw_record"))),
            tags$section(class = "lr-source-card",
                         tags$div(class = "lr-card-header", tags$span(class = "lr-step", "2"),
                                  tags$div(tags$h4("Normalized reference"),
                                           tags$p("Structured benchmark target"))),
                         tags$div(class = "lr-card-body", tags$pre(textOutput("reference_json")))),
            tags$section(class = "lr-source-card",
                         tags$div(class = "lr-card-header", tags$span(class = "lr-step", "3"),
                                  tags$div(tags$h4("LibeRary extraction"),
                                           tags$p("Selected model/vision variant"))),
                         tags$div(class = "lr-card-body", tags$pre(textOutput("prediction_json"))))
          ),
          uiOutput("semantic_summary"),
          tags$div(class = "lr-diff-panel",
                   tags$div(class = "lr-section-title",
                            tags$div(tags$h3("Field comparison"),
                                     tags$p("Select a row to record a curation decision.")),
                            uiOutput("difference_summary")),
                   DTOutput("difference_table"),
                   uiOutput("field_editor"),
                   tags$div(class = "lr-recorded-fields",
                            tags$h4("Recorded field decisions"), DTOutput("field_decisions_table")))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  corpus_root <- reactiveVal(default_corpus)
  predictions_root <- reactiveVal(default_predictions)
  source_root <- reactiveVal(default_source)
  model_index <- reactiveVal(data.frame())
  selected_id <- reactiveVal("")
  comparison <- reactiveVal(NULL)
  field_decisions <- reactiveVal(.reference_empty_fields())
  model_decisions <- reactiveVal(.reference_empty_decisions())
  selected_pointer <- reactiveVal("")

  load_index <- function(notify = TRUE, corpus_override = NULL,
                         predictions_override = NULL, source_override = NULL) {
    corpus <- trimws(corpus_override %||% input$corpus_root %||% corpus_root())
    predictions <- trimws(predictions_override %||% input$predictions_root %||% predictions_root())
    source <- trimws(source_override %||% input$source_root %||% source_root())
    if (!dir.exists(corpus) || !file.exists(file.path(corpus, "manifest.json"))) {
      showNotification("Select a valid reference-corpus version.", type = "error")
      return(FALSE)
    }
    validation <- tryCatch(.reference_call("library_reference_validate", corpus), error = identity)
    if (inherits(validation, "error") || !isTRUE(validation$valid)) {
      problem <- if (inherits(validation, "error")) conditionMessage(validation) else
        paste(validation$errors, collapse = "; ")
      showNotification(paste("Reference corpus is invalid:", problem), type = "error", duration = NULL)
      return(FALSE)
    }
    index <- .reference_call("library_reference_list", corpus)
    prediction_files <- if (dir.exists(predictions)) {
      list.files(predictions, pattern = "[.]json$", recursive = TRUE, full.names = FALSE)
    } else character()
    prediction_ids <- tools::file_path_sans_ext(basename(prediction_files))
    index$prediction <- ifelse(index$reference_id %in% prediction_ids, "Available", "Missing")
    corpus_root(normalizePath(corpus, winslash = "/", mustWork = TRUE))
    predictions_root(if (dir.exists(predictions)) normalizePath(predictions, winslash = "/", mustWork = TRUE) else predictions)
    source_root(if (dir.exists(source)) normalizePath(source, winslash = "/", mustWork = TRUE) else source)
    model_index(index)
    updateSelectInput(session, "compound_filter",
                      choices = c("All" = "", sort(unique(index$compound))))
    selected_id("")
    comparison(NULL)
    selected_pointer("")
    if (notify) showNotification(paste(nrow(index), "reference models loaded."), type = "message")
    TRUE
  }

  observeEvent(input$load_sources, {
    previous <- c(corpus_root(), predictions_root())
    if (isTRUE(load_index(TRUE))) {
      current <- c(corpus_root(), predictions_root())
      if (!identical(previous, current)) {
        field_decisions(.reference_empty_fields())
        model_decisions(.reference_empty_decisions())
        showNotification("Source changed; session review decisions were cleared.",
                         type = "warning", duration = 5)
      }
    }
  }, ignoreInit = TRUE)
  session$onFlushed(function() isolate(load_index(FALSE)), once = TRUE)

  filtered_models <- reactive({
    value <- model_index()
    if (!nrow(value)) return(value)
    query <- tolower(trimws(input$model_search %||% ""))
    if (nzchar(query)) {
      searchable <- apply(value[, intersect(c("reference_id", "article_id", "pmid", "compound"), names(value)),
                                drop = FALSE], 1L, paste, collapse = " ")
      value <- value[grepl(query, tolower(searchable), fixed = TRUE), , drop = FALSE]
    }
    if (nzchar(input$partition_filter %||% "")) value <- value[value$partition == input$partition_filter, , drop = FALSE]
    if (nzchar(input$tier_filter %||% "")) value <- value[value$tier == input$tier_filter, , drop = FALSE]
    if (nzchar(input$compound_filter %||% "")) value <- value[value$compound == input$compound_filter, , drop = FALSE]
    value
  })

  output$header_version <- renderUI({
    version <- .reference_manifest_version(corpus_root())
    if (!nzchar(version)) version <- "not loaded"
    tags$span(class = "lr-version-pill", paste("Corpus", version))
  })

  output$model_count <- renderText({
    paste0(format(nrow(filtered_models()), big.mark = ","), " of ",
           format(nrow(model_index()), big.mark = ","), " models")
  })

  output$models_table <- renderDT({
    value <- filtered_models()
    if (!nrow(value)) {
      return(datatable(data.frame(message = "No reference models match the filters."),
                       rownames = FALSE, selection = "none", options = list(dom = "t")))
    }
    show <- value[, c("reference_id", "compound", "pmid", "partition", "tier", "prediction"), drop = FALSE]
    names(show) <- c("Model", "Drug", "PMID", "Partition", "Tier", "Extraction")
    datatable(show, rownames = FALSE, selection = "single", escape = TRUE,
              options = list(pageLength = 8L, dom = "tip", scrollX = TRUE,
                             order = list(list(1L, "asc"), list(2L, "asc"))))
  })

  load_comparison <- function(id, variant = "prediction") {
    if (!nzchar(id)) return()
    value <- tryCatch(
      .reference_call("library_reference_compare", id, corpus_root(), predictions_root(), variant = variant),
      error = identity
    )
    if (inherits(value, "error")) {
      showNotification(conditionMessage(value), type = "error", duration = NULL)
      return()
    }
    comparison(value)
    choices <- value$variants
    if (!length(choices)) choices <- "prediction"
    labels <- stats::setNames(choices, ifelse(choices == "prediction", "Primary prediction", tools::toTitleCase(choices)))
    updateSelectInput(session, "prediction_variant", choices = labels,
                      selected = value$selected_variant)
    selected_pointer("")
  }

  observeEvent(input$models_table_rows_selected, {
    row <- input$models_table_rows_selected
    value <- filtered_models()
    if (length(row) != 1L || row > nrow(value)) return()
    id <- value$reference_id[[row]]
    selected_id(id)
    load_comparison(id)
    record <- .reference_call("library_reference_get", id, corpus_root())
    prior <- model_decisions()
    prior <- prior[prior$reference_id == id, , drop = FALSE]
    updateSelectInput(session, "review_tier", selected = if (nrow(prior)) prior$tier[[1L]] else record$quality$tier)
    updateSelectInput(session, "review_status", selected = if (nrow(prior)) prior$review_status[[1L]] else record$quality$review_status)
    training <- if (nrow(prior)) isTRUE(prior$training_eligible[[1L]]) else isTRUE(record$quality$training_eligible)
    updateCheckboxInput(session, "training_eligible", value = training && !identical(record$partition, "test"))
    updateTextAreaInput(session, "review_notes", value = if (nrow(prior)) prior$notes[[1L]] else "")
    session$sendCustomMessage("referenceToggleTraining", list(disabled = identical(record$partition, "test")))
  }, ignoreInit = TRUE)

  observeEvent(input$prediction_variant, {
    current <- comparison()
    id <- selected_id()
    variant <- input$prediction_variant %||% "prediction"
    if (nzchar(id) && !is.null(current) && !identical(current$selected_variant, variant)) {
      load_comparison(id, variant)
    }
  }, ignoreInit = TRUE)

  output$hasSelection <- reactive(nzchar(selected_id()) && !is.null(comparison()))
  outputOptions(output, "hasSelection", suspendWhenHidden = FALSE)

  output$selection_header <- renderUI({
    value <- comparison()
    if (is.null(value)) {
      return(tags$div(class = "lr-empty-state", icon("columns"), tags$h3("Select a reference model"),
                      tags$p("The appendix, normalized benchmark, and LibeRary extraction will appear side by side.")))
    }
    record <- value$record
    title <- record$reference$extraction_target$title %||% ""
    if (!nzchar(title)) title <- paste(record$reference$study$first_author %||% "Reference", record$reference$study$year %||% "")
    tags$div(class = "lr-selection-header",
             tags$div(tags$h3(title), tags$code(record$reference_id)),
             tags$div(class = "lr-badges",
                      tags$span(class = paste("lr-badge lr-partition", record$partition), record$partition),
                      tags$span(class = paste("lr-badge lr-tier", tolower(record$quality$tier)), paste("Tier", record$quality$tier)),
                      tags$span(class = paste("lr-badge", if (is.null(value$prediction)) "lr-missing" else "lr-available"),
                                if (is.null(value$prediction)) "No extraction" else "Extraction available")))
  })

  output$selected_model_hint <- renderUI({
    if (!nzchar(selected_id())) tags$p(class = "help-block", "Select a model first.") else
      tags$p(class = "lr-selected-hint", tags$code(selected_id()))
  })

  output$raw_record <- renderUI({
    value <- comparison(); req(value)
    raw <- value$raw
    if (!length(raw)) return(tags$p(class = "help-block", "No appendix transcription is stored."))
    tags$dl(class = "lr-raw-list", lapply(names(raw), function(name) {
      list(tags$dt(gsub("_", " ", tools::toTitleCase(name))),
           tags$dd(as.character(raw[[name]] %||% "\u2014")))
    }))
  })
  output$reference_json <- renderText({ req(comparison()); .reference_json_pretty(comparison()$reference) })
  output$prediction_json <- renderText({ req(comparison()); .reference_json_pretty(comparison()$prediction) })

  output$semantic_summary <- renderUI({
    value <- comparison(); req(value)
    target <- value$reference %||% list()
    structural <- target$structural_model %||% list()
    implementations <- structural$implementations %||% list()
    implementation <- if (length(implementations)) implementations[[1L]] else list()
    population <- target$population_details %||% list(cohorts = list())
    dosing <- target$dosing %||% list()
    targets <- target$reproduction_targets %||% list()
    blockers <- character()
    if (!length(dosing) || is.null(dosing[[1L]]$amount)) blockers <- c(blockers, "numerical dose")
    if (!length(targets)) blockers <- c(blockers, "digitized PK target")
    if (is.null(implementation$advan)) blockers <- c(blockers, "executable mapping")
    tags$div(class = "lr-semantic-grid",
      tags$div(class = "lr-semantic-card", tags$span("Implementation"), tags$strong(
        if (length(implementation)) paste0(
          "ADVAN", implementation$advan %||% "?", "/TRANS", implementation$trans %||% "?",
          " · ", implementation$status %||% "unknown"
        ) else "Unresolved"
      ), tags$small(implementation$rationale %||% "No implementation rationale recorded.")),
      tags$div(class = "lr-semantic-card", tags$span("Population"),
        tags$strong(paste(length(population$cohorts %||% list()), "cohort(s) · N=",
                          population$n_total %||% "?")),
        tags$small("Age, weight and pharmacogenetics remain linked to source evidence.")),
      tags$div(class = "lr-semantic-card", tags$span("Reproduction readiness"),
        tags$strong(if (!length(blockers)) "Ready to plan" else "Incomplete"),
        tags$small(if (length(blockers)) paste("Missing:", paste(blockers, collapse = ", ")) else
          paste(length(targets), "published target(s) available.")))
    )
  })

  output$pdf_action <- renderUI({
    value <- comparison(); req(value)
    relative <- value$record$source$publication_pdf$path %||% ""
    path <- if (nzchar(relative) && grepl("^[A-Za-z]:[/\\\\]|^/", relative)) relative else
      file.path(source_root(), relative)
    url <- .reference_register_pdf(session, path, value$reference_id)
    if (!nzchar(url)) return(tags$button(class = "btn btn-default", disabled = NA,
                                         icon("file-pdf"), " PDF unavailable"))
    tags$a(class = "btn btn-default", href = url, target = "_blank", rel = "noopener noreferrer",
           icon("file-pdf"), " Open source PDF")
  })

  comparison_view <- reactive({
    value <- comparison(); req(value)
    table <- value$comparison
    if (isTRUE(input$differences_only)) table <- table[table$status != "Match", , drop = FALSE]
    table$delta <- ifelse(is.na(table$delta_percent), "\u2014",
                          ifelse(is.infinite(table$delta_percent), "\u221e",
                                 sprintf("%+.1f%%", table$delta_percent)))
    table
  })

  output$difference_summary <- renderUI({
    value <- comparison(); req(value)
    counts <- table(factor(value$comparison$status,
                           levels = c("Different", "Review only", "LibeRary only", "Match")))
    tags$div(class = "lr-summary-pills",
             tags$span(class = "lr-summary-different", paste(counts[["Different"]], "different")),
             tags$span(paste(counts[["Review only"]] + counts[["LibeRary only"]], "one-sided")),
             tags$span(class = "lr-summary-match", paste(counts[["Match"]], "matching")))
  })

  output$difference_table <- renderDT({
    value <- comparison_view()
    show <- value[, c("pointer", "field", "reference", "liberary", "delta", "status"), drop = FALSE]
    names(show) <- c("Pointer", "Field", "Review reference", "LibeRary", "Delta", "Status")
    widget <- datatable(show, rownames = FALSE, selection = "single", escape = TRUE,
                        options = list(pageLength = 12L, scrollX = TRUE, autoWidth = FALSE,
                                       columnDefs = list(list(targets = 0L, visible = FALSE),
                                                         list(targets = c(2L, 3L), width = "30%"),
                                                         list(targets = 1L, width = "18%"))))
    formatStyle(widget, "Status", target = "row",
                backgroundColor = styleEqual(c("Different", "Review only", "LibeRary only", "Match"),
                                             c("rgba(190,69,69,.10)", "rgba(200,139,48,.10)",
                                               "rgba(62,125,99,.10)", "transparent")))
  })

  observeEvent(input$difference_table_rows_selected, {
    row <- input$difference_table_rows_selected
    value <- comparison_view()
    if (length(row) != 1L || row > nrow(value)) return(selected_pointer(""))
    selected_pointer(value$pointer[[row]])
  }, ignoreInit = FALSE)

  output$field_editor <- renderUI({
    pointer <- selected_pointer()
    value <- comparison_view()
    if (!nzchar(pointer)) return(tags$div(class = "lr-field-empty", "Select a comparison row to review it."))
    row <- value[value$pointer == pointer, , drop = FALSE]
    if (!nrow(row)) return(NULL)
    prior <- field_decisions()
    prior <- prior[prior$reference_id == selected_id() & prior$pointer == pointer, , drop = FALSE]
    selected_source <- if (nrow(prior)) prior$source[[1L]] else "reference"
    custom_value <- if (nrow(prior) && identical(selected_source, "custom")) prior$value_json[[1L]] else ""
    tags$div(class = "lr-field-editor",
             tags$div(class = "lr-field-editor-head",
                      tags$div(tags$span(class = "lr-kicker", "Field decision"), tags$h4(row$field[[1L]])),
                      tags$code(pointer)),
             fluidRow(
               column(7, radioButtons("field_source", NULL,
                                      choices = c("Keep review value" = "reference",
                                                  "Use LibeRary value" = "liberary",
                                                  "Enter custom JSON value" = "custom"),
                                      selected = selected_source, inline = TRUE),
                      conditionalPanel("input.field_source === 'custom'",
                                       textAreaInput("field_custom", "Custom JSON", value = custom_value, rows = 3,
                                                     placeholder = "A JSON string, number, boolean, null, array, or object"))),
               column(5, tags$div(class = "lr-value-preview",
                                  tags$div(tags$span("Review"), tags$code(row$reference[[1L]])),
                                  tags$div(tags$span("LibeRary"), tags$code(row$liberary[[1L]]))),
                      actionButton("record_field", "Record field decision", icon = icon("check"),
                                   class = "btn-primary btn-block"))
             ))
  })

  observeEvent(input$record_field, {
    pointer <- selected_pointer(); req(nzchar(pointer), comparison())
    row <- comparison()$comparison[comparison()$comparison$pointer == pointer, , drop = FALSE]
    source <- input$field_source %||% "reference"
    value_json <- switch(source,
                         reference = row$reference_json[[1L]],
                         liberary = row$liberary_json[[1L]],
                         custom = trimws(input$field_custom %||% ""))
    if (source == "liberary" && (is.na(value_json) || !nzchar(value_json))) {
      return(showNotification("This field has no LibeRary value to accept.", type = "warning"))
    }
    if (source == "custom") {
      valid <- tryCatch({ jsonlite::fromJSON(value_json, simplifyVector = FALSE); TRUE }, error = function(e) e)
      if (inherits(valid, "error")) return(showNotification(paste("Invalid JSON:", conditionMessage(valid)), type = "error"))
    }
    if (is.na(value_json) || !nzchar(value_json)) value_json <- "null"
    item <- data.frame(reference_id = selected_id(), pointer = pointer, field = row$field[[1L]],
                       source = source, value_json = value_json,
                       recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                       stringsAsFactors = FALSE)
    field_decisions(.reference_upsert(field_decisions(), item, c("reference_id", "pointer")))
    showNotification("Field decision recorded.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  output$field_decisions_table <- renderDT({
    value <- field_decisions()
    value <- value[value$reference_id == selected_id(), c("field", "source", "recorded_at"), drop = FALSE]
    if (!nrow(value)) value <- data.frame(message = "No field decisions recorded for this model.")
    datatable(value, rownames = FALSE, selection = "none", escape = TRUE,
              options = list(dom = "t", pageLength = 8L))
  })

  observeEvent(input$save_model_review, {
    id <- selected_id()
    if (!nzchar(id)) return(showNotification("Select a reference model first.", type = "warning"))
    record <- comparison()$record
    training <- isTRUE(input$training_eligible) && !identical(record$partition, "test")
    row <- data.frame(reference_id = id, tier = input$review_tier,
                      training_eligible = training, review_status = input$review_status,
                      notes = trimws(input$review_notes %||% ""), stringsAsFactors = FALSE)
    model_decisions(.reference_upsert(model_decisions(), row, "reference_id"))
    showNotification("Model review recorded for this session.", type = "message")
  }, ignoreInit = TRUE)

  output$decision_count <- renderUI({
    tags$p(class = "lr-decision-count",
           paste(nrow(model_decisions()), "model reviews \u2022", nrow(field_decisions()), "field decisions"))
  })

  observeEvent(input$open_revision, {
    current <- .reference_manifest_version(corpus_root())
    proposed <- .reference_next_version(current)
    output_dir <- file.path(dirname(corpus_root()), proposed)
    audit_dir <- file.path(dirname(corpus_root()), "review-audits", proposed)
    showModal(modalDialog(
      title = "Create immutable successor corpus",
      size = "l", easyClose = TRUE,
      tags$p("The current corpus is copied, reviewed normalized fields are updated, and the historical appendix remains unchanged."),
      fluidRow(column(4, textInput("revision_version", "New corpus version", proposed)),
               column(8, textInput("revision_curator", "Curator name / identifier", ""))),
      textInput("revision_output", "Successor corpus directory", output_dir),
      textInput("revision_audit", "Decision audit directory", audit_dir),
      tags$div(class = "lr-modal-summary",
               tags$strong(paste(nrow(model_decisions()), "model reviews")), " and ",
               tags$strong(paste(nrow(field_decisions()), "field decisions")), " will be written."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("create_revision", "Create successor", icon = icon("check"),
                                    class = "btn-success"))
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$create_revision, {
    decisions <- model_decisions()
    fields <- field_decisions()
    if (!nrow(decisions)) return(showNotification("Record at least one model review first.", type = "warning"))
    orphaned <- setdiff(unique(fields$reference_id), decisions$reference_id)
    if (length(orphaned)) {
      return(showNotification(paste("Record model reviews for field decisions:", paste(orphaned, collapse = ", ")),
                              type = "warning", duration = NULL))
    }
    version <- trimws(input$revision_version %||% "")
    curator <- trimws(input$revision_curator %||% "")
    output_dir <- path.expand(trimws(input$revision_output %||% ""))
    audit_dir <- path.expand(trimws(input$revision_audit %||% ""))
    if (!nzchar(version) || !nzchar(curator) || !nzchar(output_dir) || !nzchar(audit_dir)) {
      return(showNotification("Version, curator, successor directory, and audit directory are required.", type = "error"))
    }
    if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
      return(showNotification("The successor directory already exists and is not empty.", type = "error"))
    }
    if (dir.exists(audit_dir) && length(list.files(audit_dir, all.files = TRUE, no.. = TRUE))) {
      return(showNotification("The decision audit directory already exists and is not empty.", type = "error"))
    }
    corrections <- fields[fields$source %in% c("liberary", "custom"),
                          c("reference_id", "pointer", "source", "value_json"), drop = FALSE]
    outcome <- tryCatch({
      dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
      getFromNamespace(".library_reference_write_csv", "LibeRary")(
        decisions, file.path(audit_dir, "model-decisions.csv"))
      getFromNamespace(".library_reference_write_csv", "LibeRary")(
        fields, file.path(audit_dir, "field-decisions.csv"))
      audit <- list(schema_version = "1.0.0", source_corpus = corpus_root(),
                    source_version = .reference_manifest_version(corpus_root()),
                    successor_version = version, curator = curator,
                    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                    counts = list(models = nrow(decisions), fields = nrow(fields),
                                  corrections = nrow(corrections)))
      getFromNamespace(".library_atomic_write", "LibeRary")(
        audit, file.path(audit_dir, "audit.json"))
      .reference_call("library_reference_revise",
        corpus_root(), output_dir, decisions = decisions, version = version,
        curator = curator, corrections = corrections
      )
      TRUE
    }, error = identity)
    if (inherits(outcome, "error")) {
      return(showNotification(conditionMessage(outcome), type = "error", duration = NULL))
    }
    removeModal()
    updateTextInput(session, "corpus_root", value = normalizePath(output_dir, winslash = "/", mustWork = TRUE))
    corpus_root(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
    field_decisions(.reference_empty_fields())
    model_decisions(.reference_empty_decisions())
    load_index(FALSE, corpus_override = output_dir)
    showNotification(paste("Successor corpus", version, "created."), type = "message", duration = 8)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
