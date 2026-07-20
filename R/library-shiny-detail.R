.library_status_color <- function(status) {
  switch(
    as.character(status %||% ""),
    validated = "#198754",
    machine_consistent = "#236a45",
    machine_adjudicated = "#2f7d55",
    review = "#39875f",
    draft = "#c56d16",
    stub = "#6c757d",
    discovered = "#6c757d",
    deprecated = "#dc3545",
    mbma_source = "#6610f2",
    "#495057"
  )
}

.library_register_pdf <- function(session, library_id, root) {
  path <- tryCatch(library_source_pdf(library_id, root), error = function(e) "")
  if (!nzchar(path)) return("")
  name <- paste0("source_pdf_", gsub("[^A-Za-z0-9_]", "_", library_id), "_",
                 substr(digest::digest(path, algo = "sha256"), 1L, 12L))
  response <- utils::getFromNamespace("httpResponse", "shiny")
  session$registerDataObj(name, path, function(data, request) {
    if (!file.exists(data)) {
      return(response(404L, "text/plain", "Source PDF is no longer available."))
    }
    response(
      200L, "application/pdf", list(file = data, owned = FALSE),
      c(
        `Content-Disposition` = 'inline; filename="article.pdf"',
        `Cache-Control` = "private, no-store",
        `X-Content-Type-Options` = "nosniff"
      )
    )
  })
}

.library_entry_detail_ui <- function(entry, root, pdf_url = "") {
  if (!requireNamespace("shiny", quietly = TRUE)) return(NULL)
  tags <- shiny::tags
  m <- entry$manifest
  id <- entry$library_id %||% m$library_id %||% ""
  st <- m$status %||% ""
  keywords <- as.character(unlist(m$study$keywords %||% character()))
  kw_txt <- if (length(keywords)) paste(keywords, collapse = ", ") else "\u2014"
  abstract <- as.character(m$study$abstract %||% "")[[1L]]
  prov <- m$provenance %||% list()
  implementations <- m$model$implementations %||% list()
  implementation <- if (length(implementations)) implementations[[1L]] else list()
  reproduction <- m$qualification$reproduction %||% list(status = "not_planned")
  ledger_path <- file.path(entry$paths$extraction, "evidence-ledger.json")
  ledger <- if (file.exists(ledger_path)) {
    tryCatch(jsonlite::fromJSON(ledger_path, simplifyVector = FALSE), error = function(e) NULL)
  } else NULL
  ledger_checks <- ledger$deterministic_checks %||% list()
  ledger_ui <- if (is.list(ledger)) {
    tags$div(
      class = "lib-meta-grid",
      tags$div(tags$span(class = "lib-meta-label", "Evidence claims"),
               length(ledger$claims %||% list())),
      tags$div(tags$span(class = "lib-meta-label", "Open questions"),
               length(ledger$questions %||% list())),
      tags$div(tags$span(class = "lib-meta-label", "Coverage"),
               if (!is.null(ledger_checks$coverage_fraction)) {
                 paste0(round(100 * as.numeric(ledger_checks$coverage_fraction), 0), "%")
               } else "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "Evidence gate"),
               if (isTRUE(ledger_checks$ready)) "passed" else "review required")
    )
  } else tags$p(class = "text-muted", "No staged evidence ledger is available for this version.")
  population_details <- m$study$population_details %||% list(cohorts = list())
  cohort_ui <- if (length(population_details$cohorts %||% list())) {
    tags$div(class = "lib-cohort-list", lapply(population_details$cohorts, function(cohort) {
      descriptors <- vapply(cohort$descriptors %||% list(), function(descriptor) {
        statistics <- descriptor$statistics %||% list()
        value <- if (length(statistics)) paste(unlist(statistics[[1L]]), collapse = " ") else "reported"
        paste0(descriptor$name %||% "descriptor", ": ", value, " ", descriptor$unit %||% "")
      }, character(1))
      tags$div(class = "lib-cohort-card",
        tags$strong(cohort$label %||% cohort$id %||% "Cohort"),
        tags$span(class = "text-muted", paste0("  N=", cohort$n$analysed %||% "\u2014")),
        if (length(descriptors)) tags$div(paste(descriptors, collapse = " \u00b7 "))
      )
    }))
  } else tags$p(class = "text-muted", "No structured cohort demographics are available.")
  prov_lines <- if (length(prov)) {
    tags$ul(
      class = "lib-prov-list",
      lapply(names(prov), function(nm) {
        value <- prov[[nm]]
        if (is.list(value) || length(value) > 1L) {
          value <- paste(as.character(unlist(value)), collapse = "; ")
        }
        tags$li(tags$strong(paste0(nm, ": ")), as.character(value %||% ""))
      })
    )
  } else tags$p(class = "text-muted", "No provenance recorded.")
  links <- list()
  if (nzchar(pdf_url)) {
    links <- c(links, list(tags$a(
      href = pdf_url, target = "_blank", rel = "noopener noreferrer",
      class = "btn btn-primary btn-sm", shiny::icon("file-pdf"), " Open PDF"
    )))
  }
  pmid <- as.character(prov$pmid %||% "")[[1L]]
  if (nzchar(pmid)) {
    links <- c(links, list(tags$a(
      href = paste0("https://pubmed.ncbi.nlm.nih.gov/", utils::URLencode(pmid), "/"),
      target = "_blank", rel = "noopener noreferrer", class = "btn btn-default btn-sm",
      "PubMed"
    )))
  }
  doi <- as.character(prov$doi %||% "")[[1L]]
  if (nzchar(doi)) {
    links <- c(links, list(tags$a(
      href = paste0("https://doi.org/", utils::URLencode(doi, reserved = TRUE)),
      target = "_blank", rel = "noopener noreferrer", class = "btn btn-default btn-sm",
      "DOI"
    )))
  }

  shiny::tagList(
    tags$div(class = "lib-detail-title", m$title %||% id),
    tags$div(class = "lib-detail-id", id),
    tags$span(class = "lib-status-pill",
              style = paste0("background:", .library_status_color(st), ";"), st),
    if (length(links)) tags$div(class = "lib-detail-actions", links),
    tags$div(
      class = "lib-meta-grid",
      tags$div(tags$span(class = "lib-meta-label", "Compound"), m$study$compound %||% "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "Population"), m$study$population %||% "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "Route"), m$study$route %||% "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "ADVAN"),
               if (!is.null(m$model$advan)) paste0("ADVAN ", m$model$advan) else "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "Mapping"),
               if (length(implementation)) paste0(
                 implementation$status %||% "unknown", " (",
                 round(as.numeric(implementation$confidence %||% 0), 2), ")"
               ) else "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "Reproduction"),
               reproduction$status %||% "not_planned"),
      tags$div(tags$span(class = "lib-meta-label", "Confidence"),
               if (!is.null(m$confidence$overall)) round(as.numeric(m$confidence$overall), 2) else "\u2014"),
      tags$div(tags$span(class = "lib-meta-label", "Keywords"), kw_txt)
    ),
    if (nzchar(abstract)) shiny::tagList(tags$h5("Abstract"), tags$p(style = "font-size:13px;", abstract)),
    tags$h5("Structured population"),
    cohort_ui,
    tags$h5("Investigative evidence"),
    ledger_ui,
    tags$h5("Reproduction readiness"),
    tags$p(
      tags$strong(reproduction$status %||% "not_planned"),
      if (length(reproduction$blockers %||% character())) {
        paste0(" \u2014 ", paste(unlist(reproduction$blockers), collapse = ", "))
      }
    ),
    tags$h5("Provenance"),
    prov_lines,
    tags$h5("Model (model.ctl)"),
    tags$pre(
      class = "lib-ctl-box",
      paste(tryCatch(library_model(id, root),
                     error = function(e) paste("#", conditionMessage(e))), collapse = "\n")
    )
  )
}
