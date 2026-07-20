.library_semantic_scalar <- function(value, default = "") {
  if (is.null(value) || !length(value)) return(default)
  value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (!length(value)) return(default)
  value <- as.character(value[[1L]])
  if (is.na(value[[1L]])) default else trimws(value[[1L]])
}

.library_semantic_number <- function(value) {
  value <- suppressWarnings(as.numeric(value %||% NA_real_))
  if (length(value) && is.finite(value[[1L]])) value[[1L]] else NULL
}

.library_semantic_source <- function(status = "reported", locator = "", evidence = "",
                                     confidence = if (status == "reported") 1 else 0.5) {
  list(
    status = status,
    source_locator = .library_semantic_scalar(locator),
    evidence = .library_semantic_scalar(evidence),
    confidence = max(0, min(1, as.numeric(confidence %||% 0)))
  )
}

.library_summary_statistics <- function(text) {
  text <- .library_semantic_scalar(text)
  number <- "[-+]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)"
  output <- list()
  mean_sd <- regmatches(text, regexec(
    paste0("(", number, ")\\s*(?:\u00b1|\\+/-|[+]\\s*/\\s*-)\\s*(", number, ")"),
    text, perl = TRUE
  ))[[1L]]
  if (length(mean_sd) >= 3L) {
    output[[length(output) + 1L]] <- list(
      type = "mean_sd", mean = as.numeric(mean_sd[[2L]]), sd = as.numeric(mean_sd[[3L]])
    )
  }
  range <- regmatches(text, regexec(
    paste0("[(]\\s*(`?", number, ")\\s*(?:-|\u2013|\u2014|to)\\s*(`?", number, ")\\s*[)]"),
    text, perl = TRUE, ignore.case = TRUE
  ))[[1L]]
  if (length(range) >= 3L) {
    output[[length(output) + 1L]] <- list(
      type = "range", minimum = as.numeric(gsub("`", "", range[[2L]], fixed = TRUE)),
      maximum = as.numeric(gsub("`", "", range[[3L]], fixed = TRUE))
    )
  }
  if (!length(output)) {
    center <- regmatches(text, regexec(number, text, perl = TRUE))[[1L]]
    if (length(center)) {
      output[[1L]] <- list(
        type = "reported_center", value = as.numeric(center[[1L]]),
        statistic = "unknown"
      )
    }
  }
  output
}

.library_population_descriptor <- function(name, text, unit, source_locator = "") {
  if (!nzchar(.library_semantic_scalar(text))) return(NULL)
  list(
    name = name,
    unit = unit,
    statistics = .library_summary_statistics(text),
    categories = list(),
    source = .library_semantic_source("reported", source_locator, text, 1)
  )
}

.library_population_segments <- function(text) {
  text <- .library_semantic_scalar(text)
  hits <- gregexpr("(?i)\\bN\\s*[:=]\\s*[0-9]+", text, perl = TRUE)[[1L]]
  if (!length(hits) || hits[[1L]] < 0L) return(list(list(label = "overall", text = text)))
  label_starts <- integer(length(hits))
  labels <- character(length(hits))
  for (index in seq_along(hits)) {
    prefix <- substr(text, 1L, hits[[index]] - 1L)
    candidate <- regmatches(prefix, regexec(
      "([[:alpha:]][[:alpha:] /_-]{0,50})\\s*$", prefix, perl = TRUE
    ))[[1L]]
    label <- if (length(candidate) >= 2L) trimws(candidate[[2L]]) else ""
    if (tolower(label) %in% c("age", "weight", "wt", "height", "ht", "bmi", "bsa")) label <- ""
    labels[[index]] <- if (nzchar(label)) label else
      if (length(hits) == 1L) "overall" else paste0("cohort_", index)
    label_starts[[index]] <- if (nzchar(label)) hits[[index]] - nchar(candidate[[1L]]) else hits[[index]]
  }
  output <- vector("list", length(hits))
  for (index in seq_along(hits)) {
    end <- if (index < length(hits)) label_starts[[index + 1L]] - 1L else nchar(text)
    output[[index]] <- list(label = labels[[index]], text = trimws(substr(text, hits[[index]], end)))
  }
  output
}

.library_population_cohort <- function(segment, index, source_locator = "") {
  text <- .library_semantic_scalar(segment$text)
  n_match <- regmatches(text, regexec("(?i)\\bN\\s*[:=]\\s*([0-9]+)", text, perl = TRUE))[[1L]]
  n <- if (length(n_match) >= 2L) as.integer(n_match[[2L]]) else NULL
  take <- function(pattern) {
    match <- regmatches(text, regexec(pattern, text, perl = TRUE, ignore.case = TRUE))[[1L]]
    if (length(match) >= 2L) trimws(match[[2L]]) else ""
  }
  value_pattern <- "([^;]*?(?:\u00b1|\\+/-|[+]\\s*/\\s*-)?[^;]*?(?:[(][^)]*[)])?)(?=\\s+(?:WT|Weight|HT|Height|BMI|BSA|Age|CYP)[ :]?|$)"
  age <- take(paste0("\\bAge\\s*[:=]?\\s*", value_pattern))
  weight <- take(paste0("\\b(?:WT|Weight)\\s*[:=]?\\s*", value_pattern))
  height <- take(paste0("\\b(?:HT|Height)\\s*[:=]?\\s*", value_pattern))
  bmi <- take(paste0("\\bBMI\\s*[:=]?\\s*", value_pattern))
  descriptors <- Filter(Negate(is.null), list(
    .library_population_descriptor("age", age, "years", source_locator),
    .library_population_descriptor("weight", weight, "kg", source_locator),
    .library_population_descriptor("height", height, "cm", source_locator),
    .library_population_descriptor("bmi", bmi, "kg/m2", source_locator)
  ))
  genes <- unique(toupper(regmatches(text, gregexpr(
    "\\bCYP(?:1A2|2A6|2B6|2C8|2C9|2C19|2D6|2E1|3A4|3A5)\\b", text,
    perl = TRUE, ignore.case = TRUE
  ))[[1L]]))
  pharmacogenetics <- lapply(genes, function(gene) list(
    gene = gene, variant = NULL, diplotype = NULL, phenotype = NULL,
    n = NULL, proportion = NULL, assay = NULL,
    source = .library_semantic_source("reported", source_locator, text, 0.7)
  ))
  list(
    id = if (identical(segment$label, "overall")) "overall" else paste0("cohort_", index),
    label = segment$label,
    role = "unspecified",
    n = list(enrolled = NULL, dosed = NULL, analysed = n),
    health_status = if (grepl("healthy", text, ignore.case = TRUE)) "healthy" else
      if (grepl("patient", text, ignore.case = TRUE)) "patients" else "unspecified",
    country = NULL,
    descriptors = descriptors,
    pharmacogenetics = pharmacogenetics,
    source = .library_semantic_source("reported", source_locator, text, 1)
  )
}

#' Normalize a reported study population into cohorts and descriptors
#'
#' The original population text is always retained. Summary-statistic records
#' can coexist, so a paper may report mean/SD and a range for the same variable.
#'
#' @param population Reported population text or an existing normalized object.
#' @param n_subjects Optional legacy subject count.
#' @param source_locator Optional page/table/appendix locator.
#' @return A normalized population object.
#' @export
library_population_normalize <- function(population, n_subjects = NULL,
                                         source_locator = "") {
  if (is.list(population) && !is.null(population$cohorts)) return(population)
  raw <- .library_semantic_scalar(population)
  cohorts <- if (nzchar(raw)) {
    segments <- .library_population_segments(raw)
    lapply(seq_along(segments), function(index) {
      .library_population_cohort(segments[[index]], index, source_locator)
    })
  } else list()
  counts <- vapply(cohorts, function(cohort) {
    as.numeric(cohort$n$analysed %||% NA_real_)
  }, numeric(1))
  finite <- is.finite(counts)
  total <- if (sum(finite) > 1L) sum(counts[finite]) else
    .library_semantic_number(n_subjects) %||% if (any(finite)) counts[finite][[1L]] else NULL
  list(
    raw = if (nzchar(raw)) raw else NULL,
    n_total = total,
    cohorts = cohorts,
    assumptions = if (sum(finite) > 1L) {
      "Total N is the sum of separately reported cohorts; overlap was not reported."
    } else character(),
    source = .library_semantic_source(
      if (nzchar(raw)) "reported" else "missing", source_locator, raw,
      if (nzchar(raw)) 1 else 0
    )
  )
}

.library_dose_unit <- function(text) {
  match <- regmatches(text, regexec(
    "(?i)(mg|g|ug|\u00b5g|mcg)(?:\\s*/\\s*(?:kg|m2))?", text, perl = TRUE
  ))[[1L]]
  if (length(match)) match[[1L]] else NULL
}

#' Normalize reported dosing information
#'
#' @param dose Reported dose text or existing regimen list.
#' @param route Optional normalized route.
#' @param source_locator Evidence locator.
#' @return A list of dosing regimens.
#' @export
library_dosing_normalize <- function(dose, route = NULL, source_locator = "") {
  # A structured regimen may explicitly contain route = NULL. Test for the
  # schema field itself rather than its value so valid extracted arrays are not
  # mistaken for legacy free text.
  if (is.list(dose) && length(dose) && is.list(dose[[1L]]) &&
      "route" %in% names(dose[[1L]])) return(dose)
  raw <- .library_semantic_scalar(dose)
  if (!nzchar(raw)) return(list())
  amount_match <- regmatches(raw, regexec(
    "(?i)([0-9]+(?:[.][0-9]+)?)\\s*(mg|g|ug|\u00b5g|mcg)(?:\\s*/\\s*(kg|m2))?", raw,
    perl = TRUE
  ))[[1L]]
  amount <- if (length(amount_match) >= 2L) as.numeric(amount_match[[2L]]) else NULL
  unit <- .library_dose_unit(raw)
  interval_match <- regmatches(raw, regexec(
    "(?i)(?:every|q)\\s*([0-9]+(?:[.][0-9]+)?)\\s*(h|hr|hour|hours|day|days)", raw,
    perl = TRUE
  ))[[1L]]
  interval <- if (length(interval_match) >= 2L) as.numeric(interval_match[[2L]]) else NULL
  interval_unit <- if (length(interval_match) >= 3L) interval_match[[3L]] else NULL
  list(list(
    cohort_id = "overall",
    route = .library_semantic_scalar(route, NA_character_),
    administration = if (grepl("infusion", raw, ignore.case = TRUE)) "infusion" else
      if (grepl("bolus", raw, ignore.case = TRUE)) "bolus" else "unspecified",
    amount = amount,
    amount_unit = unit,
    interval = interval,
    interval_unit = interval_unit,
    duration = NULL,
    duration_unit = NULL,
    repetitions = NULL,
    steady_state = grepl("steady[ -]?state", raw, ignore.case = TRUE),
    raw = raw,
    source = .library_semantic_source("reported", source_locator, raw, 1)
  ))
}

.library_parameter_names_semantic <- function(parameters) {
  theta <- parameters$theta %||% list()
  unique(toupper(trimws(vapply(theta, function(item) {
    .library_semantic_scalar(item$name)
  }, character(1)))))
}

.library_structural_canonical <- function(structural_model, route, parameters) {
  structural_model <- structural_model %||% list()
  description <- .library_semantic_scalar(structural_model$description)
  names <- .library_parameter_names_semantic(parameters %||% list())
  compartments <- .library_semantic_number(structural_model$compartments)
  if (is.null(compartments)) compartments <- .library_reference_compartments(description)
  compartments <- .library_semantic_number(compartments) %||% NA_integer_
  compartments <- as.integer(compartments)
  route <- tolower(.library_semantic_scalar(route))
  absorption <- if (route == "oral" || "KA" %in% names || grepl("absorption", description, ignore.case = TRUE)) {
    if (grepl("zero[ -]?order", description, ignore.case = TRUE)) "zero_order" else
      if (grepl("transit", description, ignore.case = TRUE)) "transit" else
        if (grepl("lag", description, ignore.case = TRUE)) "first_order_with_lag" else "first_order"
  } else if (grepl("infusion", description, ignore.case = TRUE)) "infusion" else "direct"
  nonlinear <- grepl("michaelis|menten|saturab|nonlinear|target.mediated", description,
                     ignore.case = TRUE)
  parameterization <- if (any(names %in% c("K", "K10", "K12", "K21", "K13", "K31"))) {
    "micro_rate_constants"
  } else if (any(names %in% c("CL", "CL/F")) &&
             (any(grepl("^V", names)) || any(names %in% c("VC", "VP")))) {
    "clearance_volume"
  } else "unknown"
  list(
    compartments = list(count = if (is.finite(compartments)) compartments else NULL,
                        names = character()),
    input = list(route = if (nzchar(route)) route else NULL, process = absorption,
                 depot = absorption %in% c("first_order", "first_order_with_lag", "transit")),
    elimination = list(type = if (nonlinear) "nonlinear" else "linear",
                       from = "central"),
    parameterization = list(type = parameterization, parameters = names),
    description = description
  )
}

#' Infer an executable NONMEM implementation from model semantics
#'
#' Reported ADVAN/TRANS values are retained as reported. Otherwise common
#' linear one-, two-, and three-compartment models are mapped deterministically;
#' ambiguous/general models retain candidates and require review.
#'
#' @param structural_model Structural-model object.
#' @param route Administration route.
#' @param parameters Extracted parameter object.
#' @param dose Optional dose description.
#' @return Implementation record with provenance, rationale, and alternatives.
#' @export
library_infer_implementation <- function(structural_model, route = NULL,
                                         parameters = list(), dose = NULL) {
  structural_model <- structural_model %||% list()
  canonical <- structural_model$canonical %||%
    .library_structural_canonical(structural_model, route, parameters)
  reported_advan <- as.integer(.library_semantic_number(structural_model$advan) %||% NA_integer_)
  reported_trans <- as.integer(.library_semantic_number(structural_model$trans) %||% NA_integer_)
  ncomp <- as.integer(.library_semantic_number(canonical$compartments$count) %||% NA_integer_)
  route <- tolower(.library_semantic_scalar(canonical$input$route %||% route))
  oral <- route %in% c("oral", "po", "per os") || isTRUE(canonical$input$depot)
  linear <- identical(.library_semantic_scalar(canonical$elimination$type), "linear")
  parameterization <- .library_semantic_scalar(canonical$parameterization$type, "unknown")
  inferred_advan <- if (linear && is.finite(ncomp)) switch(
    as.character(ncomp),
    `1` = if (oral) 2L else 1L,
    `2` = if (oral) 4L else 3L,
    `3` = if (oral) 12L else 11L,
    NA_integer_
  ) else NA_integer_
  advan <- if (is.finite(reported_advan)) reported_advan else inferred_advan
  default_trans <- if (is.finite(advan)) {
    if (parameterization == "micro_rate_constants") 1L else
      if (advan %in% c(1L, 2L)) 2L else if (advan %in% c(3L, 4L, 11L, 12L)) 4L else NA_integer_
  } else NA_integer_
  trans <- if (is.finite(reported_trans)) reported_trans else default_trans
  reported <- is.finite(reported_advan) || is.finite(reported_trans)
  resolved <- is.finite(advan) && is.finite(trans)
  status <- if (reported && resolved) "reported" else if (resolved) "inferred" else "unresolved"
  confidence <- if (status == "reported") 1 else if (resolved && parameterization != "unknown") 0.95 else
    if (resolved) 0.82 else 0.25
  rationale <- if (status == "reported") {
    "ADVAN and/or TRANS were explicitly reported in the extracted evidence."
  } else if (resolved) {
    paste0(ncomp, "-compartment ", if (oral) "extravascular" else "direct/IV",
           " input with ", canonical$elimination$type, " elimination and ",
           parameterization, " parameterization.")
  } else {
    "The reported structure does not identify a unique built-in ADVAN/TRANS implementation."
  }
  alternatives <- if (!resolved || !linear) list(
    list(advan = 6L, trans = NULL, reason = "General ODE implementation"),
    list(advan = 13L, trans = NULL, reason = "Stiff/general ODE implementation when required")
  ) else list()
  list(
    engine = "NONMEM",
    advan = if (is.finite(advan)) advan else NULL,
    trans = if (is.finite(trans)) trans else NULL,
    status = status,
    confidence = confidence,
    rationale = rationale,
    evidence = .library_semantic_source(
      if (reported) "reported" else if (resolved) "inferred" else "unresolved",
      "", paste(.library_semantic_scalar(structural_model$description),
                .library_semantic_scalar(dose)), confidence
    ),
    alternatives = alternatives,
    review_required = !reported || !resolved
  )
}

#' Enrich a LibeRary extraction with executable semantics
#'
#' This is backward compatible: legacy population, route, subject-count,
#' structural-model, and parameter fields remain in place.
#'
#' @param extraction Parsed literature extraction.
#' @param source_locator Optional source locator used for appendix-derived data.
#' @return Enriched extraction.
#' @export
library_model_enrich <- function(extraction, source_locator = "") {
  if (is.null(extraction) || !is.list(extraction)) stop("`extraction` must be a list.", call. = FALSE)
  extraction$population_details <- library_population_normalize(
    extraction$population_details %||% extraction$population,
    extraction$n_subjects, source_locator
  )
  if (!is.null(extraction$population_details$n_total)) {
    extraction$n_subjects <- extraction$population_details$n_total
  }
  extraction$dosing <- library_dosing_normalize(
    extraction$dosing %||% extraction$dose %||% extraction$dose_description,
    extraction$route, source_locator
  )
  structural <- extraction$structural_model %||% list(
    advan = NULL, trans = NULL, compartments = NULL, description = ""
  )
  structural$canonical <- structural$canonical %||%
    .library_structural_canonical(structural, extraction$route, extraction$parameters)
  implementation <- library_infer_implementation(
    structural, extraction$route, extraction$parameters,
    if (length(extraction$dosing)) extraction$dosing[[1L]]$raw else NULL
  )
  implementations <- structural$implementations %||% list()
  if (is.list(implementations) && length(implementations) && !is.null(names(implementations)) &&
      any(c("engine", "advan", "trans", "status") %in% names(implementations))) {
    implementations <- list(implementations)
  }
  implementations <- Filter(is.list, implementations)
  if (!length(implementations)) implementations <- list(implementation)
  structural$implementations <- implementations
  if (is.null(structural$advan) && !is.null(implementation$advan)) structural$advan <- implementation$advan
  if (is.null(structural$trans) && !is.null(implementation$trans)) structural$trans <- implementation$trans
  if (is.null(structural$compartments)) {
    structural$compartments <- structural$canonical$compartments$count
  }
  extraction$structural_model <- structural
  extraction$reproduction_targets <- extraction$reproduction_targets %||% list()
  extraction$semantic_version <- "1.0.0"
  extraction
}

.library_semantic_source_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    status = list(type = "string", enum = c("reported", "derived", "inferred", "unresolved", "missing")),
    source_locator = list(type = "string"), evidence = list(type = "string"),
    confidence = list(type = "number", minimum = 0, maximum = 1)
  ), required = c("status", "source_locator", "evidence", "confidence")
)

.library_summary_statistic_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    type = list(type = "string", enum = c(
      "mean_sd", "mean_se", "median_range", "median_quantiles", "quantiles",
      "range", "geometric_mean_cv", "count_proportion", "individual_values",
      "reported_center"
    )),
    mean = list(type = c("number", "null")), sd = list(type = c("number", "null")),
    se = list(type = c("number", "null")), median = list(type = c("number", "null")),
    minimum = list(type = c("number", "null")), maximum = list(type = c("number", "null")),
    value = list(type = c("number", "null")), statistic = list(type = c("string", "null")),
    cv_percent = list(type = c("number", "null")), count = list(type = c("number", "null")),
    proportion = list(type = c("number", "null")),
    quantiles = list(type = "array", items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(probability = list(type = "number"), value = list(type = "number")),
      required = c("probability", "value")
    )),
    values = list(type = "array", items = list(type = "number"))
  ), required = c("type", "mean", "sd", "se", "median", "minimum", "maximum",
                  "value", "statistic", "cv_percent", "count", "proportion", "quantiles", "values")
)

.library_structural_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    advan = list(type = c("integer", "null")), trans = list(type = c("integer", "null")),
    compartments = list(type = c("integer", "null")), description = list(type = "string"),
    canonical = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        compartments = list(type = "object", additionalProperties = FALSE,
          properties = list(count = list(type = c("integer", "null")),
                            names = list(type = "array", items = list(type = "string"))),
          required = c("count", "names")),
        input = list(type = "object", additionalProperties = FALSE,
          properties = list(route = list(type = c("string", "null")), process = list(type = "string"),
                            depot = list(type = "boolean")),
          required = c("route", "process", "depot")),
        elimination = list(type = "object", additionalProperties = FALSE,
          properties = list(type = list(type = "string"), from = list(type = "string")),
          required = c("type", "from")),
        parameterization = list(type = "object", additionalProperties = FALSE,
          properties = list(type = list(type = "string"),
                            parameters = list(type = "array", items = list(type = "string"))),
          required = c("type", "parameters")),
        description = list(type = "string")
      ), required = c("compartments", "input", "elimination", "parameterization", "description")
    ),
    implementations = list(type = "array", items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        engine = list(type = "string"), advan = list(type = c("integer", "null")),
        trans = list(type = c("integer", "null")),
        status = list(type = "string", enum = c("reported", "derived", "inferred", "unresolved")),
        confidence = list(type = "number", minimum = 0, maximum = 1),
        rationale = list(type = "string"), evidence = .library_semantic_source_schema(),
        alternatives = list(type = "array", items = list(
          type = "object", additionalProperties = FALSE,
          properties = list(advan = list(type = c("integer", "null")),
                            trans = list(type = c("integer", "null")), reason = list(type = "string")),
          required = c("advan", "trans", "reason")
        )), review_required = list(type = "boolean")
      ), required = c("engine", "advan", "trans", "status", "confidence", "rationale",
                      "evidence", "alternatives", "review_required")
    ))
  ), required = c("advan", "trans", "compartments", "description", "canonical", "implementations")
)

.library_population_schema <- function() list(
  type = "object", additionalProperties = FALSE,
  properties = list(
    raw = list(type = c("string", "null")),
    n_total = list(type = c("number", "null")),
    cohorts = list(type = "array", maxItems = 12L, items = list(
      type = "object", additionalProperties = FALSE,
      properties = list(
        id = list(type = "string"), label = list(type = "string"),
        role = list(type = "string"),
        n = list(type = "object", additionalProperties = FALSE,
          properties = list(enrolled = list(type = c("integer", "null")),
                            dosed = list(type = c("integer", "null")),
                            analysed = list(type = c("integer", "null"))),
          required = c("enrolled", "dosed", "analysed")),
        health_status = list(type = "string"),
        country = list(type = c("string", "null")),
        descriptors = list(type = "array", maxItems = 30L, items = list(
          type = "object", additionalProperties = FALSE,
          properties = list(
            name = list(type = "string"), unit = list(type = c("string", "null")),
            statistics = list(type = "array", maxItems = 8L,
                              items = .library_summary_statistic_schema()),
            categories = list(type = "array", maxItems = 30L, items = list(
              type = "object", additionalProperties = FALSE,
              properties = list(label = list(type = "string"), count = list(type = c("integer", "null")),
                                proportion = list(type = c("number", "null"))),
              required = c("label", "count", "proportion")
            )), source = .library_semantic_source_schema()
          ), required = c("name", "unit", "statistics", "categories", "source")
        )),
        pharmacogenetics = list(type = "array", maxItems = 30L, items = list(
          type = "object", additionalProperties = FALSE,
          properties = list(
            gene = list(type = "string"), variant = list(type = c("string", "null")),
            diplotype = list(type = c("string", "null")), phenotype = list(type = c("string", "null")),
            n = list(type = c("integer", "null")), proportion = list(type = c("number", "null")),
            assay = list(type = c("string", "null")), source = .library_semantic_source_schema()
          ), required = c("gene", "variant", "diplotype", "phenotype", "n", "proportion", "assay", "source")
        )),
        source = .library_semantic_source_schema()
      ), required = c("id", "label", "role", "n", "health_status", "country",
                      "descriptors", "pharmacogenetics", "source")
    )),
    assumptions = list(type = "array", maxItems = 20L, items = list(type = "string")),
    source = .library_semantic_source_schema()
  ), required = c("raw", "n_total", "cohorts", "assumptions", "source")
)

.library_dosing_schema <- function() list(
  type = "array", maxItems = 30L, items = list(
    type = "object", additionalProperties = FALSE,
    properties = list(
      cohort_id = list(type = "string"), route = list(type = c("string", "null")),
      administration = list(type = "string"), amount = list(type = c("number", "null")),
      amount_unit = list(type = c("string", "null")), interval = list(type = c("number", "null")),
      interval_unit = list(type = c("string", "null")), duration = list(type = c("number", "null")),
      duration_unit = list(type = c("string", "null")), repetitions = list(type = c("integer", "null")),
      steady_state = list(type = "boolean"), raw = list(type = c("string", "null")),
      source = .library_semantic_source_schema()
    ), required = c("cohort_id", "route", "administration", "amount", "amount_unit",
                    "interval", "interval_unit", "duration", "duration_unit", "repetitions",
                    "steady_state", "raw", "source")
  )
)

.library_reproduction_target_schema <- function() list(
  type = "array", maxItems = 30L, items = list(
    type = "object", additionalProperties = FALSE,
    properties = list(
      id = list(type = "string"), kind = list(type = "string",
        enum = c("concentration_time", "concentration_distribution", "pk_summary", "other")),
      source_type = list(type = "string", enum = c("figure", "table", "text", "supplement")),
      source_locator = list(type = "string"), cohort_id = list(type = c("string", "null")),
      analyte = list(type = c("string", "null")), statistic = list(type = "string"),
      x_unit = list(type = c("string", "null")), y_unit = list(type = c("string", "null")),
      scale = list(type = "string", enum = c("linear", "log", "unknown")),
      points = list(type = "array", maxItems = 250L, items = list(type = "object",
        properties = list(time = list(type = c("number", "null")), value = list(type = "number"),
                          lower = list(type = c("number", "null")), upper = list(type = c("number", "null"))),
        required = c("time", "value", "lower", "upper"))),
      evidence = list(type = "string"), confidence = list(type = "number", minimum = 0, maximum = 1)
    ), required = c("id", "kind", "source_type", "source_locator", "cohort_id", "analyte",
                    "statistic", "x_unit", "y_unit", "scale", "points", "evidence", "confidence")
  )
)
