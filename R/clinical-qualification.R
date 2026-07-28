.library_clinical_statuses <- c(
  "unreviewed", "research_only", "candidate", "qualified", "suspended", "retired"
)

.library_character_set <- function(value) {
  value <- trimws(as.character(unlist(value %||% character(), use.names = FALSE)))
  unique(value[nzchar(value) & !is.na(value)])
}

.library_clinical_scope_defaults <- function() list(
  drugs = character(),
  analytes = character(),
  metabolites = character(),
  indications = character(),
  routes = character(),
  formulations = character(),
  regimens = character(),
  endpoint_ids = character(),
  endpoint_kinds = character(),
  population = list(),
  covariates = list(
    required = character(),
    optional = character(),
    supported_imputation = list(),
    ranges = list()
  ),
  assays = list(
    required = FALSE,
    matrices = character(),
    methods = character(),
    units = character()
  )
)

.library_merge_named <- function(base, update) {
  if (!is.list(update)) stop("Qualification scope sections must be lists.", call. = FALSE)
  for (name in names(update)) {
    if (is.list(base[[name]]) && is.list(update[[name]])) {
      base[[name]] <- .library_merge_named(base[[name]], update[[name]])
    } else {
      base[[name]] <- update[[name]]
    }
  }
  base
}

.library_validate_range <- function(value, label) {
  if (!is.list(value)) stop(label, " must be a list.", call. = FALSE)
  minimum <- suppressWarnings(as.numeric(value$min %||% -Inf))
  maximum <- suppressWarnings(as.numeric(value$max %||% Inf))
  if (length(minimum) != 1L || length(maximum) != 1L ||
      is.na(minimum) || is.na(maximum) || minimum > maximum) {
    stop(label, " has invalid min/max limits.", call. = FALSE)
  }
  value$min <- minimum
  value$max <- maximum
  value$unit <- as.character(value$unit %||% "")[[1L]]
  value$required <- isTRUE(value$required)
  value$hard <- !identical(value$hard, FALSE)
  value$covariate <- as.character(value$covariate %||% "")[[1L]]
  value
}

#' Define a scoped clinical-use qualification
#'
#' A clinical-use qualification applies to one immutable model version and a
#' specific population, indication, route, assay, endpoint, and governance
#' context. It is deliberately not a universal approval flag.
#'
#' @param status Qualification lifecycle status.
#' @param scope Applicability scope. See Details.
#' @param evidence Validation and implementation evidence.
#' @param governance Issuer, reviewer, dates, and optional signature metadata.
#' @param limitations Known limitations shown to downstream users.
#' @param out_of_domain_rules Structured rules or explanatory metadata.
#' @param qualification_id Optional stable identifier.
#' @param version Qualification-record version.
#' @param supersedes Identifier of an earlier record superseded by this one.
#' @param notes Additional review notes.
#' @return A serializable `library_clinical_qualification`.
#' @details
#' `scope` can restrict drugs, analytes, indications, routes, formulations,
#' endpoint ids/kinds, population ranges, covariates, and assays. A `qualified`
#' record must name at least one drug, one endpoint id or kind, and its issuer
#' and reviewer. The assertion belongs to that issuer; LibeRary does not certify
#' clinical suitability.
#' @export
library_clinical_qualification <- function(
    status = c("candidate", "qualified", "research_only", "unreviewed",
               "suspended", "retired"),
    scope = list(), evidence = list(), governance = list(),
    limitations = character(), out_of_domain_rules = list(),
    qualification_id = NULL, version = "1.0.0", supersedes = "", notes = "") {
  status <- match.arg(status, .library_clinical_statuses)
  if (!is.list(scope) || !is.list(evidence) || !is.list(governance) ||
      !is.list(out_of_domain_rules)) {
    stop("Scope, evidence, governance, and out-of-domain rules must be lists.",
         call. = FALSE)
  }
  scope <- .library_merge_named(.library_clinical_scope_defaults(), scope)
  for (name in c(
    "drugs", "analytes", "metabolites", "indications", "routes",
    "formulations", "regimens", "endpoint_ids", "endpoint_kinds"
  )) scope[[name]] <- .library_character_set(scope[[name]])
  for (name in c("required", "optional")) {
    scope$covariates[[name]] <- .library_character_set(scope$covariates[[name]])
  }
  for (name in c("matrices", "methods", "units")) {
    scope$assays[[name]] <- .library_character_set(scope$assays[[name]])
  }
  scope$assays$required <- isTRUE(scope$assays$required)
  for (name in names(scope$population)) {
    scope$population[[name]] <- .library_validate_range(
      scope$population[[name]], paste0("scope$population$", name)
    )
  }
  for (name in names(scope$covariates$ranges)) {
    scope$covariates$ranges[[name]] <- .library_validate_range(
      scope$covariates$ranges[[name]], paste0("scope$covariates$ranges$", name)
    )
  }
  for (name in names(scope$covariates$supported_imputation)) {
    policy <- scope$covariates$supported_imputation[[name]]
    if (!is.list(policy)) {
      stop("Supported-imputation policies must be named lists.", call. = FALSE)
    }
    policy$methods <- .library_character_set(policy$methods)
    policy$max_age_hours <- suppressWarnings(as.numeric(policy$max_age_hours %||% Inf))
    if (length(policy$max_age_hours) != 1L || is.na(policy$max_age_hours) ||
        policy$max_age_hours < 0) {
      stop("Imputation max_age_hours must be non-negative.", call. = FALSE)
    }
    scope$covariates$supported_imputation[[name]] <- policy
  }
  governance$issuer <- trimws(as.character(governance$issuer %||% "")[[1L]])
  governance$reviewer <- trimws(as.character(governance$reviewer %||% "")[[1L]])
  governance$valid_from <- as.character(
    governance$valid_from %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )[[1L]]
  governance$review_due <- as.character(governance$review_due %||% "")[[1L]]
  if (identical(status, "qualified")) {
    if (!length(scope$drugs)) {
      stop("A qualified record must identify at least one drug.", call. = FALSE)
    }
    if (!length(scope$endpoint_ids) && !length(scope$endpoint_kinds)) {
      stop("A qualified record must identify an endpoint id or endpoint kind.",
           call. = FALSE)
    }
    if (!nzchar(governance$issuer) || !nzchar(governance$reviewer)) {
      stop("A qualified record requires an issuer and reviewer.", call. = FALSE)
    }
  }
  created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  qualification_id <- as.character(qualification_id %||% "")[[1L]]
  if (!nzchar(qualification_id)) {
    qualification_id <- paste0(
      "cq-", substr(digest::digest(
        list(status, scope, evidence, governance, created_at),
        algo = "sha256", serialize = TRUE
      ), 1L, 20L)
    )
  }
  qualification <- structure(list(
    schema = "liberary.clinical_qualification",
    schema_version = "1.0.0",
    qualification_id = .library_valid_id(qualification_id),
    version = as.character(version)[[1L]],
    status = status,
    scope = scope,
    evidence = evidence,
    governance = governance,
    limitations = .library_character_set(limitations),
    out_of_domain_rules = out_of_domain_rules,
    supersedes = as.character(supersedes %||% "")[[1L]],
    notes = as.character(notes %||% "")[[1L]],
    created_at = created_at
  ), class = c("library_clinical_qualification", "list"))
  library_clinical_qualification_validate(qualification)
}

#' Validate a clinical-use qualification
#' @param qualification Qualification record.
#' @return Validated qualification.
#' @export
library_clinical_qualification_validate <- function(qualification) {
  if (is.list(qualification) &&
      identical(qualification$schema, "liberary.clinical_qualification") &&
      !inherits(qualification, "library_clinical_qualification")) {
    class(qualification) <- c("library_clinical_qualification", "list")
  }
  if (!inherits(qualification, "library_clinical_qualification") ||
      !identical(qualification$schema, "liberary.clinical_qualification") ||
      !identical(as.character(qualification$schema_version), "1.0.0")) {
    stop("Invalid LibeRary clinical-use qualification.", call. = FALSE)
  }
  .library_valid_id(qualification$qualification_id)
  if (!qualification$status %in% .library_clinical_statuses) {
    stop("Unknown clinical qualification status.", call. = FALSE)
  }
  if (!is.list(qualification$scope) || !is.list(qualification$evidence) ||
      !is.list(qualification$governance)) {
    stop("Invalid clinical qualification sections.", call. = FALSE)
  }
  qualification
}

#' @export
print.library_clinical_qualification <- function(x, ...) {
  cat("LibeRary clinical-use qualification\n")
  cat("  id:", x$qualification_id, " status:", x$status, "\n")
  if (length(x$scope$drugs)) cat("  drugs:", paste(x$scope$drugs, collapse = ", "), "\n")
  cat("  issuer:", x$governance$issuer %||% "", " reviewer:",
      x$governance$reviewer %||% "", "\n")
  invisible(x)
}

.library_current_clinical_records <- function(records) {
  if (!length(records)) return(list())
  records <- lapply(records, library_clinical_qualification_validate)
  superseded <- .library_character_set(lapply(records, `[[`, "supersedes"))
  records[!vapply(records, function(record) {
    record$qualification_id %in% superseded
  }, logical(1))]
}

.library_clinical_review_overdue <- function(record) {
  due <- as.character(record$governance$review_due %||% "")[[1L]]
  if (!nzchar(due)) return(FALSE)
  parsed <- suppressWarnings(as.POSIXct(due, tz = "UTC"))
  is.na(parsed) || parsed < Sys.time()
}

#' Attach a clinical-use qualification to a catalogue model version
#'
#' The operation appends an auditable record. Existing records are not edited;
#' suspension, retirement, and revised scopes should supersede an earlier id.
#'
#' @param library_id Catalogue model identifier.
#' @param qualification Qualification from
#'   [library_clinical_qualification()].
#' @param root Catalogue root.
#' @return Updated manifest, invisibly.
#' @export
library_clinical_qualify <- function(
    library_id, qualification, root = library_catalog_root()) {
  qualification <- library_clinical_qualification_validate(qualification)
  .library_with_lock(root, {
    manifest <- .library_read_manifest(library_id, root)
    if (identical(qualification$status, "qualified") &&
        !identical(tolower(as.character(manifest$status %||% "")), "validated")) {
      stop(
        "Only a validated catalogue model can receive a qualified clinical-use record.",
        call. = FALSE
      )
    }
    records <- manifest$qualification$clinical_use %||% list()
    ids <- vapply(records, function(record) {
      as.character(record$qualification_id %||% "")
    }, character(1))
    if (qualification$qualification_id %in% ids) {
      stop("The clinical qualification id already exists.", call. = FALSE)
    }
    if (nzchar(qualification$supersedes) &&
        !qualification$supersedes %in% ids) {
      stop("`supersedes` does not identify an existing qualification.", call. = FALSE)
    }
    artifact <- file.path(.library_entry_dir(library_id, root),
                          as.character(manifest$model$artifact %||% "model.ctl"))
    qualification$model <- list(
      library_id = library_id,
      version = as.character(manifest$version %||% ""),
      model_sha256 = if (file.exists(artifact)) {
        digest::digest(file = artifact, algo = "sha256", serialize = FALSE)
      } else "",
      manifest_schema_version = as.character(manifest$schema_version %||% "")
    )
    manifest$qualification$clinical_use <- c(records, list(unclass(qualification)))
    manifest$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    .library_atomic_write(
      manifest, file.path(.library_entry_dir(library_id, root), "manifest.json")
    )
    .library_rebuild_index(root)
    invisible(manifest)
  })
}

#' List scoped clinical-use qualifications
#'
#' @param library_id Optional catalogue model identifier.
#' @param status Optional qualification-status filter.
#' @param current Return only records that have not been superseded.
#' @param root Catalogue root.
#' @return A list of records enriched with catalogue title and model version.
#' @export
library_clinical_qualifications <- function(
    library_id = NULL, status = NULL, current = TRUE,
    root = library_catalog_root()) {
  ids <- if (is.null(library_id)) {
    as.character(library_list(root = root)$library_id)
  } else .library_valid_id(library_id)
  output <- list()
  for (id in ids) {
    manifest <- .library_read_manifest(id, root)
    records <- manifest$qualification$clinical_use %||% list()
    if (isTRUE(current)) records <- .library_current_clinical_records(records)
    for (record in records) {
      record <- library_clinical_qualification_validate(record)
      if (!is.null(status) && !record$status %in% status) next
      record$library_id <- id
      record$model_version <- as.character(manifest$version %||% "")
      record$title <- as.character(manifest$title %||% id)
      output[[length(output) + 1L]] <- record
    }
  }
  output
}
