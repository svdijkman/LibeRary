.library_reproduction_points <- function(targets) {
  rows <- list()
  for (target in targets %||% list()) {
    if (!identical(target$kind %||% "", "concentration_time")) next
    for (point in target$points %||% list()) {
      time <- .library_semantic_number(point$time)
      value <- .library_semantic_number(point$value)
      if (is.null(time) || is.null(value)) next
      rows[[length(rows) + 1L]] <- data.frame(
        target_id = .library_semantic_scalar(target$id, "target"),
        time = time, value = value,
        lower = .library_semantic_number(point$lower) %||% NA_real_,
        upper = .library_semantic_number(point$upper) %||% NA_real_,
        statistic = .library_semantic_scalar(target$statistic, "unknown"),
        source_locator = .library_semantic_scalar(target$source_locator),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(
    target_id = character(), time = numeric(), value = numeric(), lower = numeric(),
    upper = numeric(), statistic = character(), source_locator = character(),
    stringsAsFactors = FALSE
  )
}

.library_reproduction_statistic <- function(statistics) {
  for (statistic in statistics %||% list()) {
    type <- statistic$type %||% ""
    value <- switch(type,
      mean_sd = statistic$mean,
      mean_se = statistic$mean,
      median_range = statistic$median,
      median_quantiles = statistic$median,
      reported_center = statistic$value,
      NULL
    )
    value <- .library_semantic_number(value)
    if (!is.null(value)) return(value)
  }
  NULL
}

.library_reproduction_covariates <- function(population_details) {
  cohorts <- population_details$cohorts %||% list()
  if (!length(cohorts)) return(list())
  descriptors <- cohorts[[1L]]$descriptors %||% list()
  output <- list()
  column_names <- c(age = "AGE", weight = "WT", height = "HT", bmi = "BMI", bsa = "BSA")
  for (descriptor in descriptors) {
    key <- tolower(.library_semantic_scalar(descriptor$name))
    column <- unname(column_names[key])
    if (!length(column) || is.na(column) || !nzchar(column)) {
      column <- toupper(gsub("[^A-Za-z0-9_]", "_", key))
      column <- gsub("_+", "_", gsub("^_+|_+$", "", column))
    }
    value <- .library_reproduction_statistic(descriptor$statistics)
    if (nzchar(column) && !is.null(value)) output[[column]] <- value
  }
  output
}

.library_reproduction_amount_mg <- function(regimen, covariates) {
  amount <- .library_semantic_number(regimen$amount)
  unit <- tolower(gsub("\\s+", "", .library_semantic_scalar(regimen$amount_unit)))
  unit <- sub("^mcg", "ug", unit)
  unit <- sub("^\u00b5g", "ug", unit, perl = TRUE)
  if (is.null(amount)) return(list(value = NULL, assumption = NULL, blocker = "dose_amount_missing"))
  if (!nzchar(unit)) return(list(value = NULL, assumption = NULL, blocker = "dose_unit_missing"))
  per_kg <- grepl("/kg", unit, fixed = TRUE)
  per_m2 <- grepl("/m2", unit, fixed = TRUE)
  base <- sub("/.*$", "", unit)
  if (!base %in% c("g", "mg", "ug")) {
    return(list(value = NULL, assumption = NULL, blocker = "dose_unit_not_convertible_to_mg"))
  }
  value <- switch(base, g = amount * 1000, ug = amount / 1000, mg = amount)
  assumption <- character()
  if (per_kg) {
    weight <- .library_semantic_number(covariates$WT)
    if (is.null(weight)) {
      return(list(value = NULL, assumption = NULL, blocker = "weight_required_for_weight_normalized_dose"))
    }
    value <- value * weight
    assumption <- paste0("Weight-normalized dose was converted with representative WT=", weight, " kg.")
  }
  if (per_m2) {
    bsa <- .library_semantic_number(covariates$BSA)
    if (is.null(bsa)) {
      return(list(value = NULL, assumption = NULL, blocker = "bsa_required_for_surface_area_normalized_dose"))
    }
    value <- value * bsa
    assumption <- paste0("Surface-area-normalized dose was converted with representative BSA=", bsa, " m2.")
  }
  list(value = value, assumption = assumption, blocker = NULL)
}

.library_reproduction_hours <- function(value, unit, field) {
  value <- .library_semantic_number(value)
  if (is.null(value)) return(list(value = NULL, blocker = NULL))
  unit <- tolower(.library_semantic_scalar(unit))
  multiplier <- if (grepl("^(h|hr|hour)", unit)) 1 else
    if (grepl("^(min|minute)", unit)) 1 / 60 else
      if (grepl("^(d|day)", unit)) 24 else NA_real_
  if (!is.finite(multiplier)) {
    return(list(value = NULL, blocker = paste0(field, "_unit_not_convertible_to_hours")))
  }
  list(value = value * multiplier, blocker = NULL)
}

.library_reproduction_times <- function(points, horizon = 24, n = 49L) {
  values <- sort(unique(as.numeric(points$time)))
  values <- values[is.finite(values) & values >= 0]
  if (length(values)) sort(unique(c(0, values))) else seq(0, horizon, length.out = n)
}

.library_reproduction_events <- function(plan, n_subjects = 1L) {
  regimen <- plan$regimen
  times <- plan$times
  n_subjects <- max(1L, as.integer(n_subjects))
  pieces <- vector("list", n_subjects)
  oral <- plan$implementation$advan %in% c(2L, 4L, 12L)
  dose_cmt <- if (oral) 1L else 1L
  obs_cmt <- if (oral) 2L else 1L
  interval_hours <- .library_reproduction_hours(
    regimen$interval, regimen$interval_unit, "dose_interval"
  )$value
  duration_hours <- .library_reproduction_hours(
    regimen$duration, regimen$duration_unit, "infusion_duration"
  )$value
  for (id in seq_len(n_subjects)) {
    doses <- if (isTRUE(regimen$steady_state)) 0 else {
      repetitions <- as.integer(regimen$repetitions %||% 1L)
      if (is.finite(repetitions) && repetitions > 1L && !is.null(interval_hours)) {
        seq(0, by = interval_hours, length.out = repetitions)
      } else 0
    }
    dose_rows <- data.frame(
      ID = id, TIME = doses, EVID = 1L, AMT = plan$amount_mg,
      RATE = if (identical(regimen$administration, "infusion") &&
                 !is.null(duration_hours)) {
        plan$amount_mg / duration_hours
      } else 0,
      II = if (isTRUE(regimen$steady_state) && !is.null(interval_hours)) interval_hours else 0,
      SS = if (isTRUE(regimen$steady_state)) 1L else 0L,
      CMT = dose_cmt, DV = NA_real_, MDV = 1L, stringsAsFactors = FALSE
    )
    obs_rows <- data.frame(
      ID = id, TIME = times, EVID = 0L, AMT = 0, RATE = 0, II = 0, SS = 0L,
      CMT = obs_cmt, DV = NA_real_, MDV = 0L, stringsAsFactors = FALSE
    )
    frame <- rbind(dose_rows, obs_rows)
    for (name in names(plan$covariates)) frame[[name]] <- plan$covariates[[name]]
    pieces[[id]] <- frame[order(frame$TIME, -frame$EVID), , drop = FALSE]
  }
  output <- do.call(rbind, pieces)
  rownames(output) <- NULL
  output
}

#' Prepare an auditable article-reproduction plan
#'
#' A plan is conservative by default: generated parameter defaults block
#' execution unless explicitly allowed. Missing concentration targets do not
#' prevent simulation, but they do prevent quantitative reproduction scoring.
#'
#' @param extraction Enriched or legacy LibeRary extraction.
#' @param times Optional simulation times.
#' @param n_subjects Optional synthetic-population size.
#' @param allow_generated_defaults Permit generated model parameters.
#' @return A `library_reproduction_plan`.
#' @export
library_reproduction_plan <- function(extraction, times = NULL, n_subjects = NULL,
                                      allow_generated_defaults = FALSE) {
  extraction <- library_model_enrich(extraction)
  implementations <- extraction$structural_model$implementations %||% list()
  implementation <- if (is.list(implementations) && length(implementations) && is.list(implementations[[1L]])) {
    implementations[[1L]]
  } else library_infer_implementation(extraction$structural_model, extraction$route,
                                      extraction$parameters)
  control <- ingest_map_to_ctl(extraction)
  mapping <- attr(control, "mapping")
  regimens <- extraction$dosing %||% list()
  regimen <- if (length(regimens)) regimens[[1L]] else list()
  covariates <- .library_reproduction_covariates(extraction$population_details)
  amount <- .library_reproduction_amount_mg(regimen, covariates)
  interval <- .library_reproduction_hours(regimen$interval, regimen$interval_unit, "dose_interval")
  duration <- .library_reproduction_hours(regimen$duration, regimen$duration_unit, "infusion_duration")
  points <- .library_reproduction_points(extraction$reproduction_targets)
  blockers <- character()
  if (is.null(implementation$advan) || is.null(implementation$trans)) blockers <- c(blockers, "implementation_unresolved")
  if (!is.null(amount$blocker)) blockers <- c(blockers, amount$blocker)
  if (!is.null(interval$blocker)) blockers <- c(blockers, interval$blocker)
  if (!is.null(duration$blocker)) blockers <- c(blockers, duration$blocker)
  if (isTRUE(regimen$steady_state) && is.null(interval$value)) {
    blockers <- c(blockers, "steady_state_interval_missing")
  }
  if (identical(regimen$administration, "infusion") && is.null(duration$value)) {
    blockers <- c(blockers, "infusion_duration_missing")
  }
  if (!is.null(mapping$validation_error)) blockers <- c(blockers, "model_compilation_failed")
  if (!isTRUE(allow_generated_defaults) && length(mapping$generated_defaults %||% character())) {
    blockers <- c(blockers, "generated_parameter_defaults_present")
  }
  n_total <- .library_semantic_number(extraction$population_details$n_total)
  if (is.null(n_subjects)) n_subjects <- if (is.null(n_total)) 100L else min(100L, max(1L, as.integer(n_total)))
  default_horizon <- interval$value %||% 24
  plan <- list(
    schema_version = "1.0.0",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    eligible = !length(blockers),
    scorable = nrow(points) > 0L,
    blockers = unique(blockers),
    assumptions = unique(c(
      extraction$population_details$assumptions %||% character(),
      amount$assumption %||% character(),
      if (!nrow(points)) "No digitized concentration target is available; execution can only test model plausibility." else character(),
      if (length(mapping$generated_defaults %||% character()))
        paste("Generated defaults:", paste(mapping$generated_defaults, collapse = ", ")) else character()
    )),
    implementation = implementation,
    mapping = mapping,
    regimen = regimen,
    amount_mg = amount$value,
    covariates = covariates,
    n_subjects = as.integer(n_subjects),
    times = as.numeric(times %||% .library_reproduction_times(points, default_horizon)),
    targets = points,
    control_stream = unclass(control),
    source_extraction = extraction
  )
  class(plan) <- c("library_reproduction_plan", "list")
  plan
}

#' Compare simulated and digitized concentration-time targets
#'
#' @param simulated Simulation output from LibeRation.
#' @param targets Target data frame from a reproduction plan.
#' @param value Simulation column, normally `IPRED` or `DV`.
#' @return Per-point comparison and summary metrics.
#' @export
library_reproduction_score <- function(simulated, targets, value = "IPRED") {
  targets <- as.data.frame(targets, stringsAsFactors = FALSE)
  if (!nrow(targets)) return(list(scorable = FALSE, summary = list(), points = data.frame()))
  observed <- simulated[simulated$EVID == 0L & simulated$MDV == 0L, , drop = FALSE]
  if (!value %in% names(observed)) stop("Simulation does not contain `", value, "`.", call. = FALSE)
  split_time <- split(as.numeric(observed[[value]]), observed$TIME)
  summary <- data.frame(
    time = as.numeric(names(split_time)),
    simulated = vapply(split_time, stats::median, numeric(1), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  summary <- summary[order(summary$time), , drop = FALSE]
  predicted <- stats::approx(summary$time, summary$simulated, xout = targets$time,
                             rule = 2, ties = "ordered")$y
  points <- targets
  points$simulated <- predicted
  points$residual <- predicted - points$value
  points$relative_error <- points$residual /
    pmax(abs(points$value), sqrt(.Machine$double.eps))
  points$within_reported_interval <- ifelse(
    is.finite(points$lower) & is.finite(points$upper),
    predicted >= points$lower & predicted <= points$upper,
    NA
  )
  scale <- max(diff(range(targets$value, finite = TRUE)), mean(abs(targets$value), na.rm = TRUE),
               sqrt(.Machine$double.eps))
  nrmse <- sqrt(mean(points$residual^2, na.rm = TRUE)) / scale
  coverage <- if (any(!is.na(points$within_reported_interval))) {
    mean(points$within_reported_interval, na.rm = TRUE)
  } else NA_real_
  status <- if (!is.finite(nrmse)) "not_scorable" else if (nrmse <= 0.1) "close_reproduction" else
    if (nrmse <= 0.25) "approximate_reproduction" else "material_discrepancy"
  list(
    scorable = TRUE,
    summary = list(
      n = nrow(points), nrmse = nrmse,
      median_absolute_relative_error = stats::median(abs(points$relative_error), na.rm = TRUE),
      interval_coverage = coverage, status = status,
      interpretation = "Reproduction agreement is evidence of computational consistency, not independent model validation."
    ),
    points = points
  )
}

.library_reproduction_plot <- function(result, path) {
  typical <- result$typical
  population <- result$population
  targets <- result$score$points
  grDevices::png(path, width = 1200, height = 760, res = 130)
  on.exit(grDevices::dev.off(), add = TRUE)
  obs <- population[population$EVID == 0L & population$MDV == 0L, , drop = FALSE]
  split_time <- split(obs$IPRED, obs$TIME)
  curve <- data.frame(
    time = as.numeric(names(split_time)),
    median = vapply(split_time, stats::median, numeric(1), na.rm = TRUE),
    lower = vapply(split_time, stats::quantile, numeric(1), probs = 0.05, na.rm = TRUE, names = FALSE),
    upper = vapply(split_time, stats::quantile, numeric(1), probs = 0.95, na.rm = TRUE, names = FALSE)
  )
  curve <- curve[order(curve$time), ]
  ylim <- range(c(curve$lower, curve$upper, targets$value %||% numeric()), finite = TRUE)
  graphics::plot(curve$time, curve$median, type = "n", xlab = "Time", ylab = "Concentration",
                 ylim = ylim, main = "Article model reproduction")
  graphics::polygon(c(curve$time, rev(curve$time)), c(curve$lower, rev(curve$upper)),
                    col = grDevices::adjustcolor("#3C9A68", alpha.f = 0.22), border = NA)
  graphics::lines(curve$time, curve$median, col = "#1F6B45", lwd = 2)
  typical_obs <- typical[typical$EVID == 0L & typical$MDV == 0L, , drop = FALSE]
  graphics::lines(typical_obs$TIME, typical_obs$IPRED, col = "#174F35", lty = 2, lwd = 2)
  if (nrow(targets)) graphics::points(targets$time, targets$value, pch = 16, col = "#202B25")
  graphics::legend("topright", c("Population median", "5-95% interval", "Typical profile", "Published target"),
                   col = c("#1F6B45", "#3C9A68", "#174F35", "#202B25"),
                   lty = c(1, 1, 2, NA), pch = c(NA, NA, NA, 16), bty = "n")
}

#' Execute an article-reproduction plan with LibeRation
#'
#' @param x Extraction or object returned by [library_reproduction_plan()].
#' @param output_dir Optional artifact directory.
#' @param nsim Number of population simulations.
#' @param seed RNG seed.
#' @param n_cores Simulation workers.
#' @param allow_generated_defaults Allow generated parameter defaults.
#' @return A `library_reproduction_result`.
#' @export
library_reproduction_run <- function(x, output_dir = NULL, nsim = 200L,
                                     seed = 20260716L, n_cores = 1L,
                                     allow_generated_defaults = FALSE) {
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    stop("Install LibeRation >= 0.6.0 to execute reproduction plans.", call. = FALSE)
  }
  plan <- if (inherits(x, "library_reproduction_plan")) x else
    library_reproduction_plan(x, allow_generated_defaults = allow_generated_defaults)
  if (!isTRUE(plan$eligible)) {
    stop("Reproduction plan is not executable: ", paste(plan$blockers, collapse = ", "), call. = FALSE)
  }
  control <- LibeRation::nm_control_read(plan$control_stream, strict = TRUE)
  typical_events <- .library_reproduction_events(plan, 1L)
  population_events <- .library_reproduction_events(plan, plan$n_subjects)
  typical <- LibeRation::nm_simulate(control$model, typical_events, random_effects = FALSE,
                                     residual = FALSE, seed = seed)
  population <- LibeRation::nm_simulate(
    control$model, population_events, nsim = max(1L, as.integer(nsim)),
    random_effects = nrow(control$model$OMEGAS) > 0L,
    residual = nrow(control$model$SIGMAS) > 0L &&
      !identical(control$model$ERROR_TYPE %||% "none", "none"), seed = seed,
    n_cores = max(1L, as.integer(n_cores))
  )
  value <- if ("DV" %in% names(population) && any(is.finite(population$DV))) "DV" else "IPRED"
  score <- library_reproduction_score(population, plan$targets, value)
  result <- list(
    schema_version = "1.0.0", status = if (isTRUE(score$scorable)) score$summary$status else "simulated_not_scorable",
    plan = plan, typical = typical, population = population, score = score,
    runtime = list(nsim = as.integer(nsim), seed = as.integer(seed), n_cores = as.integer(n_cores)),
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
  class(result) <- c("library_reproduction_result", "list")
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(plan$control_stream, file.path(output_dir, "model.ctl"), useBytes = TRUE)
    utils::write.csv(typical_events, file.path(output_dir, "events-typical.csv"), row.names = FALSE)
    utils::write.csv(population_events, file.path(output_dir, "events-population.csv"), row.names = FALSE)
    utils::write.csv(typical, file.path(output_dir, "simulation-typical.csv"), row.names = FALSE)
    utils::write.csv(population, file.path(output_dir, "simulation-population.csv"), row.names = FALSE)
    if (nrow(score$points)) utils::write.csv(score$points, file.path(output_dir, "comparison.csv"), row.names = FALSE)
    .library_reproduction_plot(result, file.path(output_dir, "reproduction.png"))
    audit <- result
    audit$typical <- NULL
    audit$population <- NULL
    audit$plan$source_extraction <- NULL
    .library_atomic_write(audit, file.path(output_dir, "reproduction.json"))
    result$output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  }
  result
}
