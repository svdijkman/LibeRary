.library_reference_escape_pointer <- function(value) {
  value <- gsub("~", "~0", as.character(value), fixed = TRUE)
  gsub("/", "~1", value, fixed = TRUE)
}

.library_reference_unescape_pointer <- function(value) {
  value <- gsub("~1", "/", as.character(value), fixed = TRUE)
  gsub("~0", "~", value, fixed = TRUE)
}

.library_reference_pointer_tokens <- function(pointer) {
  pointer <- as.character(pointer %||% "")[[1L]]
  if (!nzchar(pointer)) return(character())
  if (!startsWith(pointer, "/")) stop("JSON pointer must start with '/'.", call. = FALSE)
  vapply(strsplit(substring(pointer, 2L), "/", fixed = TRUE)[[1L]],
         .library_reference_unescape_pointer, character(1))
}

.library_reference_is_array <- function(value) {
  is.list(value) && (is.null(names(value)) || !length(names(value)) ||
                       all(!nzchar(names(value))))
}

.library_reference_flatten <- function(value, pointer = "", label = "") {
  leaf <- function(value, pointer, label) {
    list(list(pointer = pointer, field = if (nzchar(label)) label else "(root)", value = value))
  }
  if (is.null(value) || !is.list(value)) return(leaf(value, pointer, label))
  if (!length(value)) return(if (nzchar(pointer)) leaf(value, pointer, label) else list())
  output <- list()
  if (.library_reference_is_array(value)) {
    for (index in seq_along(value)) {
      child_pointer <- paste0(pointer, "/", index - 1L)
      child_label <- paste0(label, "[", index, "]")
      output <- c(output, .library_reference_flatten(value[[index]], child_pointer, child_label))
    }
  } else {
    for (name in names(value)) {
      child_pointer <- paste0(pointer, "/", .library_reference_escape_pointer(name))
      child_label <- if (nzchar(label)) paste(label, name, sep = ".") else name
      output <- c(output, .library_reference_flatten(value[[name]], child_pointer, child_label))
    }
  }
  output
}

.library_reference_json_value <- function(value) {
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA,
                                pretty = FALSE, na = "null"))
}

.library_reference_display_value <- function(value, limit = 220L) {
  text <- if (is.null(value)) {
    "null"
  } else if (is.character(value) && length(value) == 1L) {
    value
  } else {
    .library_reference_json_value(value)
  }
  text <- gsub("[\r\n\t]+", " ", as.character(text))
  text <- trimws(gsub(" +", " ", text))
  if (nchar(text, type = "chars") > limit) paste0(substr(text, 1L, limit - 1L), "\u2026") else text
}

.library_reference_leaf_equal <- function(left, right, relative_tolerance, absolute_tolerance,
                                          pointer = "") {
  if (is.null(left) && is.null(right)) return(TRUE)
  if (is.null(left) || is.null(right)) return(FALSE)
  if (length(left) == 1L && length(right) == 1L &&
      is.numeric(left) && is.numeric(right) && is.finite(left) && is.finite(right)) {
    return(.library_reference_numeric_close(left, right, relative_tolerance, absolute_tolerance))
  }
  if (is.character(left) && is.character(right) && length(left) == 1L && length(right) == 1L) {
    if (identical(pointer, "/compound")) {
      return(identical(.library_reference_compound(left), .library_reference_compound(right)))
    }
    return(identical(.library_reference_normalize_string(left),
                     .library_reference_normalize_string(right)))
  }
  identical(.library_reference_json_value(left), .library_reference_json_value(right))
}

.library_reference_numeric_delta <- function(left, right) {
  if (is.null(left) || is.null(right) || length(left) != 1L || length(right) != 1L ||
      !is.numeric(left) || !is.numeric(right) || !is.finite(left) || !is.finite(right)) {
    return(NA_real_)
  }
  denominator <- abs(as.numeric(left))
  if (denominator <= .Machine$double.eps) {
    return(if (abs(as.numeric(right)) <= .Machine$double.eps) 0 else Inf)
  }
  100 * (as.numeric(right) - as.numeric(left)) / denominator
}

.library_reference_compare_values <- function(reference, prediction,
                                               relative_tolerance = 0.05,
                                               absolute_tolerance = 1e-08) {
  left <- .library_reference_flatten(reference)
  right <- .library_reference_flatten(prediction)
  left_index <- stats::setNames(left, vapply(left, `[[`, character(1), "pointer"))
  right_index <- stats::setNames(right, vapply(right, `[[`, character(1), "pointer"))
  pointers <- unique(c(names(left_index), names(right_index)))
  rows <- lapply(pointers, function(pointer) {
    left_leaf <- left_index[[pointer]]
    right_leaf <- right_index[[pointer]]
    left_present <- !is.null(left_leaf)
    right_present <- !is.null(right_leaf)
    left_value <- if (left_present) left_leaf$value else NULL
    right_value <- if (right_present) right_leaf$value else NULL
    status <- if (!left_present) {
      "LibeRary only"
    } else if (!right_present) {
      "Review only"
    } else if (.library_reference_leaf_equal(left_value, right_value,
                                              relative_tolerance, absolute_tolerance,
                                              pointer = pointer)) {
      "Match"
    } else {
      "Different"
    }
    data.frame(
      pointer = pointer,
      field = (left_leaf %||% right_leaf)$field,
      reference = if (left_present) .library_reference_display_value(left_value) else "\u2014",
      liberary = if (right_present) .library_reference_display_value(right_value) else "\u2014",
      delta_percent = .library_reference_numeric_delta(left_value, right_value),
      status = status,
      reference_json = if (left_present) .library_reference_json_value(left_value) else NA_character_,
      liberary_json = if (right_present) .library_reference_json_value(right_value) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output <- output[order(match(output$status, c("Different", "Review only", "LibeRary only", "Match")),
                         output$field), , drop = FALSE]
  rownames(output) <- NULL
  output
}

.library_reference_prediction_envelope <- function(reference_id, predictions) {
  if (is.null(predictions) || !nzchar(as.character(predictions)[[1L]])) return(NULL)
  if (!dir.exists(predictions)) return(NULL)
  predictions <- normalizePath(predictions, winslash = "/", mustWork = TRUE)
  candidates <- c(
    file.path(predictions, paste0(reference_id, ".json")),
    file.path(predictions, "predictions", paste0(reference_id, ".json"))
  )
  path <- candidates[file.exists(candidates)][1L]
  if (is.na(path)) {
    files <- .library_reference_prediction_files(predictions)
    hit <- files[tolower(tools::file_path_sans_ext(basename(files))) == tolower(reference_id)][1L]
    path <- hit
  }
  if (is.na(path) || !file.exists(path)) return(NULL)
  value <- jsonlite::read_json(path, simplifyVector = FALSE)
  attr(value, "path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  value
}

.library_reference_variant <- function(envelope, variant) {
  if (is.null(envelope)) return(NULL)
  if (identical(variant, "prediction")) {
    return(envelope$prediction %||% envelope$extraction %||% envelope$triage)
  }
  envelope$variants[[variant]] %||% NULL
}

#' Compare a systematic-review reference with a LibeRary extraction
#'
#' Returns the historical appendix transcription, the normalized reference
#' target, a selected prediction variant, and a leaf-level comparison table.
#' The function is read-only; curation is applied only through
#' [library_reference_revise()].
#'
#' @param reference_id Stable model identifier in the reference corpus.
#' @param root Reference-corpus root.
#' @param predictions Prediction directory produced by
#'   [library_reference_run()] (or its `predictions` subdirectory).
#' @param variant Primary `prediction` or a stored extraction variant such as
#'   `text`, `vision`, or `reconciled`.
#' @param relative_tolerance Relative numeric tolerance used to label matches.
#' @param absolute_tolerance Absolute numeric tolerance used to label matches.
#' @return A list containing the record, prediction envelope, available
#'   variants, selected prediction, and comparison data frame.
#' @export
library_reference_compare <- function(reference_id, root, predictions,
                                      variant = "prediction",
                                      relative_tolerance = 0.05,
                                      absolute_tolerance = 1e-08) {
  record <- library_reference_get(reference_id, root)
  envelope <- .library_reference_prediction_envelope(reference_id, predictions)
  variants <- if (is.null(envelope)) character() else {
    unique(c(if (!is.null(envelope$prediction) || !is.null(envelope$extraction)) "prediction",
             names(envelope$variants %||% list())))
  }
  if (!length(variants)) variant <- "prediction"
  if (length(variants) && !variant %in% variants) {
    stop("Prediction variant not found: ", variant, call. = FALSE)
  }
  prediction <- .library_reference_variant(envelope, variant)
  comparison <- .library_reference_compare_values(
    record$reference$extraction_target %||% list(), prediction %||% list(),
    relative_tolerance = relative_tolerance,
    absolute_tolerance = absolute_tolerance
  )
  list(
    reference_id = reference_id,
    record = record,
    raw = record$raw %||% list(),
    reference = record$reference$extraction_target %||% list(),
    envelope = envelope,
    prediction_path = attr(envelope, "path") %||% "",
    variants = variants,
    selected_variant = variant,
    prediction = prediction,
    comparison = comparison
  )
}

.library_reference_pointer_get <- function(value, pointer, missing = structure(list(), class = "library_missing")) {
  tokens <- .library_reference_pointer_tokens(pointer)
  current <- value
  for (token in tokens) {
    if (!is.list(current)) return(missing)
    if (.library_reference_is_array(current)) {
      index <- suppressWarnings(as.integer(token)) + 1L
      if (!is.finite(index) || index < 1L || index > length(current)) return(missing)
      current <- current[[index]]
    } else {
      if (!token %in% names(current)) return(missing)
      current <- current[[token]]
    }
  }
  current
}

.library_reference_pointer_set <- function(value, pointer, replacement) {
  tokens <- .library_reference_pointer_tokens(pointer)
  if (!length(tokens)) return(replacement)
  set_one <- function(current, remaining) {
    token <- remaining[[1L]]
    tail <- remaining[-1L]
    if (!is.list(current)) current <- list()
    array <- .library_reference_is_array(current) && grepl("^[0-9]+$", token)
    if (array) {
      index <- as.integer(token) + 1L
      if (index > length(current)) length(current) <- index
      child <- if (length(tail)) current[[index]] else NULL
      current[index] <- list(if (length(tail)) set_one(child, tail) else replacement)
    } else {
      child <- if (token %in% names(current)) current[[token]] else NULL
      current[token] <- list(if (length(tail)) set_one(child, tail) else replacement)
    }
    current
  }
  set_one(value, tokens)
}

.library_reference_corrections <- function(corrections) {
  if (is.null(corrections)) return(data.frame())
  if (is.character(corrections) && length(corrections) == 1L) {
    corrections <- utils::read.csv(corrections, stringsAsFactors = FALSE, check.names = FALSE,
                                   na.strings = character())
  }
  required <- c("reference_id", "pointer", "source", "value_json")
  if (!is.data.frame(corrections) || !all(required %in% names(corrections))) {
    stop("`corrections` must provide reference_id, pointer, source, and value_json columns.",
         call. = FALSE)
  }
  corrections
}

.library_reference_parse_json_value <- function(value) {
  tryCatch(jsonlite::fromJSON(as.character(value), simplifyVector = FALSE), error = function(e) {
    stop("Invalid correction JSON: ", conditionMessage(e), call. = FALSE)
  })
}
