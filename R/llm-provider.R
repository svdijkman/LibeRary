.library_llm_cache <- new.env(parent = emptyenv())

#' Language-model providers available to LibeRary
#'
#' `ollama` is local by default. `openai` uses the official API, while
#' `openai_compatible` supports local servers and gateways implementing the
#' common models and chat-completions endpoints.
#' @return A data frame describing the provider registry.
#' @export
library_llm_providers <- function() {
  data.frame(
    id = c("none", "ollama", "openai", "openai_compatible"),
    name = c("No LLM (metadata stub)", "Ollama", "OpenAI", "OpenAI-compatible"),
    local_default = c(TRUE, TRUE, FALSE, TRUE),
    model_discovery = c(FALSE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

.library_llm_roles <- function() c("triage", "indexing", "vision", "assessment", "adjudication")

.library_default_instruction <- function(role = .library_llm_roles()) {
  role <- match.arg(role)
  switch(role,
    triage = TRIAGE_SYSTEM_PROMPT,
    indexing = DUAL_EXTRACTION_SYSTEM_PROMPT,
    vision = DUAL_EXTRACTION_SYSTEM_PROMPT,
    assessment = ASSESSMENT_SYSTEM_PROMPT,
    adjudication = ADJUDICATION_SYSTEM_PROMPT
  )
}

.library_role_instruction <- function(cfg, role = .library_llm_roles()) {
  role <- match.arg(role)
  custom <- as.character(cfg$llm[[role]]$instruction %||% "")[[1L]]
  if (nzchar(trimws(custom))) custom else .library_default_instruction(role)
}

.library_llm_role <- function(cfg, role = .library_llm_roles()) {
  role <- match.arg(role)
  selected <- cfg$llm[[role]] %||% list(provider = "none", model = "")
  if (identical(selected$provider %||% "", "same")) {
    selected <- ingest_merge_config(cfg$llm$indexing, selected)
    selected$provider <- cfg$llm$indexing$provider
    if (!nzchar(selected$model %||% "")) selected$model <- cfg$llm$indexing$model
  }
  selected$provider <- as.character(selected$provider %||% "none")[[1L]]
  if (!selected$provider %in% library_llm_providers()$id) {
    stop("Unknown LLM provider: ", selected$provider, call. = FALSE)
  }
  provider <- cfg$llm$providers[[selected$provider]] %||% list()
  ingest_merge_config(provider, selected)
}

.library_llm_local_url <- function(url) {
  host <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) "")
  tolower(host %||% "") %in% c("", "localhost", "127.0.0.1", "::1")
}

.library_llm_request <- function(req) {
  transport <- getOption("LibeRary.llm_transport", NULL)
  if (is.function(transport)) return(transport(req))
  req <- httr2::req_error(req, is_error = function(response) FALSE)
  httr2::req_perform(req)
}

.library_llm_http_error <- function(provider, response, context = "request") {
  status <- httr2::resp_status(response)
  detail <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
  detail <- trimws(gsub("[\r\n]+", " ", detail))
  if (nchar(detail) > 1000L) detail <- paste0(substr(detail, 1L, 1000L), "...")
  stop(provider, " ", context, " failed (HTTP ", status, ")",
       if (nzchar(detail)) paste0(": ", detail) else ".", call. = FALSE)
}

.library_llm_auth <- function(req, provider, endpoint) {
  env_name <- as.character(endpoint$api_key_env %||% "")[[1L]]
  key <- if (nzchar(env_name)) Sys.getenv(env_name, "") else ""
  if (identical(provider, "openai") && !nzchar(key)) {
    stop("Set OPENAI_API_KEY before using the OpenAI provider.", call. = FALSE)
  }
  if (nzchar(key)) req <- httr2::req_auth_bearer_token(req, key)
  req
}

.library_ollama_model_capabilities <- function(base, model, endpoint) {
  req <- httr2::request(paste0(base, "/api/show")) |>
    httr2::req_body_json(list(model = model)) |>
    httr2::req_timeout(min(3, endpoint$timeout_seconds %||% 3L))
  req <- .library_llm_auth(req, "ollama", endpoint)
  response <- .library_llm_request(req)
  if (httr2::resp_status(response) >= 400L) return(NA_character_)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  capabilities <- tolower(as.character(unlist(body$capabilities %||% character())))
  unique(capabilities[nzchar(capabilities)])
}

.library_models_for_role <- function(models, role) {
  if (!nrow(models)) return(models)
  models$usable <- models$text_usable
  if (role %in% c("vision", "adjudication")) {
    # Unknown is retained for OpenAI-compatible endpoints, whose model-list API
    # does not standardize modality metadata. Ollama capabilities are explicit.
    models$usable <- models$usable &
      (is.na(models$vision_capable) | models$vision_capable)
  }
  models
}

#' Discover models offered by an LLM endpoint
#'
#' @param provider Provider id from [library_llm_providers()].
#' @param cfg LibeRary configuration or `NULL`.
#' @param role Configuration role, used for endpoint defaults.
#' @param refresh Ignore the short in-memory discovery cache.
#' @return A data frame with model ids and endpoint metadata.
#' @export
library_llm_models <- function(provider = NULL, cfg = NULL,
                               role = .library_llm_roles(), refresh = FALSE) {
  role <- match.arg(role)
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  endpoint <- .library_llm_role(cfg, role)
  provider <- as.character(provider %||% endpoint$provider)[[1L]]
  if (provider == "none") return(data.frame(id = character(), name = character(), provider = character()))
  if (!provider %in% library_llm_providers()$id) stop("Unknown LLM provider: ", provider)
  endpoint <- ingest_merge_config(cfg$llm$providers[[provider]] %||% list(), endpoint)
  endpoint$provider <- provider
  base <- sub("/+$", "", as.character(endpoint$base_url %||% ""))
  if (!nzchar(base)) stop("No base URL is configured for provider ", provider, ".")
  key <- paste(provider, base, sep = "|")
  cached <- .library_llm_cache[[key]]
  if (!isTRUE(refresh) && !is.null(cached) &&
      as.numeric(difftime(Sys.time(), cached$at, units = "secs")) < 60) {
    return(.library_models_for_role(cached$value, role))
  }
  url <- if (provider == "ollama") paste0(base, "/api/tags") else paste0(base, "/v1/models")
  req <- httr2::request(url) |>
    httr2::req_timeout(min(3, endpoint$timeout_seconds %||% 3L))
  req <- .library_llm_auth(req, provider, endpoint)
  response <- .library_llm_request(req)
  if (httr2::resp_status(response) >= 400L) {
    .library_llm_http_error(provider, response, "model discovery")
  }
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  ids <- if (provider == "ollama") {
    vapply(body$models %||% list(), function(x) as.character(x$name %||% x$model %||% ""), character(1))
  } else {
    vapply(body$data %||% list(), function(x) as.character(x$id %||% ""), character(1))
  }
  ids <- sort(unique(ids[nzchar(ids)]))
  non_text <- grepl("embed|moderation|tts|whisper|transcrib|realtime|audio|image|sora", ids,
                    ignore.case = TRUE)
  vision_capable <- rep(NA, length(ids))
  if (identical(provider, "ollama") && length(ids)) {
    vision_capable <- vapply(ids, function(model) {
      capabilities <- tryCatch(
        .library_ollama_model_capabilities(base, model, endpoint),
        error = function(e) NA_character_
      )
      if (length(capabilities) == 1L && is.na(capabilities)) NA else "vision" %in% capabilities
    }, logical(1))
  }
  result <- data.frame(
    id = ids, name = ids, provider = provider,
    text_usable = !non_text, vision_capable = vision_capable,
    stringsAsFactors = FALSE
  )
  .library_llm_cache[[key]] <- list(at = Sys.time(), value = result)
  .library_models_for_role(result, role)
}

#' Check whether an LLM role is usable
#' @param cfg LibeRary configuration.
#' @param role One of `triage`, `indexing`, `vision`, `assessment`, or
#'   `adjudication`.
#' @return `TRUE` or `FALSE`.
#' @export
library_llm_available <- function(cfg = NULL, role = .library_llm_roles()) {
  role <- match.arg(role)
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  endpoint <- .library_llm_role(cfg, role)
  if (endpoint$provider == "none") return(FALSE)
  tryCatch({
    models <- library_llm_models(endpoint$provider, cfg, role, refresh = TRUE)
    nrow(models) > 0L && any(models$usable)
  }, error = function(e) FALSE)
}

.library_ollama_runtime <- function(base, model) {
  tryCatch({
    response <- httr2::request(paste0(base, "/api/ps")) |>
      httr2::req_timeout(3) |>
      httr2::req_perform()
    if (httr2::resp_status(response) >= 400L) return(NULL)
    payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
    models <- payload$models %||% list()
    selected <- Filter(function(item) {
      candidate <- as.character(item$model %||% item$name %||% "")
      length(candidate) && (identical(candidate[[1L]], model) ||
        identical(sub(":latest$", "", candidate[[1L]]), sub(":latest$", "", model)))
    }, models)
    if (!length(selected)) return(NULL)
    item <- selected[[1L]]
    size <- suppressWarnings(as.numeric(item$size %||% NA_real_))
    vram <- suppressWarnings(as.numeric(item$size_vram %||% NA_real_))
    gpu <- if (length(size) && length(vram) && is.finite(size[[1L]]) && size[[1L]] > 0 &&
               is.finite(vram[[1L]])) max(0, min(100, round(100 * vram[[1L]] / size[[1L]]))) else NA_real_
    list(
      processor = if (is.finite(gpu)) paste0(gpu, "% GPU / ", 100 - gpu, "% CPU") else "Ollama placement unavailable",
      gpu_percent = if (is.finite(gpu)) gpu else NULL,
      cpu_percent = if (is.finite(gpu)) 100 - gpu else NULL,
      size_bytes = if (length(size) && is.finite(size[[1L]])) size[[1L]] else NULL,
      size_vram_bytes = if (length(vram) && is.finite(vram[[1L]])) vram[[1L]] else NULL,
      context_length = item$context_length %||% NULL
    )
  }, error = function(e) NULL)
}

#' Send a chat request through a configured LibeRary LLM role
#' @param messages List of role/content messages.
#' @param cfg LibeRary configuration.
#' @param role One of `triage`, `indexing`, `vision`, `assessment`, or
#'   `adjudication`.
#' @param format `NULL`, `"json"`, or a JSON schema list.
#' @param sensitive Whether the request contains publication or patient-derived content.
#' @return A `library_llm_response` with content, model, provider, usage and timing.
#' @export
library_llm_chat <- function(messages, cfg = NULL, role = .library_llm_roles(),
                             format = NULL, sensitive = TRUE) {
  role <- match.arg(role)
  cfg <- if (is.null(cfg)) ingest_load_config() else ingest_validate_config(cfg)
  endpoint <- .library_llm_role(cfg, role)
  provider <- endpoint$provider
  if (provider == "none") stop("The ", role, " role is configured without an LLM.")
  base <- sub("/+$", "", endpoint$base_url %||% "")
  if (isTRUE(sensitive) && !.library_llm_local_url(base) &&
      !isTRUE(cfg$llm$allow_remote_content)) {
    stop("Remote LLM content transfer is disabled. Set llm$allow_remote_content: true to explicitly permit transfer to the configured provider.", call. = FALSE)
  }
  model <- as.character(endpoint$model %||% "")[[1L]]
  if (!nzchar(model)) {
    models <- library_llm_models(provider, cfg, role)
    models <- models[models$usable, , drop = FALSE]
    if (!nrow(models)) stop("No model is available for the ", role, " role.")
    model <- models$id[[1L]]
  } else if (role %in% c("vision", "adjudication") && identical(provider, "ollama")) {
    models <- library_llm_models(provider, cfg, role)
    selected <- models[models$id == model, , drop = FALSE]
    if (nrow(selected) && !isTRUE(selected$usable[[1L]])) {
      alternatives <- models$id[models$usable]
      stop(
        "Ollama model '", model, "' does not support images. Select a vision-capable model for the ", role, " role",
        if (length(alternatives)) paste0(": ", paste(alternatives, collapse = ", ")) else ".",
        call. = FALSE
      )
    }
  }
  started <- Sys.time()
  if (provider == "ollama") {
    ollama_options <- list(temperature = endpoint$temperature %||% 0)
    context <- suppressWarnings(as.integer(endpoint$num_ctx %||% cfg$ollama$num_ctx %||% 16384L))
    if (is.finite(context) && context >= 4096L) ollama_options$num_ctx <- context
    prediction <- suppressWarnings(as.integer(endpoint$num_predict %||% cfg$ollama$num_predict %||% 8192L))
    if (is.finite(prediction) && prediction >= 512L) ollama_options$num_predict <- prediction
    body <- list(model = model, messages = .library_ollama_messages(messages), stream = FALSE,
                 options = ollama_options)
    if (!is.null(format)) body$format <- format
    if (isFALSE(endpoint$think %||% cfg$ollama$think %||% FALSE)) body$think <- FALSE
    req <- httr2::request(paste0(base, "/api/chat")) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(endpoint$timeout_seconds %||% 600L)
  } else {
    body <- list(model = model, messages = messages,
                 temperature = endpoint$temperature %||% 0)
    # `store` is an OpenAI-specific option. Omitting it for compatible
    # endpoints keeps the provider layer usable with LM Studio, vLLM and other
    # servers that implement only the common chat-completions fields.
    if (provider == "openai") body$store <- FALSE
    if (!is.null(format)) {
      body$response_format <- if (is.list(format) && provider == "openai") {
        list(type = "json_schema", json_schema = list(name = paste0("liberary_", role), strict = TRUE, schema = format))
      } else list(type = "json_object")
    }
    req <- httr2::request(paste0(base, "/v1/chat/completions")) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(endpoint$timeout_seconds %||% 600L)
    req <- .library_llm_auth(req, provider, endpoint)
  }
  response <- .library_llm_request(req)
  status <- httr2::resp_status(response)
  if (status >= 400L) .library_llm_http_error(provider, response)
  out <- httr2::resp_body_json(response, simplifyVector = FALSE)
  content <- if (provider == "ollama") out$message$content else out$choices[[1L]]$message$content
  usage <- if (provider == "ollama") {
    list(input_tokens = out$prompt_eval_count %||% NA_integer_, output_tokens = out$eval_count %||% NA_integer_,
         total_duration_ns = out$total_duration %||% NA_real_,
         done = out$done %||% NULL, done_reason = out$done_reason %||% "")
  } else out$usage %||% list()
  runtime <- if (provider == "ollama") .library_ollama_runtime(base, model) else NULL
  structure(list(content = as.character(content %||% ""), provider = provider,
                 model = model, role = role, usage = usage,
                 runtime = runtime,
                 done_reason = if (provider == "ollama") out$done_reason %||% "" else
                   out$choices[[1L]]$finish_reason %||% "",
                 elapsed_seconds = unname(as.numeric(difftime(Sys.time(), started, units = "secs"))),
                 created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")),
            class = "library_llm_response")
}

.library_image_mime <- function(path) {
  extension <- tolower(tools::file_ext(path))
  switch(extension, jpg = "image/jpeg", jpeg = "image/jpeg", webp = "image/webp",
         gif = "image/gif", "image/png")
}

.library_file_data_url <- function(path) {
  if (!file.exists(path)) stop("Image not found: ", path, call. = FALSE)
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  paste0("data:", .library_image_mime(path), ";base64,", jsonlite::base64_enc(bytes))
}

#' Build a provider-neutral multimodal user message
#'
#' Image files are encoded as data URLs for OpenAI-compatible chat endpoints.
#' The Ollama adapter converts the same blocks to its native `images` array.
#' @param prompt Text instruction.
#' @param image_paths Local PNG, JPEG, WebP, or GIF paths.
#' @param labels Optional labels inserted before each image, normally page ids.
#' @param detail Image detail hint.
#' @return A chat message list accepted by [library_llm_chat()].
#' @export
library_image_message <- function(prompt, image_paths, labels = NULL,
                                  detail = c("high", "auto", "low", "original")) {
  detail <- match.arg(detail)
  paths <- normalizePath(image_paths, winslash = "/", mustWork = TRUE)
  if (!length(paths)) stop("At least one image is required.", call. = FALSE)
  if (is.null(labels)) labels <- paste("Image", seq_along(paths))
  labels <- rep_len(as.character(labels), length(paths))
  content <- list(list(type = "text", text = as.character(prompt)[[1L]]))
  for (index in seq_along(paths)) {
    content <- c(content, list(
      list(type = "text", text = labels[[index]]),
      list(type = "image_url", image_url = list(
        url = .library_file_data_url(paths[[index]]), detail = detail
      ))
    ))
  }
  list(role = "user", content = content)
}

.library_ollama_messages <- function(messages) {
  lapply(messages, function(message) {
    content <- message$content %||% ""
    if (!is.list(content)) return(message)
    text <- character(); images <- character()
    for (block in content) {
      type <- as.character(block$type %||% "")[[1L]]
      if (identical(type, "text")) text <- c(text, as.character(block$text %||% ""))
      if (identical(type, "image_url")) {
        url <- as.character(block$image_url$url %||% "")[[1L]]
        if (grepl("^data:[^;]+;base64,", url)) {
          images <- c(images, sub("^data:[^;]+;base64,", "", url))
        } else if (nzchar(url)) {
          stop("Ollama vision requests require local images encoded as data URLs.", call. = FALSE)
        }
      }
    }
    message$content <- paste(text[nzchar(text)], collapse = "\n")
    # `req_body_json(auto_unbox = TRUE)` would otherwise collapse a one-image
    # character vector to a scalar. Ollama always requires `images` to be a
    # JSON array, including for a single rendered page.
    if (length(images)) message$images <- unname(as.list(images))
    message
  })
}

# Compatibility wrappers retained for the prototype API.
#' Legacy Ollama provider wrappers
#'
#' Compatibility functions for scripts written against the original ingest
#' prototype. New code should use [library_llm_available()] and
#' [library_llm_chat()].
#' @param cfg LibeRary configuration.
#' @param messages Chat messages.
#' @param format Optional structured-output schema.
#' @return `ingest_ollama_available()` returns a logical scalar;
#'   `ingest_ollama_chat()` returns the response content as text.
#' @name ingest_ollama_compatibility
NULL

#' @rdname ingest_ollama_compatibility
#' @export
ingest_ollama_available <- function(cfg) {
  cfg <- ingest_validate_config(cfg)
  cfg$llm$indexing$provider <- "ollama"
  library_llm_available(cfg, "indexing")
}

#' @rdname ingest_ollama_compatibility
#' @export
ingest_ollama_chat <- function(messages, cfg, format = NULL) {
  cfg <- ingest_validate_config(cfg)
  cfg$llm$indexing$provider <- "ollama"
  if (nzchar(cfg$ollama$model %||% "")) cfg$llm$indexing$model <- cfg$ollama$model
  library_llm_chat(messages, cfg, "indexing", format)$content
}
