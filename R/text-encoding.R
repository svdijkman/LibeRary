.library_normalize_utf8 <- function(x) {
  x <- as.character(x)
  if (!length(x)) return(x)

  normalized <- vapply(x, function(value) {
    if (is.na(value)) return(NA_character_)

    # Explicitly validate the underlying bytes. Some base R substring
    # operations on Windows drop the UTF-8 marker even though the bytes remain
    # UTF-8, which makes later case conversion fail on names such as Röshammar.
    utf8 <- suppressWarnings(iconv(value, from = "UTF-8", to = "UTF-8", sub = NA))
    if (!is.na(utf8)) return(utf8)

    # Plain-text supplements are sometimes saved by Windows applications as
    # Windows-1252. Decode that legacy input at the boundary so every later
    # retrieval and prompt-building operation receives valid UTF-8.
    legacy <- suppressWarnings(iconv(
      value, from = "windows-1252", to = "UTF-8", sub = "\uFFFD"
    ))
    if (is.na(legacy)) "\uFFFD" else legacy
  }, character(1), USE.NAMES = FALSE)

  Encoding(normalized[!is.na(normalized)]) <- "UTF-8"
  normalized
}

.library_read_text_utf8 <- function(path) {
  path <- as.character(path %||% "")[[1L]]
  if (!nzchar(path) || !file.exists(path)) return("")

  size <- suppressWarnings(as.numeric(file.info(path)$size[[1L]]))
  if (!is.finite(size) || size <= 0) return("")
  bytes <- readBin(path, what = "raw", n = size)
  # NUL bytes are not meaningful in Markdown/control-stream text and cannot be
  # represented inside an R character scalar.
  bytes <- bytes[as.integer(bytes) != 0L]
  if (!length(bytes)) return("")
  if (length(bytes) >= 3L && identical(as.integer(bytes[1:3]), c(239L, 187L, 191L))) {
    bytes <- bytes[-(1:3)]
  }

  .library_normalize_utf8(rawToChar(bytes))[[1L]]
}
