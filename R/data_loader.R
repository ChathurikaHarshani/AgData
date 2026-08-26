## File reads are shared across Shiny sessions in the same R worker. The key
## includes the file timestamp, size, and reader arguments so edited source
## files are picked up automatically without restarting the application.
.agdata_file_cache <- new.env(parent = emptyenv())

agdata_file_cache_key <- function(reader, path, args) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  file_details <- file.info(normalized_path)
  argument_signature <- paste(capture.output(str(args, give.attr = FALSE)), collapse = "")

  paste(
    reader,
    normalized_path,
    as.numeric(file_details$mtime),
    file_details$size,
    argument_signature,
    sep = "|"
  )
}

agdata_cached_read_csv <- function(file, ...) {
  key <- agdata_file_cache_key("csv", file, list(...))
  if (exists(key, envir = .agdata_file_cache, inherits = FALSE)) {
    return(get(key, envir = .agdata_file_cache, inherits = FALSE))
  }

  value <- utils::read.csv(file, ...)
  assign(key, value, envir = .agdata_file_cache)
  value
}

agdata_cached_st_read <- function(dsn, ...) {
  key <- agdata_file_cache_key("sf", dsn, list(...))
  if (exists(key, envir = .agdata_file_cache, inherits = FALSE)) {
    return(get(key, envir = .agdata_file_cache, inherits = FALSE))
  }

  value <- sf::st_read(dsn, ...)
  assign(key, value, envir = .agdata_file_cache)
  value
}

agdata_clear_file_cache <- function() {
  rm(list = ls(envir = .agdata_file_cache, all.names = TRUE), envir = .agdata_file_cache)
  invisible(TRUE)
}

agdata_path <- function(cfg, column) {
  value <- cfg[[column]][1]
  if (is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  file.path(cfg$folder[1], value)
}

agdata_read_csv <- function(cfg, column, ...) {
  agdata_cached_read_csv(agdata_path(cfg, column), ...)
}

agdata_read_sf <- function(cfg, column, ...) {
  agdata_cached_st_read(agdata_path(cfg, column), ...)
}

agdata_load_treatment <- function(year, field) {
  cfg <- agdata_field_config(year, field)
  list(
    config = cfg,
    shape = agdata_read_sf(cfg, "treatment_shp", quiet = TRUE),
    data = agdata_read_csv(cfg, "treatment_csv")
  )
}

agdata_load_si <- function(year, field) {
  cfg <- agdata_field_config(year, field)
  agdata_read_csv(cfg, "si_csv")
}

agdata_load_yield <- function(year, field) {
  cfg <- agdata_field_config(year, field)
  list(
    shape_path = agdata_path(cfg, "yield_shp"),
    data = agdata_read_csv(cfg, "yield_csv")
  )
}
