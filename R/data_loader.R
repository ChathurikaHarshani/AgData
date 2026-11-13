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
  if (!is.na(cfg$yield_xlsx[1])) {
    path <- agdata_path(cfg, "yield_xlsx")
    key <- agdata_file_cache_key("xlsx", path, list())
    if (!exists(key, envir = .agdata_file_cache, inherits = FALSE)) {
      assign(key, as.data.frame(readxl::read_excel(path)), envir = .agdata_file_cache)
    }
    data <- get(key, envir = .agdata_file_cache, inherits = FALSE)
    required <- c("easting", "northing", "yield", "moisture")
    stopifnot(all(required %in% names(data)))
    data <- data[is.finite(data$easting) & is.finite(data$northing) & is.finite(data$yield), ]
    points <- sf::st_as_sf(data, coords = c("easting", "northing"), crs = cfg$yield_crs[1], remove = FALSE)
    plots <- sf::st_transform(agdata_read_sf(cfg, "treatment_shp", quiet = TRUE), cfg$yield_crs[1])
    # Aggregate harvest readings within each trial plot. Assign boundary
    # points to only one plot; outside points do not enter plot summaries.
    matches <- sf::st_intersects(points, plots)
    plot_index <- vapply(matches, function(x) if (length(x)) x[1] else NA_integer_, integer(1))
    attributes <- sf::st_drop_geometry(plots)[plot_index, c("Plot_Numbe", "Treatment", "Cover_Crop")]
    rownames(attributes) <- NULL
    points <- cbind(points, attributes)
    summary <- sf::st_drop_geometry(points) |>
      dplyr::filter(!is.na(Plot_Numbe)) |>
      dplyr::group_by(Plot_Numbe, Treatment, Cover_Crop) |>
      dplyr::summarise(Yield = mean(yield), Moisture = mean(moisture, na.rm = TRUE), Harvest_Records = dplyr::n(), .groups = "drop")
    plot_yield <- dplyr::left_join(plots, summary, by = c("Plot_Numbe", "Treatment", "Cover_Crop"))
    return(list(plots = sf::st_transform(plot_yield, 4326), points = sf::st_transform(points, 4326),
                data = sf::st_drop_geometry(points), summary = summary))
  }
  list(
    shape_path = agdata_path(cfg, "yield_shp"),
    data = agdata_read_csv(cfg, "yield_csv")
  )
}
