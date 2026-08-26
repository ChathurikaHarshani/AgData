remove_first_n_columns <- function(df, n) {
  df %>% dplyr::select(-1:-n)
}

add_imagery_tiles <- function(map) {
  map %>% leaflet::addProviderTiles("Esri.WorldImagery")
}
