applied_rate_columns <- function(data) {
  grep("^AppliedRate", names(data), value = TRUE)
}

si_columns <- function(data) {
  grep("^SI_", names(data), value = TRUE)
}
