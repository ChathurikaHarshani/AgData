local_library_candidates <- c(
  file.path(getwd(), "R", "library"),
  normalizePath(file.path(getwd(), "..", "..", "R-library"), mustWork = FALSE)
)
local_library <- local_library_candidates[dir.exists(local_library_candidates)][1]
if (!is.na(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}
