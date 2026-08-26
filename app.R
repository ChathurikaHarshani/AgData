local_library_candidates <- c(
  file.path(getwd(), "R", "library"),
  normalizePath(file.path(getwd(), "..", "..", "R-library"), mustWork = FALSE)
)
local_library <- local_library_candidates[dir.exists(local_library_candidates)][1]
if (!is.na(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}

ensure_namespace_version <- function(package, minimum_version) {
  if (!package %in% loadedNamespaces()) {
    return(invisible(TRUE))
  }

  loaded_version <- getNamespaceVersion(package)
  if (loaded_version >= package_version(minimum_version)) {
    return(invisible(TRUE))
  }

  stop(
    sprintf(
      paste(
        "Please restart your R session before running the app.",
        "%s %s is already loaded, but this app needs %s >= %s from the project library."
      ),
      package,
      as.character(loaded_version),
      package,
      minimum_version
    ),
    call. = FALSE
  )

  invisible(TRUE)
}

ensure_namespace_version("rlang", "1.1.7")
ensure_namespace_version("promises", "1.5.0")
ensure_namespace_version("data.table", "1.18.2.1")
ensure_namespace_version("dplyr", "1.2.1")
ensure_namespace_version("tidyr", "1.3.2")
ensure_namespace_version("plotly", "4.11.0")

library(shiny)

# Keep implementation details and local file paths out of the browser. Full
# errors are still written to the R console for application administrators.
options(shiny.sanitize.errors = TRUE)

source("R/config.R")
source("R/data_loader.R")
source("R/map_helpers.R")
source("R/plot_helpers.R")
source("R/modules/mod_summary.R")
source("ui.r")
source("server.r")

shinyApp(ui= ui, server = server)








