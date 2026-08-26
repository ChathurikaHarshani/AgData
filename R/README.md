# AgData Shiny Code Structure

This folder is the shared code layer for the N-SIGHT Shiny app.

- `config.R`: one source of truth for years, fields, file paths, output IDs, and date selector choices.
- `data_loader.R`: reusable readers for CSV and shapefile data based on `config.R`.
- `map_helpers.R`: shared map utilities.
- `plot_helpers.R`: shared plotting/data-column utilities.
- `modules/`: future home for Shiny modules that replace repeated UI/server blocks.

The current app still uses the existing `ui.r` and `server.r` layout. Move code here gradually so each change is easy to test.
