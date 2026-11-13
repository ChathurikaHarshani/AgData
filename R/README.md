# AgData Shiny Code Structure

This folder is the shared code layer for the N-SIGHT Shiny app.

- `config.R`: one source of truth for years, fields, file paths, output IDs, and date selector choices.
- `data_loader.R`: reusable readers for CSV, harvest Excel workbooks, and shapefile data based on `config.R`.
- `map_helpers.R`: shared map utilities.
- `plot_helpers.R`: shared plotting/data-column utilities.
- `modules/`: future home for Shiny modules that replace repeated UI/server blocks.

The current app still uses the existing `ui.r` and `server.r` layout. Move code here gradually so each change is easy to test.

The 2026 harvest workbooks contain `easting`, `northing`, `yield` (bu/ac), and
`moisture` (%). Coordinates are interpreted as WGS 84 / UTM zone 14N
(EPSG:32614), which aligns with the trial polygons. Yield maps show trial plot polygons colored by the mean yield of harvest
readings within each plot, matching the earlier seasons. Charts compare plot mean yields, grouped by treatment and cover
crop; reports include plot mean yield, moisture, and harvest record counts.
Only points intersecting a trial polygon contribute to plot summaries. Boundary
points are assigned once, to the first intersecting polygon. Excel reads are
cached by path, modification time, and size, so edited workbooks are reloaded.
