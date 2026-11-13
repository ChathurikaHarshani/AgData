field_config <- data.frame(
  year = c("2024", "2024", "2025", "2025", "2025", "2026", "2026"),
  field = c("2436 S", "SCAL", "1402 W", "1912", "2436 N", "1402 E", "2436 S"),
  field_key = c("2436S", "SCAL", "1402W", "1912", "2436N", "1402E_26", "2436S_26"),
  folder = c(
    "data/2024/2436S",
    "data/2024/SCAL",
    "data/2025/1402W",
    "data/2025/1912",
    "data/2025/2436N",
    "data/2026/1402E",
    "data/2026/2436S"
  ),
  treatment_shp = c(
    "Treatment_Polygons/24_2436S_buffered_plotmap.shp",
    "Treatment_Polygons/SCALPivot_TreatmentSectors_Buffered_60ft.shp",
    "Treatment_Polygons/25_1402W_plotmap_040325.shp",
    "Treatment_Polygons/25_1912_PlotMap_031025.shp",
    "Treatment_Polygons/25_2436N_Plotmap.shp",
    "Treatment_Polygons/26_1402E_plotmap.shp",
    "Treatment_Polygons/26_2436S_plotmap.shp"
  ),
  treatment_csv = c(
    "Treatment_Polygons/24_2436S_buffered_plotmap.csv",
    "Treatment_Polygons/SCALPivot_TreatmentSectors_Buffered_60ft.csv",
    "Treatment_Polygons/25_1402W_plotmap_040325.csv",
    NA,
    "Treatment_Polygons/25_2436N_Plotmap.csv",
    NA,
    NA
  ),
  si_csv = c(
    "Aerial_Imagery/SI_2436S.csv",
    "Aerial_Imagery/SCAL_SI.csv",
    "Aerial_Imagery/SI_1402W.csv",
    "Aerial_Imagery/SI_1912.csv",
    "Aerial_Imagery/SI_2436N.csv",
    NA,
    NA
  ),
  yield_csv = c(
    "Harvest/24_2436S_clean_yield_join.csv",
    "Yield/SCALPivot_YieldSummary_TreatmentSectors_2024.csv",
    "Yield/Yield_1402W.csv",
    NA,
    "Yield/Yield_2436N.csv",
    NA,
    NA
  ),
  yield_shp = c(
    "Harvest/24_2436S_clean_yield_join.shp",
    NA,
    "Yield/Yield_1402W.shp",
    "Yield/Yield.shp",
    "Yield/Yield_2436N.shp",
    NA,
    NA
  ),
  field_map_output = c(
    "Field_map_data2436",
    "Field_map_dataSCAL",
    "Field_map_data1402W",
    "Field_map_data1912",
    "Field_map_data2436N",
    "Field_map_data1402E_26",
    "Field_map_data2436S_26"
  ),
  nitrogen_output = c(
    "NRxmap2436",
    "NRxmapSCAL",
    "NRxmap1402W_25",
    "NRxmap1912_25",
    "NRxmap2436N_25",
    "NRxmap1402E_26",
    "NRxmap2436S_26"
  ),
  si_output = c(
    "SImap2436S",
    "SImapSCAL",
    "SImap1402W_25",
    "SImap1912_25",
    "SImap2436N_25",
    NA,
    NA
  ),
  yield_output = c(
    "Yield_2436S1",
    "Yield_SCAL",
    "Yield_1402W",
    "Yield_1912",
    "Yield_2436N",
    "Yield_1402E_26",
    "Yield_2436S_26"
  ),
  summary_output = c(
    "data_table_2436S",
    "data_table_SCAL",
    "data_table_1402W",
    "data_table_1912",
    "data_table_2436N",
    "data_table_1402E_26",
    "data_table_2436S_26"
  ),
  stringsAsFactors = FALSE
)

field_config$yield_xlsx <- NA_character_
field_config$yield_xlsx[field_config$year == "2026"] <- c(
  "Yield/26_1402E_Harvest_clean.xlsx",
  "Yield/26_2436S_Harvest_clean.xlsx"
)
# Harvest eastings/northings use WGS 84 / UTM zone 14N for both fields.
field_config$yield_crs <- ifelse(field_config$year == "2026", 32614L, NA_integer_)

nitrogen_date_choices <- list(
  "2024|2436 S" = c(
    "2024_March_11" = "AppliedRate_03_11",
    "2024_June_24" = "AppliedRate_06_24"
  ),
  "2024|SCAL" = c(
    "2024_April_15" = "AppliedRate_04_15",
    "2024_July_12" = "AppliedRate_07_12",
    "2024_July_30" = "AppliedRate_07_30"
  ),
  "2025|1402 W" = c(
    "2025 April 07" = "April_07",
    "2025 May 07" = "May_07"
  ),
  "2025|1912" = c(
    "2025 April 03" = "April_03"
  ),
  "2025|2436 N" = c(
    "2025_April_08" = "April_08"
  )
)

si_date_choices <- list(
  "2024|2436 S" = c(
    "2024_June_07" = "SI_06_07",
    "2024_June_12" = "SI_06_12",
    "2024_June_21" = "SI_06_21",
    "2024_June_26" = "SI_06_26",
    "2024_July_05" = "SI_07_05",
    "2024_July_11" = "SI_07_11",
    "2024_July_17" = "SI_07_17",
    "2024_July_22" = "SI_07_22",
    "2024_August_01" = "SI_08_01",
    "2024_August_05" = "SI_08_05",
    "2024_August_16" = "SI_08_16",
    "2024_August_31" = "SI_08_31"
  ),
  "2024|SCAL" = c(
    "2024_June_05" = "SI_06_05",
    "2024_June_09" = "SI_06_09",
    "2024_June_16" = "SI_06_16",
    "2024_June_25" = "SI_06_25",
    "2024_July_08" = "SI_07_08",
    "2024_July_17" = "SI_07_17",
    "2024_July_27" = "SI_07_27",
    "2024_August_01" = "SI_08_01",
    "2024_August_07" = "SI_08_07",
    "2024_August_17" = "SI_08_17",
    "2024_August_26" = "SI_08_26",
    "2024_August_31" = "SI_08_31"
  ),
  "2025|1402 W" = c(
    "2025_April_15" = "SI_04_15",
    "2025_April_21" = "SI_04_21",
    "2025_June_27" = "SI_06_27",
    "2025_June_30" = "SI_06_30",
    "2025_July_03" = "SI_07_03",
    "2025_July_09" = "SI_07_09",
    "2025_July_15" = "SI_07_15",
    "2025_July_23" = "SI_07_23",
    "2025_Aug_01" = "SI_08_01",
    "2025_Aug_18" = "SI_08_18"
  ),
  "2025|1912" = c(
    "2025_June_19" = "SI_06_19",
    "2025_June_22" = "SI_06_22",
    "2025_June_28" = "SI_06_28",
    "2025_July_01" = "SI_07_01",
    "2025_July_08" = "SI_07_08",
    "2025_July_14" = "SI_07_14",
    "2025_July_19" = "SI_07_19",
    "2025_July_28" = "SI_07_28",
    "2025_Aug_08" = "SI_08_08",
    "2025_Aug_12" = "SI_08_12",
    "2025_Aug_17" = "SI_08_17",
    "2025_Aug_21" = "SI_08_21",
    "2025_Aug_25" = "SI_08_25"
  ),
  "2025|2436 N" = c(
    "2025_April_15" = "SI_04_15",
    "2025_April_21" = "SI_04_21",
    "2025_June_27" = "SI_06_27",
    "2025_July_03" = "SI_07_03",
    "2025_July_09" = "SI_07_09",
    "2025_July_15" = "SI_07_15",
    "2025_July_23" = "SI_07_23",
    "2025_Aug_01" = "SI_08_01",
    "2025_Aug_18" = "SI_08_18"
  )
)

field_choice_key <- function(year, field) {
  paste(year, field, sep = "|")
}

agdata_year_choices <- function() {
  sort(unique(field_config$year), decreasing = TRUE)
}

agdata_field_choices <- function(year, include_all = TRUE) {
  choices <- field_config$field[field_config$year == year]
  if (include_all) {
    choices <- c("Fields", choices)
  }
  choices
}

agdata_field_config <- function(year, field) {
  field_config[field_config$year == year & field_config$field == field, , drop = FALSE]
}

agdata_nitrogen_choices <- function(year, field) {
  nitrogen_date_choices[[field_choice_key(year, field)]]
}

agdata_si_choices <- function(year, field) {
  si_date_choices[[field_choice_key(year, field)]]
}

agdata_map_output_ids <- function() {
  output_ids <- unname(unlist(field_config[c(
    "field_map_output",
    "nitrogen_output",
    "si_output",
    "yield_output"
  )]))
  output_ids[!is.na(output_ids)]
}
