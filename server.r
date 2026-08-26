local_library_candidates <- c(
  file.path(getwd(), "R", "library"),
  normalizePath(file.path(getwd(), "..", "..", "R-library"), mustWork = FALSE)
)
local_library <- local_library_candidates[dir.exists(local_library_candidates)][1]
if (!is.na(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(DT)
library(tidyverse)
library(tidyr)
library(mapview)
library(tmaptools)
library(plotly)
library(gridExtra)
library(shinyalert)
library(shinyBS)
library(shinyjs)

if (!exists("field_config", mode = "any") && file.exists("R/config.R")) {
  source("R/config.R")
}
if (!exists("summary_modal_title", mode = "function") && file.exists("R/modules/mod_summary.R")) {
  source("R/modules/mod_summary.R")
}

if (!exists("summary_modal_title", mode = "function")) {
  summary_modal_title <- function(field) {
    paste("Summary Report -", field)
  }
}




server <- function(input, output, session) {

  # Preserve the existing server logic while sharing expensive disk reads
  # between sessions. R's copy-on-modify semantics keep cached inputs safe.
  read.csv <- agdata_cached_read_csv
  st_read <- agdata_cached_st_read

  shinyjs::show("loading_message")

  register_si_proxy_map <- function(output_id, variable, data, sector_col, treatment_col) {
    map_data <- reactive({ sf::st_transform(if (is.function(data)) data() else data, 4326) })

    layer <- reactive({
      column <- variable()
      current_data <- map_data()
      req(column, column %in% names(current_data))
      values <- current_data[[column]]
      palette <- leaflet::colorBin(
        c("#d73027", "#f2c94c", "#2e7d32"),
        bins = c(-Inf, 0.95, 0.96, Inf), domain = values,
        na.color = "#d1d5db", right = FALSE
      )
      popup <- paste0(
        "<strong>Sector:</strong> ", htmltools::htmlEscape(current_data[[sector_col]]),
        "<br><strong>Treatment:</strong> ", htmltools::htmlEscape(current_data[[treatment_col]]),
        "<br><strong>SI:</strong> ", ifelse(is.na(values), "Not available", round(values, 3))
      )
      list(values = values, palette = palette, popup = popup)
    })

    draw_layer <- function(map) {
      current <- layer()
      map %>%
        addPolygons(
          fillColor = current$palette(current$values), fillOpacity = 0.72,
          color = "#4b5563", weight = 0.5, popup = current$popup,
          group = paste0(output_id, "-polygons")
        ) %>%
        addLegend(
          pal = current$palette, values = current$values,
          title = "Sufficiency Index", opacity = 0.9,
          layerId = paste0(output_id, "-legend")
        )
    }

    output[[output_id]] <- renderLeaflet({
      current_data <- map_data(); map_bounds <- sf::st_bbox(current_data)
      draw_layer(leaflet(current_data) %>% addProviderTiles("Esri.WorldImagery")) %>%
        fitBounds(map_bounds[["xmin"]], map_bounds[["ymin"]], map_bounds[["xmax"]], map_bounds[["ymax"]])
    })

    observeEvent(variable(), {
      draw_layer(
        leafletProxy(output_id, data = map_data()) %>%
          clearGroup(paste0(output_id, "-polygons")) %>%
          removeControl(paste0(output_id, "-legend"))
      )
    }, ignoreInit = TRUE)
  }

  register_n_proxy_map <- function(output_id, trigger, layer_data) {
    layer <- reactive({
      spec <- layer_data()
      data <- sf::st_transform(spec$data, 4326)
      values <- data[[spec$value_col]]
      palette <- leaflet::colorNumeric("YlOrRd", domain = values, na.color = "#d1d5db")
      popup <- rep("", nrow(data))
      for (label in names(spec$popup_cols)) {
        column <- spec$popup_cols[[label]]
        value <- if (column %in% names(data)) data[[column]] else NA
        popup <- paste0(
          popup, "<strong>", htmltools::htmlEscape(label), ":</strong> ",
          htmltools::htmlEscape(ifelse(is.na(value), "Not available", as.character(value))), "<br>"
        )
      }
      list(data = data, values = values, palette = palette, popup = popup)
    })

    draw_layer <- function(map) {
      current <- layer()
      map %>%
        addPolygons(
          data = current$data,
          fillColor = current$palette(current$values), fillOpacity = 0.72,
          color = "#4b5563", weight = 0.5, popup = current$popup,
          group = paste0(output_id, "-polygons")
        ) %>%
        addLegend(
          pal = current$palette, values = current$values,
          title = "Applied N Rate (lbs N/ac)", opacity = 0.9,
          layerId = paste0(output_id, "-legend")
        )
    }

    output[[output_id]] <- renderLeaflet({
      current <- layer()
      bounds <- sf::st_bbox(current$data)
      draw_layer(leaflet(current$data) %>% addProviderTiles("Esri.WorldImagery")) %>%
        fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])
    })

    observeEvent(trigger(), {
      draw_layer(
        leafletProxy(output_id) %>%
          clearGroup(paste0(output_id, "-polygons")) %>%
          removeControl(paste0(output_id, "-legend"))
      )
    }, ignoreInit = TRUE)
  }






  toggleTile <- function(tile_id){

    shinyjs::runjs(sprintf("
    (function(){
      var tile = document.getElementById('%s');
      var backdrop = document.getElementById('ns_backdrop');
      if(!tile || !backdrop) return;

      var isMax = tile.classList.contains('maximized');

      // clear any existing maximized tile
      document.querySelectorAll('.ns-tile.maximized').forEach(function(t){
        t.classList.remove('maximized');
      });

      if(isMax){
        document.body.classList.remove('has-max');
        backdrop.style.display = 'none';
      } else {
        tile.classList.add('maximized');
        document.body.classList.add('has-max');
        backdrop.style.display = 'block';
      }

      // After layout settles, force resize a few times
      function doResize(){
        window.dispatchEvent(new Event('resize'));

        // preferred: your UI-defined function
        if(window.nsPlotlyResize) {
          try { nsPlotlyResize(); } catch(e) {}
        }

        // fallback: resize real plotly graph divs inside this tile only
        if(window.Plotly){
          tile.querySelectorAll('.js-plotly-plot, .plotly-graph-div').forEach(function(gd){
            try { Plotly.Plots.resize(gd); } catch(e) {}
          });
        }
      }

      setTimeout(doResize, 150);
      setTimeout(doResize, 450);
      setTimeout(doResize, 900);

    })();
  ", tile_id))

  }

  observeEvent(input$max_field,    { toggleTile("tile_field") })
  observeEvent(input$max_nitrogen, { toggleTile("tile_nitrogen") })
  observeEvent(input$max_si,       { toggleTile("tile_si") })
  observeEvent(input$max_yield,    { toggleTile("tile_yield") })

  observeEvent(input$ns_backdrop_close, {
    shinyjs::runjs("
    document.querySelectorAll('.ns-tile.maximized').forEach(t=>t.classList.remove('maximized'));
    document.body.classList.remove('has-max');
    var b = document.getElementById('ns_backdrop');
    if(b) b.style.display='none';
    setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 250);
  ")
  })





  observeEvent(c(input$max_field, input$max_nitrogen, input$max_si, input$max_yield), {
    shinyjs::runjs("
    setTimeout(function(){
      window.dispatchEvent(new Event('resize'));
      if(window.Plotly){
        document.querySelectorAll('.plotly-graph-div').forEach(function(g){
          try { Plotly.Plots.resize(g); } catch(e) {}
        });
      }
    }, 400);

setTimeout(function(){
  window.dispatchEvent(new Event('resize'));
  if(window.nsPlotlyResize) nsPlotlyResize();
}, 350);

setTimeout(function(){
  if(window.Plotly){
    document.querySelectorAll('.js-plotly-plot, .plotly-graph-div').forEach(function(gd){
      try { Plotly.Plots.resize(gd); } catch(e) {}
    });
  }
}, 450);




  ")
  })


  # ------------------------------
  # Tile drawer toggles (2x2 dashboard)
  # ------------------------------
  observeEvent(input$toggleDrawerN,     { shinyjs::toggleClass("drawerN", "is-open") })
  observeEvent(input$toggleDrawerSI,    { shinyjs::toggleClass("drawerSI", "is-open") })
  observeEvent(input$toggleDrawerYield, { shinyjs::toggleClass("drawerYield", "is-open") })

  # After Year/Field changes, force Leaflet to resize inside grid tiles
  observeEvent(list(input$GrowingSeason, input$AvailableFields), {
    session$sendCustomMessage("resizeLeaflet", TRUE)
  })

  #Sys.sleep(5)


  observeEvent(input$info_app, {
    shinyalert::shinyalert(
      title = "N-SIGHT",
      html = TRUE,
      text = HTML(
        '<div>
         <p>Effective nitrogen management is crucial for optimizing crop growth and promoting sustainable farming practices. However, research extension centers currently lack a platform to evaluate the performance of nitrogen treatments in their research fields.
As a result, there is a need for a tool that can evaluate and visualize nitrogen treatment outcomes. Here aims to address this gap by developing this N-SIGHT web-based platform for assessing nitrogen management.
</p>
      </div>'
      ),
      type = "info"
    )
  })


  ##########################2026 N Treatment Summary############################

  build_2026_n_summary <- function(nrx_data) {
    nrx_data %>%
      st_drop_geometry() %>%
      transmute(
        `Plot No` = Plot_Numbe,
        Treatment = Treatment,
        `Cover Crop` = Cover_Crop,
        `Base Rate (lb-N/ac)` = Base_Rate,
        `Side Rate (lb-N/ac)` = Side_Rate,
        `Planned Total N (lb-N/ac)` = Base_Rate + Side_Rate,
        `Prescription Target (lb-N/ac)` = TargetRate,
        `Avg Applied N (lb-N/ac)` = AppliedRate,
        `Applied - Target` = AppliedRate - TargetRate,
        Product = Product,
        `Application Records` = Records
      ) %>%
      mutate(
        `Rate Quality` = case_when(
          is.na(`Avg Applied N (lb-N/ac)`) ~ "No application data",
          `Applied - Target` < -5 ~ "Under Target",
          `Applied - Target` > 5 ~ "Over Target",
          TRUE ~ "Near Target"
        ),
        across(where(is.numeric), ~ round(.x, 1))
      ) %>%
      arrange(`Plot No`)
  }

  output$data_table_1402E_26 <- renderDT({
    datatable(
      build_2026_n_summary(selected_map_data_1402E_26()),
      options = list(pageLength = 15, scrollX = TRUE)
    ) %>%
      formatStyle(
        "Rate Quality",
        color = styleEqual(
          c("Under Target", "Near Target", "Over Target", "No application data"),
          c("#b91c1c", "#166534", "#92400e", "#6b7280")
        )
      )
  })

  output$data_table_2436S_26 <- renderDT({
    datatable(
      build_2026_n_summary(field_2436S_26()$map_data),
      options = list(pageLength = 15, scrollX = TRUE)
    ) %>%
      formatStyle(
        "Rate Quality",
        color = styleEqual(
          c("Under Target", "Near Target", "Over Target", "No application data"),
          c("#b91c1c", "#166534", "#92400e", "#6b7280")
        )
      )
  })












  observeEvent(input$info_N, {
    shinyalert::shinyalert(
      title = "Nitrogen Treatments Overview",
      html = TRUE,
      text = HTML(
        '
      <div>
        <h1>Nitrogen Treatments Overview</h1>
        <p>
          Nitrogen (N) management is a vital component of agricultural practices, as it directly influences crop yields, economic returns, and environmental sustainability. The nitrogen treatments applied in these studies encompass a range of traditional and advanced methods, from grower-applied practices to sensor-based precision agriculture techniques. Each method aims to optimize nitrogen use efficiency (NUE) while ensuring profitability and minimizing negative environmental impacts.
        </p>

        <h2>Nitrogen Treatment Descriptions</h2>

        <h3>1. Control Treatment (<span style="font-weight: bold; color: #27ae60;">0_0</span>)</h3>
        <ul>
          <li><strong>Description:</strong> No nitrogen is applied to the plots.</li>
          <li><strong>Purpose:</strong> This serves as a baseline to evaluate crop performance without any nitrogen input and compare it to other treatments.</li>
        </ul>

        <h3>2. Standard Nitrogen Application Rates</h3>
        <ul>
          <li><strong>100_0:</strong> 100 lbs of NH₃ and 0 lbs of UAN.</li>
          <li><strong>150_0:</strong> 150 lbs of NH₃ and 0 lbs of UAN.</li>
          <li><strong>200_0:</strong> 200 lbs of NH₃ and 0 lbs of UAN.</li>
          <li><strong>Purpose:</strong> These fixed-rate applications test the crop\'s response to different nitrogen levels, from moderate (100_0) to maximum application rates (200_0).</li>
        </ul>

        <h3>3. Mixed Treatments (e.g., <span style="font-weight: bold; color: #27ae60;">50_100</span>)</h3>
        <ul>
          <li><strong>Description:</strong> Combines two nitrogen sources—NH₃ and UAN—applied at specified rates.</li>
          <li><strong>Purpose:</strong> Evaluates the efficacy of combining multiple nitrogen sources to enhance nutrient availability and crop yield.</li>
        </ul>

        <h3>4. Farm Rate (Grower Treatment)</h3>
        <ul>
          <li><strong>Description:</strong> The nitrogen rate is determined based on the grower\'s standard practices, often tailored to the field\'s specific needs.</li>
          <li><strong>Purpose:</strong> Serves as a benchmark to compare traditional grower practices with advanced or experimental treatments.</li>
        </ul>

        <h2>Advanced Nitrogen Management Techniques</h2>

        <h3>1. Sensor-Based Nitrogen Management (SENSE)</h3>
        <ul>
          <li><strong>Description:</strong> Uses crop canopy sensors or satellite imagery to determine nitrogen requirements in real-time.</li>
          <li><strong>Purpose:</strong> Enhances precision and reduces over-application by tailoring nitrogen rates to the crop\'s immediate needs during critical growth stages.</li>
        </ul>

        <h3>2. Digital Tools for N Recommendations</h3>
        <ul>
          <li><strong>Description:</strong> Tools like Adapt-N and N-Time® use crop modeling and multispectral imaging to provide in-season nitrogen recommendations.</li>
          <li><strong>Purpose:</strong> Ensures site-specific nitrogen management, integrating data from sensors or satellites for accurate decision-making.</li>
        </ul>

        <h3>3. Nitrogen Rate Ramps and Indicator Blocks</h3>
        <ul>
          <li><strong>Description:</strong> Rate ramps involve applying varying nitrogen rates (e.g., 0–200 lbs N/ac) across plots to determine the Economically Optimum Nitrogen Rate (EONR).</li>
          <li><strong>Purpose:</strong> Provides valuable data for calibrating precision agriculture tools and identifying optimal nitrogen rates for maximum profitability.</li>
        </ul>

        <h3>4. Integration of Fertigation and Base Applications</h3>
        <ul>
          <li><strong>Description:</strong> Combines fertigation (nitrogen delivered via irrigation systems) with pre-plant or base applications.</li>
          <li><strong>Purpose:</strong> Improves nitrogen distribution throughout the growing season, reducing waste and enhancing crop uptake efficiency.</li>
        </ul>

        <h2>Key Outcomes of Nitrogen Treatments</h2>
        <ul>
          <li><strong>Comparison of Traditional vs. Advanced Techniques:</strong> Sensor-based and digital tools provided greater precision and responsiveness compared to uniform grower-applied methods.</li>
          <li><strong>Economic and Environmental Balance:</strong> Tools like rate ramps and digital platforms helped identify nitrogen levels that maximize Marginal Net Return (MNR) while maintaining sustainability.</li>
          <li><strong>Field-Specific Insights:</strong> Tailoring nitrogen management to local conditions, crop types, and growth stages is critical for success.</li>
        </ul>

        <h2>Conclusion</h2>
        <p>
          These nitrogen treatments illustrate the transition from traditional, uniform methods to more dynamic, precision-driven approaches. By leveraging technologies like sensors, satellite imagery, and advanced modeling platforms, farmers can achieve higher yields, better profitability, and improved environmental outcomes. This comprehensive approach to nitrogen management underscores the importance of innovation in sustainable agriculture.
        </p>
      </div>'
      ),
      type = "info"
    )
  })




  observeEvent(input$info_SI, {
    shinyalert::shinyalert(
      title = "Sufficiency Index (SI)",
      html = TRUE,
      text = HTML(
        '<div style="height: 400px; overflow-y: scroll;">  <!-- Add scrollable div -->
        <p>The ratio of a real-time sensed crop property to the same property measured from a known or standard crop (reference). SI represents the sufficiency index, a dimensionless value ranging from 0 to 1.
        <a href="Factsheet_SI.pdf" download>For more info.</a></p>
        <br>
        <img src="SI_EQ.png" alt="SI Image" style="width: 100%; max-width: 150px;">
        <br>
        <br>
        <img src="SI_IMG.png" alt="SI Image" style="width: 100%; max-width: 400px;">
        <br>

      </div>'
      ),
      type = "info"
    )
  })



  #-------------------------------------------------------------------2024---------------------------------------------------------------------------






  ############################################################################################################
  #2436S
  ##########Treatment_Polygons
  field_2436S_24 <- reactive({
    map <- st_read("data/2024/2436S/Treatment_Polygons/24_2436S_buffered_plotmap.shp")
    treatment <- read.csv("data/2024/2436S/Treatment_Polygons/24_2436S_buffered_plotmap.csv")
    march <- read.csv("data/2024/2436S/Prescription_File/3_11/3-11.csv"); june <- read.csv("data/2024/2436S/Prescription_File/6-24/6-24.csv")
    march_map <- inner_join(map,march); june_map <- inner_join(map,june)
    si <- inner_join(map,read.csv("data/2024/2436S/Aerial_Imagery/SI_2436S.csv"))
    yield_map <- st_read("data/2024/2436S/Harvest/24_2436S_clean_yield_join.shp"); yield_data <- read.csv("data/2024/2436S/Harvest/24_2436S_clean_yield_join.csv")
    treatment_map <- inner_join(map,treatment); joined_yield <- inner_join(yield_map,yield_data)
    list(map=map,treatment=treatment,treatment_map=treatment_map,march=march,june=june,march_map=march_map,june_map=june_map,
      march_clean=remove_first_n_columns(march_map,3),june_clean=remove_first_n_columns(june_map,3),si=si,si_clean=remove_first_n_columns(si,3),
      yield_map=yield_map,yield_data=yield_data,joined_yield=joined_yield,yield_clean=remove_first_n_columns(joined_yield,5),treatment_clean=remove_first_n_columns(treatment_map,3))
  })
  lazy_2436S_24_binding <- function(component) { force(component); function(value) { if (!missing(value)) stop("2436 S lazy bindings are read-only"); isolate(field_2436S_24())[[component]] }}
  bindings_2436S_24 <- c(map2436="map",data2436="treatment",map2436_and_data2436="treatment_map",data2436_PF_3_11="march",data2436_PF_6_24="june",
    map_data_2436_PF_3_11="march_map",map_data_2436_PF_6_24="june_map",map_data_2436_PF_3_11_cleaned="march_clean",map_data_2436_PF_6_24_cleaned="june_clean",
    SI2436_0405="si",SI2436_0405_cleaned="si_clean",map_y2436="yield_map",data_yield_2436s="yield_data",map_yield_2436s="joined_yield",
    mapYield_cleaned="yield_clean",map2436_and_data2436_cleaned="treatment_clean")
  for (binding_name in names(bindings_2436S_24)) makeActiveBinding(binding_name,lazy_2436S_24_binding(bindings_2436S_24[[binding_name]]),environment())



  ################################################################################################################
  #SCAL

  ##########Treatment_Polygons

  field_SCAL_24 <- reactive({
    map <- st_read("data/2024/SCAL/Treatment_Polygons/SCALPivot_TreatmentSectors_Buffered_60ft.shp")
    treatment <- read.csv("data/2024/SCAL/Treatment_Polygons/SCALPivot_TreatmentSectors_Buffered_60ft.csv")
    n_data <- read.csv("data/2024/SCAL/Prescription_File/N_Data.csv")
    sector_column <- grep("section_nu$", names(n_data), value = TRUE)
    if (length(sector_column) == 1L) names(n_data)[names(n_data) == sector_column] <- "section_nu"
    raw_si <- read.csv("data/2024/SCAL/Aerial_Imagery/SCAL_SI.csv")
    si <- inner_join(map, raw_si)
    yield_data <- read.csv("data/2024/SCAL/Yield/SCALPivot_YieldSummary_TreatmentSectors_2024.csv")
    map_treatment <- inner_join(map, treatment); map_n <- inner_join(map, n_data); map_yield <- inner_join(map, yield_data)
    list(map=map, treatment=treatment, map_treatment=map_treatment, n_data=n_data, map_n=map_n, raw_si=raw_si,
         map_n_clean=remove_first_n_columns(map_n,3), si=si, si_clean=remove_first_n_columns(si,3),
         yield_data=yield_data, map_yield=map_yield, yield_clean=remove_first_n_columns(map_yield,3),
         map_treatment_clean=remove_first_n_columns(map_treatment,3))
  })
  lazy_SCAL_binding <- function(component) { force(component); function(value) { if (!missing(value)) stop("SCAL lazy bindings are read-only"); isolate(field_SCAL_24())[[component]] }}
  bindings_SCAL <- c(mapSCAL="map", dataSCAL="treatment", mapSCAL_and_dataSCAL="map_treatment", SCAL_N_Data="n_data",
    map_SCAL_N="map_n", map_SCAL_N_1="map_n_clean", ndre_SCAL="raw_si", SI_SCAL="si", SI_SCAL_cleaned="si_clean",
    data_yield_SCAL="yield_data", map_yield_SCAL="map_yield", mapYieldL_cleaned="yield_clean", mapSCAL_and_dataSCAL_cleaned="map_treatment_clean")
  for (binding_name in names(bindings_SCAL)) makeActiveBinding(binding_name, lazy_SCAL_binding(bindings_SCAL[[binding_name]]), environment())


  # Combined overview shown when "Fields" is selected for 2024.
  Combined_map_2024 <- reactive({
    req(input$GrowingSeason == "2024", identical(input$AvailableFields, "Fields"))
    overview_2436 <- inner_join(
      st_read("data/2024/2436S/Treatment_Polygons/24_2436S_buffered_plotmap.shp", quiet = TRUE),
      read.csv("data/2024/2436S/Treatment_Polygons/24_2436S_buffered_plotmap.csv")
    )
    overview_scal <- inner_join(
      st_read("data/2024/SCAL/Treatment_Polygons/SCALPivot_TreatmentSectors_Buffered_60ft.shp", quiet = TRUE),
      read.csv("data/2024/SCAL/Treatment_Polygons/SCALPivot_TreatmentSectors_Buffered_60ft.csv")
    )
    tm_shape(overview_2436) +
    tm_polygons(
      popup.vars = c(
        "Sector No: " = "FID_22_243",
        "Treatment Type: " = "TRT_Name"
      )
    ) +
    tm_shape(overview_scal) +
    tm_polygons(
      popup.vars = c(
        "Sector No: " = "section_nu",
        "Treatment: " = "Treatment"
      )
    ) +
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })

  Combined_map_leaflet_2024 <- reactive(tmap_leaflet(Combined_map_2024()))

  output$all_maps_2024 <- renderLeaflet({
    Combined_map_leaflet_2024() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(
        lng = c(-96.47808, -98.13160),
        lat = c(41.17067, 40.58057),
        popup = c("2436 S Field", "SCAL Field")
      ) %>%
      fitBounds(
        lng1 = -98.13378, lat1 = 40.57891,
        lng2 = -96.47623, lat2 = 41.17217
      )
  })


  ###############################################################
  ##################Maps#####################################
  ############################Field_Map###########################


  #2436 Map data

  Field_map2436 <- reactive({ tm_shape(map2436_and_data2436_cleaned) +tm_polygons(midpoint = 0,col = "TRT_Name",
                                                                       popup.vars = c("Sector No: " = "FID_22_243",
                                                                                      "Treatment Type: " = "TRT_Name"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Field_map_data2436 <- reactive(tmap_leaflet(Field_map2436()))


  output$Field_map_data2436 <- renderLeaflet({

    Field_map_data2436() %>%
      addProviderTiles("Esri.WorldImagery")
  })


  #SCAL Map data
  Field_mapSCAL <- reactive({ tm_shape(mapSCAL_and_dataSCAL_cleaned) +tm_polygons(midpoint = 0,col = "Treatment",
                                                                       popup.vars = c("Sector No: " = "section_nu",
                                                                                      "Treatment: " = "Treatment"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Field_map_dataSCAL <- reactive(tmap_leaflet(Field_mapSCAL()))


  output$Field_map_dataSCAL <- renderLeaflet({

    Field_map_dataSCAL() %>%
      addProviderTiles("Esri.WorldImagery")
  })


  ###########################Nitrogen_Map############################################

  ####2436 S


  # Reactive function for NRx map based on selected input
  NRx_tm_map2436 <- reactive({
    if (input$NRx_variable2436 == 'AppliedRate_03_11') {
      tm_shape(map_data_2436_PF_3_11_cleaned) +
        tm_polygons(
          col = input$NRx_variable2436,
          popup.vars = c(
            "Sector No: " = "FID_22_243",
            "Target Rate: "="TargetRate",
            "N rate for date" = input$NRx_variable2436,
            "Treatment Method: " = "TRT_Name",
            "Product: " = "Product"
          ),
          midpoint = 0
        ) +
        tm_borders(lwd = 0.5) +
        tm_layout(
          frame = FALSE,
          inner.margins = c(0, 0, 0, 0)
        )
    } else {
      tm_shape(map_data_2436_PF_6_24_cleaned) +
        tm_polygons(
          #col = input$NRx_variable2436,
          col = "AppliedRat",
          popup.vars = c(
            "Sector No: " = "FID_22_243",
            "Target Rate: "="TargetRate",
            "N rate for date:" =  "AppliedRat",
            "Treatment Method: " = "TRT_Name",
            "Product: " = "Product"
          ),
          midpoint = 0
        ) +
        tm_borders(lwd = 0.5) +
        tm_layout(
          frame = FALSE,
          inner.margins = c(0, 0, 0, 0)
        )
    }
  })

  # Render the Leaflet map
  register_n_proxy_map(
    "NRxmap2436", reactive(input$NRx_variable2436), reactive({
      if (identical(input$NRx_variable2436, "AppliedRate_03_11")) {
        list(data = map_data_2436_PF_3_11_cleaned, value_col = "AppliedRate_03_11",
             popup_cols = c("Sector" = "FID_22_243", "Target Rate" = "TargetRate", "Applied Rate" = "AppliedRate_03_11", "Treatment" = "TRT_Name", "Product" = "Product"))
      } else {
        list(data = map_data_2436_PF_6_24_cleaned, value_col = "AppliedRat",
             popup_cols = c("Sector" = "FID_22_243", "Target Rate" = "TargetRate", "Applied Rate" = "AppliedRat", "Treatment" = "TRT_Name", "Product" = "Product"))
      }
    }))

  ##############scal


  NRx_tm_mapSCAL <- reactive({
    tm_mapSCAL <- tm_shape(map_SCAL_N_1) +
      tm_polygons(
        col = input$NRx_variableSCAL,
        popup.vars = c(
          "Sector No: " = "section_nu",
          "N rate for date" = input$NRx_variableSCAL,
          "Treatment Method: " = "Treatment"
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      )
    return(tm_mapSCAL)
  })

  register_n_proxy_map(
    "NRxmapSCAL", reactive(input$NRx_variableSCAL), reactive({
      column <- input$NRx_variableSCAL
      req(column, column %in% names(map_SCAL_N_1))
      list(data = map_SCAL_N_1, value_col = column,
           popup_cols = c("Sector" = "section_nu", "Applied Rate" = column, "Treatment" = "Treatment"))
    }))

  ########################SI_Map##############################################

  ####2436S


  si_2436_layer <- reactive({
    variable <- input$SI_variable2436
    req(variable, variable %in% names(SI2436_0405_cleaned))

    values <- SI2436_0405_cleaned[[variable]]
    palette <- leaflet::colorBin(
      palette = c("#d73027", "#f2c94c", "#2e7d32"),
      bins = c(-Inf, 0.95, 0.96, Inf),
      domain = values,
      na.color = "#d1d5db",
      right = FALSE
    )

    popup <- paste0(
      "<strong>Sector:</strong> ", htmltools::htmlEscape(SI2436_0405_cleaned$FID_22_243),
      "<br><strong>Treatment:</strong> ", htmltools::htmlEscape(SI2436_0405_cleaned$TRT_Name),
      "<br><strong>SI:</strong> ", ifelse(is.na(values), "Not available", round(values, 3))
    )

    list(values = values, palette = palette, popup = popup)
  })

  # Build the basemap once. Date changes update only the SI polygons and legend.
  output$SImap2436S <- renderLeaflet({
    layer <- si_2436_layer()
    bounds <- sf::st_bbox(sf::st_transform(SI2436_0405_cleaned, 4326))

    leaflet(SI2436_0405_cleaned) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(
        fillColor = layer$palette(layer$values),
        fillOpacity = 0.72,
        color = "#4b5563",
        weight = 0.5,
        popup = layer$popup,
        group = "si-2436"
      ) %>%
      addLegend(
        pal = layer$palette,
        values = layer$values,
        title = "Sufficiency Index",
        opacity = 0.9,
        layerId = "si-2436-legend"
      ) %>%
      fitBounds(bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]])
  })

  observeEvent(input$SI_variable2436, {
    layer <- si_2436_layer()

    leafletProxy("SImap2436S", data = SI2436_0405_cleaned) %>%
      clearGroup("si-2436") %>%
      removeControl("si-2436-legend") %>%
      addPolygons(
        fillColor = layer$palette(layer$values),
        fillOpacity = 0.72,
        color = "#4b5563",
        weight = 0.5,
        popup = layer$popup,
        group = "si-2436"
      ) %>%
      addLegend(
        pal = layer$palette,
        values = layer$values,
        title = "Sufficiency Index",
        opacity = 0.9,
        layerId = "si-2436-legend"
      )
  }, ignoreInit = TRUE)


  ######SCAL


  reactive_tm_mapSCAL <- reactive({
    tm_map <- tm_shape(SI_SCAL_cleaned) +

      tm_polygons(midpoint = 0,
                  col = input$SI_variableSCAL, #col for legend
                  style = "fixed",
                  breaks = c(-Inf, 0.95, 0.96, Inf),
                  palette = c("red", "yellow", "green"),

                  popup.vars = c("Sector No: " = "section_nu",
                                 "Treatment: " = "Treatment",
                                 "SI" = input$SI_variableSCAL))+


      tm_borders(lwd = 0.5) +
      tm_layout(
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      )
    return(tm_map)
  })

  # Create the Leaflet map using the reactive_tm_map
  register_si_proxy_map(
    "SImapSCAL", reactive(input$SI_variableSCAL),
    reactive(SI_SCAL_cleaned), "section_nu", "Treatment"
  )

  ##############################Yield_Map######################################

  #2436

  # Create a thematic map object for the yield data
  Field_map2436s <- reactive({ tm_shape(mapYield_cleaned) +
    tm_polygons(midpoint = 0, col = "yield",
                popup.vars = c("Sector No:" = "FID_22_243",
                               "Yield bu/ac:" = "yield",
                               "Treatment:" = "TRT_Name")) +
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })

  # Convert to Leaflet interactive map
  Yield_2436S <- reactive(tmap_leaflet(Field_map2436s()))
  output$Yield_2436S1 <- renderLeaflet({
    Yield_2436S() %>%
      addProviderTiles("Esri.WorldImagery")
  })

  #SCAL

  Field_mapSCAL <- reactive({ tm_shape(mapYieldL_cleaned) +tm_polygons(midpoint = 0,col = "Yield_mean",
                                                            popup.vars = c("Sector No: " = "section_nu",
                                                                           "Yield bu/ac: " = "Yield_mean",
                                                                           "Treatment: " = "Treatment"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Yield_SCAL <- reactive(tmap_leaflet(Field_mapSCAL()))


  output$Yield_SCAL <- renderLeaflet({

    Yield_SCAL() %>%
      addProviderTiles("Esri.WorldImagery")
  })

  #################Plots#################################################

  ###############N rate plot################################################


  # 2436S


  output$Nplot2_2436S <- plotly::renderPlotly({
    # Calculate the averages for each treatment and date
    avg_data <- data2436_PF_3_11 %>%
      group_by(TRT_Name) %>%
      summarize(
        Avg_AppliedRate_03_11 = mean(AppliedRate_03_11),
        Avg_AppliedRate_06_24 = mean(AppliedRate_06_24)
      )

    # Reshape the data to a long format for plotting
    plot_data <- avg_data %>%
      pivot_longer(
        cols = starts_with("Avg_"),
        names_to = "Date",
        values_to = "Avg_AppliedRate"
      ) %>%
      mutate(Date = recode(Date,
                           "Avg_AppliedRate_03_11" = "2024-03-11",
                           "Avg_AppliedRate_06_24" = "2024-06-24"))

    # Generate the ggplot object
    gg <- ggplot(plot_data, aes(x = Date, y = Avg_AppliedRate, color = TRT_Name, group = TRT_Name)) +
      geom_line() +
      geom_point(aes(
        text = paste(
          "Treatment:", TRT_Name,
          "<br>Date:", Date,
          "<br>Avg Applied Rate:", round(Avg_AppliedRate, 2), "lbs N"
        )
      ), size = 3) +
      labs(
        title = "Average Applied Rate by Treatment",
        x = "Date",
        y = "Average Applied Rate (lbs N)",
        color = "Treatment"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Convert ggplot to Plotly and remove redundant hover info
    plotly::ggplotly(gg, tooltip = "text") %>%
      plotly::layout(hovermode = "closest")
  })


  output$Nplot1_2436S <- plotly::renderPlotly({

    data_long <- data2436_PF_3_11 %>%
      pivot_longer(
        cols = starts_with("AppliedRate"),
        names_to = "Date",
        values_to = "AppliedRate"
      )

    # Create the ggplot with custom tooltip text
    p <- ggplot(data_long, aes(x = FID_22_243, y = AppliedRate, color = Date, group = Date)) +
      #geom_line(size = 0.5) +
      geom_point(size = 1, aes(text = paste("Sector No:", FID_22_243,
                                            "<br>Treatment:", TRT_Name,
                                            "<br>Applied Rate:", AppliedRate,
                                            "<br>Date:", Date))) +

      #make x axis all the values
      scale_x_continuous(breaks = unique(data_long$FID_22_243)) +


      labs(
        x = "Sector",
        y = "Applied Rate (lbs N)",
        title = "Applied Rate per Sector by Date"
      ) +
      theme_minimal()
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

    # Convert ggplot to plotly with specified tooltip
    plotly::ggplotly(p, tooltip = "text")

  })


  output$Nplot3_2436S <- plotly::renderPlotly({
    # Combine the applied rates into a single long-format column for box plot visualization
    boxplot_data <- data2436_PF_3_11 %>%
      pivot_longer(
        cols = c(AppliedRate_03_11, AppliedRate_06_24),
        names_to = "Date",
        values_to = "AppliedRate"
      ) %>%
      mutate(Date = recode(Date,
                           "AppliedRate_03_11" = "2024-03-11",
                           "AppliedRate_06_24" = "2024-06-24"))

    # Calculate Total_N_rate for each treatment
    total_n_rate <- data2436_PF_3_11 %>%
      group_by(TRT_Name) %>%
      summarize(Total_N_rate = mean(AppliedRate_03_11, na.rm = TRUE) +
                  mean(AppliedRate_06_24, na.rm = TRUE)) %>%
      ungroup()

    # Generate the ggplot object
    gg <- ggplot(boxplot_data, aes(x = TRT_Name, y = AppliedRate, fill = TRT_Name)) +
      geom_boxplot(outlier.colour = "red", outlier.size = 2) +
      labs(
        title = "Distribution of Applied Rates by Treatment",
        x = "Treatment",
        y = "Applied Rate (lbs N)",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      # Overlay a single point for Total_N_rate per treatment
      geom_point(
        data = total_n_rate,
        aes(x = TRT_Name, y = Total_N_rate,
            text = paste("Treatment:", TRT_Name,
                         "<br>Total N Rate:", round(Total_N_rate, 2))),
        color = "blue", size = 4, shape = 18
      )

    # Convert ggplot to Plotly for interactivity
    plotly::ggplotly(gg, tooltip = "text")
  })

  ##### Scal

  output$Nplot1_SCAL <- plotly::renderPlotly({

    data_long <- SCAL_N_Data %>%
      pivot_longer(
        cols = starts_with("AppliedRate"),
        names_to = "Date",
        values_to = "AppliedRate"
      )

    # Create the ggplot with text for hover info
    p <- ggplot(data_long, aes(x = section_nu, y = AppliedRate, color = Date, group = Date)) +
      # geom_line(size = 0.5) +
      geom_point(size = 2, aes(text = paste("Sector No:", section_nu,
                                            "<br>Treatment:", Treatment,
                                            "<br>Applied Rate:", AppliedRate))) +
      #make x axis all the values
      scale_x_continuous(breaks = unique(data_long$section_nu)) +
      #make y axis all the values
      scale_y_continuous(breaks = unique(data_long$AppliedRate)) +

      labs(
        x = "Sector",
        y = "Applied Rate (lbs N)",
        title = "Applied Rate per Sector by Date"
      ) +
      theme_minimal()

    # Convert ggplot to plotly and specify tooltips
    plotly::ggplotly(p, tooltip = "text")

  })


  output$Nplot2_SCAL <- plotly::renderPlotly({
    # Calculate the averages for each treatment and date
    avg_data <- SCAL_N_Data %>%
      group_by(Treatment) %>%
      summarize(
        Avg_AppliedRate_04_15 = mean(AppliedRate_04_15),
        Avg_AppliedRate_07_12 = mean(AppliedRate_07_12),
        Avg_AppliedRate_07_30 = mean(AppliedRate_07_30)
      )

    # Reshape the data to a long format for plotting
    plot_data <- avg_data %>%
      pivot_longer(
        cols = starts_with("Avg_"),
        names_to = "Date",
        values_to = "Avg_AppliedRate"
      ) %>%
      mutate(Date = recode(Date,
                           "Avg_AppliedRate_04_15" = "2024-04-15",
                           "Avg_AppliedRate_07_12" = "2024-07-12",
                           "Avg_AppliedRate_07_30" = "2024-07-30"))

    # Generate the ggplot object
    gg <- ggplot(plot_data, aes(x = Date, y = Avg_AppliedRate, color = Treatment, group = Treatment)) +
      geom_line() +
      geom_point(aes(
        text = paste(
          "Treatment:", Treatment,
          "<br>Date:", Date,
          "<br>Avg Applied Rate:", round(Avg_AppliedRate, 2), "lbs N"
        )
      ), size = 3) +
      labs(
        title = "Average Applied Rate by Treatment",
        x = "Date",
        y = "Average Applied Rate (lbs N)",
        color = "Treatment"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Convert ggplot to Plotly and remove redundant hover info
    plotly::ggplotly(gg, tooltip = "text") %>%
      plotly::layout(hovermode = "closest")
  })


  output$Nplot3_SCAL <- plotly::renderPlotly({
    # Calculate the average applied rate for each sector and treatment
    avg_sector_data <- SCAL_N_Data %>%
      group_by(section_nu, Treatment) %>%
      summarize(
        Avg_AppliedRate_04_15 = mean(AppliedRate_04_15),
        Avg_AppliedRate_07_12 = mean(AppliedRate_07_12),
        Avg_AppliedRate_07_30 = mean(AppliedRate_07_30)
      )

    # Reshape the data to long format for plotting
    boxplot_data <- avg_sector_data %>%
      pivot_longer(
        cols = starts_with("Avg_"),
        names_to = "Date",
        values_to = "Avg_AppliedRate"
      ) %>%
      mutate(Date = recode(Date,
                           "Avg_AppliedRate_04_15" = "2024-04-15",
                           "Avg_AppliedRate_07_12" = "2024-07-12",
                           "Avg_AppliedRate_07_30" = "2024-07-30"))


    # Calculate Total_N_rate for each treatment
    total_n_rate <- SCAL_N_Data %>%
      group_by(Treatment) %>%
      summarize(Total_N_rate = mean(AppliedRate_04_15, na.rm = TRUE) +
                  mean(AppliedRate_07_12, na.rm = TRUE)+ mean(AppliedRate_07_30, na.rm = TRUE)) %>%
      ungroup()



    gg <- ggplot(boxplot_data, aes(x = Treatment, y = Avg_AppliedRate, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.size = 2) +
      labs(
        title = "Distribution of Applied Rates by Treatment",
        x = "Treatment",
        y = "Applied Rate (lbs N)",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      # Overlay a single point for Total_N_rate per treatment
      geom_point(
        data = total_n_rate,
        aes(x = Treatment, y = Total_N_rate,
            text = paste("Treatment:", Treatment,
                         "<br>Total N Rate:", round(Total_N_rate, 2))),
        color = "blue", size = 4, shape = 18
      )

    # Convert ggplot to Plotly for interactivity
    plotly::ggplotly(gg, tooltip = "text")

  })

  ###############SI plot################################################

  ####SCAL

  output$SIplot1_SCAL <- plotly::renderPlotly({

    # Reshape the data to a long format
    data_long <- ndre_SCAL %>%
      pivot_longer(cols = starts_with("SI_"),
                   names_to = "Date",
                   values_to = "SI") %>%
      # mutate(Date = as.Date(sub("SI_", "", Date), format = "%m_%d"))
      mutate(Date = as.Date(paste0("2024-", sub("SI_", "", Date)), format = "%Y-%m_%d"))

    # Calculate the average SI for each treatment and date
    avg_si <- data_long %>%
      group_by(Treatment, Date) %>%
      summarise(avg_SI = mean(SI, na.rm = TRUE))

    # Create the line chart with average SI for each treatment
    ggplot(avg_si, aes(x = Date, y = avg_SI, color = Treatment, #group = Treatment
    )) +
      geom_line() +
      geom_point() +
      geom_vline(aes(xintercept = as.numeric(as.Date("2024-08-05"))),
                 linetype = "dashed", color = "red", size = 1, inherit.aes = FALSE) +
      geom_text(aes(x = as.Date("2024-08-05"), y = max(avg_si$avg_SI, na.rm = FALSE),
                    label = "Treatment Stopped"), color = "red", hjust = 0, vjust = 1.5) +
      labs(title = "Average SI Values by Treatment",
           x = "Date",
           y = "Average SI Value",
           color = "Treatment") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      theme(legend.position = "right")

  })


  ####2436

  output$SIplot1_2436 <- plotly::renderPlotly({

    # Reshape the data to a long format
    data_long <- SI2436_0405 %>%
      pivot_longer(cols = starts_with("SI_"),
                   names_to = "Date",
                   values_to = "SI") %>%
      mutate(Date = as.Date(paste0("2024-", sub("SI_", "", Date)), format = "%Y-%m_%d"))

    # Calculate the average SI for each treatment and date
    avg_si <- data_long %>%
      group_by(TRT_Name, Date) %>%
      summarise(avg_SI = mean(SI, na.rm = TRUE))

    # Create the line chart with average SI for each treatment
    p <- ggplot(avg_si, aes(x = Date, y = avg_SI, color = TRT_Name)) +
      geom_line() +
      geom_point() +
      geom_vline(aes(xintercept = as.numeric(as.Date("2024-08-03"))),
                 linetype = "dashed", color = "red", size = 1, inherit.aes = FALSE) +
      geom_text(aes(x = as.Date("2024-08-03"), y = max(avg_si$avg_SI, na.rm = FALSE),
                    label = "Treatment Stopped"), color = "red", hjust = 0, vjust = 1.5) +
      labs(title = "Average SI Values by Treatment",
           x = "Date",
           y = "Average SI Value",
           color = "Treatment") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      theme(legend.position = "right")

    # Convert ggplot to plotly
    plotly_p <- plotly::ggplotly(p)

    return(plotly_p)

  })


  #################Yield Plot######################################################

  # SCAL

  output$Yieldplot1_SCAL <- plotly::renderPlotly({

    mean_yield <- data_yield_SCAL %>%
      group_by(Treatment) %>%
      summarize(mean_yield = mean(Yield_mean, na.rm = TRUE))


    ggplot_object <- ggplot(data_yield_SCAL, aes(x = Treatment, y = Yield_mean, fill = Treatment)) +
      geom_boxplot(color = "black") +
      labs(title = "Distribution of Yield by Treatment",
           x = "Treatment",
           y = "Yield (Bu/ac)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3") +
      # Add mean points with popup text
      geom_point(data = mean_yield, aes(x = Treatment, y = mean_yield,
                                        text = paste("Treatment:", Treatment,
                                                     "<br>Mean Yield:", round(mean_yield, 2))),
                 color = "red", size = 3, shape = 18)

    plotly_object <- plotly::ggplotly(ggplot_object, tooltip = "text")

    plotly_object

  })

  # 2436S

  output$Yieldplot1_2436 <- plotly::renderPlotly({


    mean_yield <- data_yield_2436s %>%
      group_by(TRT_Name) %>%
      summarize(mean_yield = mean(yield, na.rm = TRUE))


    ggplot_object <- ggplot(data_yield_2436s, aes(x = TRT_Name, y = yield, fill = TRT_Name)) +
      geom_boxplot(color = "black") +
      labs(title = "Distribution of Yield by Treatment",
           x = "Treatment",
           y = "Yield (Bu/ac)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3") +
      # Add mean points with popup text
      geom_point(data = mean_yield, aes(x = TRT_Name, y = mean_yield,
                                        text = paste("Treatment:", TRT_Name,
                                                     "<br>Mean Yield:", round(mean_yield, 2))),
                 color = "red", size = 3, shape = 18)


    plotly_object <- plotly::ggplotly(ggplot_object, tooltip = "text")


    plotly_object

  })

  ##########################Summary Report ############################

  ##########################2436############################

  output$data_table_2436S <- renderDT({

    Area_in_Acres <- data2436_PF_3_11 %>%
      group_by(TRT_Name) %>%
      summarize(
        Area_in_Acres = mean(Area * 2.47105)  # Convert area to acres (mean for each treatment)
      )

    # Calculate the average yield per treatment
    mean_yield <- data_yield_2436s %>%
      group_by(TRT_Name) %>%
      summarize(mean_yield = mean(yield, na.rm = TRUE))

    # Calculate the required metrics for each treatment from prescription data
    result <- data2436_PF_3_11 %>%
      group_by(TRT_Name) %>%
      summarize(
        # Calculate the average applied rate for each treatment
        Avg_AppliedRate_03_11 = mean(AppliedRate_03_11, na.rm = TRUE),
        Avg_AppliedRate_06_24 = mean(AppliedRate_06_24, na.rm = TRUE),

        # Calculate total N rate as the sum of the two applied rates
        Total_N_rate = mean(AppliedRate_03_11, na.rm = TRUE) + mean(AppliedRate_06_24, na.rm = TRUE)
      )


    # Merge the calculated Total N rate data with the average yield data
    result_with_yield <- result %>%
      left_join(mean_yield, by = "TRT_Name")

    # Merge the Area_in_Acres data with the result_with_yield data
    final_result <- result_with_yield %>%
      left_join(Area_in_Acres, by = "TRT_Name")

    # Calculate Nitrogen Efficiency (lb N/bu grain) and Marginal Net Return (MNR)
    final_result <- final_result %>%
      mutate(
        NUE = Total_N_rate / mean_yield,  # lb N per bu grain

        # Calculate Partial Profit ($/ac) with specific costs for N sources
        MNR = (mean_yield * 4) - (Avg_AppliedRate_03_11 * 0.43) - (Avg_AppliedRate_06_24 * 0.6)
      )

    # Select and print only the specified columns
    final_result_selected <- final_result %>%
      mutate(across(where(is.numeric))) %>%
      #mutate(across(where(is.numeric), ~ round(., 1))) %>%
      select(TRT_Name, Total_N_rate, mean_yield, NUE, MNR)

    final_result_selected
    # Merge the dataframes so that Total_N_rate and yield can be used in the ANOVA
    merged_data <- data2436_PF_3_11 %>%
      left_join(data_yield_2436s, by = "TRT_Name") %>%
      mutate(
        Total_N_rate = AppliedRate_03_11 + AppliedRate_06_24,
        NUE = Total_N_rate / yield,

        # Adjust MNR calculation to reflect specific costs for N sources
        MNR = (yield * 4) - (AppliedRate_03_11 * 0.43) - (AppliedRate_06_24 * 0.6)
      )

    # Perform ANOVA for each of the variables (across all treatments)
    anova_results <- data.frame(
      Total_N_rate_pvalue = summary(aov(Total_N_rate ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1],
      mean_yield_pvalue = summary(aov(yield ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1],
      NUE_pvalue = summary(aov(NUE ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1],
      MNR_pvalue = summary(aov(MNR ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1]
    )

    # Format p-values: If p-value < 0.01, print "<0.01", otherwise print rounded p-values
    anova_results <- anova_results %>%
      mutate(
        Total_N_rate_pvalue = ifelse(Total_N_rate_pvalue < 0.01, " <0.01", round(Total_N_rate_pvalue, 3)),
        mean_yield_pvalue = ifelse(mean_yield_pvalue < 0.01, " <0.01", round(mean_yield_pvalue, 3)),
        NUE_pvalue = ifelse(NUE_pvalue < 0.01, " <0.01", NUE_pvalue),
        MNR_pvalue = ifelse(MNR_pvalue < 0.01, "<0.01", round(MNR_pvalue, 3))
      )

    # Round numeric values to 2 decimal places, except for p-values
    final_result_selected_rounded <- final_result_selected %>%
      mutate(
        Total_N_rate = round(Total_N_rate),
        mean_yield = round(mean_yield),
        #NUE = NUE,
        NUE = round(NUE, 2),
        MNR = round(MNR)
      )

    # Convert the numeric columns to characters before binding rows
    final_result_selected_char <- final_result_selected_rounded %>%
      mutate(across(everything(), as.character))

    # Now append these p-values to the final_result_selected table
    final_result_with_pvalues <- final_result_selected_char %>%
      bind_rows(
        tibble(
          TRT_Name = "P-Value",
          Total_N_rate = as.character(anova_results$Total_N_rate_pvalue),
          mean_yield = as.character(anova_results$mean_yield_pvalue),
          NUE = as.character(anova_results$NUE_pvalue),
          MNR = as.character(anova_results$MNR_pvalue)
        )
      )

    final_result_with_pvalues <- final_result_with_pvalues %>%
      rename(
        "Treatment Type" = TRT_Name ,
        "Total N Rate (lb-N/ac)" = Total_N_rate,
        " Yield (bu/ac)" = mean_yield,
        "Nitrogen Efficiency (lb N/bu grain)" = NUE,
        "MNR($/ac)" = MNR
      )


    # Render the datatable and apply styling to font color of p-value row
    datatable(final_result_with_pvalues) %>%
      formatStyle(
        'Treatment Type',
        target = 'row',
        color = styleEqual("P-Value", 'blue')  # Change font color of P-Value row to blue
      )

  })


  ##########################SCAL###########################


  output$data_table_SCAL <- renderDT({

    # Calculate the average yield per treatment
    mean_yield_SCAL <- data_yield_SCAL %>%
      group_by(Treatment) %>%
      summarize(mean_yield = mean(Yield_mean, na.rm = TRUE))

    # Calculate the required metrics for each treatment
    result <- SCAL_N_Data %>%
      group_by(Treatment) %>%
      summarize(
        Avg_AppliedRate_04_15 = mean(AppliedRate_04_15, na.rm = TRUE),
        Avg_AppliedRate_07_12 = mean(AppliedRate_07_12, na.rm = TRUE),
        Avg_AppliedRate_07_30 = mean(AppliedRate_07_30, na.rm = TRUE),
        Total_N_rate = mean(AppliedRate_04_15, na.rm = TRUE) +
          mean(AppliedRate_07_12, na.rm = TRUE) +
          mean(AppliedRate_07_30, na.rm = TRUE)
      )

    # Merge with yield data
    result_with_yield <- result %>%
      left_join(mean_yield_SCAL, by = "Treatment")

    # Calculate NUE and MNR
    final_result <- result_with_yield %>%
      mutate(
        NUE = Total_N_rate / mean_yield,
        MNR = (mean_yield * 4) - (Total_N_rate * 0.6)
      )

    # Combine the original result table with the p-value row
    final_result_selected <- final_result %>%
      select(Treatment, Total_N_rate, mean_yield, NUE, MNR)

    # Merge the dataframes so that Total_N_rate and yield can be used in the ANOVA
    merged_data <- SCAL_N_Data %>%
      left_join(data_yield_SCAL, by = "Treatment") %>%
      mutate(
        Total_N_rate = AppliedRate_04_15 + AppliedRate_07_12 + AppliedRate_07_30,
        NUE = Total_N_rate / Yield_mean,
        MNR = (Yield_mean * 4) - (Total_N_rate * 0.6)
      )

    # Perform ANOVA for each of the variables (across all treatments)
    anova_results <- data.frame(
      Total_N_rate_pvalue = summary(aov(Total_N_rate ~ Treatment, data = merged_data))[[1]]$`Pr(>F)`[1],
      mean_yield_pvalue = summary(aov(Yield_mean ~ Treatment, data = merged_data))[[1]]$`Pr(>F)`[1],
      NUE_pvalue = summary(aov(NUE ~ Treatment, data = merged_data))[[1]]$`Pr(>F)`[1],
      MNR_pvalue = summary(aov(MNR ~ Treatment, data = merged_data))[[1]]$`Pr(>F)`[1]
    )

    # Format p-values: If p-value < 0.01, print "<0.01", otherwise print rounded p-values
    anova_results <- anova_results %>%
      mutate(
        Total_N_rate_pvalue = ifelse(Total_N_rate_pvalue < 0.01, " <0.01", round(Total_N_rate_pvalue, 3)),
        mean_yield_pvalue = ifelse(mean_yield_pvalue < 0.01, " <0.01", round(mean_yield_pvalue, 3)),
        NUE_pvalue = ifelse(NUE_pvalue < 0.01, " <0.01", round(NUE_pvalue, 3)),
        MNR_pvalue = ifelse(MNR_pvalue < 0.01, "<0.01", round(MNR_pvalue, 3))
      )

    # Round numeric values to 2 decimal places, except for p-values
    final_result_selected_rounded <- final_result_selected %>%
      mutate(
        Total_N_rate = round(Total_N_rate, 2),
        mean_yield = round(mean_yield),
        NUE = round(NUE, 2),
        MNR = round(MNR)
      )

    # Convert the numeric columns to characters before binding rows
    final_result_selected_char <- final_result_selected_rounded %>%
      mutate(across(everything(), as.character))

    # Now append these p-values to the final_result_selected table
    final_result_with_pvalues <- final_result_selected_char %>%
      bind_rows(
        tibble(
          Treatment = "P-Value",
          Total_N_rate = as.character(anova_results$Total_N_rate_pvalue),
          mean_yield = as.character(anova_results$mean_yield_pvalue),
          NUE = as.character(anova_results$NUE_pvalue),
          MNR = as.character(anova_results$MNR_pvalue)
        )
      )
    # Rename the column headings in final_result_with_pvalues
    final_result_with_pvalues <- final_result_with_pvalues %>%
      rename(
        "Treatment Type" = Treatment,
        "Total N Rate (lb-N/ac)" = Total_N_rate,
        " Yield (bu/ac)" = mean_yield,
        "Nitrogen Efficiency (lb N/bu grain)" = NUE,
        "MNR($/ac)" = MNR
      )


    # Render the datatable and apply styling to font color of p-value row
    datatable(final_result_with_pvalues) %>%
      formatStyle(
        'Treatment Type',
        target = 'row',
        color = styleEqual("P-Value", 'blue')  # Change font color of P-Value row to blue
      )

  })


  outputOptions(output, "data_table_SCAL", suspendWhenHidden = TRUE)
  onFlush(function() {
    shinyjs::hide("loading_message")  # Hide the loading message
  }, once = TRUE)




  observeEvent(input$GrowingSeason, {
    updateSelectInput(
      session,
      "AvailableFields",
      choices = agdata_field_choices(input$GrowingSeason)
    )
  })











  #-------------------------------------------------------------------2025---------------------------------------------------------------------------





  #################################---------------Data--------------------------##############################


  #1402W


  ##########Treatment_Polygons
  field_1402W_25 <- reactive({
    map <- st_read("data/2025/1402W/Treatment_Polygons/25_1402W_plotmap_040325.shp")
    treatment <- read.csv("data/2025/1402W/Treatment_Polygons/25_1402W_plotmap_040325.csv")
    buffered <- read.csv("data/2025/1402W/Treatment_Polygons/25_1402W_bufferedplotmap.csv")
    april <- read.csv("data/2025/1402W/Prescription_File/04_07/04_07.csv")
    may <- read.csv("data/2025/1402W/Prescription_File/05_07/05_07.csv")
    yield <- read.csv("data/2025/1402W/Yield/Yield_1402W.csv")
    april_map <- inner_join(map, april); may_map <- inner_join(map, may)
    april_df <- april_map %>% st_drop_geometry() %>% select(Field_ID, Treatment, Product, AppliedRate = Rt_Apd_Ms_) %>% mutate(Date = as.Date("2025-04-07"))
    may_df <- may_map %>% st_drop_geometry() %>% select(Field_ID, Treatment, Product, AppliedRate = Rt_Apd_Liq) %>% mutate(Date = as.Date("2025-05-07"))
    list(map = map, treatment = treatment, buffered = buffered,
         map_data = inner_join(map, treatment), april = april, may = may,
         april_map = april_map, may_map = may_map, combined_df = bind_rows(april_df, may_df),
         si = inner_join(map, read.csv("data/2025/1402W/Aerial_Imagery/SI_1402W.csv")),
         yield = yield, yield_map = inner_join(map, yield))
  })
  lazy_1402W_binding <- function(component) { force(component); function(value) {
    if (!missing(value)) stop("1402 W lazy data bindings are read-only")
    isolate(field_1402W_25())[[component]]
  }}
  bindings_1402W <- c(map1402W="map", data1402W="treatment", buffdata1402W="buffered",
                      map_data_1402W="map_data", data1402W_25_0407="april", data1402W_25_0507="may",
                      map_data_1402W_25_0407="april_map", map_data_1402W_25_0507="may_map",
                      SI_1402W_25="si", Yield_1402W_df="yield", Yield_1402W_25="yield_map",
                      combined_df="combined_df")
  for (binding_name in names(bindings_1402W)) {
    makeActiveBinding(binding_name, lazy_1402W_binding(bindings_1402W[[binding_name]]), environment())
  }


  #1912


  ##########Treatment_Polygons
  field_1912_25 <- reactive({
    map <- st_read("data/2025/1912/Treatment_Polygons/25_1912_PlotMap_031025.shp")
    nrx <- st_read("data/2025/1912/Prescription_File/UNL ARDC_Farm Operati_1912_Application_2025-04-03_00.shp")
    if (is.na(st_crs(nrx)$epsg)) nrx <- st_set_crs(nrx, 4326)
    nrx <- st_transform(nrx, st_crs(map))
    nrx_by_plot <- st_join(nrx, map %>% select(Field_Id, Rate), join = st_intersects, left = FALSE) %>%
      st_drop_geometry() %>% filter(!is.na(Field_Id)) %>% group_by(Field_Id, Rate) %>%
      summarise(AppliedRate = mean(AppliedRate, na.rm = TRUE), TargetRate = mean(TargetRate, na.rm = TRUE),
                ControlRate = mean(ControlRate, na.rm = TRUE), Product = first(na.omit(as.character(Product)), default = NA_character_),
                Records = n(), .groups = "drop")
    map_data <- map %>% left_join(nrx_by_plot, by = c("Field_Id", "Rate"))
    si <- inner_join(map, read.csv("data/2025/1912/Aerial_Imagery/SI_1912.csv"))
    yield <- st_read("data/2025/1912/Yield/Yield.shp")
    list(map = map, map_data = map_data, si = si, yield = yield,
         yield_df = yield %>% st_drop_geometry() %>% mutate(VRYIELDVOL = as.numeric(VRYIELDVOL)),
         n_df = map_data %>% st_drop_geometry() %>% mutate(Field_Id = as.factor(Field_Id), Rate = as.factor(Rate), AppliedRate = as.numeric(AppliedRate)))
  })

  lazy_1912_binding <- function(component) {
    force(component)
    function(value) {
      if (!missing(value)) stop("1912 lazy data bindings are read-only")
      isolate(field_1912_25())[[component]]
    }
  }
  makeActiveBinding("map1912", lazy_1912_binding("map"), environment())
  makeActiveBinding("map_data_1912_25_0403", lazy_1912_binding("map_data"), environment())
  makeActiveBinding("SI_1912_25", lazy_1912_binding("si"), environment())
  makeActiveBinding("Yield_1912", lazy_1912_binding("yield"), environment())
  makeActiveBinding("Yield_1912_df", lazy_1912_binding("yield_df"), environment())
  makeActiveBinding("df_1912", lazy_1912_binding("n_df"), environment())



  #2436N


  ##########Treatment_Polygons
  field_2436N_25 <- reactive({
    map <- st_read("data/2025/2436N/Treatment_Polygons/25_2436N_Plotmap.shp")
    treatment <- read.csv("data/2025/2436N/Treatment_Polygons/25_2436N_Plotmap.csv")
    n_data <- read.csv("data/2025/2436N/Prescription_File/04_08/04_08.csv")
    map_data <- inner_join(map, treatment)
    n_map <- inner_join(map, n_data)
    si <- inner_join(map, read.csv("data/2025/2436N/Aerial_Imagery/SI_2436N.csv"))
    yield <- read.csv("data/2025/2436N/Yield/Yield_2436N.csv")
    list(map = map, treatment = treatment, map_data = map_data, n_data = n_data, n_map = n_map,
         si = si, yield = yield, yield_map = inner_join(map, yield),
         map_clean = remove_first_n_columns(map_data, 1), n_map_clean = remove_first_n_columns(n_map, 1),
         n_df = n_map %>% st_drop_geometry() %>% select(Sector = Plot_Numbe, Treatment = TRT_Name, Product, AppliedRate = Rt_Apd_Ms_) %>% mutate(Date = as.Date("2025-04-08")))
  })
  lazy_2436N_binding <- function(component) { force(component); function(value) {
    if (!missing(value)) stop("2436 N lazy data bindings are read-only")
    isolate(field_2436N_25())[[component]]
  }}
  bindings_2436N <- c(map2436N="map", data2436N="treatment", map_data_2436N="map_data",
                    data2436N_25_0408="n_data", map_data_2436N_25_0408="n_map",
                    SI_2436N_25="si", Yield_2436N_df="yield",
                    Yield_2436N_25="yield_map", map_data_2436N_cleaned="map_clean",
                    map_data_2436N_25_0408_cleaned="n_map_clean", df_2436N="n_df")
  for (binding_name in names(bindings_2436N)) {
    makeActiveBinding(binding_name, lazy_2436N_binding(bindings_2436N[[binding_name]]), environment())
  }





  #################################---------------Code--------------------------##############################






  # Combine all three datasets into a single map
  Combined_map_2025 <- reactive({
    req(input$GrowingSeason == "2025", identical(input$AvailableFields, "Fields"))
    overview_1402w <- inner_join(
      st_read("data/2025/1402W/Treatment_Polygons/25_1402W_plotmap_040325.shp", quiet = TRUE),
      read.csv("data/2025/1402W/Treatment_Polygons/25_1402W_plotmap_040325.csv")
    )
    overview_1912 <- st_read("data/2025/1912/Treatment_Polygons/25_1912_PlotMap_031025.shp", quiet = TRUE)
    overview_2436n <- remove_first_n_columns(inner_join(
      st_read("data/2025/2436N/Treatment_Polygons/25_2436N_Plotmap.shp", quiet = TRUE),
      read.csv("data/2025/2436N/Treatment_Polygons/25_2436N_Plotmap.csv")
    ), 1)
    tm_shape(overview_1402w) +
    tm_polygons(popup.vars = c("Sector No: " = "Field_ID",
                               "Treatment Type: " = "Treatment")) +
    tm_shape(overview_1912) +
    tm_polygons(popup.vars = c("Field ID: " = "Field_Id",
                               "N Rate: " = "Rate")) +
    tm_shape(overview_2436n) +
    tm_polygons(popup.vars = c("Sector No: " = "Plot_Numbe",
                               "Treatment Type: " = "TRT_Name"
    )) +
    tm_borders(lwd = 0.5) +
    tm_layout(frame = FALSE,
              inner.margins = c(0, 0, 0, 0)) })

  # Convert the combined map to Leaflet format
  Combined_map_leaflet_2025 <- reactive(tmap_leaflet(Combined_map_2025()))

  output$all_maps_2025 <- renderLeaflet({
    # Render the map after loading
    Combined_map_leaflet_2025() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(
        lng = c(-96.4784223, -96.4962672, -96.4488),
        lat = c(41.1743165, 41.1780504, 41.17298),
        popup = c("2436 N Field", "1402 W Field", "1912 Field")
      ) %>%
      fitBounds(
        lng1 = -96.4962672, lat1 = 41.17092,
        lng2 = -96.44770, lat2 = 41.1780504
      )


  })



  #################################---------------Map_Code--------------------------##############################




  #1402W Map data


  Field_map1402W <- reactive({ tm_shape(map_data_1402W) +tm_polygons(midpoint = 0,col = "Treatment",
                                                                       popup.vars = c("Sector No: " = "Field_ID",
                                                                                      "Treatment Type: " = "Treatment"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Field_map_data1402W <- reactive(tmap_leaflet(Field_map1402W()))


  output$Field_map_data1402W<- renderLeaflet({

    Field_map_data1402W() %>%
      addProviderTiles("Esri.WorldImagery")
  })


  #1912 Map data

  Field_map1912 <- reactive({ tm_shape(map1912) +
    tm_polygons(
      midpoint = 0,
      col = "Rate",
      popup.vars = c(
        "Field ID: " = "Field_Id",
        "N Rate: " = "Rate"
      )
    ) +
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Field_map_data1912 <- reactive(tmap_leaflet(Field_map1912()))


  output$Field_map_data1912 <- renderLeaflet({

    Field_map_data1912() %>%
      addProviderTiles("Esri.WorldImagery")
  })





  #2436N Map data

  Field_map2436N <- reactive({ tm_shape(map_data_2436N_cleaned) +tm_polygons(midpoint = 0,col = "TRT_Name",
                                                                       popup.vars = c("Sector No: " = "Plot_Numbe",
                                                                                      "Treatment Type: " = "TRT_Name"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Field_map_data2436N <- reactive(tmap_leaflet(Field_map2436N()))


  output$Field_map_data2436N <- renderLeaflet({

    Field_map_data2436N() %>%
      addProviderTiles("Esri.WorldImagery")
  })




  #-------------------------------------------------------------------2026---------------------------------------------------------------------------

  field_1402E_26 <- reactive({
    map <- st_read("data/2026/1402E/Treatment_Polygons/26_1402E_plotmap.shp")
    nrx_march <- st_read("data/2026/1402E/Prescription_File/26_1402E_NH3_asapplied.shp")
    nrx_june <- st_read("data/2026/1402E/Prescription_File/06_30/06_30.shp")

    if (is.na(st_crs(nrx_march)$epsg)) nrx_march <- st_set_crs(nrx_march, 4326)
    nrx_march <- st_transform(nrx_march, st_crs(map))
    if (is.na(st_crs(nrx_june)$epsg)) nrx_june <- st_set_crs(nrx_june, st_crs(map))
    nrx_june <- st_transform(nrx_june, st_crs(map))

    march_by_plot <- st_join(
      nrx_march, map %>% select(Plot_Numbe),
      join = st_intersects, left = FALSE
    ) %>%
      mutate(TreatmentDate = as.Date(as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))) %>%
      st_drop_geometry() %>% filter(!is.na(Plot_Numbe)) %>%
      group_by(Plot_Numbe) %>%
      summarise(
        AppliedRate = mean(AppliedRat, na.rm = TRUE),
        TargetRate = mean(TargetRate, na.rm = TRUE),
        ControlRate = mean(ControlRat, na.rm = TRUE),
        Product = first(na.omit(as.character(Product)), default = NA_character_),
        Records = n(), Date = first(na.omit(TreatmentDate), default = as.Date("2026-03-13")),
        .groups = "drop"
      )

    june_by_plot <- nrx_june %>%
      mutate(TreatmentDate = as.Date(as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))) %>%
      st_drop_geometry() %>% filter(!is.na(Plot_Numbe)) %>%
      group_by(Plot_Numbe) %>%
      summarise(
        AppliedRate = mean(AppliedRat, na.rm = TRUE),
        TargetRate = mean(TargetRate, na.rm = TRUE),
        ControlRate = mean(ControlRat, na.rm = TRUE),
        Product = first(na.omit(as.character(Product)), default = NA_character_),
        Records = sum(!is.na(Time) & AppliedRat > 0, na.rm = TRUE),
        Date = first(na.omit(TreatmentDate), default = as.Date("2026-06-30")),
        .groups = "drop"
      )

    list(
      map = map,
      map_labeled = map %>% mutate(Field = "1402 E"),
      march_data = map %>% left_join(march_by_plot, by = "Plot_Numbe"),
      june_data = map %>% left_join(june_by_plot, by = "Plot_Numbe")
    )
  })
  field_2436S_26 <- reactive({
    map <- st_read("data/2026/2436S/Treatment_Polygons/26_2436S_plotmap.shp")
    nrx_raw <- st_read("data/2026/2436S/Prescription_File/26_2436S_NH3_asapplied.shp")
    if (is.na(st_crs(nrx_raw)$epsg)) {
      nrx_raw <- st_set_crs(nrx_raw, 4326)
    }
    nrx_raw <- st_transform(nrx_raw, st_crs(map))

    nrx_by_plot <- st_join(
      nrx_raw,
      map %>% select(Plot_Numbe),
      join = st_intersects,
      left = FALSE
    ) %>%
      mutate(TreatmentDate = as.Date(as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))) %>%
      st_drop_geometry() %>%
      filter(!is.na(Plot_Numbe)) %>%
      group_by(Plot_Numbe) %>%
      summarise(
        AppliedRate = mean(AppliedRat, na.rm = TRUE),
        TargetRate = mean(TargetRate, na.rm = TRUE),
        ControlRate = mean(ControlRat, na.rm = TRUE),
        Product = first(na.omit(as.character(Product)), default = NA_character_),
        Records = n(),
        Date = first(na.omit(TreatmentDate), default = as.Date("2026-03-19")),
        .groups = "drop"
      )

    list(
      map = map,
      map_labeled = map %>% mutate(Field = "2436 S"),
      map_data = map %>% left_join(nrx_by_plot, by = "Plot_Numbe")
    )
  })

  selected_map_data_1402E_26 <- reactive({
    selected_date <- input$NRx_variable1402E_26
    if (is.null(selected_date)) {
      selected_date <- "March_13"
    }

    if (selected_date == "June_30") {
      field_1402E_26()$june_data
    } else {
      field_1402E_26()$march_data
    }
  })

  output$Field_map_data1402E_26 <- renderLeaflet({
    field_map <- tm_shape(field_1402E_26()$map) +
      tm_polygons(
        midpoint = 0, col = "Treatment",
        popup.vars = c(
          "Plot No: " = "Plot_Numbe", "Treatment: " = "Treatment",
          "Cover Crop: " = "Cover_Crop", "Base Rate: " = "Base_Rate",
          "Side Rate: " = "Side_Rate"
        )
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
    tmap_leaflet(field_map) %>%
      addProviderTiles("Esri.WorldImagery")
  })

  output$Field_map_data2436S_26 <- renderLeaflet({
    field_map <- tm_shape(field_2436S_26()$map) +
      tm_polygons(
        midpoint = 0, col = "Treatment",
        popup.vars = c(
          "Plot No: " = "Plot_Numbe", "Treatment: " = "Treatment",
          "Cover Crop: " = "Cover_Crop", "Base Rate: " = "Base_Rate",
          "Side Rate: " = "Side_Rate", "Irrigation: " = "Irrigation"
        )
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
    tmap_leaflet(field_map) %>%
      addProviderTiles("Esri.WorldImagery")
  })

  output$all_maps_2026 <- renderLeaflet({
    req(input$GrowingSeason == "2026", identical(input$AvailableFields, "Fields"))
    overview_1402e <- st_read("data/2026/1402E/Treatment_Polygons/26_1402E_plotmap.shp", quiet = TRUE) %>% mutate(Field = "1402 E")
    overview_2436s <- st_read("data/2026/2436S/Treatment_Polygons/26_2436S_plotmap.shp", quiet = TRUE) %>% mutate(Field = "2436 S")
    combined_map <- tm_shape(overview_1402e) +
      tm_polygons(popup.vars = c("Field: " = "Field", "Treatment: " = "Treatment", "Plot No: " = "Plot_Numbe", "Cover Crop: " = "Cover_Crop")) +
      tm_shape(overview_2436s) +
      tm_polygons(popup.vars = c("Field: " = "Field", "Treatment: " = "Treatment", "Plot No: " = "Plot_Numbe", "Cover Crop: " = "Cover_Crop")) +
      tm_borders(lwd = 0.5) +
      tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
    tmap_leaflet(combined_map) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(
        lng = c(-96.4936478, -96.4780217),
        lat = c(41.1786222, 41.1706935),
        popup = c("1402 E Field", "2436 S Field")
      ) %>%
      fitBounds(
        lng1 = -96.4936478, lat1 = 41.1706935,
        lng2 = -96.4780217, lat2 = 41.1786222
      )
  })

  NRx_tm_map1402E_26 <- reactive({
    req(input$NRx_variable1402E_26)

    tm_shape(selected_map_data_1402E_26()) +
      tm_polygons(
        col = "AppliedRate",
        title = "Applied N Rate (lbs N/ac)",
        popup.vars = c(
          "Plot No: " = "Plot_Numbe",
          "Target Rate: " = "TargetRate",
          "Applied Rate: " = "AppliedRate",
          "Treatment: " = "Treatment",
          "Product: " = "Product",
          "Records: " = "Records"
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
  })

  register_n_proxy_map(
    "NRxmap1402E_26", reactive(input$NRx_variable1402E_26), reactive({
      list(data = selected_map_data_1402E_26(), value_col = "AppliedRate",
           popup_cols = c("Plot" = "Plot_Numbe", "Target Rate" = "TargetRate", "Applied Rate" = "AppliedRate", "Treatment" = "Treatment", "Product" = "Product", "Records" = "Records"))
    }))

  NRx_tm_map2436S_26 <- reactive({
    req(input$NRx_variable2436S_26)

    tm_shape(field_2436S_26()$map_data) +
      tm_polygons(
        col = "AppliedRate",
        title = "Applied N Rate (lbs N/ac)",
        popup.vars = c(
          "Plot No: " = "Plot_Numbe",
          "Target Rate: " = "TargetRate",
          "Applied Rate: " = "AppliedRate",
          "Treatment: " = "Treatment",
          "Product: " = "Product",
          "Irrigation: " = "Irrigation",
          "Records: " = "Records"
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
  })

  output$NRxmap2436S_26 <- renderLeaflet({
    tm_map2436S_26 <- NRx_tm_map2436S_26()
    tmap_leaflet(tm_map2436S_26) %>%
      addProviderTiles("Esri.WorldImagery")
  })




  #################################---------------N_Treatment_Map_Code--------------------------##############################




  ####1402W



  NRx_tm_map1402W_25 <- reactive({
    selected_date <- input$NRx_variable1402W_25

    if (selected_date == "April_07") {
      tm_shape(map_data_1402W_25_0407) +
        tm_polygons(
          col = "Rt_Apd_Ms_", title = "Applied N Rate (lbs N/ac)",
          popup.vars = c(
            "Sector No: " = "Field_ID",
            "Target Rate: " = "Tgt_Rate_l",
            "N rate for date" = "Rt_Apd_Ms_",
            "Treatment Method: " = "Treatment",
            "Product: " = "Product"
          ),
          midpoint = 0
        ) +
        tm_borders(lwd = 0.5) +
        tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))

    } else if (selected_date == "May_07") {
      tm_shape(map_data_1402W_25_0507) +
        tm_polygons(
          col = "Rt_Apd_Liq", title = "Applied N Rate (lbs N/ac)",
          popup.vars = c(
            "Sector No: " = "Field_ID",
            "Target Rate: " = "Tgt_Rate_g",
            "N rate for date" = "Rt_Apd_Liq",
            "Treatment Method: " = "Treatment",
            "Product: " = "Product"
          ),
          midpoint = 0
        ) +
        tm_borders(lwd = 0.5) +
        tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
    }
  })



  # Render the Leaflet map
  register_n_proxy_map(
    "NRxmap1402W_25", reactive(input$NRx_variable1402W_25), reactive({
      if (identical(input$NRx_variable1402W_25, "April_07")) {
        list(data = map_data_1402W_25_0407, value_col = "Rt_Apd_Ms_",
             popup_cols = c("Sector" = "Field_ID", "Target Rate" = "Tgt_Rate_l", "Applied Rate" = "Rt_Apd_Ms_", "Treatment" = "Treatment", "Product" = "Product"))
      } else {
        list(data = map_data_1402W_25_0507, value_col = "Rt_Apd_Liq",
             popup_cols = c("Sector" = "Field_ID", "Target Rate" = "Tgt_Rate_g", "Applied Rate" = "Rt_Apd_Liq", "Treatment" = "Treatment", "Product" = "Product"))
      }
    }))



  ####2436N








  NRx_tm_map2436N_25 <- reactive({
    selected_date <- input$NRx_variable2436N_25

    if (selected_date == "April_08") {
      tm_shape(map_data_2436N_25_0408_cleaned) +
        tm_polygons(
          col = "Rt_Apd_Ms_", title = "Applied N Rate (lbs N/ac)",
          popup.vars = c(
          "Sector No: " = "Plot_Numbe",
          "Target Rate: " = "Tgt_Rate_l",
            "N rate for date" = "Rt_Apd_Ms_",
            "Treatment Method: " = "TRT_Name",
            "Product: " = "Product"
          ),
          midpoint = 0
        ) +
        tm_borders(lwd = 0.5) +
        tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))

    }
  })



  # Render the Leaflet map
  output$NRxmap2436N_25 <- renderLeaflet({
    tm_map2436N_25 <- NRx_tm_map2436N_25()
    tmap_leaflet(tm_map2436N_25) %>%
      addProviderTiles("Esri.WorldImagery")
  })


  ####1912


  NRx_tm_map1912_25 <- reactive({
    req(input$NRx_variable1912_25)

    tm_shape(map_data_1912_25_0403) +
      tm_polygons(
        col = "AppliedRate",
        title = "Applied N Rate (lbs N/ac)",
        popup.vars = c(
          "Field ID: " = "Field_Id",
          "Target Rate: " = "TargetRate",
          "Applied Rate: " = "AppliedRate",
          "Treatment Rate: " = "Rate",
          "Product: " = "Product",
          "Records: " = "Records"
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(frame = FALSE, inner.margins = c(0, 0, 0, 0))
  })


  output$NRxmap1912_25 <- renderLeaflet({
    tm_map1912_25 <- NRx_tm_map1912_25()
    tmap_leaflet(tm_map1912_25) %>%
      addProviderTiles("Esri.WorldImagery")
  })












  #################################---------------SI_Map_Code--------------------------##############################




  ####1402W



  reactive_tm_SImap1402w <- reactive({
    tm_map <- tm_shape(SI_1402W_25) +
      tm_polygons(
        col = input$SI_variable1402W_25,
        style = "fixed",
        breaks = c(-Inf, 0.95, 0.96, Inf),
        palette = c("red", "yellow", "green"),
        popup.vars = c(
          "Sector No: " = "Field_ID",
          "Treatment: " = "Treatment",
          "SI" = input$SI_variable1402W_25
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      )
    return(tm_map)
  })

  # Create the Leaflet map using the reactive_tm_map
  register_si_proxy_map(
    "SImap1402W_25", reactive(input$SI_variable1402W_25),
    reactive(SI_1402W_25), "Field_ID", "Treatment"
  )







  ####2436N



  reactive_tm_SImap2436N <- reactive({
    tm_map <- tm_shape(SI_2436N_25) +
      tm_polygons(
        col = input$SI_variable2436N_25,
        style = "fixed",
        breaks = c(-Inf, 0.95, 0.96, Inf),
        palette = c("red", "yellow", "green"),
        popup.vars = c(
          "Sector No: " = "Plot_Numbe",
          "Treatment: " = "TRT_Name",
          "SI" = input$SI_variable2436N_25
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      )
    return(tm_map)
  })

  # Create the Leaflet map using the reactive_tm_map
  register_si_proxy_map(
    "SImap2436N_25", reactive(input$SI_variable2436N_25),
    reactive(SI_2436N_25), "Plot_Numbe", "TRT_Name"
  )


  ####1912


  reactive_tm_SImap1912 <- reactive({
    tm_map <- tm_shape(SI_1912_25) +
      tm_polygons(
        col = input$SI_variable1912_25,
        style = "fixed",
        breaks = c(-Inf, 0.95, 0.96, Inf),
        palette = c("red", "yellow", "green"),
        popup.vars = c(
          "Field ID: " = "Field_Id",
          "N Rate: " = "Rate",
          "SI" = input$SI_variable1912_25
        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      )
    return(tm_map)
  })

  register_si_proxy_map(
    "SImap1912_25", reactive(input$SI_variable1912_25),
    reactive(SI_1912_25), "Field_Id", "Rate"
  )






  #################################---------------Yield_Map_Code--------------------------##############################





  ####1402W

  # Create a thematic map object for the yield data
  Field_map1402W <- reactive({ tm_shape(Yield_1402W_25) +
    tm_polygons(midpoint = 0, col = "yield",
                popup.vars = c("Sector No:" = "Field_ID",
                               "Yield bu/ac:" = "yield",
                               "Treatment:" = "Treatment")) +
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })

  # Convert to Leaflet interactive map
  Yield_1402W <- reactive(tmap_leaflet(Field_map1402W()))
  output$Yield_1402W <- renderLeaflet({
    Yield_1402W()%>%
      addProviderTiles("Esri.WorldImagery")
  })


  #1912

  Field_map1912_yield <- reactive({ tm_shape(Yield_1912) +
    tm_polygons(
      midpoint = 0,
      col = "VRYIELDVOL",
      title = "Yield (bu/ac)",
      popup.vars = c(
        "Field ID: " = "TARGET_FID",
        "Yield bu/ac: " = "VRYIELDVOL",
        "N Rate: " = "Rate"
      )
    ) +
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })

  Yield_1912_map <- reactive(tmap_leaflet(Field_map1912_yield()))
  output$Yield_1912 <- renderLeaflet({
    Yield_1912_map() %>%
      addProviderTiles("Esri.WorldImagery")
  })






  #2436N

  Field_map2436N <- reactive({ tm_shape(Yield_2436N_25) +tm_polygons(midpoint = 0,col = "Yield",
                                                            popup.vars = c("Sector No: " = "Plot_Numbe",
                                                                           "Yield bu/ac: " = "Yield",
                                                                           "Treatment: " = "TRT_Name"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    ) })
  Yield_2436N  <- reactive(tmap_leaflet(Field_map2436N()))


  output$Yield_2436N <- renderLeaflet({

    Yield_2436N() %>%
      addProviderTiles("Esri.WorldImagery")
  })




  #################--------------------------Plots----------------------------#################################################





  ###############------------------N Treatment plot Code------------------------------################################################



  ####1402E 2026

  selected_df_1402E_26 <- reactive({
    selected_map_data_1402E_26() %>%
      st_drop_geometry() %>%
      select(
        Sector = Plot_Numbe,
        Treatment,
        Product,
        AppliedRate,
        TargetRate,
        Records,
        Date
      )
  })

  output$Nplot1_1402E_26 <- plotly::renderPlotly({
    p1 <- ggplot(selected_df_1402E_26(), aes(x = as.factor(Sector), y = AppliedRate)) +
      geom_jitter(aes(color = Treatment), width = 0.3, height = 0, size = 1.5) +
      labs(
        title = "Applied Rate per Plot",
        x = "Plot",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p1)
  })

  output$Nplot2_1402E_26 <- plotly::renderPlotly({
    avg_df <- selected_df_1402E_26() %>%
      group_by(Treatment) %>%
      summarise(
        AverageRate = mean(AppliedRate, na.rm = TRUE),
        AverageTarget = mean(TargetRate, na.rm = TRUE),
        .groups = "drop"
      )

    p2 <- ggplot(avg_df, aes(x = reorder(Treatment, AverageRate), y = AverageRate, fill = Treatment)) +
      geom_col() +
      geom_point(aes(y = AverageTarget), color = "black", size = 2) +
      coord_flip() +
      labs(
        title = "Average Applied Rate by Treatment",
        x = "Treatment",
        y = "Average Rate (lbs N)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p2)
  })

  output$Nplot3_1402E_26 <- plotly::renderPlotly({
    p3 <- ggplot(selected_df_1402E_26(), aes(x = Treatment, y = AppliedRate, fill = Treatment)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, shape = 18, color = "blue", size = 2) +
      labs(
        title = "Distribution of Applied Rates by Treatment",
        x = "Treatment",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p3)
  })


  ####2436S 2026

  df_2436S_26 <- reactive({
    field_2436S_26()$map_data %>%
      st_drop_geometry() %>%
      select(
        Sector = Plot_Numbe, Treatment, Product, Irrigation,
        AppliedRate, TargetRate, Records, Date
      )
  })

  output$Nplot1_2436S_26 <- plotly::renderPlotly({
    p1 <- ggplot(df_2436S_26(), aes(x = as.factor(Sector), y = AppliedRate)) +
      geom_jitter(aes(color = Treatment), width = 0.3, height = 0, size = 1.5) +
      labs(
        title = "Applied Rate per Plot",
        x = "Plot",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p1)
  })

  output$Nplot2_2436S_26 <- plotly::renderPlotly({
    avg_df <- df_2436S_26() %>%
      group_by(Treatment) %>%
      summarise(
        AverageRate = mean(AppliedRate, na.rm = TRUE),
        AverageTarget = mean(TargetRate, na.rm = TRUE),
        .groups = "drop"
      )

    p2 <- ggplot(avg_df, aes(x = reorder(Treatment, AverageRate), y = AverageRate, fill = Treatment)) +
      geom_col() +
      geom_point(aes(y = AverageTarget), color = "black", size = 2) +
      coord_flip() +
      labs(
        title = "Average Applied Rate by Treatment",
        x = "Treatment",
        y = "Average Rate (lbs N)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p2)
  })

  output$Nplot3_2436S_26 <- plotly::renderPlotly({
    p3 <- ggplot(df_2436S_26(), aes(x = Treatment, y = AppliedRate, fill = Treatment)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, shape = 18, color = "blue", size = 2) +
      labs(
        title = "Distribution of Applied Rates by Treatment",
        x = "Treatment",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p3)
  })



  ####1402W

  output$Nplot1_1402W_25 <- plotly::renderPlotly({
    p1 <- ggplot(combined_df, aes(x = Field_ID, y = AppliedRate, color = as.factor(Date))) +
      geom_jitter(width = 0.3, height = 0, size = 1.5) +
      labs(
        title = "Applied Rate per Sector by Date",
        x = "Sector",
        y = "Applied Rate (lbs N)",
        color = "Date"
      ) +
      theme_minimal()
    ggplotly(p1)
  })



  output$Nplot2_1402W_25 <- plotly::renderPlotly({
    avg_df <- combined_df %>%
      group_by(Date, Treatment) %>%
      summarise(AverageRate = mean(AppliedRate, na.rm = TRUE), .groups = 'drop')

    p2 <- ggplot(avg_df, aes(x = Date, y = AverageRate, group = Treatment, color = Treatment)) +
      geom_point(size = 2) +
      geom_line() +
      labs(
        title = "Average Applied Rate by Treatment",
        x = "Date",
        y = "Average Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p2)
  })


  output$Nplot3_1402W_25 <- plotly::renderPlotly({
    p3 <- ggplot(combined_df, aes(x = Treatment, y = AppliedRate, fill = Treatment)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, shape = 18, color = "blue", size = 2) +
      labs(
        title = "Distribution of Applied Rates by Treatment",
        x = "Treatment",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p3)
  })





  ####2436N


  output$Nplot1_2436N_25 <- plotly::renderPlotly({
    p1 <- ggplot(df_2436N, aes(x = as.factor(Sector), y = AppliedRate)) +
      geom_jitter(aes(color = Treatment), width = 0.3, size = 1.5) +
      labs(
        title = "Applied Rate per Sector",
        x = "Sector",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p1)
  })




  output$Nplot2_2436N_25 <- plotly::renderPlotly({
    avg_df <- df_2436N %>%
      group_by(Treatment) %>%
      summarise(AverageRate = mean(AppliedRate, na.rm = TRUE), .groups = 'drop')

    p2 <- ggplot(avg_df, aes(x = Treatment, y = AverageRate, group = 1)) +
      geom_line(color = "steelblue") +
      geom_point(size = 3, color = "steelblue") +
      labs(
        title = "Average Applied Rate by Treatment",
        x = "Treatment",
        y = "Average Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p2)
  })



  output$Nplot3_2436N_25 <- plotly::renderPlotly({
    p3 <- ggplot(df_2436N, aes(x = Treatment, y = AppliedRate, fill = Treatment)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, shape = 18, color = "blue", size = 2) +
      labs(
        title = "Distribution of Applied Rates by Treatment",
        x = "Treatment",
        y = "Applied Rate (lbs N)"
      ) +
      theme_minimal()
    ggplotly(p3)
  })


  ####1912

  output$Nplot1_1912_25 <- plotly::renderPlotly({
    p1 <- ggplot(df_1912, aes(x = Field_Id, y = AppliedRate)) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Applied N Rate by Field ID",
        x = "Field ID",
        y = "Applied N Rate (lbs N/ac)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p1)
  })

  output$Nplot2_1912_25 <- plotly::renderPlotly({
    avg_df <- df_1912 %>%
      group_by(Rate) %>%
      summarise(AverageRate = mean(AppliedRate, na.rm = TRUE), .groups = "drop")

    p2 <- ggplot(avg_df, aes(x = Rate, y = AverageRate, fill = Rate)) +
      geom_col(color = "black") +
      labs(
        title = "Average Applied N Rate by Treatment Rate",
        x = "Treatment Rate",
        y = "Average Applied N Rate (lbs N/ac)"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
    ggplotly(p2)
  })

  output$Nplot3_1912_25 <- plotly::renderPlotly({
    p3 <- ggplot(df_1912, aes(x = Rate, y = AppliedRate, fill = Rate)) +
      geom_boxplot(color = "black") +
      labs(
        title = "Distribution of Applied N Rate by Treatment Rate",
        x = "Treatment Rate",
        y = "Applied N Rate (lbs N/ac)"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
    ggplotly(p3)
  })
















  ###############------------------SI plot Code------------------------------################################################





  ####1402W






  output$SIplot_1402W_25<- plotly::renderPlotly({

    # Reshape the data to a long format
    data_long <- SI_1402W_25 %>%
      pivot_longer(cols = starts_with("SI_"),
                   names_to = "Date",
                   values_to = "SI") %>%
      mutate(Date = as.Date(paste0("2025-", sub("SI_", "", Date)), format = "%Y-%m_%d"))

    # Calculate the average SI for each treatment and date
    avg_si <- data_long %>%
      group_by(Treatment, Date) %>%
      summarise(avg_SI = mean(SI, na.rm = TRUE))

    # Create the line chart with average SI for each treatment
    p <- ggplot(avg_si, aes(x = Date, y = avg_SI, color = Treatment)) +
      geom_line() +
      geom_point() +
      #geom_vline(aes(xintercept = as.numeric(as.Date("2025-08-03"))),
                # linetype = "dashed", color = "red", size = 1, inherit.aes = FALSE) +
      #geom_text(aes(x = as.Date("2025-08-03"), y = max(avg_si$avg_SI, na.rm = FALSE),
                   # label = "Treatment Stopped"), color = "red", hjust = 0, vjust = 1.5) +
      labs(title = "Average SI Values by Treatment",
           x = "Date",
           y = "Average SI Value",
           color = "Treatment") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      theme(legend.position = "right")

    # Convert ggplot to plotly
    plotly_p <- plotly::ggplotly(p)

    return(plotly_p)

  })









  ####2436N



  output$SIplot_2436N_25<- plotly::renderPlotly({

    # Reshape the data to a long format
    data_long <- SI_2436N_25 %>%
      pivot_longer(cols = starts_with("SI_"),
                   names_to = "Date",
                   values_to = "SI") %>%
      mutate(Date = as.Date(paste0("2025-", sub("SI_", "", Date)), format = "%Y-%m_%d"))

    # Calculate the average SI for each treatment and date
    avg_si <- data_long %>%
      group_by(TRT_Name, Date) %>%
      summarise(avg_SI = mean(SI, na.rm = TRUE))

    # Create the line chart with average SI for each treatment
    p <- ggplot(avg_si, aes(x = Date, y = avg_SI, color = TRT_Name)) +
      geom_line() +
      geom_point() +
      #geom_vline(aes(xintercept = as.numeric(as.Date("2025-08-03"))),
      # linetype = "dashed", color = "red", size = 1, inherit.aes = FALSE) +
      #geom_text(aes(x = as.Date("2025-08-03"), y = max(avg_si$avg_SI, na.rm = FALSE),
      # label = "Treatment Stopped"), color = "red", hjust = 0, vjust = 1.5) +
      labs(title = "Average SI Values by Treatment",
           x = "Date",
           y = "Average SI Value",
           color = "TRT_Name") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      theme(legend.position = "right")

    # Convert ggplot to plotly
    plotly_p <- plotly::ggplotly(p)

    return(plotly_p)

  })


  ####1912


  output$SIplot_1912_25 <- plotly::renderPlotly({

    data_long <- SI_1912_25 %>%
      st_drop_geometry() %>%
      pivot_longer(cols = starts_with("SI_"),
                   names_to = "Date",
                   values_to = "SI") %>%
      mutate(
        Date = as.Date(paste0("2025-", sub("SI_", "", Date)), format = "%Y-%m_%d"),
        Rate = as.factor(Rate)
      )

    avg_si <- data_long %>%
      group_by(Rate, Date) %>%
      summarise(avg_SI = mean(SI, na.rm = TRUE), .groups = "drop")

    p <- ggplot(avg_si, aes(x = Date, y = avg_SI, color = Rate)) +
      geom_line() +
      geom_point() +
      labs(title = "Average SI Values by Treatment Rate",
           x = "Date",
           y = "Average SI Value",
           color = "Treatment Rate") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      theme(legend.position = "right")

    plotly::ggplotly(p)

  })





  ###############------------------Yield plot Code------------------------------################################################





  ####1402W


  output$Yieldplot1_1402W <- plotly::renderPlotly({


    mean_yield <- Yield_1402W_df %>%
      group_by(Treatment) %>%
      summarize(mean_yield = mean(yield, na.rm = TRUE), .groups = "drop")

    ggplot_object <- ggplot(Yield_1402W_df, aes(x = Treatment, y = yield, fill = Treatment)) +
      geom_boxplot(color = "black") +
      labs(title = "Distribution of Yield by Treatment",
           x = "Treatment", y = "Yield (Bu/ac)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3") +
      geom_point(
        data = mean_yield,
        aes(x = Treatment, y = mean_yield,
            text = paste("Treatment:", Treatment,
                         "<br>Mean Yield:", round(mean_yield, 2))),
        color = "red", size = 3, shape = 18, inherit.aes = FALSE
      )

    plotly_object <- plotly::ggplotly(ggplot_object, tooltip = "text")
    plotly_object


  })


  ####1912


  output$Yieldplot1_1912 <- plotly::renderPlotly({

    mean_yield <- Yield_1912_df %>%
      group_by(Rate) %>%
      summarize(mean_yield = mean(VRYIELDVOL, na.rm = TRUE), .groups = "drop")

    ggplot_object <- ggplot(
      Yield_1912_df,
      aes(x = as.factor(Rate), y = VRYIELDVOL, fill = as.factor(Rate))
    ) +
      geom_boxplot(color = "black") +
      labs(title = "Distribution of Yield by Treatment Rate",
           x = "Treatment Rate", y = "Yield (Bu/ac)",
           fill = "Treatment Rate") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      geom_point(
        data = mean_yield,
        aes(x = as.factor(Rate), y = mean_yield,
            text = paste("Treatment Rate:", Rate,
                         "<br>Mean Yield:", round(mean_yield, 2))),
        color = "red", size = 3, shape = 18, inherit.aes = FALSE
      )

    plotly::ggplotly(ggplot_object, tooltip = "text")

  })



  ####2436N


  output$Yieldplot1_2436N <- plotly::renderPlotly({


    mean_yield <- Yield_2436N_df %>%
      group_by(TRT_Name) %>%
      summarize(mean_yield = mean(Yield, na.rm = TRUE), .groups = "drop")

    ggplot_object <- ggplot(Yield_2436N_df, aes(x = TRT_Name, y = Yield, fill = TRT_Name)) +
      geom_boxplot(color = "black") +
      labs(title = "Distribution of Yield by Treatment",
           x = "Treatment", y = "Yield (Bu/ac)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3") +
      geom_point(
        data = mean_yield,
        aes(x = TRT_Name, y = mean_yield,
            text = paste("Treatment:", TRT_Name,
                         "<br>Mean Yield:", round(mean_yield, 2))),
        color = "red", size = 3, shape = 18, inherit.aes = FALSE
      )

    plotly_object <- plotly::ggplotly(ggplot_object, tooltip = "text")
    plotly_object


  })


  ##########################Summary Report ############################

  ##########################1912############################

  output$data_table_1912 <- renderDT({

    area_by_rate <- map1912 %>%
      st_transform(5070) %>%
      mutate(Area_in_Acres = as.numeric(st_area(.)) * 0.000247105) %>%
      st_drop_geometry() %>%
      mutate(Rate = as.character(Rate)) %>%
      group_by(Rate) %>%
      summarize(Area_in_Acres = mean(Area_in_Acres, na.rm = TRUE), .groups = "drop")

    mean_yield <- Yield_1912_df %>%
      mutate(Rate = as.character(Rate)) %>%
      group_by(Rate) %>%
      summarize(mean_yield = mean(VRYIELDVOL, na.rm = TRUE), .groups = "drop")

    n_summary <- df_1912 %>%
      mutate(Rate = as.character(Rate)) %>%
      group_by(Rate) %>%
      summarize(Total_N_rate = mean(AppliedRate, na.rm = TRUE), .groups = "drop")

    final_result_selected <- n_summary %>%
      left_join(mean_yield, by = "Rate") %>%
      left_join(area_by_rate, by = "Rate") %>%
      mutate(
        NUE = Total_N_rate / mean_yield,
        MNR = (mean_yield * 4) - (Total_N_rate * 0.5)
      ) %>%
      select(Rate, Total_N_rate, mean_yield, NUE, MNR)

    merged_data_clean <- Yield_1912_df %>%
      select(Rate, VRYIELDVOL) %>%
      mutate(Rate = as.character(Rate)) %>%
      left_join(
        df_1912 %>%
          select(Rate, AppliedRate) %>%
          mutate(Rate = as.character(Rate)),
        by = "Rate"
      ) %>%
      mutate(
        Total_N_rate = AppliedRate,
        NUE = Total_N_rate / VRYIELDVOL,
        MNR = (VRYIELDVOL * 4) - (Total_N_rate * 0.5)
      ) %>%
      filter(
        !is.na(Rate),
        !is.na(Total_N_rate),
        !is.na(VRYIELDVOL),
        !is.na(NUE),
        !is.na(MNR),
        is.finite(NUE),
        is.finite(MNR)
      )

    anova_results <- data.frame(
      Total_N_rate_pvalue = tryCatch(
        summary(aov(Total_N_rate ~ as.factor(Rate), data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      ),
      mean_yield_pvalue = tryCatch(
        summary(aov(VRYIELDVOL ~ as.factor(Rate), data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      ),
      NUE_pvalue = tryCatch(
        summary(aov(NUE ~ as.factor(Rate), data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      ),
      MNR_pvalue = tryCatch(
        summary(aov(MNR ~ as.factor(Rate), data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      )
    ) %>%
      mutate(across(
        everything(),
        ~ ifelse(is.na(.x), NA, ifelse(.x < 0.01, "<0.01", as.character(round(.x, 3))))
      ))

    final_result_with_pvalues <- final_result_selected %>%
      mutate(
        Total_N_rate = round(Total_N_rate),
        mean_yield = round(mean_yield),
        NUE = round(NUE, 2),
        MNR = round(MNR)
      ) %>%
      mutate(across(everything(), as.character)) %>%
      bind_rows(
        tibble(
          Rate = "P-Value",
          Total_N_rate = as.character(anova_results$Total_N_rate_pvalue),
          mean_yield = as.character(anova_results$mean_yield_pvalue),
          NUE = as.character(anova_results$NUE_pvalue),
          MNR = as.character(anova_results$MNR_pvalue)
        )
      ) %>%
      rename(
        "Treatment Rate" = Rate,
        "Total N Rate (lb-N/ac)" = Total_N_rate,
        "Yield (bu/ac)" = mean_yield,
        "Nitrogen Efficiency (lb N/bu grain)" = NUE,
        "MNR($/ac)" = MNR
      )

    datatable(final_result_with_pvalues) %>%
      formatStyle(
        "Treatment Rate",
        target = "row",
        color = styleEqual("P-Value", "blue")
      )
  })


  ##########################2436N############################

  output$data_table_2436N <- renderDT({

    Area_in_Acres <-  data2436N_25_0408 %>%
      group_by(TRT_Name) %>%
      summarize(
        Area_in_Acres = mean(Area_1 * 2.47105)  # Convert area to acres (mean for each treatment)
      )

    # Calculate the average yield per treatment
    mean_yield <- Yield_2436N_df %>%
      group_by(TRT_Name) %>%
      summarize(mean_yield = mean(Yield, na.rm = TRUE))

    # Calculate the required metrics for each treatment from prescription data
    result <- data2436N_25_0408 %>%
      group_by(TRT_Name) %>%
      summarize(
        # Calculate the average applied rate for each treatment
        Avg_AppliedRate_0408 = mean(Rt_Apd_Ms_, na.rm = TRUE),
        #Avg_AppliedRate_06_24 = mean(AppliedRate_06_24, na.rm = TRUE),

        # Calculate total N rate as the sum of the two applied rates
        Total_N_rate = mean(Rt_Apd_Ms_, na.rm = TRUE)
      )


    # Merge the calculated Total N rate data with the average yield data
    result_with_yield <- result %>%
      left_join(mean_yield, by = "TRT_Name")

    # Merge the Area_in_Acres data with the result_with_yield data
    final_result <- result_with_yield %>%
      left_join(Area_in_Acres, by = "TRT_Name")

    # Calculate Nitrogen Efficiency (lb N/bu grain) and Marginal Net Return (MNR)
    final_result <- final_result %>%
      mutate(
        NUE = Total_N_rate / mean_yield,  # lb N per bu grain

        # Calculate Partial Profit ($/ac) with specific costs for N sources
        MNR = (mean_yield * 4) - (Avg_AppliedRate_0408 * 0.5)
      )

    # Select and print only the specified columns
    final_result_selected <- final_result %>%
      mutate(across(where(is.numeric))) %>%
      #mutate(across(where(is.numeric), ~ round(., 1))) %>%
      select(TRT_Name, Total_N_rate, mean_yield, NUE, MNR)

    final_result_selected
    # Merge the dataframes so that Total_N_rate and yield can be used in the ANOVA
    merged_data <- data2436N_25_0408 %>%
      left_join(Yield_2436N_df, by = "TRT_Name") %>%
      mutate(
        Total_N_rate = Rt_Apd_Ms_ ,
        NUE = Total_N_rate / Yield,

        # Adjust MNR calculation to reflect specific costs for N sources
        MNR = (Yield * 4) - (Rt_Apd_Ms_ * 0.43)
      )

    # Perform ANOVA for each of the variables (across all treatments)
    anova_results <- data.frame(
      Total_N_rate_pvalue = summary(aov(Total_N_rate ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1],
      mean_yield_pvalue = summary(aov(Yield ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1],
      NUE_pvalue = summary(aov(NUE ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1],
      MNR_pvalue = summary(aov(MNR ~ TRT_Name, data = merged_data))[[1]]$`Pr(>F)`[1]
    )

    # Format p-values: If p-value < 0.01, print "<0.01", otherwise print rounded p-values
    anova_results <- anova_results %>%
      mutate(
        Total_N_rate_pvalue = ifelse(Total_N_rate_pvalue < 0.01, " <0.01", round(Total_N_rate_pvalue, 3)),
        mean_yield_pvalue = ifelse(mean_yield_pvalue < 0.01, " <0.01", round(mean_yield_pvalue, 3)),
        NUE_pvalue = ifelse(NUE_pvalue < 0.01, " <0.01", NUE_pvalue),
        MNR_pvalue = ifelse(MNR_pvalue < 0.01, "<0.01", round(MNR_pvalue, 3))
      )

    # Round numeric values to 2 decimal places, except for p-values
    final_result_selected_rounded <- final_result_selected %>%
      mutate(
        Total_N_rate = round(Total_N_rate),
        mean_yield = round(mean_yield),
        #NUE = NUE,
        NUE = round(NUE, 2),
        MNR = round(MNR)
      )

    # Convert the numeric columns to characters before binding rows
    final_result_selected_char <- final_result_selected_rounded %>%
      mutate(across(everything(), as.character))

    # Now append these p-values to the final_result_selected table
    final_result_with_pvalues <- final_result_selected_char %>%
      bind_rows(
        tibble(
          TRT_Name = "P-Value",
          Total_N_rate = as.character(anova_results$Total_N_rate_pvalue),
          mean_yield = as.character(anova_results$mean_yield_pvalue),
          NUE = as.character(anova_results$NUE_pvalue),
          MNR = as.character(anova_results$MNR_pvalue)
        )
      )

    final_result_with_pvalues <- final_result_with_pvalues %>%
      rename(
        "Treatment Type" = TRT_Name ,
        "Total N Rate (lb-N/ac)" = Total_N_rate,
        " Yield (bu/ac)" = mean_yield,
        "Nitrogen Efficiency (lb N/bu grain)" = NUE,
        "MNR($/ac)" = MNR
      )


    # Render the datatable and apply styling to font color of p-value row
    datatable(final_result_with_pvalues) %>%
      formatStyle(
        'Treatment Type',
        target = 'row',
        color = styleEqual("P-Value", 'blue')  # Change font color of P-Value row to blue
      )

  })



  ##########################1402W############################


  output$data_table_1402W <- renderDT({

    data1402W_25_0407 <- data1402W_25_0407 %>%
      mutate(Rt_Apd_Ms_ = as.numeric(Rt_Apd_Ms_))

    data1402W_25_0507 <- data1402W_25_0507 %>%
      mutate(Rt_Apd_Liq = as.numeric(Rt_Apd_Liq))

    Yield_1402W_df <- Yield_1402W_df %>%
      mutate(yield = as.numeric(yield))

    buffdata1402W <- buffdata1402W %>%
      mutate(area = as.numeric(area))

    # -----------------------------
    # Area in acres by treatment
    # -----------------------------
    Area_in_Acres <- buffdata1402W %>%
      group_by(Treatment) %>%
      summarize(
        Area_in_Acres = mean(area * 2.47105, na.rm = TRUE),
        .groups = "drop"
      )

    # -----------------------------
    # Mean yield by treatment
    # -----------------------------
    mean_yield <- Yield_1402W_df %>%
      group_by(Treatment) %>%
      summarize(
        mean_yield = mean(yield, na.rm = TRUE),
        .groups = "drop"
      )

    # -----------------------------
    # Mean applied rates by treatment
    # -----------------------------
    summary_0407 <- data1402W_25_0407 %>%
      group_by(Treatment) %>%
      summarize(
        Avg_AppliedRate_04_07 = mean(Rt_Apd_Ms_, na.rm = TRUE),
        .groups = "drop"
      )

    summary_0507 <- data1402W_25_0507 %>%
      group_by(Treatment) %>%
      summarize(
        Avg_AppliedRate_05_07 = mean(Rt_Apd_Liq, na.rm = TRUE),
        .groups = "drop"
      )

    # -----------------------------
    # Final summarized table for display
    # -----------------------------
    result <- summary_0407 %>%
      left_join(summary_0507, by = "Treatment") %>%
      mutate(
        Avg_AppliedRate_04_07 = replace_na(Avg_AppliedRate_04_07, 0),
        Avg_AppliedRate_05_07 = replace_na(Avg_AppliedRate_05_07, 0),
        Total_N_rate = Avg_AppliedRate_04_07 + Avg_AppliedRate_05_07
      )

    result_with_yield <- result %>%
      left_join(mean_yield, by = "Treatment")

    final_result <- result_with_yield %>%
      left_join(Area_in_Acres, by = "Treatment") %>%
      mutate(
        NUE = Total_N_rate / mean_yield,
        MNR = (mean_yield * 4) - (Avg_AppliedRate_04_07 * 0.5) - (Avg_AppliedRate_05_07 * 0.7)
      )

    final_result_selected <- final_result %>%
      select(Treatment, Total_N_rate, mean_yield, NUE, MNR)

    # -----------------------------
    # RAW merged data for p-value calculations
    # IMPORTANT:
    # Replace "Sector" below if your common plot ID column has another name
    # such as Plot, Plot_ID, Grid_ID, etc.
    # -----------------------------
    merged_data <- data1402W_25_0407 %>%
      select(Treatment, Rt_Apd_Ms_) %>%
      left_join(
        data1402W_25_0507 %>% select(Treatment,  Rt_Apd_Liq),
        by = c("Treatment")
      ) %>%
      left_join(
        Yield_1402W_df %>% select(Treatment, yield),
        by = c("Treatment")
      ) %>%
      mutate(
        Rt_Apd_Ms_ = replace_na(Rt_Apd_Ms_, 0),
        Rt_Apd_Liq = replace_na(Rt_Apd_Liq, 0),
        Total_N_rate = Rt_Apd_Ms_ + Rt_Apd_Liq,
        NUE = Total_N_rate / yield,
        MNR = (yield * 4) - (Rt_Apd_Ms_ * 0.5) - (Rt_Apd_Liq * 0.7)
      )

    # Optional: remove rows with missing/invalid values for ANOVA
    merged_data_clean <- merged_data %>%
      filter(
        !is.na(Treatment),
        !is.na(Total_N_rate),
        !is.na(yield),
        !is.na(NUE),
        !is.na(MNR),
        is.finite(NUE),
        is.finite(MNR)
      )

    # -----------------------------
    # ANOVA p-values
    # -----------------------------
    anova_results <- data.frame(
      Total_N_rate_pvalue = tryCatch(
        summary(aov(Total_N_rate ~ Treatment, data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      ),
      mean_yield_pvalue = tryCatch(
        summary(aov(yield ~ Treatment, data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      ),
      NUE_pvalue = tryCatch(
        summary(aov(NUE ~ Treatment, data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      ),
      MNR_pvalue = tryCatch(
        summary(aov(MNR ~ Treatment, data = merged_data_clean))[[1]]$`Pr(>F)`[1],
        error = function(e) NA
      )
    )

    # -----------------------------
    # Format p-values
    # -----------------------------
    anova_results <- anova_results %>%
      mutate(
        Total_N_rate_pvalue = ifelse(
          is.na(Total_N_rate_pvalue), NA,
          ifelse(Total_N_rate_pvalue < 0.01, "<0.01", as.character(round(Total_N_rate_pvalue, 3)))
        ),
        mean_yield_pvalue = ifelse(
          is.na(mean_yield_pvalue), NA,
          ifelse(mean_yield_pvalue < 0.01, "<0.01", as.character(round(mean_yield_pvalue, 3)))
        ),
        NUE_pvalue = ifelse(
          is.na(NUE_pvalue), NA,
          ifelse(NUE_pvalue < 0.01, "<0.01", as.character(round(NUE_pvalue, 3)))
        ),
        MNR_pvalue = ifelse(
          is.na(MNR_pvalue), NA,
          ifelse(MNR_pvalue < 0.01, "<0.01", as.character(round(MNR_pvalue, 3)))
        )
      )

    # -----------------------------
    # Round summarized table values
    # -----------------------------
    final_result_selected_rounded <- final_result_selected %>%
      mutate(
        Total_N_rate = round(Total_N_rate),
        mean_yield   = round(mean_yield),
        NUE          = round(NUE, 2),
        MNR          = round(MNR)
      )

    # Convert to character before binding p-value row
    final_result_selected_char <- final_result_selected_rounded %>%
      mutate(across(everything(), as.character))

    # -----------------------------
    # Add P-Value row
    # -----------------------------
    final_result_with_pvalues <- final_result_selected_char %>%
      bind_rows(
        tibble(
          Treatment    = "P-Value",
          Total_N_rate = as.character(anova_results$Total_N_rate_pvalue),
          mean_yield   = as.character(anova_results$mean_yield_pvalue),
          NUE          = as.character(anova_results$NUE_pvalue),
          MNR          = as.character(anova_results$MNR_pvalue)
        )
      )

    # -----------------------------
    # Rename columns
    # -----------------------------
    final_result_with_pvalues <- final_result_with_pvalues %>%
      rename(
        "Treatment Type" = Treatment,
        "Total N Rate (lb-N/ac)" = Total_N_rate,
        "Yield (bu/ac)" = mean_yield,
        "Nitrogen Efficiency (lb N/bu grain)" = NUE,
        "MNR($/ac)" = MNR
      )

    # Print final table
    final_result_with_pvalues

    # -----------------------------
    # Render datatable
    # -----------------------------
    datatable(final_result_with_pvalues) %>%
      formatStyle(
        'Treatment Type',
        target = 'row',
        color = styleEqual("P-Value", "blue")
      )
  })







































  activate_selected_maps <- function() {
    growing_season <- isolate(input$GrowingSeason)
    available_field <- isolate(input$AvailableFields)
    if (is.null(growing_season) || is.null(available_field) ||
        !nzchar(growing_season) || !nzchar(available_field)) {
      return(invisible(FALSE))
    }
    all_field_maps <- agdata_map_output_ids()
    for (id in all_field_maps) {
      try(outputOptions(output, id, suspendWhenHidden = TRUE), silent = TRUE)
    }

    selected_config <- field_config[
      field_config$year == growing_season &
        field_config$field == available_field,
      , drop = FALSE
    ]
    if (nrow(selected_config) == 1L) {
      # Materialize the selected field once before four Leaflet outputs request
      # it concurrently. This avoids interrupted active-binding promises.
      field_key <- paste(growing_season, available_field, sep = "|")
      switch(
        field_key,
        "2024|2436 S" = isolate(field_2436S_24()),
        "2024|SCAL" = isolate(field_SCAL_24()),
        "2025|1402 W" = isolate(field_1402W_25()),
        "2025|1912" = isolate(field_1912_25()),
        "2025|2436 N" = isolate(field_2436N_25()),
        "2026|1402 E" = isolate(field_1402E_26()),
        "2026|2436 S" = isolate(field_2436S_26())
      )

      selected_ids <- unname(unlist(selected_config[c(
        "field_map_output", "nitrogen_output", "si_output", "yield_output"
      )]))
      for (id in selected_ids[!is.na(selected_ids)]) {
        try(outputOptions(output, id, suspendWhenHidden = FALSE), silent = TRUE)
      }
    }
    invisible(TRUE)
  }

  observeEvent(list(input$GrowingSeason, input$AvailableFields), {
    activate_selected_maps()
  }, ignoreInit = FALSE, priority = 100)

  session$onFlushed(function() {

    map_outputs <- agdata_map_output_ids()

    for (id in map_outputs) {
      # Keep maps for unselected fields dormant. Shiny automatically resumes
      # an output when its field card becomes visible.
      try(outputOptions(output, id, suspendWhenHidden = TRUE), silent = TRUE)
    }

    # Overview maps live inside conditional panels and can otherwise remain
    # suspended after switching years. They use lightweight boundary-only
    # loaders, separate from the detailed field datasets.
    for (id in c("all_maps_2024", "all_maps_2025", "all_maps_2026")) {
      try(outputOptions(output, id, suspendWhenHidden = FALSE), silent = TRUE)
    }

    activate_selected_maps()

    summary_outputs <- na.omit(field_config$summary_output)
    for (id in summary_outputs) {
      # Reports stay dormant until the user explicitly opens the modal.
      try(outputOptions(output, id, suspendWhenHidden = TRUE), silent = TRUE)
    }

  }, once = TRUE)







    # ✅ ---- ADD THIS BLOCK HERE ----
    observeEvent(input$show_summary, {

      req(input$AvailableFields)

      selected_summary <- field_config$summary_output[
        field_config$year == input$GrowingSeason &
          field_config$field == input$AvailableFields
      ]
      selected_summary <- na.omit(selected_summary)
      if (length(selected_summary)) {
        try(outputOptions(output, selected_summary[[1]], suspendWhenHidden = FALSE), silent = TRUE)
      }

      if (input$GrowingSeason == "2025" && input$AvailableFields == "2436 N") {
        showModal(modalDialog(
          title = summary_modal_title("2436 N"),
          DTOutput("data_table_2436N"),
          size = "l",
          easyClose = TRUE
        ))
      }

      if (input$GrowingSeason == "2025" && input$AvailableFields == "1402 W") {
        showModal(modalDialog(
          title = summary_modal_title("1402 W"),
          DTOutput("data_table_1402W"),
          size = "l",
          easyClose = TRUE
        ))
      }

      if (input$GrowingSeason == "2025" && input$AvailableFields == "1912") {
        showModal(modalDialog(
          title = summary_modal_title("1912"),
          DTOutput("data_table_1912"),
          size = "l",
          easyClose = TRUE
        ))
      }

      if (input$GrowingSeason == "2024" && input$AvailableFields == "2436 S") {
        showModal(modalDialog(
          title = summary_modal_title("2436 S"),
          DTOutput("data_table_2436S"),
          size = "l",
          easyClose = TRUE
        ))
      }

      if (input$GrowingSeason == "2024" && input$AvailableFields == "SCAL") {
        showModal(modalDialog(
          title = summary_modal_title("SCAL"),
          DTOutput("data_table_SCAL"),
          size = "l",
          easyClose = TRUE
        ))
      }

      if (input$GrowingSeason == "2026") {
        if (input$AvailableFields == "1402 E") {
          showModal(modalDialog(
            title = summary_modal_title("1402 E"),
            DTOutput("data_table_1402E_26"),
            size = "l",
            easyClose = TRUE
          ))
        } else if (input$AvailableFields == "2436 S") {
          showModal(modalDialog(
            title = summary_modal_title("2436 S"),
            DTOutput("data_table_2436S_26"),
            size = "l",
            easyClose = TRUE
          ))
        } else {
          showModal(modalDialog(
            title = "Summary Report - 2026 Fields",
            tabsetPanel(
              type = "tabs",
              tabPanel("1402 E", DTOutput("data_table_1402E_26")),
              tabPanel("2436 S", DTOutput("data_table_2436S_26"))
            ),
            size = "l",
            easyClose = TRUE
          ))
        }
      }

    })
















}
