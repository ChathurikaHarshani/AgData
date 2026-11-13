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

ui <- fluidPage(
  useShinyjs(),
  tags$a(class = "ns-skip-link", href = "#nsMain", "Skip to dashboard content"),

  tags$head(
    tags$title("N-SIGHT"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

    tags$script(HTML("
  $(document).on('click', '#toggleSidebar', function(e){
    e.preventDefault();
    $('html').toggleClass('ns-sidebar-collapsed');
    this.setAttribute('aria-expanded', $('html').hasClass('ns-sidebar-collapsed') ? 'false' : 'true');
  });

  function nsSetInitialSidebar(){
    if(window.innerWidth <= 760){ $('html').addClass('ns-sidebar-collapsed'); }
  }
  $(nsSetInitialSidebar);

  Shiny.addCustomMessageHandler('resizeLeaflet', function(x){
    setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 250);
  });

  // Close fullscreen when clicking dark background
  document.addEventListener('click', function(e){
    if(e.target && e.target.id === 'ns_backdrop'){
      Shiny.setInputValue('ns_backdrop_close', Date.now());
    }
  });

  function nsPlotlyResize(){
    if(!window.Plotly) return;
    document.querySelectorAll('.js-plotly-plot, .plotly-graph-div').forEach(function(gd){
      try { Plotly.Plots.resize(gd); } catch(e) {}
    });
  }

  function resizeAllPlotly(){ nsPlotlyResize(); }

  // Resize when Bootstrap tab becomes visible
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]', function(){
    setTimeout(resizeAllPlotly, 120);
  });

  // Resize when window size changes
  window.addEventListener('resize', function(){ setTimeout(resizeAllPlotly, 200); });

  function nsUpdatePageContext(){
    var season = $('#GrowingSeason').val() || 'Not selected';
    var field = $('#AvailableFields').val() || 'Not selected';
    var fieldLabel = field === 'Fields' ? 'All fields' : field;
    var title = field === 'Not selected' ? 'Select a field to begin' :
                field === 'Fields' ? 'All fields' : field + ' field insights';
    $('#header_season').text(season);
    $('#header_field').text(fieldLabel);
    $('#page_title').text(title);
  }

  function nsPrettyDateLabel(label){
    if(!label) return label;
    var match = String(label).replace(/_/g, ' ').match(/^(20\\d{2})\\s+([A-Za-z]+)\\s+(\\d{1,2})$/);
    if(!match) return String(label).replace(/_/g, ' ');
    var months = {jan:0,january:0,feb:1,february:1,mar:2,march:2,apr:3,april:3,
                  may:4,jun:5,june:5,jul:6,july:6,aug:7,august:7,sep:8,september:8,
                  oct:9,october:9,nov:10,november:10,dec:11,december:11};
    var month = months[match[2].toLowerCase()];
    if(month === undefined) return String(label).replace(/_/g, ' ');
    var date = new Date(Number(match[1]), month, Number(match[3]));
    return date.toLocaleDateString('en-US', {month:'long', day:'numeric', year:'numeric'});
  }

  function nsFormatDateControls(){
    $('[id^=NRx_variable], [id^=SI_variable]').each(function(){
      var control = this.selectize;
      if(!control) return;
      var labelField = control.settings.labelField || 'text';
      Object.keys(control.options).forEach(function(value){
        var option = control.options[value];
        var updated = Object.assign({}, option);
        updated[labelField] = nsPrettyDateLabel(option[labelField]);
        control.updateOption(value, updated);
      });
      control.refreshItems();
    });
    $('[id^=NRx_variable]').closest('.form-group').find('label').text('Application date');
    $('[id^=SI_variable]').closest('.form-group').find('label').text('Imagery date');
  }

  function nsFormatAnalysisTabs(){
    var labels = {
      'Per Sector': 'By sector',
      'Average': 'Treatment averages',
      'Dist': 'Distribution'
    };
    $('.ns-tile-drawer .nav-tabs a').each(function(){
      var current = $(this).text().trim();
      if(labels[current]) $(this).text(labels[current]);
    });
  }

  $(document).on('change', '#GrowingSeason, #AvailableFields', nsUpdatePageContext);
  $(document).on('shiny:connected', function(){
    nsUpdatePageContext();
    setTimeout(nsUpdatePageContext, 250);
    setTimeout(nsFormatDateControls, 350);
    setTimeout(nsFormatAnalysisTabs, 400);
  });
  $(document).on('shiny:value', function(){
    setTimeout(nsFormatAnalysisTabs, 80);
  });

  // Resize after maximize/minimize click
  document.addEventListener('click', function(e){
    if (e.target.closest('.ns-max-btn, .ns-close-btn, #toggleDrawerN, #toggleDrawerSI, #toggleDrawerYield')) {
      setTimeout(resizeAllPlotly, 300);
      setTimeout(resizeAllPlotly, 900);
    }
  });

  document.addEventListener('click', function(e){
    var button = e.target.closest('.ns-max-btn button');
    if(!button) return;
    window.nsLastMaxButton = button;
    setTimeout(function(){
      button.setAttribute('aria-expanded', document.querySelector('.ns-tile.maximized') ? 'true' : 'false');
    }, 120);
  });

  document.addEventListener('keydown', function(e){
    if(e.key !== 'Escape' || !document.querySelector('.ns-tile.maximized')) return;
    document.querySelectorAll('.ns-tile.maximized').forEach(function(tile){ tile.classList.remove('maximized'); });
    document.body.classList.remove('has-max');
    var backdrop = document.getElementById('ns_backdrop');
    if(backdrop) backdrop.style.display = 'none';
    if(window.nsLastMaxButton){
      window.nsLastMaxButton.setAttribute('aria-expanded', 'false');
      window.nsLastMaxButton.focus();
    }
    setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 200);
  });

  var nsDrawerButtons = {
    toggleDrawerN: 'drawerN',
    toggleDrawerSI: 'drawerSI',
    toggleDrawerYield: 'drawerYield'
  };
  document.addEventListener('click', function(e){
    var button = e.target.closest('#toggleDrawerN, #toggleDrawerSI, #toggleDrawerYield');
    if(!button) return;
    setTimeout(function(){
      var drawer = document.getElementById(nsDrawerButtons[button.id]);
      var open = drawer && drawer.classList.contains('is-open');
      button.setAttribute('aria-expanded', open ? 'true' : 'false');
      var label = button.querySelector('span');
      if(label) label.textContent = open ? 'Hide charts' : 'View charts';
    }, 100);
  });

"))
  ),

  div(id = "ns_backdrop", class = "ns-backdrop"),

  fluidRow(
    column(
      width = 2,
      div(
        id = "nsSidebarWrap",
        class = "sidebar-wrap",
        div(
          id = "nsSidebar",
          class = "left-panel",

          div(
            class = "logo-card",
            div(
              class = "logo-container-vertical",
              img(src = "n_sight_logo.png", class = "app-logo-large"),
              div(class = "logo-text-large")
            )
          ),

          div(
            class = "sidebar-section ns-selection-card",
            div(class = "sidebar-title", "Field selection"),
            div(
              class = "ns-sidebar-step",
              span(class = "ns-step-number", "1"),
              div(
                class = "ns-step-content",
                selectInput("GrowingSeason", "Growing season", choices = agdata_year_choices())
              )
            ),
            div(
              class = "ns-sidebar-step",
              span(class = "ns-step-number", "2"),
              div(
                class = "ns-step-content",
                selectInput("AvailableFields", "Field", choices = NULL)
              )
            ),
            p(class = "sidebar-note", icon("info-circle"), " Select a season first to see its available fields.")
          ),

          div(
            class = "sidebar-section",
            div(class = "sidebar-title", "Field report"),
            p(class = "sidebar-note ns-report-note", "Review the selected field's treatment and performance data."),
            actionButton("show_summary", "View summary report", icon = icon("table"), class = "ns-primary-action")
          ),

          div(
            class = "sidebar-section",
            div(class = "sidebar-title", "Help & methodology"),
            div(
              class = "ns-help-links",
              actionButton("info_app", "About N-SIGHT", icon = icon("info-circle")),
              actionButton("info_N", "Nitrogen treatments", icon = icon("flask")),
              actionButton("info_SI", "Sufficiency Index (SI)", icon = icon("leaf"))
            )
          )
        )
      )
    ),

    div(
      id = "nsMain",
      class = "right-scroll",
      tabindex = "-1",

      actionButton(
        "toggleSidebar",
        label = NULL,
        icon = icon("bars"),
        class = "ns-sidebar-toggle",
        title = "Show or hide sidebar",
        `aria-label` = "Show or hide field selection sidebar",
        `aria-expanded` = "true"
      ),

      div(
        class = "ns-page-header",
        div(
          class = "ns-page-heading",
          div(class = "ns-eyebrow", "FIELD PERFORMANCE OVERVIEW"),
          h1(tags$span(id = "page_title", "Select a field to begin")),
          p("Compare field conditions, nitrogen applications, crop sufficiency, and yield performance.")
        ),
        div(
          class = "ns-context-chips",
          div(
            class = "ns-context-chip",
            icon("calendar-alt"),
            div(
              span("Growing season"),
              strong(tags$span(id = "header_season", "Not selected"))
            )
          ),
          div(
            class = "ns-context-chip",
            icon("map-marker-alt"),
            div(
              span("Selected field"),
              strong(tags$span(id = "header_field", "Not selected"))
            )
          )
        )
      ),

      div(
        id = "loading_message",
        class = "ns-loading-panel",
        div(
          class = "ns-loading-visual",
          div(class = "ns-loading-spinner"),
          icon("seedling", class = "ns-loading-icon")
        ),
        div(
          class = "ns-loading-copy",
          strong("Preparing your field insights"),
          span("Loading spatial layers, treatment data, and field analytics. This may take a moment.")
        ),
        div(
          class = "ns-loading-status",
          span(class = "ns-pulse-dot"),
          "Loading data"
        ),
        style = "display: none;"
      ),

      conditionalPanel(
        condition = "!input.AvailableFields || input.AvailableFields === ''",
        div(
          class = "ns-empty-state",
          div(
            class = "ns-empty-illustration",
            div(class = "ns-empty-ring ns-ring-one"),
            div(class = "ns-empty-ring ns-ring-two"),
            div(class = "ns-empty-icon", icon("map-marked-alt"))
          ),
          div(
            class = "ns-empty-copy",
            div(class = "ns-eyebrow", "GET STARTED"),
            h2("Choose a field to explore"),
            p("Select a growing season and field from the sidebar to compare treatments, crop sufficiency, and yield performance."),
            div(
              class = "ns-empty-steps",
              div(
                class = "ns-empty-step",
                span("1"),
                div(strong("Select a season"), tags$small("Choose the growing year."))
              ),
              div(
                class = "ns-empty-step",
                span("2"),
                div(strong("Choose a field"), tags$small("Available fields update automatically."))
              ),
              div(
                class = "ns-empty-step",
                span("3"),
                div(strong("Explore insights"), tags$small("Compare maps, charts, and reports."))
              )
            )
          )
        )
      ),

      conditionalPanel(
        condition = "input.GrowingSeason == '2024' && input.AvailableFields == 'Fields'",
        leafletOutput("all_maps_2024", height = "1250px")
      ),

      conditionalPanel(
        condition = "input.GrowingSeason == '2025' && input.AvailableFields == 'Fields'",
        leafletOutput("all_maps_2025", height = "1250px")
      ),

      conditionalPanel(
        condition = "input.GrowingSeason == '2026' && input.AvailableFields == 'Fields'",
        leafletOutput("all_maps_2026", height = "1250px")
      ),

      conditionalPanel(
        condition = "input.GrowingSeason != '' && input.AvailableFields != '' && input.AvailableFields != 'Fields'",

        div(
          class = "ns-grid-2x2",

          # ---- Field Map ----
          div(
            id = "tile_field",
            class = "ns-tile",
            div(
              class = "ns-tile-title",
              span(class = "ns-tile-icon", icon("map")),
              div(
                strong("Field map"),
                tags$small("Treatment zones and field boundaries")
              )
            ),

            div(
              class = "ns-max-btn",
              actionButton("max_field", NULL, icon = icon("expand"), title = "Expand field map",
                           `aria-label` = "Expand field map", `aria-expanded` = "false",
                           class = "btn btn-default btn-xs")
            ),

            div(
              class = "ns-map-slot",
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                leafletOutput("Field_map_data2436", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                leafletOutput("Field_map_dataSCAL", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                leafletOutput("Field_map_data1402W", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                leafletOutput("Field_map_data1912", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                leafletOutput("Field_map_data2436N", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2026' && input.AvailableFields=='1402 E'",
                leafletOutput("Field_map_data1402E_26", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2026' && input.AvailableFields=='2436 S'",
                leafletOutput("Field_map_data2436S_26", height = "100%")
              )
            )
          ),

          # ---- Nitrogen Treatment ----
          div(
            id = "tile_nitrogen",
            class = "ns-tile",

            div(
              class = "ns-tile-title",
              span(class = "ns-tile-icon", icon("flask")),
              div(
                strong("Nitrogen application"),
                tags$small("Applied nitrogen rate by treatment")
              )
            ),

            div(
              class = "ns-max-btn",
              actionButton("max_nitrogen", NULL, icon = icon("expand"), title = "Expand nitrogen application map",
                           `aria-label` = "Expand nitrogen application map", `aria-expanded` = "false",
                           class = "btn btn-default btn-xs")
            ),

            div(
              class = "ns-map-slot",
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variable2436",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2024_March_11" = "AppliedRate_03_11",
                      "2024_June_24" = "AppliedRate_06_24"
                    )
                  )
                ),
                leafletOutput("NRxmap2436", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variableSCAL",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2024_April_15" = "AppliedRate_04_15",
                      "2024_July_12" = "AppliedRate_07_12",
                      "2024_July_30" = "AppliedRate_07_30"
                    )
                  )
                ),
                leafletOutput("NRxmapSCAL", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variable1402W_25",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2025 April 07" = "April_07",
                      "2025 May 07" = "May_07"
                    )
                  )
                ),
                leafletOutput("NRxmap1402W_25", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variable2436N_25",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2025_April_08" = "April_08"
                    )
                  )
                ),
                leafletOutput("NRxmap2436N_25", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variable1912_25",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2025 April 03" = "April_03"
                    )
                  )
                ),
                leafletOutput("NRxmap1912_25", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2026' && input.AvailableFields=='1402 E'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variable1402E_26",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2026 March 13" = "March_13",
                      "2026 June 30" = "June_30"
                    )
                  )
                ),
                leafletOutput("NRxmap1402E_26", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2026' && input.AvailableFields=='2436 S'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "NRx_variable2436S_26",
                    "Nitrogen Treatment Date",
                    choices = c(
                      "2026 March 19" = "March_19"
                    )
                  )
                ),
                leafletOutput("NRxmap2436S_26", height = "100%")
              )
            ),

            div(
              id = "drawerN",
              class = "ns-tile-drawer",
              div(
                class = "ns-tile-drawer-head",
                div(class = "ns-analysis-label", icon("chart-bar"), " Nitrogen analysis"),
                actionButton("toggleDrawerN", "View charts", class = "ns-mini-btn", icon = icon("chevron-up"), `aria-expanded` = "false")
              ),
              div(
                class = "ns-tile-drawer-body",
                conditionalPanel(
                  "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_2436S", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_2436S", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_2436S", height = "100%"))
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_SCAL", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_SCAL", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_SCAL", height = "100%"))
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_1402W_25", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_1402W_25", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_1402W_25", height = "100%"))
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_2436N_25", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_2436N_25", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_2436N_25", height = "100%"))
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_1912_25", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_1912_25", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_1912_25", height = "100%"))
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2026' && input.AvailableFields=='1402 E'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_1402E_26", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_1402E_26", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_1402E_26", height = "100%"))
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2026' && input.AvailableFields=='2436 S'",
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Per Sector", plotlyOutput("Nplot1_2436S_26", height = "100%")),
                    tabPanel("Average", plotlyOutput("Nplot2_2436S_26", height = "100%")),
                    tabPanel("Dist", plotlyOutput("Nplot3_2436S_26", height = "100%"))
                  )
                )
              )
            )
          ),

          # ---- Crop SI ----
          div(
            id = "tile_si",
            class = "ns-tile",
            div(
              class = "ns-tile-title",
              span(class = "ns-tile-icon", icon("leaf")),
              div(
                strong("Sufficiency Index (SI)"),
                tags$small("Crop nitrogen status from imagery")
              )
            ),

            div(
              class = "ns-max-btn",
              actionButton("max_si", NULL, icon = icon("expand"), title = "Expand Sufficiency Index map",
                           `aria-label` = "Expand Sufficiency Index map", `aria-expanded` = "false",
                           class = "btn btn-default btn-xs")
            ),

            div(
              class = "ns-map-slot",
              conditionalPanel(
                "input.GrowingSeason=='2026'",
                div(
                  class = "ns-data-state",
                  div(class = "ns-data-state-icon", icon("leaf")),
                  strong("SI imagery is not available yet"),
                  p("Crop Sufficiency Index data has not been published for this field and season."),
                  span(icon("clock-o"), " Check back after imagery processing is complete.")
                )
              ),
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "SI_variable2436",
                    "Crop SI Date",
                    choices = c(
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
                    )
                  )
                ),
                leafletOutput("SImap2436S", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "SI_variableSCAL",
                    "Crop SI Date",
                    choices = c(
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
                    )
                  )
                ),
                leafletOutput("SImapSCAL", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "SI_variable1402W_25",
                    "Crop SI Date",
                    choices = c(
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
                    )
                  )
                ),
                leafletOutput("SImap1402W_25", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "SI_variable2436N_25",
                    "Crop SI Date",
                    choices = c(
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
                ),
                leafletOutput("SImap2436N_25", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                div(
                  class = "ns-map-overlay",
                  selectInput(
                    "SI_variable1912_25",
                    "Crop SI Date",
                    choices = c(
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
                    )
                  )
                ),
                leafletOutput("SImap1912_25", height = "100%")
              )
            ),

            div(
              id = "drawerSI",
              class = "ns-tile-drawer",
              div(
                class = "ns-tile-drawer-head",
                div(class = "ns-analysis-label", icon("chart-line"), " SI analysis"),
                actionButton("toggleDrawerSI", "View charts", class = "ns-mini-btn", icon = icon("chevron-up"), `aria-expanded` = "false")
              ),
              div(
                class = "ns-tile-drawer-body",
                conditionalPanel(
                  "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                  tagList(
                    plotlyOutput("SIplot1_2436", height = "100%"),
                    bsPopover(
                      "SIplot1_2436", "Info",
                      "Click on each treatment name to add/remove from the graph.",
                      placement = "top", trigger = "hover"
                    )
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                  tagList(
                    plotlyOutput("SIplot1_SCAL", height = "100%"),
                    bsPopover(
                      "SIplot1_SCAL", "Info",
                      "Click on each treatment name to add/remove from the graph.",
                      placement = "top", trigger = "hover"
                    )
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                  tagList(
                    plotlyOutput("SIplot_1402W_25", height = "100%"),
                    bsPopover(
                      "SIplot_1402W_25", "Info",
                      "Click on each treatment name to add/remove from the graph.",
                      placement = "top", trigger = "hover"
                    )
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                  tagList(
                    plotlyOutput("SIplot_2436N_25", height = "100%"),
                    bsPopover(
                      "SIplot_2436N_25", "Info",
                      "Click on each treatment name to add/remove from the graph.",
                      placement = "top", trigger = "hover"
                    )
                  )
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                  tagList(
                    plotlyOutput("SIplot_1912_25", height = "100%"),
                    bsPopover(
                      "SIplot_1912_25", "Info",
                      "Click on each treatment name to add/remove from the graph.",
                      placement = "top", trigger = "hover"
                    )
                  )
                )
              )
            )
          ),

          # ---- Yield ----
          div(
            id = "tile_yield",
            class = "ns-tile",
            div(
              class = "ns-tile-title",
              span(class = "ns-tile-icon", icon("chart-area")),
              div(
                strong("Yield performance"),
                tags$small("Harvested yield across the field")
              )
            ),

            div(
              class = "ns-max-btn",
              actionButton("max_yield", NULL, icon = icon("expand"), title = "Expand yield map",
                           `aria-label` = "Expand yield map", `aria-expanded` = "false",
                           class = "btn btn-default btn-xs")
            ),

            div(
              class = "ns-map-slot",
              conditionalPanel(
                "input.GrowingSeason=='2026' && input.AvailableFields=='1402 E'",
                leafletOutput("Yield_1402E_26", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2026' && input.AvailableFields=='2436 S'",
                leafletOutput("Yield_2436S_26", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                leafletOutput("Yield_2436S1", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                leafletOutput("Yield_SCAL", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                leafletOutput("Yield_1402W", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                leafletOutput("Yield_1912", height = "100%")
              ),
              conditionalPanel(
                "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                leafletOutput("Yield_2436N", height = "100%")
              )
            ),

            div(
              id = "drawerYield",
              class = "ns-tile-drawer",
              div(
                class = "ns-tile-drawer-head",
                div(class = "ns-analysis-label", icon("chart-area"), " Yield analysis"),
                actionButton("toggleDrawerYield", "View charts", class = "ns-mini-btn", icon = icon("chevron-up"), `aria-expanded` = "false")
              ),
              div(
                class = "ns-tile-drawer-body",
                conditionalPanel(
                  "input.GrowingSeason=='2026' && input.AvailableFields=='1402 E'",
                  plotlyOutput("Yieldplot1_1402E_26", height = "100%")
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2026' && input.AvailableFields=='2436 S'",
                  plotlyOutput("Yieldplot1_2436S_26", height = "100%")
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2024' && input.AvailableFields=='2436 S'",
                  plotlyOutput("Yieldplot1_2436", height = "100%")
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2024' && input.AvailableFields=='SCAL'",
                  plotlyOutput("Yieldplot1_SCAL", height = "100%")
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='1402 W'",
                  plotlyOutput("Yieldplot1_1402W", height = "100%")
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='1912'",
                  plotlyOutput("Yieldplot1_1912", height = "100%")
                ),
                conditionalPanel(
                  "input.GrowingSeason=='2025' && input.AvailableFields=='2436 N'",
                  plotlyOutput("Yieldplot1_2436N", height = "100%")
                )
              )
            )
          )
        )
      )
    )
  )
)
