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



ui <- fluidPage(



  tags$head(
    tags$style(
      HTML(
        "
        .custom-box {
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 10px;
          margin: 10px;
          background-color: #f9f9f9;
        }
        "
      )
    ),

    tags$script(HTML('
      $(document).ready(function() {
        $("a[data-value=\'item1\']").click();
      });
    '))
  ),



  tags$head(
    tags$style(
      HTML(
        "
        .dataTable {
          border: 1px solid #000; /* Add a black border with a width of 1px */
        }
        "
      )
    )
  ),

  titlePanel(
    HTML(
      '<img src="banner.png" alt="Banner Image" style="width: 100%; max-height: 300px;">'
    )
  ),





  tabsetPanel(
    type = "hidden",  # Hidden tabs




    tabPanel("Tab 1",



             box(width = 3,

                 div(class = "custom-box",
                     h3("Field Data"),
                     h4( p(
                       style = "font-size: 306px;",
                      selectInput("AvailableFields", "Select Field",
                                   c( "Maps","2436 S","SCAL"))))
                 ),

                 div(class = "custom-box",
                     h3("Map Data "),
                     h4( p(sidebarMenu(
                       p(menuItem("Field Map", tabName = "item1")),



                       p(menuItem("Field Nitrogen Treatment", tabName = "item2")),



                       conditionalPanel(
                         condition = "input.AvailableFields == '2436 S'",
                         selectInput("NRx_variable2436", "Select Treatment Date",
                                     c( "2024_March_11" = "AppliedRate_03_11",
                                        "2024_June_24" = "AppliedRate_06_24"))),




                       p( menuItem("Field SI ", tabName = "item3")),




                       conditionalPanel(
                         condition = "input.AvailableFields == '2436 S'",
                         selectInput("SI_variable2436", "Select SI Date",
                                     c( "2024_April_05" = "SI_04_05",
                                        "2024_June_07" = "SI_06_07",
                                        "2024_June_12" = "SI_06_12",
                                        "2024_June_21" = "SI_06_21",
                                        "2024_June_26" = "SI_06_26",
                                        "2024_July_05" = "SI_07_05",
                                        "2024_July_11" = "SI_07_11",
                                        "2024_July_17" = "SI_07_17",
                                        "2024_July_22" = "SI_07_22",
                                        "2024_August_01" = "SI_08_01"))),

                       conditionalPanel(
                         condition = "input.AvailableFields == 'SCAL'",
                         selectInput("SI_variableSCAL", "Select SI Date",
                                     c( "2024_June_05" = "SI_06_05",
                                        "2024_June_09" = "SI_06_09",
                                        "2024_June_12" = "SI_06_12",
                                        "2024_June_16" = "SI_06_16",
                                        "2024_June_25" = "SI_06_25",
                                        "2024_June_29" = "SI_06_29",
                                        "2024_July_08" = "SI_07_08",
                                        "2024_July_17" = "SI_07_17",
                                        "2024_July_27" = "SI_07_27",
                                        "2024_June_01" = "SI_08_01"))),


                       menuItem("Yield ", tabName = "item4"))
                     )),




                 ),



                 div(class = "custom-box",
                     h3("Downloads"),
                     p(
                       downloadButton("downloadData1", "Map Data"),
                       h4(),
                       downloadButton("downloadData2", "Prescription Map")

                     )
                 )


             ),



             dashboardBody(


               box(width = 9,

                   tabItems(
                     tabItem(tabName = "item1",

                             #leafletOutput("map"),

                             conditionalPanel(
                               condition = "input.AvailableFields == 'Maps'",
                               leafletOutput("all_maps",height = "1250px")),




                             conditionalPanel(
                               condition = "input.AvailableFields == '2436 S'",
                               leafletOutput("Field_map_data2436",height = "1250px")),

                             conditionalPanel(
                               condition = "input.AvailableFields == 'SCAL'",
                               leafletOutput("Field_map_dataSCAL",height = "1250px")),



                     ),

                     tabItem(tabName = "item2",





                             conditionalPanel(
                               condition = "input.AvailableFields == '2436 S'",
                               leafletOutput("NRxmap2436"),

                               div(class = "custom-box",
                                plotly::plotlyOutput("Nplot1_2436S")),

                               # div(class = "custom-box",
                               #     plotly::plotlyOutput("Nplot2_2436S")),

                               # div(class = "custom-box",
                               #     plotly::plotlyOutput("Nplot3_2436S"))
                               )

                     ),



                     tabItem(tabName = "item3",


                             conditionalPanel(
                               condition = "input.AvailableFields == '2436 S'",
                               leafletOutput("SImap2436S"),

                               div(class = "custom-box",
                                   plotly::plotlyOutput("SIplot1_2436")),
                               # div(class = "custom-box",
                               #     plotly::plotlyOutput("SIplot2_2436")),
                               # div(class = "custom-box",
                               #     plotly::plotlyOutput("SIplot3_2436")),
                               #div(class = "custom-box",
                                #   plotly::plotlyOutput("SIplot4_2436"))
                               ),


                             conditionalPanel(
                               condition = "input.AvailableFields == 'SCAL'",
                               leafletOutput("SImapSCAL"),

                               div(class = "custom-box",
                                   plotly::plotlyOutput("SIplot1_SCAL")),
                               #div(class = "custom-box",
                                #   plotly::plotlyOutput("SIplot2_SCAL"))
                               ),


                     ),
                     tabItem(tabName = "item4",


                     )
                   ),selected = "item1"  # Set the default selected tab

               )
             )



    )

  )

)



server <- function(input, output) {



############################################################################################################
  #2436S
  ##########Treatment_Polygons
  map2436 <- st_read("data/2024/2436S/Treatment_Polygons/24_2436S_buffered_plotmap.shp")
  data2436<-read.csv("data/2024/2436S/Treatment_Polygons/24_2436S_buffered_plotmap.csv")
  str(map2436)
  map2436_and_data2436<-inner_join(map2436,data2436)

  ##########Prescription_File
  #map2436_PF_3_11 <- st_read("data/2024/2436S/Prescription_File/3_11/3-11.shp")
  data2436_PF_3_11 <-read.csv("data/2024/2436S/Prescription_File/3_11/3-11.csv")
  #str(map2436_PF_3_11)
  map_data_2436_PF_3_11<-inner_join(map2436,data2436_PF_3_11)


  ##############NDRE
  ndre1_2436<-read.csv("data/2024/2436S/Aerial_Imagery/SI_2436S_040524.csv")
  SI2436_0405<-inner_join(map2436,ndre1_2436)



################################################################################################################
  #SCAL

  ##########Treatment_Polygons
  mapSCAL <- st_read("data/SCAL/Sector/Pivot-Center-Pivot_sections.shp")
  dataSCAL<-read.csv("data/SCAL/Sector/Pivot-Center-Pivot_sections.csv")
  str(mapSCAL)
  mapSCAL_and_dataSCAL<-inner_join(mapSCAL,dataSCAL)


  ##############NDRE
  ndre_SCAL<-read.csv("data/2024/SCAL/Aerial_Imagery/SCAL_SI.csv")
  SI_SCAL<-inner_join(mapSCAL,ndre_SCAL)



  ############################################################################

  ############################Field_Map#######################################

  ############################################################################



  # Function to remove the first N columns from a dataset
  remove_first_n_columns <- function(df, n) {
    df %>% select(-1:-n)
  }

  # Example datasets with column removal
  map2436_and_data2436_cleaned <- remove_first_n_columns(map2436_and_data2436, 3)
  #mapSCAL_and_dataSCAL_cleaned <- remove_first_n_columns(mapSCAL_and_dataSCAL, 1)

  # Combine all three datasets into a single map
  Combined_map <- tm_shape(map2436_and_data2436_cleaned) +
    tm_polygons(popup.vars = c("Sector No: " = "FID_22_243",
                               "Treatment Type: " = "TRT_Name")) +
    tm_shape(mapSCAL_and_dataSCAL) +
    tm_polygons(popup.vars = c("Sector No: " = "section_nu",
                               "Irrigation: " = "irrigation"
                               )) +
    tm_borders(lwd = 0.5) +
    tm_layout(frame = FALSE,
              inner.margins = c(0, 0, 0, 0))

  # Convert the combined map to Leaflet format
  Combined_map_leaflet <- tmap_leaflet(Combined_map)

  # Render the Leaflet map in the Shiny app
  output$all_maps <- renderLeaflet({
    Combined_map_leaflet %>%
      addProviderTiles("Esri.WorldImagery")
  })










  #Map data
  #Combine all three datasets into a single map


  # Combined_map <-
  #   tm_shape(map2436_and_data2436) +
  #   tm_polygons(popup.vars = c("Sector No: " = "TRT_Num",
  #                              "Treatment Type: " = "TRT_Name")) +
  #   tm_shape(mapSCAL_and_dataSCAL) +
  #   tm_polygons(popup.vars = c("Sector No: " = "section_nu",
  #                              "Irrigation: " = "irrigation")) +
  #   tm_borders(lwd = 0.5) +
  #   tm_layout(frame = FALSE,
  #             inner.margins = c(0, 0, 0, 0))
  #
  # # Convert the combined map to Leaflet format
  # Combined_map_leaflet <- tmap_leaflet(Combined_map)
  #
  # # Render the Leaflet map in the Shiny app
  # output$all_maps <- renderLeaflet({
  #   Combined_map_leaflet %>%
  #     addProviderTiles("Esri.WorldImagery")
  # })






  #2436 Map data

  remove_first_n_columns <- function(df, n) {
    df %>% select(-1:-n)
  }

  # Example datasets with column removal
  map2436_and_data2436_cleaned <- remove_first_n_columns(map2436_and_data2436, 3)

  Field_map2436 <- tm_shape(map2436_and_data2436_cleaned) +tm_polygons(midpoint = 0,col = "TRT_Name",
                                                               popup.vars = c("Sector No: " = "FID_22_243",
                                                                              "Treatment Type: " = "TRT_Name"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
  Field_map_data2436 <- tmap_leaflet(Field_map2436)


  output$Field_map_data2436 <- renderLeaflet({

    Field_map_data2436 %>%
      addProviderTiles("Esri.WorldImagery")
  })




  #SCAL Map data

  remove_first_n_columns <- function(df, n) {
    df %>% select(-1:-n)
  }

  # Example datasets with column removal
  mapSCAL_and_dataSCAL_cleaned <- remove_first_n_columns(mapSCAL_and_dataSCAL, 3)


  Field_mapSCAL <- tm_shape(mapSCAL_and_dataSCAL_cleaned) +tm_polygons(midpoint = 0,col = "Treatment",
                                                               popup.vars = c("Sector No: " = "section_nu",
                                                                              "Treatment: " = "Treatment"))+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
  Field_map_dataSCAL <- tmap_leaflet(Field_mapSCAL)


  output$Field_map_dataSCAL <- renderLeaflet({

    Field_map_dataSCAL %>%
      addProviderTiles("Esri.WorldImagery")
  })





  ###########################Nitrogen_Data############################################



  remove_first_n_columns <- function(df, n) {
    df %>% select(-1:-n)
  }

  # Example datasets with column removal
  map_data_2436_PF_3_11_cleaned <- remove_first_n_columns(map_data_2436_PF_3_11, 3)


  #2436 Prescription

  NRx_tm_map2436 <- reactive({
    tm_map2436 <- tm_shape(map_data_2436_PF_3_11_cleaned) +
      tm_polygons(
        col = input$NRx_variable2436,
        #col="AppliedRate_03_11",
        popup.vars = c(
          "Sector No: " = "FID_22_243",
          "N rate for date" = input$NRx_variable2436,
          "Treatment Method: " = "TRT_Name",
          "Product: " = "Product"
          #"Base N Rate: " = "NH3_Rate"

        ),
        midpoint = 0
      ) +
      tm_borders(lwd = 0.5) +
      tm_layout(
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      )
    return(tm_map2436)
  })

  output$NRxmap2436 <- renderLeaflet({
    tm_map2436 <- NRx_tm_map2436()
    tmap_leaflet(tm_map2436)%>%
      addProviderTiles("Esri.WorldImagery")
  })






  ############################################################################





###############SI map 2015 2436S
  remove_first_n_columns <- function(df, n) {
    df %>% select(-1:-n)
  }

  # Example datasets with column removal
  SI2436_0405_cleaned <- remove_first_n_columns(SI2436_0405, 3)




reactive_tm_map2436 <- reactive({
  tm_map <- tm_shape(SI2436_0405_cleaned) +
    tm_polygons(
      col = input$SI_variable2436,
      #col = "SI_04_05",
      popup.vars = c(
        "Sector No: " = "FID_22_243",
        "Treatment: " = "TRT_Name",
        "SI" = input$SI_variable2436
        #"SI: " = "SI_04_05"
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
output$SImap2436S <- renderLeaflet({
  tm_map <- reactive_tm_map2436()
  tmap_leaflet(tm_map)%>%
    addProviderTiles("Esri.WorldImagery")
})



###############SI map SCAL



remove_first_n_columns <- function(df, n) {
  df %>% select(-1:-n)
}

# Example datasets with column removal
SI_SCAL_cleaned <- remove_first_n_columns(SI_SCAL, 3)





reactive_tm_mapSCAL <- reactive({
  tm_map <- tm_shape(SI_SCAL_cleaned) +

    tm_polygons(midpoint = 0,
                 col = input$SI_variableSCAL, #col for legend

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
output$SImapSCAL <- renderLeaflet({
  tm_map <- reactive_tm_mapSCAL()
  tmap_leaflet(tm_map)%>%
    addProviderTiles("Esri.WorldImagery")
})




  ############################################################################










  #######################################################################


  #######################################################################

  #################Plots#################################################

  #######################################################################







  # N rate plot1 2436S

  output$Nplot1_2436S <- plotly::renderPlotly({

    plot_ly<- ggplot(data2436_PF_3_11, aes(x = FID_22_243)) +
    geom_line(aes(y = AppliedRate_03_11, color = "Applied Rate 03/11")) +
    #geom_line(aes(y = NH3_Rate, color = "Base Rate")) +
    geom_line(aes(y = AppliedRate_06_24, color = "Applied Rate 06/24")) +
      geom_point(aes(y = AppliedRate_03_11, color = "Applied Rate 03/11")) +
      geom_point(aes(y = AppliedRate_06_24, color = "Applied Rate 06/24")) +

    labs(title = "Distribution of Nitrogen rates across different sectors",
         x = "Sector",
         y = "Nitrogen Rate") +
    scale_color_manual(values = c(
      #"Base Rate" ="darkgreen",
                                  "Applied Rate 03/11" ="sandybrown",
                                  "Applied Rate 06/24" = "blue"),
                       name = "Applied Rate Dates") +
    #scale_x_continuous(breaks = unique(data2436_PF_3_11$FID_22_243)) +
    theme_minimal() +

    theme(legend.position = "right")

  # Print the plot
  #print(plot)

  })







  #######################################################################






  output$SIplot1_2436 <- plotly::renderPlotly({


    # Reshape the data to a long format.........ndre1_2436
    data_long <- SI2436_0405 %>%
      pivot_longer(cols = starts_with("SI_"),
                   names_to = "Date",
                   values_to = "SI") %>%
      mutate(Date = as.Date(sub("SI_", "", Date), format = "%m_%d"))

    # Calculate the average SI for each treatment and date
    avg_si <- data_long %>%
      group_by(TRT_Name, Date) %>%
      summarise(avg_SI = mean(SI, na.rm = TRUE))

    # Create the line chart with average SI for each treatment
    ggplot(avg_si, aes(x = Date, y = avg_SI, color = TRT_Name, #group = TRT_Name
                       )) +
      geom_line() +
      geom_point() +
      labs(title = "Average SI Values by Treatment",
           x = "Date",
           y = "Average SI Value",
           color = "Treatment") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      theme(legend.position = "right")





  })











output$SIplot1_SCAL <- plotly::renderPlotly({

  # Reshape the data to a long format
  data_long <- ndre_SCAL %>%
    pivot_longer(cols = starts_with("SI_"),
                 names_to = "Date",
                 values_to = "SI") %>%
    mutate(Date = as.Date(sub("SI_", "", Date), format = "%m_%d"))

  # Calculate the average SI for each treatment and date
  avg_si <- data_long %>%
    group_by(Treatment, Date) %>%
    summarise(avg_SI = mean(SI, na.rm = TRUE))

  # Create the line chart with average SI for each treatment
  ggplot(avg_si, aes(x = Date, y = avg_SI, color = Treatment, #group = Treatment
                     )) +
    geom_line() +
    geom_point() +
    labs(title = "Average SI Values by Treatment",
         x = "Date",
         y = "Average SI Value",
         color = "Treatment") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
    theme(legend.position = "right")




})

















  #######################################################################

  # Yield Plot




#######################################################################












}

shinyApp(ui, server)
