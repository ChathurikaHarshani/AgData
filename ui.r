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



ui <- fluidPage(

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
      '<video controls width="100%" height="auto" poster="1.png" autoplay loop>
        <source src="WebBanner.mp4" type="video/mp4">
        Your browser does not support the video tag.
      </video>'

    )
  ),
  tabsetPanel(
    type = "hidden",  # Hidden tabs
    tabPanel("Tab 1",

             box(width = 2, background ="black",

                 h4("Field Data ",align = "left"),

                 selectInput("Fields_Info", "Select Field",
                             c( "2015 S","Field 2","Field 3")),

                 h4("Map Data ",align = "left"),

                 sidebarMenu(
                   menuItem("Field Information", tabName = "item1",selected = 1),
                   menuItem("Field Nitrogen Treatment", tabName = "item2"),
                   menuItem("Field SI ", tabName = "item3"),
                   menuItem("Yield ", tabName = "item4")
                 ),


                 h4("",align = "left"),
                 h4("Filter Data ",align = "left"),


                 selectInput("Sector_variable", "Select Sector",
                             c( "1","2","3","4","5","6","7","8","9","10","11","12")),


                 dataTableOutput("sectorTable"),



                 selectInput("NRx_variable", "Select Treatment Date",
                             c( "2023_June_20" = "Rx_6_20",
                                "2023_June_29" = "Rx_6_29",
                                "2023_July_11" = "Rx_7_11")),

                 selectInput("SI_variable", "Select SI Date",
                             c( "2023_June_15" = "SI_6_15",
                                "2023_June_23" = "SI_6_23",
                                "2023_July_06" = "SI_7_6",
                                "2023_July_19" = "SI_7_19",
                                "2023_August_03" = "SI_8_3",
                                "2023_August_18" = "SI_8_18")),



                 h4("Downloads ",align = "left"),
                 downloadButton("downloadData1", "Map Data"),
                 h4(),
                 downloadButton("downloadData2", "Prescription Map"),

             ),


             dashboardBody(
               box(width = 10,

                   tabItems(
                     tabItem(tabName = "item1",

                             leafletOutput("Field_map_data")

                     ),

                     tabItem(tabName = "item2",
                             leafletOutput("NRxmap"),
                             plotly::plotlyOutput("Nplot")

                     ),
                     tabItem(tabName = "item3",
                             leafletOutput("SImap"),
                             plotOutput(outputId = "Secplot"),
                             plotly::plotlyOutput("SIplot")

                     )
                   ),

               ),


             ),


    )

  )

)




server <- function(input, output) {

  mymap <- st_read("data/2023/21_ENREC_BuffTreatmentSectors.shp")
  mydata<-read.csv("data/2023/21_ENREC_BuffTreatmentSectors.csv")

  str(mymap)
  map_and_data<-inner_join(mymap,mydata)

  ####################################################################


  Field_map <- tm_shape(map_and_data) +tm_polygons(midpoint = 0)+
    tm_borders(lwd = 0.5) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
  Field_map_data <- tmap_leaflet(Field_map)



  output$Field_map_data <- renderLeaflet({

    Field_map_data
  })

  #######################################################################


  # Define a reactive expression to access input$variable
  NRx_tm_map <- reactive({
    tm_map <- tm_shape(map_and_data) + tm_polygons(
      col = input$NRx_variable,
      popup.vars = c("Sector No: " = "Sector", "Treatment Type: " = "Treatment"),
      midpoint = 0
    ) + tm_borders(lwd = 0.5) + tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
    return(tm_map)
  })


  output$NRxmap <- renderLeaflet({
    tm_map <- NRx_tm_map()
    tmap_leaflet(tm_map)
  })



  ############################################################################


  # Define a reactive expression to access input$variable
  reactive_tm_map <- reactive({
    tm_map <- tm_shape(map_and_data) + tm_polygons(
      col = input$SI_variable,
      popup.vars = c("Sector No: " = "Sector", "Treatment Type: " = "Treatment"),
      midpoint = 0
    ) + tm_borders(lwd = 0.5) + tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
    return(tm_map)
  })

  # Create the Leaflet map using the reactive_tm_map
  output$SImap <- renderLeaflet({
    tm_map <- reactive_tm_map()
    tmap_leaflet(tm_map)
  })

  #######################################################################



  filteredData <- reactive({
    req(input$Sector_variable)  # Ensure that the input is available
    mydata %>%
      filter(Sector == input$Sector_variable) %>%
      select(Sector, Ntreatment, Field, NH3_Base_Rx, Rx_6_20, Rx_6_29, Rx_7_11, SI_6_15, SI_6_23, SI_7_6, SI_7_19, SI_8_3) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -Sector, names_to = "Sector Info", values_to = "Values") %>%
      select(-Sector)
  })


  output$sectorTable <- renderDataTable({
    datatable(filteredData(),options = list(searching = FALSE,paging = FALSE ),
              class = "dataTable")# Disable the search bar

  })



  #########################################################################

  # output$Nplot <- renderPlot({
  output$Nplot <-  plotly::renderPlotly({

    # Create a data frame containing only the necessary columns
    plot_data <- mydata %>%
      select(Sector, NH3_Base_Rx, Rx_6_20, Rx_6_29, Rx_7_11)

    # Reshape the data from wide to long format using tidyr
    plot_data_long <- plot_data %>%
      pivot_longer(cols = -Sector, names_to = "Treatment", values_to = "Value")

    # Create a bar plot
    ggplot(plot_data_long, aes(x = factor(Sector), y = Value, fill = Treatment)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = "Sector",
        y = "Nitrogen Rate ()",
        fill = "Treatment"
      ) +
      ggtitle("Bar Plot of Treatments for Each Sector") +
      theme_minimal()



  })


  #######################################################################

  output$SIplot <-  plotly::renderPlotly({

    #Create a subset of data for the variables of interest
    subset_data <- mydata[, c("Sector", "SI_6_15", "SI_6_23", "SI_7_6", "SI_7_19", "SI_8_3", "SI_8_18")]

    # Reshape the data into long format for plotting

    subset_data_long <- pivot_longer(subset_data, cols = -Sector, names_to = "Variable", values_to = "Value")

    # Create a bar plot for each variable
    ggplot(subset_data_long, aes(x = factor(Sector), y = Value, fill = Variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Bar Plot of Variables by Sector", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")  # You can change the palette to your preference


  })





  #######################################################################





  output$Secplot <- renderPlot({

    files <- list.files("files/SI-data-test/", "Analysis", full.names = T)
    res <- purrr::map(files, foreign::read.dbf) %>%
      tibble(df = ., name = files,dates = ymd(c("2021-05-26", "2021-06-08", "2021-06-14", "2021-06-21", "2021-06-28",
                                                "2021-07-06", "2021-07-19", "2021-07-26", "2021-08-02", "2021-08-10", "2021-08-16", "2021-08-24"))) %>%
      tidyr::unnest(df)



    res %>%
      select(dates,Sector, SI) %>%
      tidyr::pivot_wider(names_from = dates, values_from = SI, values_fn = mean) %>%
      pcp_select(matches("2021")) %>%
      ggplot(aes_pcp(color=as.factor(Sector))) +xlab("Days") + ylab("SI")+geom_point()+ ggtitle("SI for Sectors ")+theme(plot.title = element_text(size = 20, face="bold"))+
      scale_colour_manual(name = "Sector",values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#FFFF00", "#3300CC", "#993300", "#FF00FF","#000000","#FF0000", "#9999CC", "#66FF00")) +
      geom_pcp()


  })



}

shinyApp(ui, server)
