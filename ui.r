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
      '<video controls width="100%" height="auto" poster="1.png" autoplay loop>
        <source src="WebBanner.mp4" type="video/mp4">
        Your browser does not support the video tag.
      </video>'

    )
  ),


  tabsetPanel(
    type = "hidden",  # Hidden tabs




    tabPanel("Tab 1",



             box(width = 4,

                 div(class = "custom-box",
                     h4("Field Data"),
                     p(

                       selectInput("Fields_Info", "Select Field",
                                   c( "2015 S","Field 2","Field 3")))
                 ),

                 div(class = "custom-box",
                     h4("Map Data "),
                     p(sidebarMenu(
                       menuItem("Field Information", tabName = "item1"),
                       menuItem("Field Nitrogen Treatment", tabName = "item2"),
                       menuItem("Field SI ", tabName = "item3"),
                       menuItem("Yield ", tabName = "item4")
                     ),


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
                                    "2023_August_18" = "SI_8_18")))
                 ),




                 div(class = "custom-box",
                     h4("Filter Data"),
                     p(

                       selectInput("Sector_variable", "Select Sector",
                                   c( "1","2","3","4","5","6","7","8","9","10","11","12")),


                       dataTableOutput("sectorTable"))
                 ),



                 div(class = "custom-box",
                     h4("Downloads"),
                     p(
                       downloadButton("downloadData1", "Map Data"),
                       h4(),
                       downloadButton("downloadData2", "Prescription Map")

                       )
                 )







             ),


             # dashboardBody(
             #   box(width = 8,
             #       #leafletOutput("Field_map_data"),
             #
             #       tabItems(
             #         tabItem(tabName = "item1",
             #
             #                 #leafletOutput("map"),
             #                 leafletOutput("Field_map_data")
             #
             #         ),
             #
             #         tabItem(tabName = "item2",
             #                 leafletOutput("NRxmap"),
             #                 plotly::plotlyOutput("Nplot")
             #
             #         ),
             #         tabItem(tabName = "item3",
             #                 leafletOutput("SImap"),
             #                 #plotOutput(outputId = "Secplot"),
             #                 plotly::plotlyOutput("SIplot")
             #
             #         ),
             #         tabItem(tabName = "item4",
             #                 leafletOutput("Yieldmap")
             #                 #plotOutput(outputId = "Secplot"),
             #                 #plotly::plotlyOutput("Yieldplot")
             #
             #         )
             #
             #
             #       ),
             #       selected = "item1"
             #
             #   ),
             #
             #
             # )


             dashboardBody(


               box(width = 8,

                   tabItems(
                     tabItem(tabName = "item1",

                             #leafletOutput("map"),
                             leafletOutput("Field_map_data")

                     ),

                     tabItem(tabName = "item2",
                             leafletOutput("NRxmap"),
                             plotly::plotlyOutput("Nplot")

                     ),
                     tabItem(tabName = "item3",
                             leafletOutput("SImap"),
                             #plotOutput(outputId = "Secplot"),
                             plotly::plotlyOutput("SIplot")

                     ),
                     tabItem(tabName = "item4",
                             leafletOutput("Yieldmap")
                             #plotOutput(outputId = "Secplot"),
                             #plotly::plotlyOutput("Yieldplot")

                     )
                   ),selected = "item1"  # Set the default selected tab

               )
             )















    )

  )

)




server <- function(input, output) {

  mymap <- st_read("data/2023/21_ENREC_BuffTreatmentSectors.shp")
  mydata<-read.csv("data/2023/21_ENREC_BuffTreatmentSectors.csv")


  str(mymap)
  map_and_data<-inner_join(mymap,mydata)

  ####################################################################


  Field_map <- tm_shape(map_and_data) +tm_polygons(midpoint = 0,
    popup.vars = c("Sector No: " = "SECTOR",
                   "Treatment Type: " = "Treatment"))+
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
      popup.vars = c("Sector No: " = "SECTOR",
                     "N rate for date"=input$NRx_variable,
                     "Treatment Method: " = "Ntreatment",
                     "N Base Rate: " = "NH3_Base_Rx",
                     "Applied N Rate:"="Applied_NRate"
      ),
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


  # SI Map
  reactive_tm_map <- reactive({
    tm_map<- tm_shape(map_and_data) + tm_polygons(
      col = input$SI_variable,
       popup.vars = c("Sector No: " = "SECTOR",
                      "SI for date"=input$SI_variable,
                      "Treatment Method: " = "Ntreatment"
                      ),
      midpoint = 0
    ) + tm_borders(lwd = 0.5) + tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
    return(tm_map)
  })

  # Create the Leaflet map using the reactive_tm_map
  output$SImap <- renderLeaflet({
    tm_map<- reactive_tm_map()
    tmap_leaflet(tm_map)
  })


  ############################################################################


  # Yield Data
  Yield_tm_map <- reactive({
    tm_map <- tm_shape(map_and_data) + tm_polygons(
      popup.vars = c("Sector No: " = "SECTOR",
                     "Yield (bu/ac): " = "Yield"),
      midpoint = 0
    ) + tm_borders(lwd = 0.5) + tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0)
    )
    return(tm_map)
  })

  # Create the Leaflet map using the reactive_tm_map
  output$Yieldmap <- renderLeaflet({
    tm_map <- Yield_tm_map()
    tmap_leaflet(tm_map)
  })

  #######################################################################



  filteredData <- reactive({
    req(input$Sector_variable)  # Ensure that the input is available
    mydata %>%
      filter(SECTOR == input$Sector_variable) %>%
      select(SECTOR, Ntreatment, Field, NH3_Base_Rx, Rx_6_20, Rx_6_29, Rx_7_11, SI_6_15, SI_6_23, SI_7_6, SI_7_19, SI_8_3) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -SECTOR, names_to = "Sector Info", values_to = "Values") %>%
      select(-SECTOR)
  })


  output$sectorTable <- renderDataTable({
    datatable(filteredData(),options = list(searching = FALSE,paging = FALSE ),
              class = "dataTable")# Disable the search bar

  })



  #########################################################################

  # N rate bar plot



  output$Nplot <-  plotly::renderPlotly({

    # Create a data frame containing only the necessary columns
    plot_data <- mydata %>%
      select(SECTOR, NH3_Base_Rx, Rx_6_20, Rx_6_29, Rx_7_11)

    # Reshape the data from wide to long format using tidyr
    plot_data_long <- plot_data %>%
      pivot_longer(cols = -SECTOR, names_to = "Treatment", values_to = "Value")

    # Create a bar plot
    ggplot(plot_data_long, aes(x = factor(SECTOR), y = Value, fill = Treatment)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = "SECTOR",
        y = "Nitrogen Rate(kg-N/ha)",
        fill = "Treatment"
      ) +
      ggtitle("Bar Plot of Treatments for Each Sector") +
      theme_minimal()

  })


  #######################################################################

  output$SIplot <-  plotly::renderPlotly({

    #Create a subset of data for the variables of interest
    subset_data <- mydata[, c("SECTOR", "SI_6_15", "SI_6_23", "SI_7_6", "SI_7_19", "SI_8_3", "SI_8_18")]

    # Reshape the data into long format for plotting

    subset_data_long <- pivot_longer(subset_data, cols = -SECTOR, names_to = "Variable", values_to = "Value")

    # Create a bar plot for each variable
    ggplot(subset_data_long, aes(x = factor(SECTOR), y = Value, fill = Variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Bar Plot of Variables by Sector", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1")  # You can change the palette to your preference


  })





  #######################################################################



  # output$Secplot <- renderPlot({
  #
  #   files <- list.files("files/SI-data-test/", "Analysis", full.names = T)
  #   res <- purrr::map(files, foreign::read.dbf) %>%
  #     tibble(df = ., name = files,dates = ymd(c("2021-05-26", "2021-06-08", "2021-06-14", "2021-06-21", "2021-06-28",
  #                                               "2021-07-06", "2021-07-19", "2021-07-26", "2021-08-02", "2021-08-10", "2021-08-16", "2021-08-24"))) %>%
  #     tidyr::unnest(df)
  #
  #
  #
  #   res %>%
  #     select(dates,Sector, SI) %>%
  #     tidyr::pivot_wider(names_from = dates, values_from = SI, values_fn = mean) %>%
  #     pcp_select(matches("2021")) %>%
  #     ggplot(aes_pcp(color=as.factor(Sector))) +xlab("Days") + ylab("SI")+geom_point()+ ggtitle("SI for Sectors ")+theme(plot.title = element_text(size = 20, face="bold"))+
  #     scale_colour_manual(name = "Sector",values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#FFFF00", "#3300CC", "#993300", "#FF00FF","#000000","#FF0000", "#9999CC", "#66FF00")) +
  #     geom_pcp()
  #
  #
  # })


}

shinyApp(ui, server)
