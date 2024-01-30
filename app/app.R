library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(tidyverse)
library(sf)
library(SkWERMevaluate)
library(leaflet)

#-- Load data (can't use project working directory shortcut)
Hab_lay <- st_read("C:/users/farne/OneDrive/Documents/Borealis_Ecological_Services/SkWERM/SkWERMevaluate/app/data/SkWERM_Bulkley TSA_forSelection.gpkg")
# reproject to datum = WGS84
Hab_lay <- st_transform(Hab_lay, crs = "+proj=longlat")

#-- Prep data
# Create column Harvested: Harvested or not harvested
Hab_lay$Harvested <- ifelse(is.na(Hab_lay$HARVEST_YEAR), "Not harvested", "Harvested")
# HAVREST_YEAR is na change it to "Not harvested"
#Hab_lay$HARVEST_YEAR[is.na(Hab_lay$HARVEST_YEAR)] <- "Not harvested"

HarvestYear_choices <- sort(unique(Hab_lay$HARVEST_YEAR[!is.na(Hab_lay$HARVEST_YEAR)]))

#-- App
ui <- fluidPage(
  titlePanel("SkWERMevaluate Shiny App"),

  # Options for filtering available polygons to select for sampling
  sidebarLayout(
    sidebarPanel(
      helpText("Choose from the options below to refine the sampling polygons"),

      # Distance to Road
      sliderInput("RoadDist",
                  label = "Distance to road",
                  min = min(Hab_lay$Road_dist),
                  max = round(max(Hab_lay$Road_dist), digits = 1),
                  value = 0),

      # Harvested or not
      checkboxGroupInput("Harvested",
                  label = "Harvested?",
                  choices = c("Harvested",
                              "Not harvested"),
                  selected = c("Harvested", "Not harvested")),

      conditionalPanel(
        condition = "input.Harvested.includes('Harvested')",
        pickerInput(
          "HarvestYear",
          label = "Harvest Year",
          choices = HarvestYear_choices, # watch out this drops the NAs (which we need for unharvested)
          options = pickerOptions(
            actionsBox = TRUE,
            size = 10),
          multiple = TRUE,
          selected = HarvestYear_choices))

    ),

  # Show a map of the area
  mainPanel(
    leafletOutput("map"),
    dataTableOutput("table")
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Loading modal to let user know map is loading
  showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))

  # Remove modal when app is ready
  observe({
    req(map,)
    removeModal()
  })


  # Change HARVEST_YEAR == NA to "Unharvested"
  #is.na(Hab_lay$HARVEST_YEAR) <- "Unharvested"

  # Filter map based on user selected Distance to road
  Hab_filter1 <- reactive({
    Hab_lay %>%
      filter(Road_dist >= input$RoadDist)
  })
  # Filter map based on harvested or not & harvest year
  filtered_map <- reactive({
    Hab_filter2 <- Hab_filter1()

    if ("Not harvested" %in% input$Harvested & "Harvested" %in% input$Harvested) {
      # If both "Not harvested" and "Harvested" are selected, return all polygons
      return(Hab_filter2)
    } else if ("Not harvested" %in% input$Harvested) {
      # If only "Not harvested" is selected, add filter for is.na(HARVEST_YEAR)
      return(Hab_filter2 %>% filter(is.na(HARVEST_YEAR)))
    } else {
      # If only "Harvested" is selected, add filter for selected harvest years
      return(Hab_filter2 %>% filter(HARVEST_YEAR %in% input$HarvestYear))
    }
  })


  output$map <- renderLeaflet({

    # Map
    filtered_map() %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% #
      addPolygons(
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          bringToFront = TRUE
        )
      )
  })

  output$table <- renderDataTable({
    datatable(filtered_map())
    # would be nice to add a feature that jumps to/hiighlights the row that you click on
  })
}

# Create Shiny app ----
shinyApp(ui, server)



