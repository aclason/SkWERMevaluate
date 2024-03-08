library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(tidyverse)
library(sf)
library(SkWERMevaluate)
library(leaflet)

#-- Load data (can't use project working directory shortcut)
# Just a single layer to initially get the code working
# reproject to datum = WGS84
#Hab_lay <- st_transform(Hab_lay, crs = "+proj=longlat")
#-- Prep data
#HarvestYear_choices <- sort(unique(Hab_lay$HARVEST_YEAR[!is.na(Hab_lay$HARVEST_YEAR)]))


# We actually want to load multiple AOIs
# AOI's
Hab_lay_options <- c("Bulkley TSA", "Kispiox TSA")

Hab_lay <- list(
  Bulkley <- st_read("./app/data/SkWERM_Bulkley TSA_forSelection.gpkg"),
  Kispiox <- st_read("./app/data/SkWERM_Kispiox TSA_forSelection.gpkg")
)
# Define names for the list elements
names(Hab_lay) <- Hab_lay_options


#-- Prep data
# Extract all HARVEST_YEAR values from all spatial objects and combine into a single vector
harvest_years <- unlist(lapply(Hab_lay, function(x) x$HARVEST_YEAR))

# Get unique sorted values of HARVEST_YEAR
HarvestYear_choices <- sort(unique(harvest_years[!is.na(harvest_years)]))


#-- App
ui <- fluidPage(
  titlePanel("SkWERMevaluate Shiny App"),

  # Options for filtering available polygons to select for sampling
  sidebarLayout(
    sidebarPanel(
      h2("Instructions"),
      p("To use this app choose from the different options below to refine the sampling polygons.
        [Insert what else you want this to say]"),
      br(),
      br(),

      # Choose AOI
      selectInput("hablay",
                  label = "1. Select Area of Interest (AOI) (required)",
                  choices = Hab_lay_options),

      # User uploaded boundary data
      br(),
      fileInput(inputId = "upload",
                label = "2. Upload spatial boundary (optional)",
                accept = c(".shp",".dbf",".sbn","sbx",".shx",".prj",
                                           ".gpkg", ".kml"), multiple = TRUE),
      helpText("Upload spatial data. If uploading a shapefile, make sure to upload at least the
               .shp, .sbf, and .shx in order for it to work. .gpkg and .kml are also accepted."),

      # Distance to Road
      br(),
      sliderInput("RoadDist",
                  label = "2. Distance to road",
                  min = 0,
                  max = 15, # make sure this is the maximum for all of the Hab_lay options
                  value = 0),
      helpText("Choose the minimum distance the site is located from a road."),

      # Harvested or not
      br(),
      checkboxGroupInput("HarvestChoice",
                  label = "3. Harvested?",
                  choices = c("Harvested", "Not harvested"),
                  selected = c("Harvested", "Not harvested")),

      helpText("If harvested, choose which harvest years. You can choose multiple."),

      conditionalPanel(
        condition = "input.HarvestChoice.includes('Harvested')",
        pickerInput(
          "HarvestYear",
          label = "Harvest Year",
          choices = HarvestYear_choices,
          options = pickerOptions(
            actionsBox = TRUE,
            size = 10),
          multiple = TRUE,
          selected = HarvestYear_choices)),

      br(),
      p("Download your filtered site selection polygons."),

      # Download user filtered data
      downloadButton("downloadCSV", "Download filtered CSV"),
      downloadButton("downloadGPKG", "Download filtered GeoPackage")

    ),

  # Show a map of the area
  mainPanel(
    h1("Map of your area of interest"),
    helpText("Please wait for a map to appear before choosing options."),
    leafletOutput("map"),
    br(),
    h2("Filtered data based on your selections"),
    helpText("[We could only show certain columns if that is prefered]"),
    dataTableOutput("table")
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  # Reactive to load user-selected AOI spatial data
  selected_hablay <- reactive({
    req(input$hablay)  # Require selection of AOI
    hablay <- Hab_lay[[input$hablay]]

    # Reproject if necessary
    if(st_crs(hablay)$proj4string != "+proj=longlat") {
      hablay <- st_transform(hablay, crs = "+proj=longlat")
    }

    return(hablay)
  })

  # Reactive to load user-uploaded spatial data
  boundary <- reactive({
    if (!is.null(input$upload)) {
      shpdf <- input$upload

      # Name of the temporary directory where files are uploaded
      tempdirname <- dirname(shpdf$datapath[1])

      # Rename files
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }

      # Extract file extension
      file_ext <- tools::file_ext(shpdf$name[1])

      # Read the file based on the extension
      if (file_ext == "shp") {
        # Search for shapefile components
        shp_files <- grep("\\.shp$|\\.dbf$|\\.sbn$|\\.sbx$|\\.shx$", shpdf$name, value = TRUE)
        if (length(shp_files) != 5) {
          stop("Expected all shapefile components.")
        }
        boundary_data <- sf::st_read(file.path(tempdirname, shp_files[1]))
      } else {
        # For other formats, use the first file
        boundary_data <- sf::st_read(file.path(tempdirname, shpdf$name[1]))
      }

      return(boundary_data)
    }
    return(NULL)
  })

  # Filtered data based on user selection and boundary
  filtered_data <- reactive({
    req(selected_hablay())  # Require selected_hablay to be available

    filtered <- selected_hablay()

    if (!is.null(boundary())) {
      filtered <- sf::st_intersection(filtered, boundary())
    }

    # Distance to road
    filtered <- subset(filtered, Road_dist >= input$RoadDist)

    # Harvested or not harvested
    if ("Harvested" %in% input$HarvestChoice && "Not harvested" %in% input$HarvestChoice) {
      if (length(input$HarvestYear) > 0) {
        filtered <- subset(filtered, (is.na(HARVEST_YEAR) | HARVEST_YEAR %in% input$HarvestYear))
      }
    } else if ("Harvested" %in% input$HarvestChoice) {
      filtered <- subset(filtered, !is.na(HARVEST_YEAR) & HARVEST_YEAR %in% input$HarvestYear)
    } else if ("Not harvested" %in% input$HarvestChoice) {
      filtered <- subset(filtered, is.na(HARVEST_YEAR))
    }
    return(filtered)
  })


  # Display map
  output$map <- renderLeaflet({

    # Map
    map <- leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(data = filtered_data(), highlightOptions = highlightOptions(weight = 5, color = "#666", bringToFront = TRUE))

    # Check if boundary data is not NULL
    if (!is.null(boundary())) {
      map <- map %>% addPolygons(data = boundary(), color = "red") # Add user-uploaded boundary to map
    }

    map
  })

  # Filtered data table
  output$table <- renderDataTable({
    datatable(filtered_data())
  })


  # Download data
  # CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("filtered_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # GeoPackage
  output$downloadGPKG <- downloadHandler(
    filename = function() {
      paste("filtered_data", ".gpkg", sep = "")
    },
    content = function(file) {
      sf::st_write(filtered_data(), file, driver = "GPKG")
    }
  )
}


# Create Shiny app ----
shinyApp(ui, server)



