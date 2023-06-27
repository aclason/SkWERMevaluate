library(shiny)
runExample("03_reactivity")
#slope & aspect class (SLOPE_MOD),
#floodplain (Y/N), elevation, salmon (f = fish presence, s = spawning), wetlands (Y/N)
test_Hab

ui <- fluidPage(
  titlePanel("censusVis"),

  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  label = "BEC subzones",
                  choices = c("ESSFmk",
                              "ESSFmc",
                              "SBSmc2",
                              "SBSdk"),
                  selected = "SBSdk"),

      sliderInput("range",
                  label = "Habitat value:",
                  min = 1, max = 5, value = c(1, 5))
    ),

    mainPanel(
      dataTableOutput("hab_file")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  output$hab_file <- renderDataTable({
    input$var
  })

}

# Create Shiny app ----
shinyApp(ui, server)




