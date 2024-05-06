adsl <- pharmaverseadam::adsl

# App user interface
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    sidebarPanel = shiny::sidebarPanel(
      dv.filter::data_filter_ui(id = "data_filter")
    ),
    mainPanel = shiny::mainPanel(
      shiny::verbatimTextOutput(outputId = "table")
    )
  )
)

# App server function
server <- function(input, output, session) {
  selected <- dv.filter::data_filter_server(
    id = "data_filter",
    data = shiny::reactive(adsl)
  )

  output$table <- shiny::renderPrint({
    table(selected())
  })

  shiny::exportTestValues(
    selected = selected()
  )
}

# Run shiny app
shiny::shinyApp(ui, server)
