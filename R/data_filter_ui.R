#' UI function of the data filter module
#' @param id An ID string of the module's UI function.
#' @export
data_filter_ui <- function(id) {
  shiny::tagList(
    shiny::tags$div(
      style = "color: darkblue;",
      shiny::icon("filter"),
      shiny::textOutput(shiny::NS(id, "text"), inline = TRUE)
    ),
    shiny::uiOutput(shiny::NS(id, "filters")),
    shiny::actionButton(
      inputId = shiny::NS(id, "clear_filters"),
      label = "Clear All Filters",
      icon = shiny::icon("trash")
    ),
    shiny::uiOutput(shiny::NS(id, "controls"), style = "margin:10px;")
  )
}
