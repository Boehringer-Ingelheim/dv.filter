#' Create filter UI for a single filter
#' @param x a numeric/character vector
#' @param id namespaced variable id
#' @param var name of a filter variable
#' @param val values from input controls (range slider / drop-down menu)
#' @return UI element for a numeric or categorical filter
create_filter_ui <- function(x, id, var, val) {
  # https://github.com/rstudio/shiny/issues/2111
  na_sum <- sum(is.na(x))
  var_ui <- shiny::tags$strong(var)
  remove_ui <- shiny::actionLink(
    inputId = paste0(id, "_remove"),
    label = NULL,
    icon = shiny::icon("times-circle"),
    style = "float: right; color: #8a1501;"
  )
  if (get_input_type(x) == "slider") {
    rng <- range_slider(x)
    # https://github.com/rstudio/shiny/issues/1409
    plot_ui <- shiny::div(
      style = "margin: 0px 10px -25px 10px;",
      shiny::plotOutput(paste0(id, "_plot"), height = 25)
    )
    slider_ui <- shiny::sliderInput(
      inputId = id,
      label = NULL,
      min = rng[1],
      max = rng[2],
      value = val %||% rng
    )
    na_sum_ui <- if (na_sum > 0) {
      shiny::tags$small(
        shinyWidgets::prettyCheckbox(
          inputId = paste0(id, "_na"),
          label = "Include missing values",
          value = TRUE,
          icon = shiny::icon("check"),
          inline = FALSE
        )
      )
    }
    filter_ui <- shiny::div(var_ui, remove_ui, na_sum_ui, plot_ui, slider_ui)
  } else if (get_input_type(x) == "picker") {
    tbl_sorted <- sort(table(x), decreasing = TRUE)
    N <- c("<MISSING>" = na_sum, tbl_sorted) # nolint
    N <- N[N > 0] # nolint
    choices <- names(N)
    N_pct <- floor(100 * N / max(N)) # nolint
    color <- rep("#000000", length(N)) # "black"
    if (na_sum > 0) color[1] <- "#8B0000" # "darkred"
    # https://blog.prototypr.io/css-only-multi-color-backgrounds-4d96a5569a20?gi=ae55142ef933
    style <- glue::glue(
      "color: {color}; border: 1px solid black; background: 
       linear-gradient(90deg, rgba(173, 216, 230, 1) {N_pct}%, rgba(0, 0, 0, 0) {N_pct}%);"
    )
    picker_ui <- shinyWidgets::pickerInput(
      inputId = id,
      label = NULL,
      multiple = TRUE,
      choices = choices,
      selected = val %||% choices,
      choicesOpt = list(
        style = style,
        subtext = N
      ),
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE
      )
    )
    filter_ui <- shiny::div(var_ui, remove_ui, picker_ui)
  } else {
    msg <- paste0("Filter for `", get_first_class(x), "` is not supported")
    br_ui <- shiny::tags$br()
    control_ui <- shiny::tags$small(br_ui, msg, br_ui, br_ui)
    filter_ui <- shiny::div(var_ui, control_ui)
  }
  filter_ui
}
