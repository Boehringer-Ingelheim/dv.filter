#' Server function of the data filter module
#' @param id An ID string of the module's server function.
#' @param data A reactive data for dynamic filtering.
#' @export
data_filter_server <- function(id, data) {
  if (!is_anyreactive(data)) {
    stop("Input data needs to be reactive/metareactive")
  }

  module <- function(input, output, session) {
    # create an object that is similar to input and use it to
    # handle initial NULL values introduced by renderUI()
    input_init_null <- shiny::reactiveValues()
    returned <- shiny::reactiveVal(value = NULL)

    shiny::observeEvent(data(), {
      purrr::walk(names(data()), function(var) {
        input_init_null[[var]] <- NA
        shiny::observeEvent(input[[var]], {
          input_init_null[[var]] <- input[[var]]
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
      })
    })

    shiny::observeEvent(data(), {
      purrr::walk(names(data()), function(var) {
        x <- data()[[var]]
        na_sum <- sum(is.na(x))
        if (get_input_type(x) == "slider") {
          shiny::updateSliderInput(
            session = session,
            inputId = var,
            value = range_slider(x)
          )
        } else if (get_input_type(x) == "picker") {
          tbl <- c("<MISSING>" = na_sum, sort(table(x), decreasing = TRUE))
          tbl <- tbl[tbl > 0]
          choices <- names(tbl)
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = var,
            choices = choices,
            selected = choices
          )
        }
      })
    })

    output$filters <- shiny::renderUI({
      choices <- sort(names(data()))
      content <- get_icon_label(data()[, choices])
      shinyWidgets::pickerInput(
        inputId = session$ns("vars"),
        label = NULL,
        multiple = TRUE,
        choices = choices,
        choicesOpt = list(content = content),
        options = shinyWidgets::pickerOptions(
          actionsBox = FALSE,
          liveSearch = TRUE,
          dropupAuto = FALSE,
          title = "Add / Remove Filters",
          selectedTextFormat = "static",
          style = "btn-primary"
        )
      )
    })

    shiny::observeEvent(input$clear_filters, {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "vars",
        selected = character(0)
      )
    })

    shiny::observe({
      purrr::walk(input$vars, function(filter_var) {
        shiny::observeEvent(input[[paste0(filter_var, "_remove")]], {
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "vars",
            selected = input$vars[input$vars != filter_var]
          )
        })
      })
    })

    selected <- shiny::reactive({
      if (is.null(data())) stop("Input data can't be NULL")
      out <- rep(TRUE, NROW(data()))
      if (length(input$vars) > 0) {
        out_ <- purrr::map(input$vars, function(filter_var) {
          val_na <- input[[paste0(filter_var, "_na")]]
          # anyNA implements any(is.na(x)) in a possibly faster way
          shiny::req(!anyNA(input_init_null[[filter_var]]))
          expr <- create_filter_expr(
            x = data()[[filter_var]],
            var = filter_var,
            val = input[[filter_var]],
            include_na = val_na %||% TRUE
          )
          selected_ <- rlang::eval_tidy(expr, data = data())
          attr(selected_, "expr") <- expr
          selected_
        })
        names(out_) <- input$vars
        out <- purrr::reduce(out_, `&`)
        attr(out, "expr") <- purrr::map(input$vars, ~ attr(out_[[.x]], "expr"))
        names(attr(out, "expr")) <- input$vars
      }
      out
    }) %>% shiny::debounce(1000)

    output$controls <- shiny::renderUI({
      # https://gist.github.com/wch/5436415/
      purrr::map(input$vars, function(filter_var) {
        x <- data()[[filter_var]]
        # use isolate() to keep input values from existing filters so users
        # don't need to re-enter the values when a filter is added or removed.
        val <- shiny::isolate(input[[filter_var]])
        if (get_input_type(x) == "slider") {
          output[[paste0(filter_var, "_plot")]] <- shiny::renderPlot({
            hist_plot(x, selected())
          }, bg = "transparent")
        }
        create_filter_ui(x = x, id = session$ns(filter_var), var = filter_var, val = val)
      })
    })

    output$text <- shiny::renderText({
      glue::glue(
        "{sum(selected())} of {length(selected())} total entries selected"
      )
    })
    
    shiny::observeEvent(selected(), {
      shiny::req(length(input$vars) > 0)
      purrr::walk(input$vars, function(var) {
        x <- data()[[var]]
        if (get_input_type(x) == "picker") {
          y <- factor(selected(), levels = c(FALSE, TRUE))
          # create a contingency table to calculate number of rows in filtered and full data
          tbl <- stats::addmargins(table(x, y), margin = 2) %>% 
            as.data.frame.matrix()
          # arrange the contingency table in an descending order by the number of rows in full data
          tbl_desc <- dplyr::arrange(tbl, -.data[["Sum"]])
          # subset the contingency table by excluding categories that have empty rows in full data
          tbl_desc <- dplyr::filter(tbl_desc, .data[["Sum"]] > 0)
          choices <- rownames(tbl_desc)
          n <- tbl_desc[["TRUE"]]
          N <- tbl_desc[["Sum"]] # nolint
          color <- rep("#000000", length(N)) # "black"
          if (any(is.na(x))) {
            choices <- c("<MISSING>", choices)
            n <- c(sum(is.na(x[y == TRUE])), n)
            N <- c(sum(is.na(x)), N) # nolint
            color <- c("#8B0000", color) # "darkred"
          }
          N_pct <- floor(100 * N / max(N)) # nolint
          n_pct <- floor(100 * n / max(N))
          # https://blog.prototypr.io/css-only-multi-color-backgrounds-4d96a5569a20?gi=ae55142ef933
          style <- glue::glue(
            "color: {color}; border: 1px solid black; background:
             linear-gradient(90deg, rgba(173, 216, 230, 1) {n_pct}%, rgba(0, 0, 0, 0) {n_pct}%),
             linear-gradient(90deg, rgba(211, 211, 211, 1) {N_pct}%, rgba(0, 0, 0, 0) {N_pct}%);"
          )
          
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = var,
            choices = choices,
            selected = input[[var]],
            choicesOpt = list(
              subtext = glue::glue("{n} / {N}"),
              style = style
            )
          )
        }
      })
    }, ignoreInit = TRUE)
    # Do not update the filter unless there has been any change in the returned vector
    # The returned attribute maybe inconsistent, as filterings that return exactly the same
    # logical vector may not update the expression attribute. 
    # This should not be much of a problem as no is using that expression.
    # A solution to the above is that the code is returned as part of a list of two reactives,
    # list (value, code). This way altering the code does not necessarily update the depending
    # reactives, and the code can be read independently.

    shiny::observeEvent(
      selected(),
      {
        if (is.null(returned()) || !all(selected() == returned())) {
          returned(selected())
        }
      }
    )
    return(returned)
  }

  shiny::moduleServer(id, module)
}
