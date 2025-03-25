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

server <- function(input, output, session) {
  data <- data.frame(
    id = c("1", "2", "3", "4", "5"),
    group = factor(c("A", "B", "A", "B", "A"), levels = c("A", "B", "C")),
    age = c(1L, 3L, 5L, 7L, 9L),
    weight = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  selected <- dv.filter::data_filter_server(
    id = "data_filter",
    data = shiny::reactive(data)
  )

  ret_value_update_count <- shiny::reactiveVal(0)
  shiny::observeEvent(selected(), ret_value_update_count(ret_value_update_count() + 1))

  output$table <- shiny::renderText({
    sprintf(
      "Selected: %d; Update count: %d\nFilter expression: %s",
      sum(selected()), ret_value_update_count(),
      as.character(attr(selected(), "expr"))
    )
  })

  shiny::exportTestValues(
    selected = selected(), update_count = ret_value_update_count()
  )
}

app <- shiny::shinyApp(ui, server)

test_that("dv.filter only triggers once when selection changes", {
  # NOTE: dv.filter used to produce several reactive updates per filter state change. That behavior was patched.
  app <- shinytest2::AppDriver$new(app)
  app$wait_for_idle()

  # initially, the filter returns unfiltered data
  exports <- app$get_values()[["export"]]
  testthat::expect_equal(exports[["update_count"]], 1)

  # adding a filter variable has no effect
  app$set_inputs(`data_filter-vars` = "age")
  app$wait_for_idle(1000)

  exports <- app$get_values()[["export"]]
  testthat::expect_equal(exports[["update_count"]], 1)

  # modifying filters in a way that updates the selection generates a single update
  app$set_inputs(`data_filter-age` = c(2, 9))
  app$wait_for_idle(1000)

  exports <- app$get_values()[["export"]]
  testthat::expect_equal(exports[["update_count"]], 2)

  app$stop()
})

# NOTE: These tests are a formalization of the current behavior of the filter.
#       They are here so that we are aware of changes to that behavior, because they can have downstream effects on
#       our modules. 
#       That doesn't mean we want the documented behaviors to persist. Read the comments surrounding the tests for
#       more information.

# FIXME: We don't want this test to pass
test_that("dv.filter output expression is not always in sync with actual filter state", {
  # NOTE: We call the patch mentioned in the previous test a "patch" and not a "fix" because although it
  #       addressed a crippling behavior, it introduced a minor incorrect behavior that right now doesn't
  #       impact our ecosystem but it's a known bug. That was recognized at the time of patching, but was
  #       allowed in the interest of expedience.
  #       If dv.filter is never to be included inside modules, it would be better to just integrate it as
  #       part of dv.manager. Side benefit: They are updated in lockstep.
  app <- shinytest2::AppDriver$new(app)
  app$wait_for_idle()

  app$set_inputs(`data_filter-vars` = "age")
  app$wait_for_idle(100)

  # This filter change impacts output selection and allows up-to-date expression to flow through
  app$set_inputs(`data_filter-age` = c(2, 9), priority_ = "event")
  app$wait_for_idle(1000)
  filter_expr <- app$get_values()[["export"]][["selected"]] |> attr("expr")
  expected <- list(age = base::bquote(is.na(age) | age >= 2L & age <= 9L))
  testthat::expect_equal(filter_expr, expected)

  # This filter change does not impact output selection and returns an outdated filter expression
  app$set_inputs(`data_filter-age` = c(3, 9), priority_ = "event")
  app$wait_for_idle(1000)
  filter_expr <- app$get_values()[["export"]][["selected"]] |> attr("expr")
  outdated <- expected
  testthat::expect_equal(filter_expr, outdated)

  app$stop()
})

# FIXME: We don't want this test to pass
test_that("dv.filter takes a bit over one second to produce a new selection", {
  # NOTE: A while ago, a shiny::debounce(1000) to mitigate usability issues around the redrawing of selectors during
  #       user interaction. The resulting behavior made the module more usable (without completely eliminating the
  #       issue) but also slower to react.
  app <- shinytest2::AppDriver$new(app)

  app$wait_for_idle()
  app$set_inputs(`data_filter-vars` = "age")
  app$wait_for_idle(100)

  # modifying filters in a way that updates the selection generates a single update
  app$set_inputs(`data_filter-age` = c(3, 9))
  t0 <- Sys.time()
  # we use busy wait instead of wait_for_idle to react as fast as possible to returned value update
  repeat {
    exports <- app$get_values()[["export"]]
    if (exports[["update_count"]] > 1) break
  }
  response_lag <- Sys.time() - t0

  testthat::expect_true(1 <= response_lag && response_lag <= 1.5)

  app$stop()
})

test_that("dv.filter does not display unused levels on selectors associated to factor variables", {
  app <- shinytest2::AppDriver$new(app)

  app$wait_for_idle()
  app$set_inputs(`data_filter-vars` = "group")
  app$wait_for_idle(1000)

  groups <- app$get_values()[["input"]][["data_filter-group"]]
  testthat::expect_equal(groups, c("A", "B"))

  app$stop()
})

# FIXME: We don't want this test to pass
test_that("dv.filter offers fractional steps for range selection of integer data", {
  # NOTE: For integer values, we would expect slider selection steps to be integer too, but they are fractional.
  filter_ui <- create_filter_ui(c(1L, 9L), NULL, "foo", c(1L, 9L))
  step <- unlist(filter_ui)[["children.children.attribs.data-step"]] |> as.numeric()

  is_fractional <- as.integer(step) != step
  testthat::expect_true(is_fractional)
})

# TODO: We (maybe) don't want this test to pass
test_that("dv.filter offers filters in alphabetical order", {
  # NOTE: Filter variable selection is offered in alphabetical order, dropping the column order of the input dataset
  #       That reduces the expressiveness of the filter. Alphabetical order would be desirable in the absence of
  #       the selector's search box.
  app <- shinytest2::AppDriver$new(app)

  app$wait_for_idle()

  filters_to_enable <- c("id", "group", "age")

  app$set_inputs(`data_filter-vars` = filters_to_enable)
  app$wait_for_idle(1000)

  enabled_filters <- app$get_values()[["input"]][["data_filter-vars"]]

  enabled_filters_are_sorted <- identical(enabled_filters, sort(filters_to_enable))

  testthat::expect_true(enabled_filters_are_sorted)

  app$stop()
})

# FIXME: We don't want this test to pass
test_that("dv.filter fails to add filter after removing it with 'remove' button", {
  # NOTE: The single-filter remove button makes the module misbehave. Trying to re-add a removed button only makes
  #       the UI blink.
  app <- shinytest2::AppDriver$new(app)

  app$wait_for_idle()

  # add age filter
  app$set_inputs(`data_filter-vars` = "age")
  app$wait_for_idle(1000)
  enabled_filters <- app$get_values()[["input"]][["data_filter-vars"]]
  testthat::expect_equal(enabled_filters, "age") # it's there

  # remove and re-add age filter
  app$click("data_filter-age_remove")
  app$wait_for_idle(1000)
  app$set_inputs(`data_filter-vars` = "age")
  app$wait_for_idle(1000)
  enabled_filters <- app$get_values()[["input"]][["data_filter-vars"]]
  testthat::expect_null(enabled_filters) # it's not there (bug)

  app$stop()
})

# FIXME: We don't want this test to pass
test_that("dv.filter collapses menus the user is interacting with", {
  # NOTE: Because of the way the interface is rendered, interacting with an expanded menu redraws and collapses it,
  #       even though the menu already reflects the correct state of the interface.
  app <- shinytest2::AppDriver$new(app)

  app$wait_for_idle()

  # add age filter
  app$set_inputs(`data_filter-vars` = "group")
  app$wait_for_idle(1000)

  # expand
  app$run_js("document.querySelector('[data-id=\"data_filter-group\"]').click()")
  html <- app$get_html("[data-id='data_filter-group']")
  expanded_state <- gsub('.*aria-expanded="([^"]*)".*', "\\1", html)
  testthat::expect_equal(expanded_state, "true")

  # click on first dropdown option
  app$run_js("$('.dropdown-menu > li:nth-child(1) > a').click();")
  app$wait_for_idle(1000)

  html <- app$get_html("[data-id='data_filter-group']")
  expanded_state <- gsub('.*aria-expanded="([^"]*)".*', "\\1", html)
  testthat::expect_equal(expanded_state, "false") # should still be expanded (bug)

  app$stop()
})

# FIXME: We don't want this test to pass
test_that("dv.filter returns logical(0) when filtering an all-NA numeric variable", {
  # NOTE: The interface also shows an opaque error message to the user
  app <- shinytest2::AppDriver$new(app)

  app$wait_for_idle()

  # add age filter
  app$set_inputs(`data_filter-vars` = "weight")
  app$wait_for_idle(1000)

  selected <- app$get_values()[["export"]][["selected"]]
  attributes(selected) <- NULL
  testthat::expect_equal(selected, logical(0)) # bug

  app$stop()
})
