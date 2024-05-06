test_that("For a chosen numeric filter, the data filter module provides a range slider and displays a histogram of the numeric variable.", {
  adsl <- pharmaverseadam::adsl
  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")

  app$set_inputs(`data_filter-vars` = "AGE")
  app$wait_for_idle(1000)
  expect_equal(app$get_value(input = "data_filter-AGE"), range(adsl$AGE))
  expect_false(is.null(app$get_value(output = "data_filter-AGE_plot")))

  app$stop()
})
