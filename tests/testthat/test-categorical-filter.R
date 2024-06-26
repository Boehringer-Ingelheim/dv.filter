test_that("For a chosen categorical fitler, the data filter module provides users with the capability to deselect all categories." %>% 
            vdoc[["add_spec"]](specs$filter_categorical), { # nolint
  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")

  app$set_inputs(`data_filter-vars` = "SEX")
  app$wait_for_idle(1000)
  expect_true(all(app$get_value(export = "selected") == TRUE))

  app$set_inputs(`data_filter-SEX` = character(0))
  app$wait_for_idle(1000)
  expect_true(all(app$get_value(export = "selected") == FALSE))

  app$stop()
})
