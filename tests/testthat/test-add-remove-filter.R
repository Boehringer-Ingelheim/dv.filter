test_that("The data filter module enables users to add or remove a filter variable." %>%
            vdoc[["add_spec"]](specs$filter_add_remove), {
  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")

  app$wait_for_idle(1000)
  expect_null(app$get_value(input = "data_filter-vars"))

  app$set_inputs(`data_filter-vars` = "AGE")
  app$wait_for_idle(1000)
  expect_equal(app$get_value(input = "data_filter-vars"), "AGE")

  app$click("data_filter-AGE_remove")
  app$wait_for_idle(1000)
  expect_null(app$get_value(input = "data_filter-vars"))

  app$stop()
})
