test_that("The data filter module allows users to remove all chosen filters simultaneously." %>%
            vdoc[["add_spec"]](specs$filter_add_remove), {
  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")

  app$set_inputs(`data_filter-vars` = "AGE")
  app$set_inputs(`data_filter-vars` = c("AGE", "SEX"))
  app$wait_for_idle(1000)
  expect_equal(app$get_value(input = "data_filter-vars"), c("AGE", "SEX"))

  app$click("data_filter-clear_filters")
  app$wait_for_idle(1000)
  expect_null(app$get_value(input = "data_filter-vars"))

  app$stop()
})
