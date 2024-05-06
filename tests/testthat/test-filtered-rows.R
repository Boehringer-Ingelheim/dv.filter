test_that("The data filter module presents a text output, signifying the number of entries selected in the filtered data.", {
  adsl <- pharmaverseadam::adsl
  N <- nrow(adsl)

  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")
  app$wait_for_idle(1000)
  expect_equal(
    app$get_value(output = "data_filter-text"),
    paste(N, "of", N, "total entries selected")
  )

  app$set_inputs(`data_filter-vars` = "SEX")
  app$set_inputs(`data_filter-SEX` = "M")
  app$wait_for_idle(1000)
  expect_equal(
    app$get_value(output = "data_filter-text"),
    paste(sum(adsl$SEX == "M"), "of", N, "total entries selected")
  )

  app$stop()
})
