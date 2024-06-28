test_that("For a chosen numeric filter with missing values, the data filter module includes a checkbox enabling 
          users to either incorporate or omit missing values. By default, all missing values are included." %>%
  vdoc[["add_spec"]](specs$filter_numeric_missing), { # nolint
  adsl <- pharmaverseadam::adsl
  expect_true(sum(is.na(adsl$EOSDT)) > 0)

  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")

  app$set_inputs(`data_filter-vars` = "EOSDT")
  app$wait_for_idle(1000)
  expect_true(all(app$get_value(export = "selected") == TRUE))

  app$set_inputs(`data_filter-EOSDT_na` = FALSE)
  app$wait_for_idle(1000)
  expect_true(all(app$get_value(export = "selected") == !is.na(adsl$EOSDT)))

  app$stop()
})
