test_that("For a chosen categorical filter with missing values, an item signifying missingness is incorporated at the top of the filter dropdown menu. By default, all missing values are included." %>% 
            vdoc[["add_spec"]](specs$filter_categorical_missing), { # nolint
  adsl <- pharmaverseadam::adsl
  expect_true(sum(is.na(adsl$EOSSTT)) > 0)

  app <- shinytest2::AppDriver$new(app_dir = "shiny/adam/")

  app$set_inputs(`data_filter-vars` = "EOSSTT")
  app$wait_for_idle(1000)
  expect_true(all(app$get_value(export = "selected")))

  app$set_inputs(`data_filter-EOSSTT` = "<MISSING>")
  app$wait_for_idle(1000)
  expect_true(all(app$get_value(export = "selected") == is.na(adsl$EOSSTT)))

  app$stop()
})
