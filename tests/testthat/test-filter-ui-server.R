get_id <- function(html, name) {
  html |>
    rvest::html_elements(paste0("#", name)) |>
    rvest::html_attr("id")
}

adsl <- pharmaverseadam::adsl

test_that(
  "The UI of the data filter module includes selection and input controls, allowing users to establish and modify filtering criteria." %>%
    vdoc[["add_spec"]](specs$filter_ui_server), # nolint 
  {
    id <- "data_filter"
    ns <- shiny::NS(id)
    ui <- data_filter_ui(id)
    html <- ui |> rvest::minimal_html()
    expect_equal(get_id(html, ns("filters")), ns("filters"))
    expect_equal(get_id(html, ns("controls")), ns("controls"))
    expect_equal(get_id(html, ns("text")), ns("text"))
    expect_equal(get_id(html, ns("clear_filters")), ns("clear_filters"))
  }
)

test_that(
  "The server component of the data filter module generates a logical vector, signifying if a row of data should be selected (TRUE) or disregarded (FALSE)." %>%
    vdoc[["add_spec"]](specs$filter_ui_server), # nolint
  {
    data <- shiny::reactive(adsl)
    shiny::testServer(
      data_filter_server,
      args = list(data = data),
      {
        session$setInputs(vars = "AGE")
        expect_equal(is.logical(selected()), TRUE)
      }
    )
  }
)

test_that("The data filter module generates an error if the input data is not reactive or metareactive." %>%
            vdoc[["add_spec"]](specs$filter_ui_server), {
  shiny::testServer(
    data_filter_server,
    args = list(data = adsl) # non-reactive data
  ) |> expect_error(
    regexp = "Input data needs to be reactive/metareactive"
  )
})

test_that("The data filter module generates an error if the input data is NULL." %>%
            vdoc[["add_spec"]](specs$filter_ui_server), {
  shiny::testServer(
    data_filter_server,
    args = list(data = shiny::reactive(NULL)),
    expect_equal(selected(), rep(TRUE, nrow(data())))
  ) |> expect_error(
    regexp = "Input data can't be NULL"
  )
})
