# Use a list to declare the specs
# nolint start line_length_linter
specs_list <- list

filter_general <- specs_list(
  "filter_add_remove" = "dv.filter allows users to add or remove filter(s).",
  "filter_ui_server" = "dv.filter contains a UI and server component.",
  "filter_nrows" = "dv.filter displays the number of rows selected in the UI."
)

filter_numeric <- specs_list(
  "filter_numeric" = "dv.filter enables filtering of a numeric filter via a range slider.",
  "filter_numeric_missing" = "dv.filter allows users to include or exclude missing values of a numeric filter."
)

filter_categorical <- specs_list(
  "filter_categorical" = "dv.filter enables filtering of a categorical filter via a dropdown menu",
  "filter_categorical_missing" = "dv.filter allows users to include or exclude missing values of a categorical filter."
)

specs <- c(
  filter_general,
  filter_numeric,
  filter_categorical
)
