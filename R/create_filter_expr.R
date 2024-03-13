#' Create filter expression for a single filter
#' @param x a numeric/character vector
#' @param var name of a filter variable
#' @param val values from input controls (range slider / drop-down menu)
#' @param include_na a logical value indicating whether missing values (NA)
#' should be included
create_filter_expr <- function(x, var, val, include_na = TRUE) {
  var_ <- as.name(var)
  if (get_input_type(x) == "slider") {
    expr <- rlang::expr(!!var_ >= !!val[1] & !!var_ <= !!val[2])
    if (include_na) {
      expr <- rlang::expr(is.na(!!var_) | !!expr)
    } else {
      expr <- rlang::expr(!is.na(!!var_) & !!expr)
    }
  } else if (get_input_type(x) == "picker") {
    # similar to factor/character, logical values are treated as categorical
    # ("TRUE"/"FALSE") in pickerInput. however, logical values (TRUE/FALSE) in
    # original and filtered datasets are not changed. thus, filter expression
    # for logical values should be treated differently
    val2 <- val
    if (get_first_class(x) == "logical") {
      val2 <- as.logical(val)
    }
    val2 <- ifelse(val2 == "<MISSING>", NA, val2)
    expr <- rlang::expr(!!var_ %in% !!val2)
  } else {
    expr <- rlang::expr(TRUE)
  }
  expr
}
