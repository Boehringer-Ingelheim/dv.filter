#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom rlang .data
NULL

is_metareactive <- function(x) {
  inherits(x, "shinymeta_reactive")
}

is_anyreactive <- function(x) {
  inherits(x, "reactive") | inherits(x, "shinymeta_reactive")
}

get_filter_info <- function(x) {
  # x could have have multiple classes, only the first one will be extracted
  first_class <- class(x)[1]
  if (first_class %in% c("numeric", "integer", "Date", "POSIXct", "POSIXlt")) {
    input_type <- "slider"
  } else if (first_class %in% c("ordered", "factor", "character", "logical")) {
    input_type <- "picker"
  } else {
    input_type <- ""
  }
  list(
    first_class = first_class,
    input_type = input_type
  )
}

get_first_class <- function(x) {
  get_filter_info(x)$first_class
}

get_input_type <- function(x) {
  get_filter_info(x)$input_type
}

get_icon_label <- function(data) {
  out <- purrr::map(names(data), function(var) {
    x <- data[[var]]
    name <- switch(
      get_first_class(x),
      numeric = "sort-numeric-up",
      integer = "sort-numeric-up",
      Date = "calendar",
      POSIXct = "calendar",
      POSIXlt = "calendar",
      ordered = "chart-bar",
      factor = "chart-bar",
      character = "keyboard",
      logical = "pause",
      unknown = "question-circle"
    )
    icon <- toString(shiny::icon(name))
    label <- attr(x, "label")
    n_miss <- sum(is.na(x))
    text_code <- paste0("<code style='color:darkblue;'>",
                        get_first_class(x), "</code>")
    text_miss <- ifelse(n_miss == 0, "", paste0("<small style='color:darkred;'>(", n_miss, " missing)</small>"))
    text_label <- ifelse(is.null(label), "", paste0("<br><small><em>", label, "</em></small>"))
    paste0(icon, " ", var, " ", text_code, text_miss, text_label)
  })
  unlist(out)
}


hist_plot <- function(x, y, bins = 30) {
  data <- data.frame(x, y)[!is.na(x), ]
  colors <- c("lightgray", "lightblue")
  values <- if (all(data$y)) colors[2] else colors
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = x, fill = y) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = values)
}


# https://github.com/rstudio/shiny/issues/2111
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/Round.html
# https://stackoverflow.com/questions/47177246/
# floor-and-ceiling-with-2-or-more-significant-digits
signif_floor <- function(x, digits = 6) {
  p <- digits - ceiling(log10(abs(x)))
  y <- floor(x * 10^p) / 10^p
  y[x == 0] <- 0
  y
}


signif_ceiling <- function(x, digits = 6) {
  p <- digits - ceiling(log10(abs(x)))
  y <- ceiling(x * 10^p) / 10^p
  y[x == 0] <- 0
  y
}


range_slider <- function(x, na_rm = TRUE, digits = 6) {
  rng <- range(x, na.rm = na_rm)
  if (get_first_class(x) %in% c("numeric", "double")) {
    rng2 <- signif(rng)
    if (rng2[1] > rng[1]) rng2[1] <- signif_floor(rng[1], digits)
    if (rng2[2] < rng[2]) rng2[2] <- signif_ceiling(rng[2], digits)
  } else if (get_first_class(x) %in% c("POSIXct", "POSIXlt")) {
    rng2 <- c(rng[1] - 1, rng[2] + 1)
  } else {
    rng2 <- rng
  }
  rng2
}
