% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_filter_ui.R
\name{create_filter_ui}
\alias{create_filter_ui}
\title{Create filter UI for a single filter}
\usage{
create_filter_ui(x, id, var, val)
}
\arguments{
\item{x}{a numeric/character vector}

\item{id}{namespaced variable id}

\item{var}{name of a filter variable}

\item{val}{values from input controls (range slider / drop-down menu)}
}
\value{
UI element for a numeric or categorical filter
}
\description{
Create filter UI for a single filter
}
