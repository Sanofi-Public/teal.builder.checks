% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkers.R
\name{check_has_values}
\alias{check_has_values}
\title{Check if a column contains specific values}
\usage{
check_has_values(column_name, values)
}
\arguments{
\item{column_name}{The column name to check values of.}

\item{values}{A vector of values expected to be present in the specified
column.}
}
\value{
Checker function that takes an ADaM dataset as argument and returns a \code{logical} check result.
See \code{?check_adam} for example.
}
\description{
Check if a column contains specific values
}
\examples{
dplyr::tibble(col_a = factor(letters)) |>
  check_has_values(column_name = "col_a", values = "a")()

}
