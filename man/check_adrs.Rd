% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkers.R
\name{check_adrs}
\alias{check_adrs}
\title{ADRS dataset checker}
\usage{
check_adrs(adam)
}
\arguments{
\item{adam}{A dataframe that follows the CDISC Analysis Data Model (ADaM)
standards.}
}
\value{
A single \code{logical}, \code{TRUE} if the check passes and \code{FALSE} if not, with attributes:
\itemize{
\item \code{msg}: \code{character} vector of reasons for check failure
\item \code{data}: \code{adam}
}
}
\description{
ADRS dataset checker
}
\examples{
dplyr::tibble(
  STUDYID = character("1"),
  USUBJID = character("1")
) |>
  check_adrs()

}
