% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_missing.R
\name{recode_missing}
\alias{recode_missing}
\title{Recode specified values to NA}
\usage{
recode_missing(data, additional_values = NULL)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{additional_values}{A character vector of other values to be replaced
with \code{NA}.}
}
\value{
A data frame with specified values in character columns recoded to \code{NA}.
}
\description{
This function finds and replaces common "missing" text values (like \code{"-99"},
\code{"NA"}, \code{""}) with \code{NA} in all character columns of a data frame.
}
\examples{
# attach required libraries
library(dplyr)
library(tidyselect)

test_df <- data.frame(
  q1 = c("A", "B", "N/A", "C"),
  q2 = c("Agree", "Disagree", "-99", "Agree"),
  q3 = c("Yes", "No", "REFUSED", ""),
  id = 1:4
)

# Recode using defaults and add "REFUSED" as a missing value
recode_missing(test_df, additional_values = "REFUSED")

}
