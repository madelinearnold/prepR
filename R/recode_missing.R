#' Recode specified values to NA
#'
#' This function finds and replaces common "missing" text values (like `"-99"`,
#' `"NA"`, `""`) with `NA` in all character columns of a data frame.
#'
#' @param data A data frame or tibble.
#' @param additional_values A character vector of other values to be replaced
#'   with `NA`.
#'
#' @return A data frame with specified values in character columns recoded to `NA`.
#'
#' @export
#'
#' @examples
#' # attach required libraries
#' library(dplyr)
#' library(tidyselect)
#'
#' test_df <- data.frame(
#'   q1 = c("A", "B", "N/A", "C"),
#'   q2 = c("Agree", "Disagree", "-99", "Agree"),
#'   q3 = c("Yes", "No", "REFUSED", ""),
#'   id = 1:4
#' )
#'
#' # Recode using defaults and add "REFUSED" as a missing value
#' recode_missing(test_df, additional_values = "REFUSED")
#'
recode_missing <- function(data, additional_values = NULL) {
  # Default values that often represent missing data
  default_na_values <- c("-99", "", "NA", "N/A")

  # Combine default and user-supplied values
  all_na_values <- c(default_na_values, additional_values)

  data |>
    mutate(
      across(
        where(is.character),
        ~ replace(.x, .x %in% all_na_values, NA_character_)
      )
    )
}
