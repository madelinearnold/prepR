#' Expand Survey Data to One Row per Response per Variable
#'
#' Converts a respondent-level wide-format data frame into a long-format
#' data frame, where each row represents a single response to a single survey
#' variable (item). This is helpful for downstream question-level summarization.
#'
#' The function uses `any_of()` when selecting columns to transform and reshape,
#' which makes it tolerant of missing variables in `variables_to_pivot`. This is
#' especially useful when working with survey data and variable lists that may
#' differ slightly in column composition (e.g. by year or intended audience).
#'
#' @param data A data frame or tibble with one row per respondent.
#' @param variables_to_pivot A character vector of column names to reshape into long format.
#'
#' @return A tibble with one row per respondent per variable, including:
#'   - `rowid`: a unique ID per respondent
#'   - `ITEM_NAME`: the name of the original variable
#'   - `response`: the corresponding character response
#'
#' @export
#'
#' @examples
#' # attach required libraries
#' library(dplyr)
#' library(tibble)
#' library(tidyr)
#'
#' sample_data <- tibble::tibble(
#'   q1 = c("Yes", "No", NA),
#'   q2 = c("A", "B", "C"),
#'   q3 = c(1, 2, NA)
#' )
#' pivot_responses_longer(sample_data, variables_to_pivot = c("q1", "q2", "q3"))
#'
pivot_responses_longer <- function(data, variables_to_pivot) {
  data |>
    rowid_to_column() |>
    mutate(across(any_of(variables_to_pivot), as.character)) |>
    pivot_longer(
      cols = any_of(variables_to_pivot),
      names_to = "ITEM_NAME",
      values_to = "response"
    ) |>
    filter(!is.na(response))
}
