#' Join Metadata and Filter Valid Survey Responses
#'
#' Joins a long-format response-level data frame with item-level metadata from
#' `varinfo` and response-level metadata from `scale_options`. It then filters out
#' responses marked with `INCLUDE = FALSE` in `scale_options` and removes
#' responses listed in `exclude_responses`, such as `"Not applicable"`
#'
#' @param pivoted_responses A tibble with one row per respondent per variable,
#'   including `ITEM_NAME` and `response` columns (e.g., output from
#'   [pivot_responses_longer()]).
#' @param varinfo A data frame that includes at least `ITEM_NAME` and `SCALE_OPTIONS`.
#' @param scale_options A data frame with one row per valid response option,
#'   including columns `SCALE_OPTIONS_NAME`, `SCALE_OPTIONS_VALIDVALUES`, and
#'   `INCLUDE`.
#' @param exclude_responses A character vector of response values to remove from
#'   the data, even if they are not marked with `INCLUDE = FALSE`. Defaults to
#'   `"Not applicable"`.
#'
#' @return A tibble containing only valid response rows, with `INCLUDE = FALSE`
#'   and excluded responses removed.
#'
#' @export
#'
#' @examples
#' # attach required libraries
#' library(dplyr)
#'
#' # Create a minimal response-level dataset
#' pivoted_responses <- tibble(
#'   rowid = 1:4,
#'   ITEM_NAME = c("q1", "q1", "q2", "q2", "q3"),
#'   response = c("Satisfied", "Did not participate", "Strongly agree", "Prefer not to say", "Not applicable")
#' )
#'
#' # Create varinfo table mapping items to scale option sets
#' varinfo <- tibble(
#'   ITEM_NAME = c("q1", "q2", "q3"),
#'   SCALE_OPTIONS = c("satisfaction1", "agree2", "agree3")
#' )
#'
#' # Create scale options metadata
#' scale_options <- tibble(
#'   SCALE_OPTIONS_NAME = c("satisfaction1", "satisfaction1", "agree2", "agree2", "agree3"),
#'   SCALE_OPTIONS_VALIDVALUES = c("Satisfied", "Did not participate", "Strongly agree", "Prefer not to say", "Not applicable"),
#'   INCLUDE = c(TRUE, FALSE, TRUE, TRUE, TRUE)
#' )
#'
#' scale_filtered <- join_and_filter_valid_responses(
#'    pivoted_responses = pivoted_by_response,
#'    varinfo = varinfo,
#'    scale_options = scale_options)
#'
#' # or exclude additional responses
#' cleaned <- join_and_filter_valid_responses(
#'   pivoted_by_response, varinfo, scale_options,
#'   exclude_responses = c("Not applicable","Prefer not to say"))
#'
join_and_filter_valid_responses <- function(pivoted_responses, varinfo, scale_options) {
  scale_options_table <- varinfo |>
    select(ITEM_NAME, SCALE_OPTIONS) |>
    left_join(
      scale_options |>
        select(
          SCALE_OPTIONS_NAME,
          SCALE_OPTIONS_VALIDVALUES,
          INCLUDE
        ),
      by = c("SCALE_OPTIONS" = "SCALE_OPTIONS_NAME"),
      relationship = "many-to-many"
    ) |>
    rename(response = SCALE_OPTIONS_VALIDVALUES)

  result <-
    pivoted_responses |>
    left_join(scale_options_table, by = c("ITEM_NAME", "response")) |>
    filter(is.na(INCLUDE) | INCLUDE != FALSE) |>
    filter(!response %in% exclude_responses) |>
    select(-INCLUDE, -SCALE_OPTIONS)

    return(result)
}
