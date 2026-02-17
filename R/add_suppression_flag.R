#' Add Suppression Flag Based on Total or Proxy Population Total
#'
#' Applies a suppression flag based on two tiers of sensitivity:
#' strict question-level counts for identifying dimensions, and population-level
#' proxy counts for broader groupings.
#'
#' @param summary_df A data frame that has already been processed by `add_proxy_population`.
#' @param strict_dimensions Column names requiring strict question-level suppression.
#' @param pop_dimensions Column names requiring population-level proxy suppression.
#' @param threshold_strict Minimum N for strict dimensions (default 10).
#' @param threshold_pop Minimum N for population-based dimensions (default 4).
#' @param baseline_strings Values indicating a dimension is inactive (default
#' `c("UC Berkeley","All students", "All years")`, see `summarize_response_counts`).
#'
#' @return The original data frame with an added logical `PRIMARY_SUPPRESSION_FLAG`.
#' @export
add_suppression_flag <- function(summary_df,
                                        strict_dimensions = c("demo_group_value"),
                                        pop_dimensions = c("acad_group_value", "time_group_value"),
                                        threshold_strict = 10,
                                        threshold_pop = 4,
                                        baseline_strings = c("UC Berkeley","All students", "All years")) {

  if(!"GROUP_PROXY_N" %in% names(summary_df)) {
    stop("Column 'GROUP_PROXY_N' not found. Please run add_proxy_population() first.")
  }

  summary_df %>%
    dplyr::mutate(
      is_strict_active = dplyr::if_any(dplyr::all_of(strict_dimensions), ~ !.x %in% baseline_strings),
      is_pop_active    = dplyr::if_any(dplyr::all_of(pop_dimensions),    ~ !.x %in% baseline_strings),

      SUPPRESSION_FLAG = dplyr::case_when(
        is_strict_active ~ total < threshold_strict,
        is_pop_active    ~ GROUP_PROXY_N < threshold_pop,
        # If neither is strictly active (unlikely given your data structure),
        # default to the population threshold for safety.
        TRUE             ~ total < threshold_pop
      )
    ) %>%
    dplyr::select(-is_strict_active, -is_pop_active)
}
