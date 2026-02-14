#' Add Proxy Population Count
#'
#' Calculates the "Proxy Population" for groupings in a summary table. This is
#' determined by finding the maximum total respondents observed across all
#' questions for a unique combination of grouping dimensions.
#'
#' @param summary_df A data frame of aggregated survey results.
#' @param group_dims A character vector of column names that define a unique
#'   population (e.g., `c("acad_group_value", "time_group_value")`).
#'
#' @return The original data frame with an added numeric column `GROUP_PROXY_N`.
#' @export
add_proxy_population <- function(summary_df, group_dims) {

  proxy_map <- summary_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_dims))) %>%
    dplyr::summarise(GROUP_PROXY_N = max(total, na.rm = TRUE), .groups = "drop")

  summary_df %>%
    dplyr::left_join(proxy_map, by = group_dims)
}
