#' Identify Groups with Low Respondent Counts
#'
#' From respondent-level data, this function counts respondents per unique
#' combination of the supplied `group_cols` and returns a data frame of the
#' combinations that fall below a given threshold.
#'
#' @param data A data frame of respondent-level data.
#' @param group_cols A character vector of one or more column names to group by.
#' @param threshold An integer for the respondent count threshold.
#'
#' @return A data frame containing the grouping columns and a new
#'   `n_respondents` column, containing only the combinations that meet the
#'   low-N criteria.
#' @export
#' @examples
#' library(dplyr)
#'
#' respondent_data <- data.frame(
#'   division = c("Humanities", "Humanities", "Engineering", "Engineering", "Engineering"),
#'   ethnicity = c("Asian", "Latinx", "Asian", "Latinx", "Latinx")
#' )
#'
#' # Identify low-N groups when grouping by both division and ethnicity
#' identify_lowN_groups(respondent_data, c("division", "ethnicity"), 3)

identify_lowN_groups <- function(data, group_cols, threshold) {
  data |>
    group_by(across(all_of(group_cols))) |>
    summarize(n_respondents = n(), .groups = "drop") |>
    filter(n_respondents < threshold) |>
    arrange(desc(n_respondents))
}
