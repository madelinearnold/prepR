#' Update Survey Administration Summary Columns
#'
#' Looks for columns in the varinfo with the pattern QualtricsVariableName.XXX
#' where XXX is the survey administration year, semester, or other date id
#' (columns can also be supplied as a vector in recency order). If a given
#' variable (row in the varinfo) is not NA for a column, we assume that question
#' was asked in that survey administration. Generates two new columns:
#' 1) `AllSurveyAdmin` with a comma separated list of admin IDs and
#' 2) `MostRecentSurveyAdmin` with only the most recent admin ID.
#'
#' @param df The varinfo data frame.
#' @param recency_order Optional vector of admin IDs in order (most recent first).
#' @importFrom dplyr rowwise mutate c_across all_of ungroup select
#' @export
#' @examples
#' test_df <- tibble::tibble(
#'   ITEM_NAME = c("Q1", "Q2"),
#'   QualtricsVariableName.2024 = c("Q1", NA),
#'   QualtricsVariableName.2025 = c("Q1", "Q2")
#' )
#'
#' update_survey_admin_summaries(test_df, recency_order = c("2025", "2024"))
update_survey_admin_summaries <- function(df, recency_order = NULL) {

  marker_cols <- names(df)[grepl("^QualtricsVariableName\\.", names(df))]
  admin_ids   <- sub("^QualtricsVariableName\\.", "", marker_cols)

  # If no order provided, we assume the IDs are numeric years and sort descending
  if (is.null(recency_order)) {
    recency_order <- admin_ids[order(as.numeric(gsub("\\D", "", admin_ids)), decreasing = TRUE)]
  }

  df_out <-
    df |>
    rowwise() |>
    mutate(
      # Find which admins have this question
      present_ids = list(admin_ids[!is.na(c_across(all_of(marker_cols)))]),

      # All administrations (sorted according to the provided recency_order)
      AllSurveyAdmin = paste(intersect(recency_order, unlist(present_ids)), collapse = ", "),

      # Most recent administration (the one that appears first in the recency_order)
      MostRecentSurveyAdmin = {
        matches <- intersect(recency_order, unlist(present_ids))
        if (length(matches) > 0) matches[1] else NA_character_
      }
    ) |>
    ungroup() |>
    select(-present_ids)

  return(df_out)
}
