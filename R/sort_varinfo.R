#' Sort Varinfo by Recency and Survey Order
#'
#' Organizes the varinfo table so that metadata is at the top, followed by
#' questions sorted by their most recent administration year and their
#' original order in the Qualtrics survey file (for the most recent admin).
#'
#' @param updated_varinfo The varinfo table containing admin summary columns
#' from \code{update_admin_summaries}.
#' @param new_info The current survey metadata from \code{load_survey_column_info}.
#' @param recency_order Vector of admin IDs in order, most recent first
#' (e.g. c("2025", "2024")).
#' @return A sorted varinfo tibble.
#' @importFrom dplyr mutate select arrange relocate last_col match coalesce
#' @family varinfo prep functions
#' @export
#' @examples
#' varinfo <- tibble::tibble(
#'   ITEM_NAME = c("AGE", "CONSENT"),
#'   ITEM_TYPE = c("question", "metadata"),
#'   MostRecentSurveyAdmin = c("2025", "2025"),
#'   QualtricsVariableName.2025 = c("Q2", "Q1")
#' )
#' current_meta <- tibble::tibble(QualtricsVariableName.2025 = c("Q1", "Q2"),
#' QuestionText.2025 = c("question one", "question two"))
#'
#' sort_varinfo(joined_varinfo, current_meta,  c("2025", "2024"))
sort_varinfo <- function(varinfo, new_info, recency_order) {

  current_qualtrics_col <- names(new_info)[1]

  recency_rank <- setNames(seq_along(recency_order), recency_order)

  varinfo |>
    mutate(
      # 1. recency priority: metadata is always -1 (top),
      # then rank based on MostRecentSurveyAdmin
      priority = if_else(
        ITEM_TYPE %in% c("administrative", "metadata"), -1L,
        recency_rank[MostRecentSurveyAdmin]),
      # 2. survey index: where did it appear in the current Qualtrics file?
      # variables from older years not in this file will get NA
      survey_index = match(.data[[current_qualtrics_col]],
                           new_info[[current_qualtrics_col]])
    ) |>
    # Sort by priority (year) -> survey order (within current year) -> ITEM_NAME
    arrange(priority, survey_index, ITEM_NAME) |>
    select(-priority, -survey_index) |>
    relocate(AllSurveyAdmin, MostRecentSurveyAdmin, .after = last_col())
}
