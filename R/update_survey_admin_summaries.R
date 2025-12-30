#' Update Survey Administration Summary Columns
#'
#' Identifies columns in the varinfo matching a specific prefix (e.g.,
#' "QualtricsVariableName.") and uses them to determine which survey
#' administrations a variable appeared in.
#'
#' @param df The varinfo data frame.
#' @param admin_prefix A character string representing the prefix used for
#'   administration-specific columns. Include any trailing separator
#'   (like a dot or underscore). Defaults to "QualtricsVariableName.".
#' @param recency_order Optional vector of admin IDs in the desired order
#'   (most recent first). If \code{NULL} (default), the function extracts digits
#'   from the admin suffixes and sorts them in descending numeric order.
#'   Especially helpful when admin IDs contain characters (e.g. Sp25).
#'
#' @details
#' Looks for columns in the varinfo with the pattern `admin_prefix` followed by
#' an admin ID (the survey administration year, semester, or other date id).
#' Admin IDs can also be supplied as a vector in recency order. If a given
#' variable (row in the varinfo) is not NA for a column, we assume that question
#' was asked in that survey administration. The function generates two new
#' columns:
#' 1) `AllSurveyAdmin` A comma-separated string of all admin IDs where
#'   the variable was present, sorted by recency.
#' 2) `MostRecentSurveyAdmin` The single most recent admin ID.
#'
#' @importFrom dplyr rowwise mutate c_across all_of ungroup select
#' @family varinfo prep functions
#' @export
#' @examples
#' test_df <- tibble::tibble(
#'   ITEM_NAME = c("Q1", "Q2"),
#'   QualtricsVariableName.2024 = c("Q1", NA),
#'   QualtricsVariableName.2025 = c("Q1", "Q2")
#' )
#'
#' update_survey_admin_summaries(test_df, recency_order = c("2025", "2024"))
update_survey_admin_summaries <- function(df,
                                          admin_prefix = "QualtricsVariableName.",
                                          recency_order = NULL) {

  # select columns that start with admin_prefix
  admin_cols <- names(df)[startsWith(names(df), admin_prefix)]

  if (length(admin_cols) == 0) {
    warning(paste0("No columns found starting with prefix: ", admin_prefix))
    return(df)
  }

  # Extract IDs by removing the prefix literally
  admin_ids <- substring(admin_cols, nchar(admin_prefix) + 1)

  # If no order provided, extract digits and sort numerically
  if (is.null(recency_order)) {
    admin_vals <- as.numeric(gsub("\\D", "", admin_ids))
    recency_order <- admin_ids[order(admin_vals, decreasing = TRUE)]
  }

  df_out <-
    df |>
    rowwise() |>
    mutate(
      # Find which admin_cols are not NA for this row & map to admin_ids
      present_ids = list(admin_ids[!is.na(c_across(all_of(admin_cols)))]),

      # filter and sort present ids by recency_order
      matches = list(intersect(recency_order, present_ids)),

      # All administrations (sorted according to the provided recency_order)
      # We use an IF check to return NA instead of an empty string ""
      AllSurveyAdmin = if (length(matches) > 0) {
        paste(matches, collapse = ", ")
      } else {
        NA_character_
      },

      # Most recent administration (appears first in the recency_order)
      MostRecentSurveyAdmin = if (length(matches) > 0) {
        matches[1]
      } else {
        NA_character_
      }
    ) |>
    ungroup() |>
    select(-present_ids, -matches)

  return(df_out)
}
