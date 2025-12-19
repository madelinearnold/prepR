#' Flag Variables for Dashboard Display
#'
#' Applies business logic to determine if a variable should be displayed
#' in the dashboard. It excludes metadata, administrative types, and questions
#' that were not asked in the current administration.
#'
#' @param df The varinfo data frame.
#' @param current_admin The ID of the current administration (e.g., "2025").
#' @return A data frame with a boolean 'DASH_DISPLAY' column.
#' @family varinfo prep functions
#' @importFrom dplyr mutate case_when
#' @export
add_dash_display_flag <- function(df, current_admin) {
  df |>
    dplyr::mutate(DASH_DISPLAY = dplyr::case_when(
      # Exclude administrative & text variables
      ITEM_TYPE %in% c("metadata", "administrative", "Open Text") ~ FALSE,

      # Exclude variables with empty/NA presentation types
      is.na(ITEM_PRESENTATION_TYPE) | ITEM_PRESENTATION_TYPE %in% c("", "NA") ~ FALSE,

      # Exclude variables not asked in the current administration
      # (Assumes MostRecentSurveyAdmin exists from update_survey_admin_summaries)
      MostRecentSurveyAdmin != current_admin ~ FALSE,

      # Default to TRUE
      .default = TRUE
    ))
}
