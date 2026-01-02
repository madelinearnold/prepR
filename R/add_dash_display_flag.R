#' Flag Variables for Dashboard Display
#'
#' Applies logic to determine if a variable should be displayed
#' in the dashboard. It excludes metadata, administrative types, open text
#' questions, variables with empty `ITEM_PRESENTATION_TYPE`, and questions that
#' were not asked in the current administration. Uses `MostRecentSurveyAdmin`
#' column from \code{update_survey_admin_summaries}.
#'
#' @param df The varinfo data frame.
#' @param admin_to_include Optional vector of admin IDs to include
#' (e.g., c( "2024", "2025")). If provided, variables must appear in at least
#' one of these admins to be flagged as TRUE.
#' @param overwrite Logical. If TRUE, resets all flags based on current logic.
#'   If FALSE (default), preserves existing TRUE/FALSE flags for existing items.
#' @return A data frame with a boolean 'DASH_DISPLAY' column.
#' @family varinfo prep functions
#' @importFrom dplyr mutate case_when
#' @export
add_dash_display_flag <- function(df, admin_to_include = NULL, overwrite = FALSE) {
  # If missing, create it. If it exists, force it to logical.
  if (!"DASH_DISPLAY" %in% names(df)) {
    df$DASH_DISPLAY <- NA
  } else {
    df$DASH_DISPLAY <- as.logical(df$DASH_DISPLAY)
  }

  # Create a regex pattern to search for admin ids, like "2024|2025"
  has_admin_filter <- !is.null(admin_to_include) && length(admin_to_include) > 0
  if (has_admin_filter) {
    admin_pattern <- paste(admin_to_include, collapse = "|")
  }

  df |>
    dplyr::mutate(DASH_DISPLAY = dplyr::case_when(
      # 1. PRESERVATION (Priority #1)
      # If we are NOT overwriting AND a value already exists, stop here and keep it.
      !overwrite & !is.na(DASH_DISPLAY) ~ DASH_DISPLAY,

      # 2. HARD RULES (Priority #2)
      # Only reached if overwrite = TRUE OR if the value was NA.
      # Exclude administrative & text variables
      ITEM_TYPE %in% c("metadata", "administrative", "auth data", "embedded data", "embedded data from panel",
                       "Open Text", "module") ~ FALSE,
      # Exclude variables with empty/NA presentation types
      is.na(ITEM_PRESENTATION_TYPE) | ITEM_PRESENTATION_TYPE %in% c("", "NA") ~ FALSE,
      # 3. Admin filter
      # If a filter is provided, check if AllSurveyAdmin contains ANY of the IDs.
      has_admin_filter & !stringr::str_detect(AllSurveyAdmin, admin_pattern) ~ FALSE,

      # 3. DEFAULT: Otherwise TRUE
      .default = TRUE
    ))
}
