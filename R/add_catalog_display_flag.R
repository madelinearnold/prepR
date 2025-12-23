#' Flag Variables for Survey Question Catalog Display
#'
#' Applies logic to determine if a variable should be displayed
#' in the survey question catalog. It excludes metadata and administrative types,
#' with the option to exclude questions that were not asked after a given year.
#' Uses `AllSurveyAdmin` column from \code{update_survey_admin_summaries}.
#'
#' @param df The varinfo data frame.
#' @param admin_to_include Optional vector of admin IDs to include
#' (e.g., c( "2024", "2025")). If provided, variables must appear in at least
#' one of these admins to be flagged as TRUE.
#' @param overwrite Logical. If TRUE, resets all flags. If FALSE (default),
#' preserves existing TRUE/FALSE flags for existing items.
#' @return A data frame with a boolean 'CATALOG_DISPLAY' column.
#' @family varinfo prep functions
#' @importFrom dplyr mutate case_when
#' @export
#' @examples
#' # 1. Create a sample varinfo table
#' varinfo_df <- data.frame(
#'   ITEM_NAME = c("AGE", "GENDER", "ADMIN_NOTE", "OLD_Q", "MANUAL_KEEP"),
#'   ITEM_TYPE = c("Question", "Question", "administrative", "Question", "Question"),
#'   AllSurveyAdmin = c("2024, 2025", "2025", "2025", "2021, 2022", "2021"),
#'   CATALOG_DISPLAY = c(NA, NA, NA, NA, TRUE), # MANUAL_KEEP already has a flag
#'   stringsAsFactors = FALSE
#' )
#'
#' # 2. Run with a filter for 2024 and 2025
#' # This should keep AGE and GENDER, but drop OLD_Q (since 2021/2022 aren't in the list)
#' # It should also drop ADMIN_NOTE (hard rule) and keep MANUAL_KEEP (preservation)
#' catalog_df <- add_catalog_display_flag(
#'   varinfo_df,
#'   admin_to_include = c("2024", "2025"),
#'   overwrite = FALSE
#' )
#'
#' print(catalog_df)
add_catalog_display_flag <- function(df, admin_to_include = NULL, overwrite = FALSE) {
  # If missing, create it. If it exists, force it to logical.
  if (!"CATALOG_DISPLAY" %in% names(df)) {
    df$CATALOG_DISPLAY <- NA
  } else {
    df$CATALOG_DISPLAY <- as.logical(df$CATALOG_DISPLAY)
  }

  # Create a regex pattern to search for admin ids, like "2024|2025"
  has_admin_filter <- !is.null(admin_to_include) && length(admin_to_include) > 0
  if (has_admin_filter) {
    admin_pattern <- paste(admin_to_include, collapse = "|")
  }

  df |>
    dplyr::mutate(CATALOG_DISPLAY = dplyr::case_when(
      # 1. Preservation
      # If we are NOT overwriting AND a value already exists, stop here and keep it.
      !overwrite & !is.na(CATALOG_DISPLAY) ~ CATALOG_DISPLAY,

      # 2. Exclude metadata
      # Only reached if overwrite = TRUE OR if the value was NA.
      ITEM_TYPE %in% c("metadata", "administrative") ~ FALSE,

      # 3. Admin filter
      # If a filter is provided, check if AllSurveyAdmin contains ANY of the IDs.
      has_admin_filter & !stringr::str_detect(AllSurveyAdmin, admin_pattern) ~ FALSE,

      # 4. Default
      .default = TRUE
    ))
}
