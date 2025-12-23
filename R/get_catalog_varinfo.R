#' Slim Varinfo for Export for Survey Question Catalog Display
#'
#' Filters the varinfo to only include variables marked for catalog display
#' and selects a specific subset of relevant columns.
#'
#' @param df The varinfo data frame.
#' @param admin_ids Optional vector of admin IDs (e.g., c("2024", "2025")).
#' Defaults to NULL. If provided, only selects text columns for these specific
#' administrations (years). If NULL, selects ALL year-specific text columns
#' found in the data (columns starting with "QualtricsVariableText.")
#' @return A filtered and column-trimmed tibble.
#' @importFrom dplyr filter select any_of
#' @family varinfo prep functions
#' @export
get_catalog_varinfo <- function(df, admin_ids = NULL) {
  core_cols <- c(
    "ITEM_NAME", "ITEM_SECTION", "ITEM_STEM", "ITEM_MEMBER",
    "MostRecentSurveyAdmin", "AllSurveyAdmin", "SCALE_OPTIONS",
    "DASH_DISPLAY", "label",  "ITEM_TYPE", "ITEM_PRESENTATION_TYPE"
  )

  # Handle Dynamic Text Columns
  if (!is.null(admin_ids)) {
    # Option A: User provided specific IDs, construct the names
    # Matches "QualtricsVariableText.2024", "QualtricsVariableText.2025", etc.
    dynamic_text_cols <- paste0("QualtricsVariableText.", admin_ids)
  } else {
    # Option B: Dynamically find ANY column starting with the pattern
    # The regex matches "QualtricsVariableText." followed by anything
    dynamic_text_cols <- grep("^QualtricsVariableText\\.", names(df), value = TRUE)
  }

  df |>
    # Only include variables flagged for catalog display
    dplyr::filter(as.logical(CATALOG_DISPLAY)) |>
    # Select core columns and the identified dynamic columns
    dplyr::select(
      dplyr::any_of(core_cols),
      dplyr::any_of(dynamic_text_cols)
    )
}
