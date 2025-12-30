#' Slim Varinfo for Export for Survey Question Catalog Display
#'
#' Filters the varinfo to only include variables marked for catalog display
#' and selects a specific subset of relevant columns.
#'
#' @param df The varinfo data frame.
#' @param text_prefix_pattern Character string to search for in column names
#' to select the text columns to keep. Defaults to "Text".
#' @param admin_ids Optional vector of admin IDs (e.g., c("2024", "2025")).
#' Defaults to NULL. If provided, only selects text columns containing
#' one of the provided admin IDs. If NULL, selects all text columns.
#' @return A filtered and column-trimmed tibble.
#' @importFrom dplyr filter select any_of
#' @family varinfo prep functions
#' @export
get_catalog_varinfo <- function(df, text_prefix_pattern = "Text",
                                admin_ids = NULL) {
  core_cols <- c(
    "ITEM_NAME", "ITEM_SECTION", "ITEM_STEM", "ITEM_MEMBER",
    "MostRecentSurveyAdmin", "AllSurveyAdmin", "SCALE_OPTIONS",
    "DASH_DISPLAY", "label",  "ITEM_TYPE", "ITEM_PRESENTATION_TYPE"
  )

  # Handle Dynamic Text Columns
  if (!is.null(admin_ids)) {
    # Matches columns containing text prefix and ending with one of the admin_ids
    id_pattern <- paste0("(", paste(admin_ids, collapse = "|"), ")$")
    dynamic_text_cols <- names(df)[grepl(text_prefix_pattern, names(df)) & grepl(id_pattern, names(df))]
  } else {
    # Matches columns containing text prefix
    dynamic_text_cols <- grep(text_prefix_pattern, names(df), value = TRUE)
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
