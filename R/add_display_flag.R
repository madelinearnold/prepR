#' Generic Flagging Function for Variable Display
#'
#' A flexible function to create boolean flag columns (e.g., DASH_DISPLAY,
#' CATALOG_DISPLAY) based on variable item types, administration years, and
#' presentation types.
#'
#' @param df The varinfo data frame.
#' @param flag_col String. The name of the column to create or update.
#' @param admin_to_include Optional vector of admin IDs to include
#'  (e.g., c("2024", "2025")). If provided, variables must appear in at least
#' one of these admins (admin_id present in \code{AllSurveyAdmin}) to be flagged
#' as TRUE.
#' @param exclude_types Character vector of \code{ITEM_TYPE} values to flag as FALSE.
#' @param exclude_empty_presentation Logical. If TRUE, flags rows with empty or
#'   NA \code{ITEM_PRESENTATION_TYPE} as FALSE.
#' @param overwrite Logical. If TRUE, ignores existing values in \code{flag_col}.
#' @return A data frame with the updated flag column.
#' @family varinfo prep functions
#' @importFrom dplyr mutate case_when sym
#' @importFrom stringr str_detect
#' @export
#' @examples
#' # 1. Create a sample varinfo table
#' varinfo_df <- data.frame(
#'   ITEM_NAME = c("AGE", "GENDER", "COMMENT", "DEPT_CODE", "MANUAL_HIDE"),
#'   ITEM_TYPE = c("Question", "Question", "Open Text", "administrative", "Question"),
#'   ITEM_PRESENTATION_TYPE = c("Bar", "Pie", NA, NA, "Bar"),
#'   AllSurveyAdmin = c("2024, 2025", "2025", "2025", "2025", "2025"),
#'   DASH_DISPLAY = c(NA, NA, NA, NA, FALSE), # Manual override already exists
#'   stringsAsFactors = FALSE
#' )
#'
#' # 2. Strict Dashboard Flagging:
#' # Exclude administrative types, Open Text, and items without a chart type.
#' # Do NOT overwrite the manual 'FALSE' for MANUAL_HIDE.
#' dash_df <- add_display_flag(
#'   varinfo_df,
#'   flag_col = "DASH_DISPLAY",
#'   admin_to_include = c("2024", "2025"),
#'   exclude_types = c("administrative", "Open Text", "metadata"),
#'   exclude_empty_presentation = TRUE,
#'   overwrite = FALSE
#' )
#'
#' # 3. Broad Catalog Flagging:
#' # Include Open Text, but exclude administrative.
#' # Ignore presentation type (NAs are OK).
#' catalog_df <- add_display_flag(
#'   varinfo_df,
#'   flag_col = "CATALOG_DISPLAY",
#'   admin_to_include = c("2025"),
#'   exclude_types = c("administrative", "metadata"),
#'   exclude_empty_presentation = FALSE,
#'   overwrite = TRUE
#' )
add_display_flag <- function(df,
                             flag_col,
                             admin_to_include = NULL,
                             exclude_types = c("metadata",
                                               "administrative",
                                               "auth data",
                                               "embedded data",
                                               "embedded data from panel",
                                               "module"),
                             exclude_empty_presentation = FALSE,
                             overwrite = FALSE) {

  # Ensure target column exists
  if (!flag_col %in% names(df)) {
    df[[flag_col]] <- NA
  } else {
    df[[flag_col]] <- as.logical(df[[flag_col]])
  }

  has_admin_filter <- !is.null(admin_to_include) && length(admin_to_include) > 0
  if (has_admin_filter) {
    admin_pattern <- paste(admin_to_include, collapse = "|")
  }

  df |>
    dplyr::mutate(!!sym(flag_col) := dplyr::case_when(
      # 1. Preservation
      !overwrite & !is.na(!!sym(flag_col)) ~ !!sym(flag_col),

      # 2. Item Type Exclusions
      ITEM_TYPE %in% exclude_types ~ FALSE,

      # 3. Presentation Type Exclusions
      exclude_empty_presentation &
        (is.na(ITEM_PRESENTATION_TYPE) | ITEM_PRESENTATION_TYPE %in% c("", "NA")) ~ FALSE,

      # 4. Admin Filter
      has_admin_filter & !stringr::str_detect(AllSurveyAdmin, admin_pattern) ~ FALSE,

      # 5. Default
      .default = TRUE
    ))
}
