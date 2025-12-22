#' Patch a Table
#'
#' Selectively updates a data frame (x) with values from a patch data frame (y).
#' If a cell in the patch is NA, the original value in x is preserved.
#' A sentinel value can be used to force an NA.
#'
#' @param x The data frame to be updated.
#' @param y The data frame containing the patches.
#' @param by A character vector of variables to join by (e.g. "ITEM_NAME" for
#' varinfo tables)
#' @param sentinel String used to force an NA (default "DELETE").
#' @return A data frame with patched values and synchronized columns.
#' @export
#' @examples
#' # 1. Setup a varinfo table
#' main_df <- data.frame(
#'   ITEM_NAME = c("AGE", "GENDER", "OLD_VAR"),
#'   label = c("Respondent Age", "Gender", "To be deleted"),
#'   DASH_DISPLAY = c("TRUE", "FALSE", "TRUE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 2. Setup a "patch" table
#' # - GENDER: We want to update DASH_DISPLAY, but leave the label alone (NA)
#' # - OLD_VAR: We want to force the label to be NA using the sentinel
#' # - NEW_VAR: This is a brand new variable not in main_df
#' patch_df <- data.frame(
#'   ITEM_NAME = c("GENDER", "OLD_VAR", "NEW_VAR"),
#'   label = c(NA, "DELETE", "Brand New Variable"),
#'   DASH_DISPLAY = c("TRUE", NA, "FALSE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 3. Run the patch
#' updated_df <- patch_table(main_df, patch_df, by = "ITEM_NAME", sentinel = "DELETE")
#'
#' # View results
#' print(updated_df)
patch_table <- function(x, y, by, sentinel = "DELETE") {

  # 1. Sync Schema: Ensure x has all columns found in y
  x <- sync_column_schema(x, y)

  # 2. Join side-by-side
  combined <- x |>
    dplyr::full_join(y, by = by, suffix = c(".orig", ".patch"))

  # 3. Coalesce Loop
  patch_cols <- setdiff(names(y), by)

  for (col in patch_cols) {
    orig_col  <- paste0(col, ".orig")
    patch_col <- paste0(col, ".patch")

    combined[[col]] <- dplyr::coalesce(combined[[patch_col]], combined[[orig_col]])
  }

  # 4. Clean up
  combined |>
    dplyr::select(dplyr::all_of(names(x))) |>
    recode_missing(additional_values = sentinel)
}
