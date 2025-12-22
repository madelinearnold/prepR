#' Coalescing Patch
#'
#' Selectively updates a main table by overwriting it with patch data, preserving
#' existing values where the patch is blank. To force delete a value from the
#' main table, a sentinel value (e.g. "DELETE") in the patch data allows for
#' intentional insertion of NAs. Automatically synchronizes column schemas and
#' adds new variables, ensuring a non-destructive update workflow.
#'
#' @param main The main varinfo data frame.
#' @param patches The patch data frame.
#' @param by The key column (e.g., "ITEM_NAME").
#' @param sentinel String used to force an NA (default "DELETE").
#' @export
patch_varinfo <- function(main, patches, by = "ITEM_NAME", sentinel = "DELETE") {

  # 1. Ensure main has all columns found in patches
  main <- sync_patch_schema(main, patches)

  # 2. Join everything side-by-side
  # .m = main, .p = patch
  combined <- main |>
    dplyr::full_join(patches, by = by, suffix = c(".m", ".p"))

  # 3. The Coalesce Loop
  # Pick patch value first. If patch is NA (empty cell), keep main.
  cols_to_patch <- setdiff(names(patches), by)

  for (col in cols_to_patch) {
    m_col <- paste0(col, ".m")
    p_col <- paste0(col, ".p")

    combined[[col]] <- dplyr::coalesce(combined[[p_col]], combined[[m_col]])
  }

  # 4. Clean up: Select main columns and recode any sentinel values to NA
  combined |>
    dplyr::select(dplyr::all_of(names(main))) |>
    recode_missing(additional_values = sentinel)
}
