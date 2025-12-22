#' Sync Schema for Upserting
#'
#' Ensures that the target data frame has all columns present in the patch data frame.
#' Missing columns are initialized with NA.
#'
#' @param data The target data frame (e.g., joined_draft).
#' @param patches The data frame containing updates (e.g., patches).
#' @export
sync_patch_schema <- function(data, patches) {
  missing_cols <- setdiff(names(patches), names(data))

  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      # Initialize with the same type as found in patches to avoid type-mismatch errors
      data[[col]] <- NA
      storage.mode(data[[col]]) <- storage.mode(patches[[col]])
    }
  }
  return(data)
}
