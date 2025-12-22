#' Synchronize Column Schema
#'
#' Ensures that a target data frame (x) contains all columns present in a
#' source data frame (y), matching both names and data types.
#' Missing columns are initialized with NA.
#'
#' @param x The data frame to be updated.
#' @param y The data frame to use as a template.
#' @family varinfo prep functions
#' @export
sync_column_schema <- function(x, y) {
  # Find columns in y that don't exist in x
  missing_cols <- setdiff(names(y), names(x))

  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      # Initialize with NA and match the storage mode (type) of the source column
      # This ensures logical stays logical, character stays character, etc.
      x[[col]] <- NA
      storage.mode(x[[col]]) <- storage.mode(y[[col]])
    }
  }
  return(x)
}
