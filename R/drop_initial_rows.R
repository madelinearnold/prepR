#' Drop Initial Rows from a Data Frame
#'
#' This function removes a specified number of initial rows from a data frame.
#' It is particularly useful for cleaning up raw data exports, such as those
#' from Qualtrics, which often include non-data header rows at the beginning.
#'
#' @param data A data frame from which to drop initial rows.
#' @param skip_rows A numeric value indicating the number of rows to skip
#'   from the beginning of the data frame. Defaults to 2.
#'
#' @return A data frame with the specified number of initial rows removed.
#' @importFrom dplyr slice
#' @family data cleaning functions
#' @examples
#' # Create a sample data frame with header rows
#' df_raw <- data.frame(
#'   V1 = c("Header 1", "Header 2", "Data_ID_1", "Data_ID_2", "Data_ID_3"),
#'   V2 = c("Info A", "Info B", "Value_X", "Value_Y", "Value_Z")
#' )
#' print("Original Data frame:")
#' print(df_raw)
#'
#' # Drop the first 2 rows (default behavior)
#' df_cleaned_default <- drop_initial_rows(df_raw)
#' print("Data frame after dropping 2 rows:")
#' print(df_cleaned_default)
#'
#' # Drop a custom number of rows
#' df_raw_custom <- data.frame(
#'   Col1 = c("Line 1", "Line 2", "Line 3", "Actual Data 1", "Actual Data 2"),
#'   Col2 = c("A", "B", "C", "D", "E")
#' )
#' print("Original Data frame (custom header):")
#' print(df_raw_custom)
#' df_cleaned_custom <- drop_initial_rows(df_raw_custom, skip_rows = 3)
#' print("Data frame after dropping 3 rows:")
#' print(df_cleaned_custom)
#'
drop_initial_rows <- function(data, skip_rows = 2) {
  # Drop initial rows
  data_trimmed <- data |>
    slice(-(0:skip_rows))
  return(data_trimmed)
}
