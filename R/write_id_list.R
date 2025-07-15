#' Write a List of Distinct IDs to a CSV File
#'
#' This function takes input data, extracts a list of distinct IDs from
#' a given ID column, and writes this list to a CSV file. This ID list can then
#' be used as input to filters in external systems like Cal Answers to pull
#' corresponding demographic or academic data. Optionally, this function can
#' filter the data by a specified filter column and filter value before
#' extracting the IDs, and append the filter value as a file name suffix.
#'
#' @param data A data frame expected to have the specified `id_col`. Also
#' expected to have `filter_col` if provided.
#' @param id_col A character string specifying the name of the column that
#'   contains the unique identifiers (e.g., "UID", "StudentID").
#' @param filter_col An optional character string specifying a column to filter
#'   by. **Must be used with `filter_value`**. Defaults to `NULL`.
#' @param filter_value An optional value to filter for in the `filter_col`.
#'   **Must be used with `filter_col`**. Defaults to `NULL`.
#' @param path_to_directory A character string specifying the directory path
#'   where the CSV file should be saved. Defaults to
#'   "/Volumes/Secure/analysis/graduate-experience/Cumulative data prep/data/ID lists/".
#'
#' @return The function's primary effect is the creation of a CSV file at the
#' specified path. It also returns the ID list tibble.
#' @importFrom dplyr filter distinct
#' @importFrom readr write_csv read_csv
#' @importFrom rlang .data
#' @family data export functions
#' @export
#' @examples
#' # Load required packages for example
#' library(readr)
#'
#' # Create a sample data frame
#' df_ids <- data.frame(
#'   UID = c("A1", "B2", "A1", "C3", "D4", "B2"),
#'   SurveyAdminYear = c(2023, 2023, 2024, 2023, 2024, 2024),
#'   Response = c("Yes", "No", "Yes", "Maybe", "No", "Yes")
#' )
#' temp_dir <- tempdir() # Create a temporary directory for output files
#'
#' # --- Example 1: Filter by a specific value ---
#' # This will create 'UID_list_2024.csv' by filtering SurveyAdminYear for 2024.
#' list_2024 <- write_id_list(
#'   data = df_ids,
#'   id_col = "UID",
#'   filter_col = "SurveyAdminYear",
#'   filter_value = 2024,
#'   path_to_directory = temp_dir
#' )
#'
#' # Verify the returned tibble (should contain distinct UIDs from 2024)
#' print(list_2024)
#'
#' # --- Example 2: No filtering (get all distinct IDs) ---
#' # filter_col and filter_value are omitted, so no filtering is applied.
#' # This will create 'UID_list.csv'.
#' list_all <- write_id_list(
#'   data = df_ids,
#'   id_col = "UID",
#'   path_to_directory = temp_dir
#' )
#'
#' # Verify the returned tibble (should contain all distinct UIDs)
#' print(list_all)
#'
#' # Clean up the temporary files
#' file.remove(file.path(temp_dir, "UID_list_2024.csv"))
#' file.remove(file.path(temp_dir, "UID_list.csv"))
#'
write_id_list <- function(data, id_col, filter_col = NULL, filter_value = NULL, path_to_directory = "/Volumes/Secure/analysis/graduate-experience/Cumulative data prep/data/ID lists/") {
  # Start with unfiltered data and no suffix
  filtered_data <- data
  suffix <- ""

  # Conditionally filter by filter_col and filter_value if provided
  if (!is.null(filter_col) && !is.null(filter_value)) {
    # Check if the filter column exists before filtering
    if (!filter_col %in% names(data)) {
      stop(paste0("Column '", filter_col, "' not found in the provided data frame."))
    }
    filtered_data <- filtered_data |>
      filter(.data[[filter_col]] == filter_value)
    suffix <- paste0("_", filter_value)
  }

  # Extract distinct IDs
  id_list <- filtered_data |>
    distinct(.data[[id_col]])

  # Construct the file path
  file_name <- paste0(id_col, "_list", suffix, ".csv")
  path <- file.path(path_to_directory, file_name)

  # Write the list to CSV
  write_csv(id_list, path)
}
