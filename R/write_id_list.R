#' Write a List of Distinct IDs to a CSV File
#'
#' This function takes input response data, filters it by a specified survey
#' administration year, extracts a list of distinct IDs from a given ID column,
#' and writes this list to a CSV file. This ID list can then be used as input
#' to filters in external systems like Cal Answers to pull corresponding
#' demographic or academic data.
#'
#' @param data A data frame containing response data, expected to have a
#'   'SurveyAdminYear' column and the specified `id_col`.
#' @param survey_admin_year A numeric value specifying the survey administration
#'   year to filter the data by.
#' @param id_col A character string specifying the name of the column that
#'   contains the unique identifiers (e.g., "UID", "StudentID").
#' @param path_to_directory A character string specifying the directory path
#'   where the CSV file should be saved. Defaults to
#'   "/Volumes/Secure/analysis/graduate-experience/Cumulative data prep/data/ID lists/".
#'
#' @return Invisibly returns the path to the written CSV file. The function's
#'   primary effect is the creation of a CSV file at the specified path.
#' @importFrom dplyr filter distinct
#' @importFrom readr write_csv
#' @importFrom rlang .data
#' @family data export functions
#' @examples
#' # Create a sample data frame
#' df_ids <- data.frame(
#'   UID = c("A1", "B2", "A1", "C3", "D4", "B2"),
#'   SurveyAdminYear = c(2023, 2023, 2024, 2023, 2024, 2024),
#'   Response = c("Yes", "No", "Yes", "Maybe", "No", "Yes")
#' )
#'
#' # Example: Write a list of UIDs for 2023 to a temporary file
#' # In a real scenario, you would specify a meaningful directory.
#' temp_dir <- tempdir()
#' write_id_list(df_ids,
#'   survey_admin_year = 2023,
#'   id_col = "UID",
#'   path_to_directory = temp_dir
#' )
#'
#' # Verify the file was created (optional)
#' list.files(temp_dir)
#'
#' # Clean up the temporary file (optional)
#' file.remove(file.path(temp_dir, "UID_list_2023.csv"))
#'
write_id_list <- function(data, survey_admin_year, id_col, path_to_directory = "/Volumes/Secure/analysis/graduate-experience/Cumulative data prep/data/ID lists/") {
  list <-
    data |>
    filter(SurveyAdminYear == survey_admin_year) |>
    distinct(.data[[id_col]])

  path <- paste0(path_to_directory, id_col,"_list_", survey_admin_year, ".csv")

  write_csv(list, path)
}
