#' Pseudonymize Data and Create an ID Mapping File
#'
#' This function adds a `pseudoID` column to a given data frame, creates a
#' mapping file between the generated `pseudoID` and specified original ID
#' columns, and writes this mapping file to a CSV.
#'
#' @param data A data frame to which the `pseudoID` will be added and from
#'   which the mapping file will be created.
#' @param map_cols A character vector specifying the names of the columns
#'   from the input `data` that should be included in the pseudonymization
#'   mapping file. These columns will be selected along with `pseudoID`.
#' @param path A character string specifying the full path (including file name
#'   and extension, e.g., "my_directory/my_map.csv") where the pseudonymization
#'   mapping CSV file will be written.
#'
#' @return A data frame identical to the input `data` but with an additional
#'   column named `pseudoID`, which contains a unique row number for each
#'   observation.
#' @export
#'
#' @examples
#' # Make sure to load dplyr and readr before running this example:
#' # library(dplyr)
#' # library(readr)
#'
#' # Create a dummy data frame for demonstration
#' dummy_data <- data.frame(
#'   StudentID = c("S001", "S002", "S003"),
#'   CalnetUID = c("U101", "U102", "U103"),
#'   SurveyYear = c(2023, 2023, 2024),
#'   Value = c(10, 20, 30)
#' )
#'
#' # Specify ID columns and a temporary path for the mapping file
#' temp_map_path <- tempfile(fileext = ".csv")
#'
#' # Run the function
#' pseudonymized_data <- pseudonymize_and_write_map(
#'   data = dummy_data,
#'   map_cols = "StudentID",
#'   path = temp_map_path
#' )
#'
#' # View the pseudonymized data
#' print(pseudonymized_data)
#'
#' # Read and view the created mapping file (for verification)
#' if (file.exists(temp_map_path)) {
#'   mapping_file_content <- read.csv(temp_map_path)
#'   print(mapping_file_content)
#'   # Clean up the temporary file
#'   file.remove(temp_map_path)
#' }
#'
#' # Example adding additional ID and non-ID columns to map:
#' # pseudonymized_data <- pseudonymize_and_write_map(
#' #   data = dummy_data,
#' #   map_cols = c("Student Id", "Calnet Uid", "SurveyAdminYear"),
#' #   path = temp_map_path
#' # )
pseudonymize_and_write_map <- function(data, map_cols, path) {
  # Add pseudoID variable based on row number
  pseudonymized <- data |>
    mutate(pseudoID = row_number())

  # Create pseudoID to ID mapping
  # Ensure all requested map_cols exist in the data frame
  if (!all(map_cols %in% names(pseudonymized))) {
    missing_cols <- map_cols[!map_cols %in% names(pseudonymized)]
    stop(paste("The following ID columns were not found in the data frame:",
               paste(missing_cols, collapse = ", ")))
  }

  # Select pseudoID and the specified ID columns for the mapping file
  # Assumes 'dplyr' is loaded for `select` and `all_of`
  pseudoID_map <- pseudonymized |>
    select(pseudoID, all_of(map_cols))

  # Write the mapping file to the specified path
  # Assumes 'readr' is loaded for `write_csv`
  write_csv(pseudoID_map, path)

  # Return the data frame with the new pseudoID column
  return(pseudonymized)
}
