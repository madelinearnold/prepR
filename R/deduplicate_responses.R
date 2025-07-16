#' Deduplicate Responses by ID and Progress
#'
#' This function takes a data frame and deduplicates rows based on a specified
#' ID column. For any duplicated IDs, it keeps only one unique row,
#' prioritizing the row with the highest value in a specified progress column.
#' Rows with missing IDs are excluded from the output.
#'
#' @param data A data frame to be deduplicated.
#' @param id_col A character string specifying the name of the ID column.
#'   Defaults to "UID".
#' @param progress_col A character string specifying the name of the column
#'   used to determine which row to keep in case of duplicates. The row with the
#'   highest value in this column will be retained. Defaults to "Progress".
#'   This column is treated as numeric for comparison.
#'
#' @return A deduplicated data frame.
#' @family data manipulation functions
#' @export
#' @examples
#' # Attach required libraries
#' library(dplyr, rlang)
#'
#' # Create a sample data frame
#' df <- data.frame(
#'   UID = c("123", "124", "124", "125", "126", NA, "128"),
#'   Progress = c(100, 90, 15, 85, 100, 10, 95),
#'   Question1 = c("X", "Y", "Z", "W", "P", "Q", "R")
#' )
#'
#' # Deduplicate using default columns
#' deduplicated_df <- deduplicate_responses(df)
#' print(deduplicated_df)
#'
#' # Deduplicate using custom column names
#' df2 <- data.frame(
#'   StudentID = c(101, 102, 101, 103, 102),
#'   Answered_Count = c(7, 11, 9, 6, 10),
#'   Question1 = c("a", "b", "c", "d", "e")
#' )
#' deduplicated_df2 <- deduplicate_responses(df2,
#'   id_col = "StudentID",
#'   progress_col = "Answered_Count"
#' )
#' print(deduplicated_df2)
#'
deduplicate_responses <- function(data, id_col = "UID", progress_col = "Progress") {

  # number rows missing ID
  n_id_na <- sum(is.na(data[[id_col]]))

  # Find duplicated data (more than one response per ID)
  duplicated_data <- data |>
    count(.data[[id_col]]) |>
    filter(!is.na(.data[[id_col]])) |>
    filter(n > 1)

  dup_ids <- duplicated_data[[id_col]]
  n_dup_ids <- nrow(duplicated_data)
  n_dup_rows <- data |> filter(.data[[id_col]] %in% dup_ids)

  # For duplicated UIDs, keep only the row with the highest progress (most data)
  deduped_rows <- data |>
    filter(.data[[id_col]] %in% dup_ids) |>
    arrange(desc(as.numeric(.data[[progress_col]]))) |>
    distinct(.data[[id_col]], .keep_all = TRUE)

  # Combine deduplicated with unique rows
  data_deduped <- data |>
    filter(!.data[[id_col]] %in% dup_ids) |>
    bind_rows(deduped_rows) |>
    filter(!is.na(.data[[id_col]]))

  # Summary messages
  message("✅ Deduplication complete.")
  message("• Duplicate IDs found: ", n_dup_ids)
  message("• Rows with missing ", id_col, ": ", n_id_na)
  message("• Rows dropped: ", nrow(data) - nrow(data_deduped))

  return(data_deduped)
}
