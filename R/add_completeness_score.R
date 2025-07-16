#' Calculate Completeness Score (Non-NA Count) by Row
#'
#' This function calculates a new column representing the sum of non-missing
#' values across a specified set of columns. This new column can serve as a
#' completeness score for deduplication or other sorting purposes. It's
#' particularly helpful for survey data where completeness can be inferred from
#' the number of answered questions.
#'
#' @param data A dataframe to which the new progress column will be added.
#' @param columns_to_sum A character vector of column names over which to sum
#'   the non-NA values. These columns are typically survey question variables or
#'   other variables indicating completeness.
#' @param new_col_name A character string specifying the name of the new column
#'   to be created, which will store the calculated non-NA count.
#'   Defaults to "completeness_score".
#'
#' @return The input data frame with an additional column containing the sum of
#'   non-NA values for the specified columns.
#' @family data manipulation functions
#' @export
#' @examples
#' # Attach required libraries
#' library(dplyr, rlang)
#'
#' # Create a sample data frame
#' df_survey <- data.frame(
#'   ID = c("A", "B", "C", "D", "E"),
#'   Q1 = c("Yes", "No", NA, "Yes", "No"),
#'   Q2 = c("A", "B", "C", NA, "D"),
#'   Q3 = c(1, NA, 3, 4, NA),
#'   OtherData = c("X", "Y", "Z", "W", "P")
#' )
#' print("Original Data frame:")
#' print(df_survey)
#'
#' # Calculate progress based on Q1, Q2, Q3
#' df_with_completeness_score <- add_completeness_score(df_survey,
#'   columns_to_sum = c("Q1", "Q2", "Q3")
#' )
#' print("Data frame with calculated completeness score:")
#' print(df_with_completeness_score)
#'
#' # Use the new progress column with deduplicate_responses
#' df_with_dups <- data.frame(
#'   ID = c("A", "B", "A", "C"),
#'   Q1 = c("Yes", "No", "Yes", "No"),
#'   Q2 = c("X", "Y", NA, "Z"),
#'   Q3 = c(1, 2, 3, NA)
#' )
#'
#' # Calculate proxy progress for deduplication
#' df_with_dups_progress <- add_completeness_score(df_with_dups,
#'   columns_to_sum = c("Q1", "Q2", "Q3"),
#'   new_col_name = "Progress Score"
#' )
#' print("Dataframe with duplicates and progress score:")
#' print(df_with_dups_progress)
#'
#' # To use with deduplicate_responses:
#' # deduplicated_result <- deduplicate_responses(df_with_dups_progress,
#' #   id_col = "ID",
#' #   progress_col = "Progress Score"
#' # )
#' # print("Deduplicated result:")
#' # print(deduplicated_result)
add_completeness_score <- function(data, columns_to_sum, new_col_name = "completeness_score") {
  data_with_progress <- data |>
    mutate(!!sym(new_col_name) := rowSums(across(all_of(columns_to_sum), ~ !is.na(.))))
  return(data_with_progress)
}
