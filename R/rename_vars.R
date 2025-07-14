#' Rename Variables in a Data frame
#'
#' This function renames columns in a data frame based on a provided mapping
#' data frame. It allows for flexible renaming using custom column names for
#' the old and new variable names within the mapping data frame. Rows with
#' missing old or new names in the mapping data frame are ignored.
#'
#' @param data A data frame whose columns are to be renamed.
#' @param rename_df A data frame containing the mapping between old and new
#'   variable names. It must contain at least two columns: one for the old
#'   names and one for the new names.
#' @param old_names_col A character string specifying the name of the column
#'   in `rename_df` that contains the current (old) variable names in `data`.
#' @param new_names_col A character string specifying the name of the column
#'   in `rename_df` that contains the desired (new) variable names.
#'   Defaults to "ITEM_NAME".
#'
#' @return A data frame with the specified columns renamed according to the
#'   `rename_df` mapping.
#' @importFrom dplyr filter rename
#' @importFrom rlang .data
#' @family data manipulation functions
#' @export
#' @examples
#' # Create a sample data frame
#' df_original <- data.frame(
#'   old_col_A = 1:3,
#'   old_col_B = letters[1:3],
#'   another_col = c(TRUE, FALSE, TRUE)
#' )
#' print("Original Data frame:")
#' print(df_original)
#'
#' # Create a renaming mapping data frame
#' rename_map <- data.frame(
#'   CurrentName = c("old_col_A", "old_col_B", "non_existent"),
#'   NewName = c("new_col_X", "new_col_Y", NA)
#' )
#' print("Renaming Map:")
#' print(rename_map)
#'
#' # Rename variables using custom column names in the map
#' df_renamed <- rename_vars(df_original,
#'   rename_df = rename_map,
#'   old_names_col = "CurrentName",
#'   new_names_col = "NewName"
#' )
#' print("Renamed Data frame:")
#' print(df_renamed)
#'
#' # Example with default new_names_col
#' rename_map_default <- data.frame(
#'   OriginalVar = c("old_col_A"),
#'   ITEM_NAME = c("Alpha")
#' )
#' df_renamed_default <- rename_vars(df_original,
#'   rename_df = rename_map_default,
#'   old_names_col = "OriginalVar"
#' )
#' print("Renamed Data frame (default new_names_col):")
#' print(df_renamed_default)
#'
rename_vars <- function(data, rename_df, old_names_col, new_names_col = "ITEM_NAME") {
  # Filter out rows with NA in either old or new name columns
  valid_renames <- rename_df |>
    filter(!is.na(.data[[old_names_col]]) & !is.na(.data[[new_names_col]]))

  # Create the renaming vector
  rename_vector <- setNames(valid_renames[[old_names_col]], valid_renames[[new_names_col]])

  # Perform the rename
  output <-
    data |>
    rename(all_of(rename_vector))
  return(output)
}
