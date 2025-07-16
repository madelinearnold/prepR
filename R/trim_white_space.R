#' Trim White space from a Data Frame
#'
#' This function removes leading and trailing white space from both the column
#' names and all character columns within a data frame.
#'
#' @param data A data frame or tibble to be cleaned.
#'
#' @return A data frame with white space trimmed from its column names and all
#'   character columns.
#'
#' @export
#'
#' @examples
#' # Attach required packages
#' library(dplyr
#' library(rlang)
#'
#' # Create a sample data frame with extra white space
#' messy_df <- data.frame(
#'   ` Col Name 1 ` = c("  Value A ", "Value B  ", "  Value C"),
#'   `Col Name 2  ` = c("  X", "Y  ", " Z "),
#'   NumericCol = c(1, 2, 3),
#'   check.names = FALSE
#' )
#'
#' # Clean the data frame
#' clean_df <- trim_white_space(messy_df)
#'
#' # View the cleaned column names
#' names(clean_df)
#'
#' # View the cleaned data
#' clean_df
#'
trim_white_space <- function(data) {
  # trim white space from column names
  names(data) <- trimws(names(data))

  # trim white space from all character columns
  data <- data |>
    mutate(across(where(is.character), trimws))

  return(data)
}
