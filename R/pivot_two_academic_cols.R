#' Pivot Two Academic Columns to One
#'
#' This function handles data complexity arising from having two academic plan
#' columns by converting them into a single, consolidated academic column. It
#' includes logic to drop the first value if a student's first plan is an
#' academic master's en route to a PhD, and removes the second value if it's
#' identical to the first. Placeholder or empty values are also filtered out.
#'
#' @param data A data frame containing the academic plan columns. It is expected
#'   to have a logical column named `ac_masters_en_route_PhD` for conditional
#'   dropping of values.
#' @param first_col A character string specifying the name of the first academic
#'   column.
#' @param second_col A character string specifying the name of the second
#'   academic column.
#' @param new_col_name A character string specifying the desired name for the
#'   new, consolidated academic column.
#'
#' @return A transformed data frame with the two academic columns pivoted into a
#'   single new column, `new_col_name`, and an additional `new_col_name_source`
#'   column indicating the original source column.
#' @importFrom dplyr case_when filter mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data sym
#' @family data manipulation functions
#' @export
#' @examples
#' # Create a sample data frame
#' df_acad <- data.frame(
#'   ID = 1:5,
#'   AcademicPlan1 = c("History MA", "Economics PhD", "Physics PhD"),
#'   AcademicPlan2 = c("History PhD", "Statistics MA", "NA"),
#'   ac_masters_en_route_PhD = c(TRUE, FALSE, FALSE)
#' )
#'
#' # Pivot academic columns
#' pivoted_df <- pivot_two_academic_cols(df_acad,
#'   first_col = "AcademicPlan1",
#'   second_col = "AcademicPlan2",
#'   new_col_name = "ConsolidatedAcademicPlan"
#' )
#' print(pivoted_df)
#'
#' # Example with a placeholder value
#' df_acad_placeholder <- data.frame(
#'   ID = 1:3,
#'   Major1 = c("History", "Art", "Philosophy"),
#'   Major2 = c(NA, "-", "Philosophy"),
#'   ac_masters_en_route_PhD = c(FALSE, FALSE, FALSE)
#' )
#' pivoted_df_placeholder <- pivot_two_academic_cols(df_acad_placeholder,
#'   first_col = "Major1",
#'   second_col = "Major2",
#'   new_col_name = "Major"
#' )
#' print(pivoted_df_placeholder)
#'

# Function to handle data complexity from two academic plans
# Converts academic columns from two to one
pivot_two_academic_cols <- function(data, first_col, second_col, new_col_name) {
  result <-
    data |>
    # Step 1: Drop first value if academic master's en route to PhD
    mutate(!!sym(first_col) := case_when(
      ac_masters_en_route_PhD ~ NA,
      TRUE ~ .data[[first_col]]
    )) |>

    # Step 2: Remove second value if it's the same as the first
    mutate(!!sym(second_col) := case_when(
      .data[[first_col]] == .data[[second_col]] ~ NA,
      TRUE ~ .data[[second_col]]
    )) |>

    # Step 3: Pivot longer
    pivot_longer(cols = all_of(c(first_col, second_col)),
                 names_to = paste0(new_col_name, "_source"),
                 values_to = new_col_name,
                 values_drop_na = TRUE) |>

    # Step 4: Drop placeholder or empty values
    filter(!is.na(.data[[new_col_name]]) & .data[[new_col_name]] != "-")

  return(result)

}
