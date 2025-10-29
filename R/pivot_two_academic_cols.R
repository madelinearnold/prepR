#' Pivot Two Academic Columns to One
#'
#' This function handles data complexity arising from having two academic plan
#' columns by converting them into a single, consolidated academic column. It
#' includes logic to drop the first value if a student's first plan is an
#' academic master's en route to a PhD (when `apply_en_route_masters_logic` is
#' TRUE), and removes the second value if it's identical to the first.
#' Placeholder or empty values are also filtered out.
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
#' @param apply_en_route_masters_logic A logical value. If `TRUE` (the default),
#'   the function will check for a column named `ac_masters_en_route_PhD`.
#'   If that column exists, it will set the value of `first_col` to `NA` where
#'   `ac_masters_en_route_PhD` is `TRUE`. If `FALSE`, the `first_col` is not
#'   altered, and the `ac_masters_en_route_PhD` column is not required to exist.
#'
#' @return A transformed data frame with the two academic columns pivoted into a
#'   single new column, `new_col_name`, and an additional `new_col_name_source`
#'   column indicating the original source column.
#' @family data manipulation functions
#' @export
#' @examples
#' # Attach required libraries
#' library(dplyr)
#' library(tidyr)
#' library(rlang)
#'
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
#'   new_col_name = "ConsolidatedAcademicPlan",
#'   apply_en_route_masters_logic = TRUE # Default behavior
#' )
#' print(pivoted_df)
#'
#' # Example setting apply_en_route_masters_logic = FALSE
#' # Using the same data
#' pivoted_df_false <- pivot_two_academic_cols(df_acad,
#'   first_col = "AcademicPlan1",
#'   second_col = "AcademicPlan2",
#'   new_col_name = "ConsolidatedAcademicPlan",
#'   apply_en_route_masters_logic = FALSE
#' )
#' print("--- Pivoted with apply_en_route_masters_logic = FALSE ---")
#' print(pivoted_df_false)
#' # Note: The "History MA" for ID 1 is KEPT.
#'
#' # Example with a "-" placeholder value
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
pivot_two_academic_cols <- function(data, first_col, second_col, new_col_name, apply_en_route_masters_logic = TRUE) {
  # --- Step 1: Conditionally modify the first column ---
  if (apply_en_route_masters_logic) {
    # Check if the 'ac_masters_en_route_PhD' column exists
    if ("ac_masters_en_route_PhD" %in% names(data)) {
      data <- data |>
        # Drop first value if academic master's en route to PhD
        mutate(!!sym(first_col) := case_when(
          ac_masters_en_route_PhD ~ NA,
          TRUE ~ .data[[first_col]]
          )
        )
    } else {
      # Column doesn't exist, issue a warning but proceed
      warning(
        paste0(
          "apply_en_route_masters_logic = TRUE but column ac_masters_en_route_PhD was not found. The first column ('",
          first_col,
          "') will not be modified."
        )
      )
    }
  }
  # If apply_en_route_masters_logic is FALSE, we simply do nothing and proceed.

  # Step 2: Remove second value if it's the same as the first
  result <-
    data |>
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
