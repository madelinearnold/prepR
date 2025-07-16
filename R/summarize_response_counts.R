#' Summarize Response Counts with Grouping Options
#'
#' This function groups and summarizes response counts from a data frame. It allows
#' for flexible grouping by any combination of academic, demographic, and/or time
#' variables. If no grouping variables are supplied, you can supply strings to add
#' to the output grouping columns (e.g., "UC Berkeley" for a campus-wide summary).
#' If both grouping variables and category or value strings are supplied, the
#' grouping variables will supersede to determine the output.
#'
#' @param data A data frame containing response data. It is expected to have
#'   columns like 'ITEM_PARENT_ID', 'ITEM_NAME', 'ITEM_TYPE', 'response', and 'rowid'.
#' @param acad_group_var An optional character string specifying the name of an
#'   academic grouping variable. Defaults to `NULL`.
#' @param demo_group_var An optional character string specifying the name of a
#'   demographic grouping variable. Defaults to `NULL`.
#' @param time_group_var An optional character string specifying the name of a
#'   time grouping variable (e.g., 'SurveyAdminYear'). Defaults to `NULL`.
#' @param acad_category A character string to use as the academic group category
#'   when no `acad_group_var` is supplied. Defaults to "None".
#' @param acad_value A character string to use as the academic group value
#'   when no `acad_group_var` is supplied. Defaults to "UC Berkeley".
#' @param demo_category A character string to use as the demographic group category
#'   when no `demo_group_var` is supplied. Defaults to "None".
#' @param demo_value A character string to use as the demographic group value
#'   when no `demo_group_var` is supplied. Defaults to "All students".
#' @param time_category A character string to use as the time group category
#'   when no `time_group_var` is supplied. Defaults to "Aggregated".
#' @param time_value A character string to use as the time group value
#'   when no `time_group_var` is supplied. Defaults to "All years".
#'
#' @return A data frame with summarized response counts, including 'response',
#'   'count', 'total', 'perc', 'ITEM_NAME', and standardized grouping columns
#'   ('acad_group_category', 'acad_group_value', 'demo_group_category',
#'   'demo_group_value', 'time_group_category', 'time_group_value').
#' @family data analysis functions
#' @export
#' @examples
#' # Attach required libraries
#' library(dplyr, purrr, rlang)
#'
#' # Create a sample data frame
#' df_counts <- data.frame(
#'   rowid = 1:10,
#'   ITEM_PARENT_ID = c("Q1", "Q1", "Q1", "Q2", "Q2", "Q1", "Q1", "Q2", "Q2", "Q1"),
#'   ITEM_NAME = c("Q1_A", "Q1_B", "Q1_A", "Q2_X", "Q2_Y", "Q1_B", "Q1_A", "Q2_X", "Q2_Y", "Q1_A"),
#'   ITEM_TYPE = c("Single Select", "Single Select", "Single Select", "Multi Select", "Multi Select",
#'                 "Single Select", "Single Select", "Multi Select", "Multi Select", "Single Select"),
#'   response = c("Yes", "No", "Yes", "Option1", "Option2", "No", "Yes", "Option1", "Option3", "Yes"),
#'   Department = c("Math", "Physics", "Math", "Physics", "Math", "Math", "Physics", "Physics", "Math", "Math"),
#'   Year = c(2023, 2023, 2024, 2024, 2023, 2023, 2024, 2024, 2023, 2024)
#' )
#'
#' # Summarize without grouping variables (campus-wide summary)
#' summary_all <- summarize_response_counts(df_counts)
#' print(summary_all)
#'
#' # Summarize by academic group (Department)
#' summary_by_dept <- summarize_response_counts(df_counts, acad_group_var = "Department")
#' print(summary_by_dept)
#'
#' # Summarize by time group (Year)
#' summary_by_year <- summarize_response_counts(df_counts, time_group_var = "Year")
#' print(summary_by_year)
#'
#' # Summarize by academic and time groups
#' summary_dept_year <- summarize_response_counts(df_counts,
#'                                                acad_group_var = "Department",
#'                                                time_group_var = "Year")
#' print(summary_dept_year)
#'
summarize_response_counts <- function(data,
                                      acad_group_var = NULL,
                                      demo_group_var = NULL,
                                      time_group_var = NULL,

                                      acad_category = "None",
                                      acad_value = "UC Berkeley",
                                      demo_category = "None",
                                      demo_value = "All students",
                                      time_category = "Aggregated",
                                      time_value = "All years"
) {
  # Assemble grouping vars vector (only non-null ones)
  grouping_vars <- c(acad_group_var, demo_group_var, time_group_var) |> discard(is.null)

  # Step 1: Calculate counts
  # Use grouping variables if supplied
  if (length(grouping_vars) > 0) {
    result <- data |>
      group_by(across(all_of(c(grouping_vars, "ITEM_PARENT_ID")))) |>
      mutate(n_by_parentid = n_distinct(rowid)) |>
      group_by(across(all_of(c(grouping_vars, "ITEM_NAME")))) |>
      mutate(n_by_item = n_distinct(rowid)) |>
      mutate(total = case_when(
        ITEM_TYPE == "Multi Select" ~ n_by_parentid,
        TRUE ~ n_by_item
      )) |>
      group_by(across(all_of(c(grouping_vars, "ITEM_NAME", "response", "total")))) |>
      summarise(count = n(), .groups = "drop") |>
      mutate(perc = round(100 * count / total, 1))
  } else {
    # No grouping
    result <- data |>
      group_by(ITEM_PARENT_ID) |>
      mutate(n_by_parentid = n_distinct(rowid)) |>
      group_by(ITEM_NAME) |>
      mutate(n_by_item = n_distinct(rowid)) |>
      mutate(total = case_when(
        ITEM_TYPE == "Multi Select" ~ n_by_parentid,
        TRUE ~ n_by_item
      )) |>
      group_by(ITEM_NAME, response, total) |>
      summarise(count = n(), .groups = "drop") |>
      mutate(perc = round(100 * count / total, 1))
  }

  # Step 2: Add standardized group category/value columns
  result <- result |>
    mutate(
      acad_group_category = if (!is.null(acad_group_var)) acad_group_var
      else if (!is.null(acad_category)) acad_category
      else NA_character_,
      acad_group_value =  if (!is.null(acad_group_var)) .data[[acad_group_var]]
      else if (!is.null(acad_value)) acad_value
      else NA_character_,

      demo_group_category = if (!is.null(demo_group_var)) demo_group_var
      else if (!is.null(demo_category)) demo_category
      else NA_character_,
      demo_group_value    = if (!is.null(demo_group_var)) .data[[demo_group_var]]
      else if (!is.null(demo_value)) demo_value
      else NA_character_,

      time_group_category = if (!is.null(time_group_var)) time_group_var
      else if (!is.null(time_category)) time_category
      else NA_character_,
      time_group_value    = if (!is.null(time_group_var)) .data[[time_group_var]]
      else if (!is.null(time_value)) time_value
      else NA_character_
    )

  # Step 3: Reorder columns if needed
  result <- result |>
    select(
      response, count, total, perc,
      acad_group_category, acad_group_value,
      demo_group_category, demo_group_value,
      time_group_category, time_group_value,
      ITEM_NAME
    )

  return(result)
}
