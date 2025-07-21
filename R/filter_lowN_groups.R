#' Filter Low-N Groups from a Summary Data Frame
#'
#' This function takes a summary data frame (produced by
#' `summarize_response_counts`) and a data frame of low-N groups (produced
#' by `identify_lowN_groups`) and filters out the rows from the summary that
#' correspond to the low-N groups. It maps the original grouping variable names
#' to the standardized `..._group_value` columns in the summary data frame.
#'
#' @param summary_df A data frame of summarized counts, typically the output of
#'   `summarize_response_counts`.
#' @param lowN_groups_df A data frame identifying groups with low respondent
#'   counts, typically the output of `identify_lowN_groups`.
#' @param acad_group_var The original academic grouping variable name used to
#'   create the summaries. Defaults to `NULL`.
#' @param demo_group_var The original demographic grouping variable name used to
#'   create the summaries. Defaults to `NULL`.
#' @param time_group_var The original time grouping variable name used to
#'   create the summaries. Defaults to `NULL`.
#'
#' @return A data frame, identical in structure to `summary_df`, but with rows
#'   corresponding to the groups in `lowN_groups_df` removed.
#' @family data analysis functions
#' @export
#' @examples
#' # Attach required libraries
#' library(dplyr)
#' library(purrr)
#' library(rlang)
#'
#' # Create the same sample data frame as before
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
#' # --- Workflow Example ---
#'
#' # 1. Define grouping variables and threshold
#' my_group_vars <- c("Department", "Year")
#' my_threshold <- 3
#'
#' # 2. Create the full summary
#' full_summary <- summarize_response_counts(df_counts,
#'                                           acad_group_var = "Department",
#'                                           time_group_var = "Year")
#' cat("--- Full Summary (before filtering) ---\n")
#' print(full_summary)
#'
#' # 3. Identify the low-N groups
#' low_n_groups <- identify_lowN_groups(df_counts,
#'                                      group_cols = my_group_vars,
#'                                      threshold = my_threshold)
#' cat("\n--- Groups Identified as Low-N (n < 3) ---\n")
#' print(low_n_groups)
#' # Note: The (Physics, 2023), (Math, 2024), and (Physics, 2024) groups
#' # have respondent counts of 1, 2, and 2 respectively.
#'
#' # 4. Filter the summary to exclude the low-N groups
#' filtered_summary <- filter_lowN_groups(full_summary, low_n_groups,
#'                                        acad_group_var = "Department",
#'                                        time_group_var = "Year")
#' cat("\n--- Filtered Summary (after excluding low-N groups) ---\n")
#' print(filtered_summary)
#' # The result only contains data for the (Math, 2023) group.
#'
filter_lowN_groups <- function(summary_df,
                               lowN_groups_df,
                               acad_group_var = NULL,
                               demo_group_var = NULL,
                               time_group_var = NULL) {
  # Build the mapping vector for the join.
  # The format is c("summary_df_col_name" = "lowN_df_col_name")
  join_cols <- c()

  if (!is.null(acad_group_var)) {
    # Check if the column exists in the lowN data frame
    if (!acad_group_var %in% names(lowN_groups_df)) {
      stop(paste("Column", shQuote(acad_group_var), "not found in lowN_groups_df."))
    }
    join_cols["acad_group_value"] <- acad_group_var
  }

  if (!is.null(demo_group_var)) {
    if (!demo_group_var %in% names(lowN_groups_df)) {
      stop(paste("Column", shQuote(demo_group_var), "not found in lowN_groups_df."))
    }
    join_cols["demo_group_value"] <- demo_group_var
  }

  if (!is.null(time_group_var)) {
    if (!time_group_var %in% names(lowN_groups_df)) {
      stop(paste("Column", shQuote(time_group_var), "not found in lowN_groups_df."))
    }
    join_cols["time_group_value"] <- time_group_var
  }

  # If no grouping variables are provided, or if the lowN df is empty,
  # there's nothing to filter.
  if (length(join_cols) == 0) {
    warning("No grouping variables provided to map for filtering. Returning original summary data frame.")
    return(summary_df)
  }

  if (nrow(lowN_groups_df) == 0) {
    # If there are no low-N groups, no filtering is needed.
    return(summary_df)
  }

  # Use anti_join to keep rows in summary_df that *don't* have a match
  # in lowN_groups_df, based on the mapping we created.
  dplyr::anti_join(summary_df, lowN_groups_df, by = join_cols)
}
