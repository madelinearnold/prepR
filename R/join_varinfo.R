#' Join Latest Survey Variables with Prior Varinfo
#'
#' Performs a two-pass join: first fuzzy matching on question text,
#' then an exact coalesce join on variable names.
#'
#' @param prior_varinfo The existing cumulative varinfo data frame.
#' @param new_info The data frame produced by load_survey_column_info.
#' @param var_name_col The column in prior_varinfo containing the variable ID (e.g., ITEM_NAME).
#' @param text_col The column in prior_varinfo containing the prior question text.
#' @param max_dist Maximum Levenshtein distance for fuzzy matching.
#' @importFrom fuzzyjoin stringdist_left_join
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr arrange distinct mutate coalesce
#' @importFrom stats setNames
#' @importFrom glue glue
#' @family varinfo prep functions
#' @export
#' @examples
#' prior <- tibble::tibble(ITEM_NAME = "Q1", text_old = "What is your age?")
#' current <- tibble::tibble(QualtricsVariableName.2025 = "Q1",
#'                           QualtricsVariableText.2025 = "What's your age?") # slight text diff
#'
#' join_varinfo(prior, current, "ITEM_NAME", "text_old")
join_varinfo <- function(prior_varinfo, new_info, var_name_col, text_col, max_dist = 3) {

  # Identify the dynamic columns in the new data
  new_var_col  <- names(new_info)[1]
  new_text_col <- names(new_info)[2]

  # Cleaning: Convert NAs to empty strings (matching can't compare NA to string)
  new_info_clean <- new_info |>
    mutate(!!new_text_col := coalesce(!!sym(new_text_col), ""))

  prior_varinfo_clean <- prior_varinfo |>
    mutate(!!text_col := coalesce(!!sym(text_col), ""))

  # --- PASS 1: Fuzzy Text Join ---
  fuzzy_joined <- prior_varinfo_clean |>
    rowid_to_column("row_id") |>
    fuzzyjoin::stringdist_left_join(
      new_info_clean,
      by = setNames(new_text_col, text_col),
      max_dist = max_dist,
      distance_col = "dist"
    ) |>
    arrange(row_id, dist) |>
    distinct(row_id, .keep_all = TRUE)

  # --- PASS 2: Exact Name Join (Coalesce) ---
  # This catches variables that changed text but kept the same Qualtrics ID
  final_joined <- fuzzy_joined |>
    coalesce_left_join(
      new_info_clean,
      by = setNames(new_var_col, var_name_col),
      keep = TRUE
    ) |>
    # clean up row_id
    select(-row_id)

  # Reporting
  total_new <- nrow(new_info)
  matched   <- sum(!is.na(final_joined[[new_var_col]]))
  cat(glue::glue("Matched {matched} out of {total_new} variables."))

  return(final_joined)
}
