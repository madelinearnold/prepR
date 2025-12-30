#' Load Qualtrics Column Metadata from Response CSV
#'
#' Reads a Qualtrics CSV and extracts the variable names and question text
#' (usually found in the first row of data).
#'
#' @param file_path Path to raw Qualtrics CSV.
#' @param survey_admin ID for this administration (e.g., "2025" or "Fa2024").
#' @param var_prefix A character string prefix to precede the survey admin ID
#' in the name of the column that contains the variable name values (defaults
#' to "QualtricsVariableName.").
#' @param text_prefix A character string prefix to precede the survey admin ID
#' in the name of the column that contains the question text (defaults to
#' "QualtricsVariableText.").
#' @importFrom dplyr mutate sym if_else
#' @family varinfo prep functions
#' @export
load_survey_column_info <- function(file_path, survey_admin,
                                    var_prefix = "QualtricsVariableName.",
                                    text_prefix = "QualtricsVariableText.") {
  # Read header and first text row
  data <- read_csv(file_path, n_max = 1, show_col_types = FALSE)

  col_var  <- paste0(var_prefix, survey_admin)
  col_text <- paste0(text_prefix, survey_admin)

  column_info <- tibble(
    !!col_var  := colnames(data),
    !!col_text := as.character(data[1, ])
  ) |>
    mutate(!!col_text := if_else(is.na(!!sym(col_text)), "", !!sym(col_text)))

  return(column_info)
}
