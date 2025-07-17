#' Find Similar Character Values in a Data Frame
#'
#' @param data A data frame or tibble.
#' @param threshold A number between 0 and 1. Pairs of values with a
#'   Jaro-Winkler similarity score above this threshold will be returned.
#'   Higher values mean a stricter match. Defaults to 0.85.
#' @param ignore_case A logical value indicating whether to ignore case when
#'   comparing strings. Defaults to TRUE.
#'
#' @return A tibble with three columns: `value1`, `value2`, and `similarity`,
#'   showing the pairs of similar values and their similarity score.
#' @export
#'
#' @examples
#' # Attach required packages
#' library(cli)
#' library(dplyr)
#' library(stringdist)
#' library(tibble)
#' library(tidyr)
#' library(tidyselect)
#'
#' # Create a sample data frame with messy values
#' messy_data <- data.frame(
#' survey_q1 = c("Very Helpful", "Very helpful", "Not helpful", "Slightly Helpful"),
#' survey_q2 = c("very helpful", "not helpful", "not helpful", "slightly helpful"),
#' occupation = c("Self Employed", "self-employed", "Consultant", "Student"),
#' city = c("New York", "newyork", "San Francisco", "Sanfrancisco"),
#' country = c("USA", "United States", "Canada", "US"))
#'
#' # Run the function on your data
#' similar_pairs <- find_similar_values(messy_data)
#' print(similar_pairs)
#'
#' # You can adjust the threshold and include case differences in similarity scoring
#' find_similar_values(messy_data,
#' threshold = 0.9, ignore_case = FALSE)
#'
find_similar_values <- function(data, threshold = 0.85, ignore_case = TRUE) {
  # 1. GATHER UNIQUE VALUES
  # Pivot to a long format and get all unique, non-NA character values.
  unique_values <- data |>
    pivot_longer(
      cols = everything(),
      names_to = NULL,
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    distinct(value) |>
    pull(value)

  # Exit early if there's nothing to compare
  if (length(unique_values) < 2) {
    cli_alert_info("Not enough unique values to compare.")
    return(tibble(value1 = character(), value2 = character(), similarity = numeric()))
  }

  # 2. CALCULATE SIMILARITY
  # Create a set of values for comparison, optionally ignoring case.
  compare_values <- if (ignore_case) tolower(unique_values) else unique_values

  # The `stringdistmatrix` function calculates "distance" (0 = identical).
  # We convert it to "similarity" (1 = identical) for easier interpretation.
  # Jaro-Winkler ('jw') is great for finding typos and minor differences.
  similarity_matrix <- 1 - stringdistmatrix(compare_values,
                                            compare_values,
                                            method = "jw")

  # 3. FIND & FORMAT MATCHES
  # Set the diagonal and lower triangle to 0 to avoid self-matches (e.g., "A" vs "A")
  # and duplicate pairs (e.g., we want "A" vs "B" but not also "B" vs "A").
  similarity_matrix[lower.tri(similarity_matrix, diag = TRUE)] <- 0

  # Find the row/column indices of matrix cells that are above our threshold
  matches <- which(similarity_matrix >= threshold, arr.ind = TRUE)

  if (nrow(matches) == 0) {
    cli_alert_info("No similar values found above the threshold of {threshold}.")
    return(tibble(value1 = character(), value2 = character(), similarity = numeric()))
  }

  # Create a final data frame with the original (not lowercased) values.
  results <- tibble(
    value1 = unique_values[matches[, "row"]],
    value2 = unique_values[matches[, "col"]],
    similarity = similarity_matrix[matches]
  ) |>
    arrange(desc(similarity))

  return(results)
}
