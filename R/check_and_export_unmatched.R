#' Check and Export Unmatched Variables
#'
#' Takes the result of the join and the new survey info. If there are
#' variables in the new survey that didn't find a match, it writes them to a
#' CSV for manual review.
#'
#' @param joined_varinfo The data frame resulting from \code{join_varinfo}.
#' @param new_info The data frame resulting from \code{load_survey_column_info}.
#' @param export_path The file path (including filename) where the CSV should be saved.
#' @return The tibble of unmatched variables (invisibly).
#' @importFrom dplyr anti_join
#' @importFrom readr write_csv
#' @family varinfo prep functions
#' @export
#' @examples
#' # check_and_export_unmatched(joined, current, "data/temp/unmatched_2025.csv")
check_and_export_unmatched <- function(joined_varinfo, new_info, export_path) {

  # Identify unmatched
  new_var_col <- names(new_info)[1]
  unmatched <- dplyr::anti_join(new_info, joined_varinfo, by = new_var_col)

  if (nrow(unmatched) > 0) {
    message(sprintf("🔍 Found %d unmatched variables. Exporting to: %s",
                    nrow(unmatched), export_path))
    readr::write_csv(unmatched, export_path)
    message("💡 Tip: Add these to your patches.csv")
  } else {
    message("✅ All variables successfully matched. No export needed.")
  }

  # Return the data invisibly so it can still be used in a pipe if needed
  return(invisible(unmatched))
}
