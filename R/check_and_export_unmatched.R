#' Check and Export Unmatched Variables
#'
#' Takes the result of the join and the new survey info. If there are
#' variables in the new survey that didn't find a match, it writes them to a
#' CSV for manual review.
#'
#' @param joined_varinfo The data frame resulting from \code{join_varinfo}.
#' @param new_info The data frame resulting from \code{load_survey_column_info}.
#' @param export_path Optional; the file path to save the unmatched CSV.
#'   If NULL (default), it only prints a summary to the console.
#' @return The tibble of unmatched variables (invisibly).
#' @importFrom dplyr anti_join
#' @importFrom readr write_csv
#' @family varinfo prep functions
#' @export
#' @examples
#' # check_and_export_unmatched(joined, current, "data/temp/unmatched_2025.csv")
check_and_export_unmatched <- function(joined_varinfo, new_info, export_path = NULL) {

  # Identify unmatched
  new_var_col <- names(new_info)[1]
  unmatched <- dplyr::anti_join(new_info, joined_varinfo, by = new_var_col)
  n_unmatched <- nrow(unmatched)

  if (n_unmatched > 0) {
    if (!is.null(export_path)) {
      # Scenario A: Unmatched found AND path provided
      message(sprintf("🔍 Found %d unmatched variables. Exporting to: %s",
                      n_unmatched, export_path))
      readr::write_csv(unmatched, export_path)
      message("💡 Tip: Add these to your patches.csv")
    } else {
      # Scenario B: Unmatched found BUT no path provided
      message(sprintf("⚠️ Found %d unmatched variables, but no export_path was provided.",
                      n_unmatched))
      message("Run again with an export_path if you want to save these to a CSV.")
    }
  } else {
    # Scenario C: Everything matches
    message("✅ All variables successfully matched. No export needed.")
  }

  return(invisible(unmatched))
}
