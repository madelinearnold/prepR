#' Slim Varinfo for Export for Dashboard Display
#'
#' Filters the varinfo to only include variables marked for dashboard display
#' and selects a specific subset of columns required by the dashboard.
#'
#' @param df The varinfo data frame.
#' @return A filtered and column-trimmed tibble.
#' @importFrom dplyr filter select any_of
#' @export
get_dashboard_varinfo <- function(df) {
  target_cols <- c(
    "ITEM_NAME", "ITEM_TYPE", "ITEM_PRESENTATION_TYPE", "ITEM_SECTION",
    "ITEM_PARENT_ID", "ITEM_STEM", "ITEM_MEMBER", "SCALE_OPTIONS",
    "label", "MostRecentSurveyAdmin", "AllSurveyAdmin"
  )

  df |>
    filter(DASH_DISPLAY == TRUE) |>
    select(any_of(target_cols))
}
