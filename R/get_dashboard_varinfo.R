#' Slim Varinfo for Export for Dashboard Display
#'
#' Filters the varinfo to only include variables marked for a specific
#' display and selects a specific subset of columns required by the dashboard.
#'
#' @param df The varinfo data frame.
#' @param display_col String. The name of the boolean flag column to filter on.
#' Defaults to "DASH_DISPLAY".
#' @return A filtered and column-trimmed tibble.
#' @importFrom dplyr filter select any_of sym
#' @family varinfo prep functions
#' @export
#' @examples
#' # Export for Program Dashboard
#' dash_export <- get_dashboard_varinfo(varinfo, "DASH_DISPLAY")
#'
#' # Export for General Dashboards
#' general_viz_export <- get_dashboard_varinfo(varinfo, "GENERAL_VIZ_DISPLAY")
get_dashboard_varinfo <- function(df, display_col = "DASH_DISPLAY") {
  target_cols <- c(
    "ITEM_NAME", "ITEM_TYPE", "ITEM_PRESENTATION_TYPE", "ITEM_SECTION",
    "ITEM_PARENT_ID", "ITEM_STEM", "ITEM_MEMBER", "SCALE_OPTIONS",
    "label", "MostRecentSurveyAdmin", "AllSurveyAdmin"
  )

  df |>
    filter(as.logical(!!sym(display_col))) |> # converts "TRUE" string --> TRUE
    select(any_of(target_cols))
}
