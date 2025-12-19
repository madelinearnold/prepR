#' Add Label and Label Length Columns
#'
#' Generates a 'label' column based on ITEM_MEMBER and calculates the character 
#' count. This is useful for identifying labels that need shortening for 
#' dashboard visualizations.
#'
#' @param df The varinfo data frame.
#' @return A data frame with 'label' and 'label_length' columns added.
#' @importFrom dplyr mutate coalesce
#' @export
#' @examples
#' varinfo <- tibble::tibble(ITEM_MEMBER = c("Strongly Agree", NA))
#' add_label_metadata(varinfo)
add_label_metadata <- function(df) {
  df |>
    dplyr::mutate(
      # Use ITEM_MEMBER if it exists, otherwise empty string
      label = dplyr::coalesce(ITEM_MEMBER, ""),
      label_length = nchar(label)
    )
}