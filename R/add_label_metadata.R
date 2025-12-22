#' Add Label and Label Length Columns
#'
#' Generates a 'label' column based on ITEM_MEMBER and calculates the character
#' count. This is useful for identifying labels that need shortening for
#' dashboard visualizations.
#'
#' @param df The varinfo data frame.
#' @param overwrite Logical. If TRUE, recalculates all labels. If FALSE (default),
#'   only populates empty/NA labels using ITEM_MEMBER.
#' @return A data frame with 'label' and 'label_length' columns added.
#' @importFrom dplyr mutate coalesce
#' @family varinfo prep functions
#' @export
#' @examples
#' varinfo <- tibble::tibble(ITEM_MEMBER = c("Strongly Agree", NA))
#' add_label_metadata(varinfo)
add_label_metadata <- function(df, overwrite = FALSE) {
  # Initialize column if missing
  if (!"label" %in% names(df)) df$label <- NA_character_

  # If overwriting, wipe existing values to force recalculation
  if (overwrite) {
    df$label <- NA_character_
  }

  df |>
    dplyr::mutate(
      # Use label if it exists, otherwise ITEM_MEMBER if it exists, otherwise empty string
      label = dplyr::coalesce(label, ITEM_MEMBER, ""),
      label_length = nchar(label)
    )
}
