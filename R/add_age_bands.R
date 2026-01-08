#' Add Age Bands and Codes
#'
#' Categorizes a numerical age column into standardized age bands and numeric codes.
#'
#' @param data A data frame or tibble.
#' @param age_col Character. The name of the column containing the numerical age
#'   (e.g., "Age At Snapshot").
#'
#' @return The original data frame with two additional columns: \code{AGE_BAND}
#'   and \code{AGE_BAND_CD}.
#' @export
add_age_bands <- function(data, age_col) {
  data |>
    dplyr::mutate(
      AGE_BAND = dplyr::case_when(
        .data[[age_col]] < 25 ~ "Below 25",
        .data[[age_col]] >= 25 & .data[[age_col]] < 30 ~ "25-29",
        .data[[age_col]] >= 30 & .data[[age_col]] < 35 ~ "30-34",
        .data[[age_col]] >= 35 & .data[[age_col]] < 40 ~ "35-39",
        .data[[age_col]] >= 40 ~ "40 or above",
        .default = NA_character_
      ),
      AGE_BAND_CD = dplyr::case_when(
        AGE_BAND == "Below 25" ~ "1",
        AGE_BAND == "25-29" ~ "2",
        AGE_BAND == "30-34" ~ "3",
        AGE_BAND == "35-39" ~ "4",
        AGE_BAND == "40 or above" ~ "5",
        is.na(AGE_BAND) ~ "Unknown"
      )
    )
}
