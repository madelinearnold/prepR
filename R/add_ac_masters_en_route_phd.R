#' Add Academic Masters En-Route to PhD Flag
#'
#' Adds a logical column `ac_masters_en_route_PhD` to the dataframe. A student is
#' flagged as TRUE if their first plan is an academic master's and their second
#' plan is a PhD in the same major.
#'
#' @param data A data frame or tibble.
#' @param plan2_col Character. Column name for the second academic plan.
#' @param major2_col Character. Column name for the second major.
#' @param degree2_col Character. Column name for the second degree offered.
#' @param program1_col Character. Column name for the first academic program.
#'
#' @return The original data frame with the logical
#' \code{ac_masters_en_route_PhD} column.
#' @export
#' @family institutional data prep
#' @examples
#' library(dplyr)
#'
#' # Create sample data with one "En-Route" case and one "Standard" case
#' sample_data <- tibble::tibble(
#'   `Academic Plan Nm - Second Plan` = c("Chemistry PhD", NA),
#'   `Major Nm - Second Major` = c("-", NA),
#'   `Degree Offered Nm - Second Plan` = c("Doctor of Philosophy", NA),
#'   `Academic Program Nm - First Plan` = c("Graduate Academic Programs", "Graduate Professional Programs")
#' )
#'
#' sample_data |>
#'   add_ac_masters_en_route_phd(
#'   "Academic Plan Nm - Second Plan",
#'   "Major Nm - Second Major",
#'   "Degree Offered Nm - Second Plan",
#'   "Academic Program Nm - First Plan")
#'
add_ac_masters_en_route_phd <- function(data, plan2_col, major2_col, degree2_col, program1_col) {

  phd_degrees <- c("Doctor of Philosophy", "Joint Doctor of Philosophy", "Juris Scientiae Doctor")
  academic_pgms <- c("Graduate Academic Programs", "Law Academic Programs")

  # Logic check:
  # 1. Second plan exists and is not a dash
  # 2. Second major is NA or a dash (indicating it's the same major as plan 1)
  # 3. Second degree is a PhD type
  # 4. First program is an Academic program
  data |>
    dplyr::mutate(
      ac_masters_en_route_PhD = dplyr::if_else(
        !is.na(.data[[plan2_col]]) & .data[[plan2_col]] != "-" &
          (is.na(.data[[major2_col]]) | .data[[major2_col]] == "-") &
          .data[[degree2_col]] %in% phd_degrees &
          .data[[program1_col]] %in% academic_pgms,
        TRUE,
        FALSE,
        missing = FALSE
      )
    )
}
