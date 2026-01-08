#' Classify Academic Program Types
#'
#' Categorizes university academic programs into standardized program types
#' (academic masters, doctoral, professional, self-supporting) based on the program
#' and degree names.
#' @param data A data frame or tibble.
#' @param program_nm_col Character. The name of the column containing the
#'   Academic Program Name (e.g., "Academic Program Nm - First Plan").
#' @param degree_nm_col Character. The name of the column containing the
#'   Degree Offered Name (e.g., "Degree Offered Nm - First Plan").
#' @param output_col Character. The name of the new column to be created
#'   (e.g., "dashboard_program_type_plan1").
#'
#' @return The original data frame with the additional column specified in
#' \code{output_col}.
#'
#' @export
#' @family institutional data prep

add_program_type_classification <- function(data, program_nm_col, degree_nm_col, output_col) {

  # Define constants for categorization logic
  phd_degrees <- c("Doctor of Philosophy", "Joint Doctor of Philosophy", "Juris Scientiae Doctor")
  academic_pgms <- c("Graduate Academic Programs", "Law Academic Programs")
  prof_pgms <- c("Graduate Professional Programs", "Law Professional Programs")
  ss_pgms <- c("Graduate Self-Supporting Pgms", "Law Self-Supporting Programs")
  other_pgms <- c("Graduate Non-Degree/Non-FinAid", "Law Non-Degree/Non-FinAid")

  data |>
    dplyr::mutate(
      # The !! (bang-bang) with := allows the output_col string to be the column name
      !!output_col := dplyr::case_when(
        # Use .data[[string]] to reference the column safely
        .data[[program_nm_col]] %in% academic_pgms &
          !.data[[degree_nm_col]] %in% phd_degrees ~ "academic masters",

        .data[[program_nm_col]] %in% academic_pgms &
          .data[[degree_nm_col]] %in% phd_degrees ~ "doctoral",

        .data[[program_nm_col]] %in% prof_pgms ~ "professional",

        .data[[program_nm_col]] %in% ss_pgms ~ "self-supporting",

        is.na(.data[[program_nm_col]]) |
          .data[[program_nm_col]] %in% other_pgms ~ "other/unknown",

        .default = .data[[program_nm_col]]
      )
    )
}



