#' Coalescing Left Join
#'
#' Performs a left join and merges overlapping columns, filling NAs in the
#' left table with values from the right table.
#'
#' @param x,y Tables to join.
#' @param by A character vector of variables to join by.
#' @param suffix Suffixes to apply to overlapping columns.
#' @importFrom dplyr left_join coalesce sym mutate select
#' @export
#' @examples
#' df1 <- tibble::tibble(ID = 1:2, val = c("A", NA))
#' df2 <- tibble::tibble(ID = 1:2, val = c("Z", "B"))
#'
#' # Standard join creates val.x and val.y
#' # Coalesce join fills the NA in df1 with the value from df2
#' coalesce_left_join(df1, df2, by = "ID")
#'
coalesce_left_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  joined <- left_join(x, y, by = by, suffix = suffix, ...)
  common_cols <- setdiff(intersect(names(x), names(y)), by)

  for (col in common_cols) {
    col_x <- paste0(col, suffix[1])
    col_y <- paste0(col, suffix[2])
    joined <- joined |>
      mutate(!!col := coalesce(!!sym(col_x), !!sym(col_y))) |>
      select(-!!sym(col_x), -!!sym(col_y))
  }
  return(joined)
}
