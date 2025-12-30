#' Coalescing Left Join
#'
#' Performs a left join and merges overlapping columns, filling NAs in the
#' left table with values from the right table.
#'
#' @param x,y Tables to join.
#' @param by A character vector of variables to join by.
#' @param suffix Suffixes to apply to overlapping columns.
#' @importFrom dplyr left_join coalesce bind_cols
#' @importFrom purrr map_dfc
#' @export
#' @examples
#' df1 <- tibble::tibble(ID = 1:2, val = c("A", NA))
#' df2 <- tibble::tibble(ID = 1:2, val = c("Z", "B"))
#'
#' # Standard join creates val.x and val.y
#' # Coalesce join fills the NA in df1 with the value from df2
#' coalesce_left_join(df1, df2, by = "ID")
#'
coalesce_left_join <- function(x, y,
                               by = NULL, suffix = c(".x", ".y"), ...) {
  joined <- dplyr::left_join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  # suppress messages about duplicate or empty column names
  coalesced <- suppressMessages(purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  )))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}
