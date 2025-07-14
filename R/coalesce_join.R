#' Coalescing Join for Data frames
#'
#' This function performs a join (defaulting to `dplyr::full_join`) between two
#' data frames and then coalesces columns that have the same name but different
#' suffixes (e.g., '.x' and '.y') resulting from the join. This is useful for
#' combining data sets with identical non-key columns with varying levels of
#' completeness. Missing values in x are filled with matching values from y.
#' This function was adapted from https://alistaire.rbind.io/blog/coalescing-joins/
#'
#' @param x The first data frame to join.
#' @param y The second data frame to join.
#' @param by A character vector of variables to join by. See `?dplyr::full_join`
#'   for details. Defaults to `NULL`, which uses common variables.
#' @param suffix A character vector of length 2, specifying the suffixes to
#'   append to the names of common variables that are not used in `by`, to
#'   make them unique. Defaults to `c(".x", ".y")`.
#' @param join The type of join to perform. Defaults to `dplyr::full_join`.
#'   Other `dplyr` join functions like `left_join`, `right_join`, `inner_join`
#'   can also be used.
#' @param ... Additional arguments passed on to the `join` function.
#'
#' @return A data frame resulting from the join, with common columns coalesced
#' into single columns, retaining the first non-missing value.
#' @importFrom dplyr full_join bind_cols coalesce
#' @importFrom purrr map_dfc
#' @family data manipulation functions
#' @export
#' @examples
#' # Create sample data frames
#' df1 <- data.frame(
#'   ID = c(1, 2, 3),
#'   Name = c("Priya", "Omar", "Sofia"),
#'   Value = c(10, 20, NA),
#'   City = c("Mumbai", NA, "Bogota")
#' )
#'
#' df2 <- data.frame(
#'   ID = c(1, 2, 4),
#'   Name = c("Priya", NA, "Kenji"),
#'   Value = c(NA, 25, 30),
#'   City = c(NA, "Cairo", "Tokyo")
#' )
#'
#' print("Data frame 1:")
#' print(df1)
#' print("Data frame 2:")
#' print(df2)
#'
#' # Perform a coalescing full join
#' joined_df <- coalesce_join(df1, df2, by = "ID")
#' print("Coalesced Join Result:")
#' print(joined_df)
#'
#' # Example with a left join
#' df3 <- data.frame(
#'   Key = c("A", "B"),
#'   Data1 = c(100, NA)
#' )
#' df4 <- data.frame(
#'   Key = c("A", "C"),
#'   Data1 = c(NA, 200),
#'   Data2 = c("X", "Y")
#' )
#'
#' left_coalesced_df <- coalesce_join(df3, df4, by = "Key",
#'                                    join = dplyr::left_join)
#' print("Coalesced Left Join Result:")
#' print(left_coalesced_df)
#'
coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
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

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}
