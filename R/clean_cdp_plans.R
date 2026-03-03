library(dplyr)

#' Map and Clean CDP Plans
#'
#' @param data The data frame with CDP academic plans
#' @param cdp_map The mapping table (CDP to non-CDP plan names)
#' @param plan_cols A character vector of the column names to be cleaned (e.g., c("Plan 1", "Plan 2"))
#' @param map_key The column in cdp_map that matches the academic plan names
#' @param map_value The column in cdp_map containing the "NonCDP" replacement text
#'
#' @return The original data frame with the academic plan column text values updated
#' @export
#' @family institutional data prep
clean_cdp_plans <- function(data,
                            cdp_map,
                            plan_cols = c("Academic Plan Nm - First Plan", "Academic Plan Nm - Second Plan"),
                            map_key = "Academic Plan Nm - First Plan",
                            map_value = "Academic Plan Nm - NonCDP") {

  # create a helper mapping named vector for fast lookup
  lookup <- setNames(cdp_map[[map_value]], cdp_map[[map_key]])

  # update the plan columns with non CDP text values
  data |>
    mutate(
      across(
        # Only act on the columns that actually exist in the data
        any_of(plan_cols),
        \(x) case_when(
          # Check if the value exists in the lookup map
          x %in% names(lookup) ~ lookup[x],

          # Specific hard-coded fix for MCP
          x == "City & Regional Planning MCP" ~ "Master of City Planning MCP",

          # Otherwise keep the original
          TRUE ~ x
        )
      )
    )
}
