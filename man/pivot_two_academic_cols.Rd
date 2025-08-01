% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_two_academic_cols.R
\name{pivot_two_academic_cols}
\alias{pivot_two_academic_cols}
\title{Pivot Two Academic Columns to One}
\usage{
pivot_two_academic_cols(data, first_col, second_col, new_col_name)
}
\arguments{
\item{data}{A data frame containing the academic plan columns. It is expected
to have a logical column named \code{ac_masters_en_route_PhD} for conditional
dropping of values.}

\item{first_col}{A character string specifying the name of the first academic
column.}

\item{second_col}{A character string specifying the name of the second
academic column.}

\item{new_col_name}{A character string specifying the desired name for the
new, consolidated academic column.}
}
\value{
A transformed data frame with the two academic columns pivoted into a
single new column, \code{new_col_name}, and an additional \code{new_col_name_source}
column indicating the original source column.
}
\description{
This function handles data complexity arising from having two academic plan
columns by converting them into a single, consolidated academic column. It
includes logic to drop the first value if a student's first plan is an
academic master's en route to a PhD, and removes the second value if it's
identical to the first. Placeholder or empty values are also filtered out.
}
\examples{
# Attach required libraries
library(dplyr)
library(tidyr)
library(rlang)

# Create a sample data frame
df_acad <- data.frame(
  ID = 1:5,
  AcademicPlan1 = c("History MA", "Economics PhD", "Physics PhD"),
  AcademicPlan2 = c("History PhD", "Statistics MA", "NA"),
  ac_masters_en_route_PhD = c(TRUE, FALSE, FALSE)
)

# Pivot academic columns
pivoted_df <- pivot_two_academic_cols(df_acad,
  first_col = "AcademicPlan1",
  second_col = "AcademicPlan2",
  new_col_name = "ConsolidatedAcademicPlan"
)
print(pivoted_df)

# Example with a placeholder value
df_acad_placeholder <- data.frame(
  ID = 1:3,
  Major1 = c("History", "Art", "Philosophy"),
  Major2 = c(NA, "-", "Philosophy"),
  ac_masters_en_route_PhD = c(FALSE, FALSE, FALSE)
)
pivoted_df_placeholder <- pivot_two_academic_cols(df_acad_placeholder,
  first_col = "Major1",
  second_col = "Major2",
  new_col_name = "Major"
)
print(pivoted_df_placeholder)

}
\seealso{
Other data manipulation functions: 
\code{\link{add_completeness_score}()},
\code{\link{coalesce_join}()},
\code{\link{deduplicate_responses}()},
\code{\link{rename_vars}()}
}
\concept{data manipulation functions}
