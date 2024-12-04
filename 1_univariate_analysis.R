# univariate_analysis ----
# Univariate analysis examines one variable at a time

## load packages & data ----
library(tidyverse)

# load Kaggle dataset 
degrees_that_pay_back <- read_csv("data/degrees-that-pay-back.csv")
salaries_by_college <- read_csv("data/salaries-by-college-type.csv")
salaries_by_region <- read_csv("data/salaries-by-region.csv")


#### Starting Median Salary by Region ----
salaries_by_region$`Starting Median Salary` <- as.numeric(
  gsub("[\\$,]", "", salaries_by_region$`Starting Median Salary`)
)

region_starting_salary_stats <- salaries_by_region |>
  group_by(Region) |>
  summarize(
    Median_Starting_Salary = median(`Starting Median Salary`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(Median_Starting_Salary))

# table 
kable(
  region_starting_salary_stats,
  col.names = c("Region", "Median Starting Salary ($)"),
  caption = "Summary of Starting Salaries by Region",
  format = "html"
) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))








