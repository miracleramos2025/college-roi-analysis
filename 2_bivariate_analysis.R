# bivariate_analysis ----
# bivariate analysis involves two variables.

## load packages & data ----
library(tidyverse)

# load Kaggle dataset 
degrees_that_pay_back <- read_csv("data/degrees-that-pay-back.csv")
salaries_by_college <- read_csv("data/salaries-by-college-type.csv")
salaries_by_region <- read_csv("data/salaries-by-region.csv")

# load College Scorecard dataset
field_of_study_data <- read_csv("data/Most-Recent-Cohorts-Field-of-Study.csv")

# selected columns from dataset
filtered_field_of_study <- field_of_study_data |>
  select(
    CIPDESC, CREDLEV, DEBT_ALL_STGP_ANY_MDN, DEBT_ALL_STGP_EVAL_MDN,
    EARN_MDN_HI_1YR, EARN_MDN_HI_2YR, EARN_NE_MDN_3YR,
    EARN_COUNT_WNE_HI_1YR, EARN_GT_THRESHOLD_1YR, EARN_IN_STATE_1YR,
    EARN_MALE_NE_MDN_3YR, EARN_NOMALE_NE_MDN_3YR, 
    EARN_PELL_NE_MDN_3YR, EARN_NOPELL_NE_MDN_3YR
  )
codebook <- read_csv("data/codebook.csv")

#### Average Starting Median Salary by Institution Type ----
salaries_by_college$`Starting Median Salary` <- as.numeric(
  gsub("[\\$,]", "", salaries_by_college$`Starting Median Salary`)
)

salaries_summary_table <- salaries_by_college |>
  group_by(`School Type`) |>
  summarise(Avg_Starting_Salary = mean(`Starting Median Salary`, na.rm = TRUE)) |>
  arrange(desc(Avg_Starting_Salary))

print(salaries_summary_table)

png("figures/avg_starting_salary_table.png", width = 800, height = 600)
grid.table(salaries_summary_table)
dev.off()

# This summarizes the starting salary by institution type, 
# representing a bivariate analysis of institution type vs. average 
# starting salary