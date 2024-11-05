# Load the tidyverse package
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)


# Load only a small portion of the dataset to view column names
degrees_that_pay_back <- read_csv("data/degrees-that-pay-back.csv")
salaries_by_college <- read_csv("data/salaries-by-college-type.csv")
salaries_by_region <- read_csv("data/salaries-by-region.csv")


# display the column names
colnames(degrees_that_pay_back)
colnames(salaries_by_college)
colnames(salaries_by_region)


# function to summarize dataset structure
dataset_summary <- function(df, name) {
  summary <- list(
    Dataset = name,
    Rows = nrow(df),
    Columns = ncol(df),
    Categorical_Variables = sum(sapply(df, is.character)) + sum(sapply(df, is.factor)),
    Numerical_Variables = sum(sapply(df, is.numeric)),
    Total_Missing_Values = sum(is.na(df)),
    Missingness_Percentage = (sum(is.na(df)) / (nrow(df) * ncol(df))) * 100
  )
  return(as.data.frame(summary))
}

# summaries for each dataset
summaries <- rbind(
  dataset_summary(degrees_that_pay_back, "Degrees That Pay You Back"),
  dataset_summary(salaries_by_college, "Salaries by College Type"),
  dataset_summary(salaries_by_region, "Salaries by Region")
)

# print the summaries
print(summaries)

# PLOT
# average mid-career salary by region
salaries_by_region$`Mid-Career Median Salary` <- as.numeric(
  gsub("[\\$,]", "", salaries_by_region$`Mid-Career Median Salary`)
)

ggplot(salaries_by_region, aes(x = Region, y = `Mid-Career Median Salary`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightgreen") +
  labs(title = "Average Mid-Career Salary by Region",
       x = "Region",
       y = "Mid-Career Median Salary ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

degrees_that_pay_back <- degrees_that_pay_back %>%
  mutate(Salary_Growth = as.numeric(gsub("%", "", `Percent change from Starting to Mid-Career Salary`)))

ggsave("plots/avg_mid_career_salary_by_region.png", width = 8, height = 6)

# PLOT
# salary growth by undergraduate major
ggplot(degrees_that_pay_back, aes(x = reorder(`Undergraduate Major`, -Salary_Growth), y = Salary_Growth)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Salary Growth by Undergraduate Major",
       x = "Undergraduate Major",
       y = "Salary Growth (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

ggsave("plots/avg_salary_growth_by_undergrad_major.png", width = 8, height = 6)

# TABLE
# starting median salary by school type
salaries_by_college$`Starting Median Salary` <- as.numeric(
  gsub("[\\$,]", "", salaries_by_college$`Starting Median Salary`)
)

salaries_summary_table <- salaries_by_college |>
  group_by(`School Type`) |>
  summarise(Avg_Starting_Salary = mean(`Starting Median Salary`, na.rm = TRUE)) |>
  arrange(desc(Avg_Starting_Salary))

print(salaries_summary_table)

png("plots/avg_starting_salary_table.png", width = 800, height = 600)
grid.table(salaries_summary_table)
dev.off()




