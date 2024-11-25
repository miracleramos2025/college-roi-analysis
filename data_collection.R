### Load packages & data ----

# load packags
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(kableExtra)


# load Kaggle dataset 
degrees_that_pay_back <- read_csv("data/degrees-that-pay-back.csv")
salaries_by_college <- read_csv("data/salaries-by-college-type.csv")
salaries_by_region <- read_csv("data/salaries-by-region.csv")

# load College Scorecard dataset
field_of_study_data <- read_csv("data/Most-Recent-Cohorts-Field-of-Study.csv")

### Filtered Data ----
# filter the relevant columns
filtered_field_of_study <- field_of_study_data |>
  select(CIPDESC, CREDLEV, DEBT_ALL_STGP_ANY_MDN, DEBT_ALL_STGP_EVAL_MDN,
         EARN_MDN_HI_1YR, EARN_MDN_HI_2YR, EARN_NE_MDN_3YR,
         EARN_COUNT_WNE_HI_1YR, EARN_GT_THRESHOLD_1YR, EARN_IN_STATE_1YR,
         EARN_MALE_NE_MDN_3YR, EARN_NOMALE_NE_MDN_3YR, 
         EARN_PELL_NE_MDN_3YR, EARN_NOPELL_NE_MDN_3YR)


# Check the filtered dataset
head(filtered_field_of_study)

#### Column Names ----
colnames(degrees_that_pay_back)
colnames(salaries_by_college)
colnames(salaries_by_region)
colnames(filtered_field_of_study)


#### function to summarize dataset structure ----
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
#### Average Mid-Career Salary by Region ----
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
#### Average Salary Growth by Undergraduate Major ----
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
#### Average Starting Salary Table ----
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

### Anaylsis ----

#### Median Earnings by Credential Level ----

# replace "PS" with "NA" and convert to numeric
field_of_study_data$EARN_NE_MDN_3YR <- as.numeric(
  ifelse(field_of_study_data$EARN_NE_MDN_3YR == "PS", NA, field_of_study_data$EARN_NE_MDN_3YR)
)

# filter relevant credential levels (Bachelor's, Master's, Doctorate)
filtered_field_of_study <- field_of_study_data |>
  filter(CREDLEV %in% c(3, 5, 6))  # Numeric codes for relevant levels

# aggregate data by credential level to calculate mean earnings
aggregated_data <- filtered_field_of_study |>
  group_by(CREDLEV) |>
  summarize(Mean_Earnings = mean(EARN_NE_MDN_3YR, na.rm = TRUE))

# recode CREDLEV to use descriptive names
aggregated_data$CREDLEV <- recode(aggregated_data$CREDLEV,
                                  `3` = "Bachelor's Degree",
                                  `5` = "Master's Degree",
                                  `6` = "Doctoral Degree")

# median earnings bar plots
median_earnings <- ggplot(aggregated_data, aes(x = CREDLEV, y = Mean_Earnings, fill = CREDLEV)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Median Earnings by Credential Level (3 Years After Graduation)",
    x = "Credential Level",
    y = "Median Earnings"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if not needed

print(median_earnings)


ggsave("plots/median_earnings_by_credential.png", width = 8, height = 6)


#### Median Debt vs. Median Earnings ----
# replace "PS" with "NA" and convert to numeric for Debt and Earnings
field_of_study_data$DEBT_ALL_STGP_ANY_MDN <- as.numeric(
  ifelse(field_of_study_data$DEBT_ALL_STGP_ANY_MDN == "PS", NA, field_of_study_data$DEBT_ALL_STGP_ANY_MDN)
)
field_of_study_data$EARN_NE_MDN_3YR <- as.numeric(
  ifelse(field_of_study_data$EARN_NE_MDN_3YR == "PS", NA, field_of_study_data$EARN_NE_MDN_3YR)
)

# filtered data to remove rows with NA values in relevant columns
filtered_field_of_study <- field_of_study_data |>
  filter(!is.na(DEBT_ALL_STGP_ANY_MDN), !is.na(EARN_NE_MDN_3YR))

# aggregate data by debt ranges 
debt_earnings_summary <- filtered_field_of_study |>
  mutate(
    Debt_Range = cut(
      DEBT_ALL_STGP_ANY_MDN,
      breaks = seq(0, max(DEBT_ALL_STGP_ANY_MDN, na.rm = TRUE), by = 5000),
      include.lowest = TRUE,
      labels = paste0("$", seq(0, max(DEBT_ALL_STGP_ANY_MDN, na.rm = TRUE) - 5000, by = 5000), 
                      " - $", seq(5000, max(DEBT_ALL_STGP_ANY_MDN, na.rm = TRUE), by = 5000))
    )
  ) |>
  group_by(Debt_Range) |>
  summarize(
    Mean_Earnings = mean(EARN_NE_MDN_3YR, na.rm = TRUE),
    Mean_Debt = mean(DEBT_ALL_STGP_ANY_MDN, na.rm = TRUE)
  )

# bar graph
med_earnings_vs_med_debt_bar <- ggplot(debt_earnings_summary, aes(x = Debt_Range, y = Mean_Earnings, fill = Debt_Range)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Median Debt vs. Median Earnings (3 Years After Graduation)",
    x = "Debt Range ($)",
    y = "Median Earnings ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none"  
  )

print(med_earnings_vs_med_debt_bar)

#### Median Salary by Field of Study ----
# replace "PS" with "NA" and convert Earnings to numeric
field_of_study_data$EARN_NE_MDN_3YR <- as.numeric(
  ifelse(field_of_study_data$EARN_NE_MDN_3YR == "PS", NA, field_of_study_data$EARN_NE_MDN_3YR)
)

# filterd data to remove rows with missing values
filtered_field_of_study <- field_of_study_data |>
  filter(!is.na(EARN_NE_MDN_3YR))

major_salary_summary <- filtered_field_of_study |>
  group_by(CIPDESC) |>
  summarize(Median_Earnings = median(EARN_NE_MDN_3YR, na.rm = TRUE)) |>
  arrange(desc(Median_Earnings))

# top 30 majors for better readability
top_majors <- major_salary_summary |>
  slice_head(n = 25)

# bar graph
median_salary_by_field_of_study <- ggplot(top_majors, aes(x = reorder(CIPDESC, -Median_Earnings), y = Median_Earnings, fill = Median_Earnings)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Median Salary by Field of Study (3 Years After Graduation)",
    x = "Field of Study",
    y = "Median Earnings ($)"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 64, hjust = 1),
    legend.position = "none"  
  )

print(median_salary_by_field_of_study)

ggsave("plots/median_salary_by_field_of_study.png", width = 8, height = 6)


#### Top 15 Institutions by Median Salary (3 Years After Graduation) ----
# replace "PS" with "NA" and convert Earnings to numeric
field_of_study_data$EARN_MDN_HI_1YR <- as.numeric(
  ifelse(field_of_study_data$EARN_MDN_HI_1YR == "PS", NA, field_of_study_data$EARN_MDN_HI_1YR)
)
field_of_study_data$EARN_MDN_HI_2YR <- as.numeric(
  ifelse(field_of_study_data$EARN_MDN_HI_2YR == "PS", NA, field_of_study_data$EARN_MDN_HI_2YR)
)
field_of_study_data$EARN_NE_MDN_3YR <- as.numeric(
  ifelse(field_of_study_data$EARN_NE_MDN_3YR == "PS", NA, field_of_study_data$EARN_NE_MDN_3YR)
)

# filtered data for bachelor's degrees and remove missing values
filtered_field_of_study <- field_of_study_data |>
  filter(CREDLEV == 3, !is.na(EARN_MDN_HI_1YR), !is.na(EARN_MDN_HI_2YR), !is.na(EARN_NE_MDN_3YR))  # Filter for bachelor's degree (CREDLEV == 3)

# aggregate data: median earnings by institution
bachelor_institution_salary_summary <- filtered_field_of_study |>
  group_by(INSTNM) |>
  summarize(
    Median_Earnings_1YR = median(EARN_MDN_HI_1YR, na.rm = TRUE),
    Median_Earnings_2YR = median(EARN_MDN_HI_2YR, na.rm = TRUE),
    Median_Earnings_3YR = median(EARN_NE_MDN_3YR, na.rm = TRUE)
  ) |>
  arrange(desc(Median_Earnings_3YR))  # Sort by third-year median earnings in descending order

# top 15 institutions
top_bachelor_institutions <- bachelor_institution_salary_summary |>
  slice_head(n = 15)

print(bachelor_institution_salary_summary)



