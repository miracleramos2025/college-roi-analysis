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


#### Average Mid-Career Salary by Region ----
salaries_by_region$`Mid-Career Median Salary` <- as.numeric(
  gsub("[\\$,]", "", salaries_by_region$`Mid-Career Median Salary`)
)

ggplot(salaries_by_region, aes(x = Region, y = `Mid-Career Median Salary`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "royalblue") +
  labs(title = "Average Mid-Career Salary by Region",
       x = "Region",
       y = "Mid-Career Median Salary ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

degrees_that_pay_back <- degrees_that_pay_back |>
  mutate(Salary_Growth = as.numeric(gsub("%", "", `Percent change from Starting to Mid-Career Salary`)))

ggsave("figures/avg_mid_career_salary_by_region.png", width = 8, height = 6)



#### Median Salary by Field of Study ----
# replace "PS" with "NA" 
field_of_study_data$EARN_NE_MDN_3YR <- as.numeric(
  ifelse(field_of_study_data$EARN_NE_MDN_3YR == "PS", NA, field_of_study_data$EARN_NE_MDN_3YR)
)

# remove rows with missing values
filtered_field_of_study <- field_of_study_data |>
  filter(!is.na(EARN_NE_MDN_3YR))

major_salary_summary <- filtered_field_of_study |>
  group_by(CIPDESC) |>
  summarize(Median_Earnings = median(EARN_NE_MDN_3YR, na.rm = TRUE)) |>
  arrange(desc(Median_Earnings))

# top 30 majors 
top_majors <- major_salary_summary |>
  slice_head(n = 25)

# bar graph
median_salary_by_field_of_study <- ggplot(top_majors, 
                                          aes(reorder(CIPDESC, -Median_Earnings), 
                                              Median_Earnings, 
                                              fill = Median_Earnings)) +
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

ggsave("figures/median_salary_by_field_of_study.png", width = 8, height = 6)



#### Average Salary Growth by Undergraduate Major ----
ggplot(degrees_that_pay_back, aes(x = reorder(`Undergraduate Major`, -Salary_Growth), y = Salary_Growth)) +
  geom_bar(stat = "identity", fill = "lightskyblue") +
  labs(title = "Salary Growth by Undergraduate Major",
       x = "Undergraduate Major",
       y = "Salary Growth (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

ggsave("figures/avg_salary_growth_by_undergrad_major.png", width = 8, height = 6)


#### Average Starting Salary Table ----
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

#### Median Earnings by Credential Level ----
# replace "PS" with "NA" 
field_of_study_data$EARN_NE_MDN_3YR <- as.numeric(
  ifelse(field_of_study_data$EARN_NE_MDN_3YR == "PS", NA, field_of_study_data$EARN_NE_MDN_3YR)
)

# credential levels 
filtered_field_of_study <- field_of_study_data |>
  filter(CREDLEV %in% c(3, 5, 6))  # numeric codes credentials

# calculate mean earnings
aggregated_data <- filtered_field_of_study |>
  group_by(CREDLEV) |>
  summarize(Mean_Earnings = mean(EARN_NE_MDN_3YR, na.rm = TRUE))

# using descriptive names
aggregated_data$CREDLEV <- factor(aggregated_data$CREDLEV, 
                                  levels = c(3, 5, 6), 
                                  labels = c("Bachelor's Degree", "Master's Degree", "Doctoral Degree"))

# colors :) 
custom_colors <- c("Bachelor's Degree" = "lightskyblue", 
                   "Master's Degree" = "mediumturquoise", 
                   "Doctoral Degree" = "lightslateblue")

# bar plot
ggplot(aggregated_data, aes(x = CREDLEV, y = Mean_Earnings, fill = CREDLEV)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Median Earnings by Credential Level (3 Years After Graduation)",
    x = "Credential Level",
    y = "Median Earnings ($)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/median_earnings_by_credential.png", width = 8, height = 6)




#### Median Salary by Gender ----

# replace "PS" with NA 
filtered_field_of_study$EARN_MALE_NE_MDN_3YR <- as.numeric(
  ifelse(filtered_field_of_study$EARN_MALE_NE_MDN_3YR == "PS", NA, 
         filtered_field_of_study$EARN_MALE_NE_MDN_3YR)
)

filtered_field_of_study$EARN_NOMALE_NE_MDN_3YR <- as.numeric(
  ifelse(filtered_field_of_study$EARN_NOMALE_NE_MDN_3YR == "PS", NA, 
         filtered_field_of_study$EARN_NOMALE_NE_MDN_3YR)
)

gender_salary_data <- filtered_field_of_study |>
  filter(!is.na(EARN_MALE_NE_MDN_3YR), !is.na(EARN_NOMALE_NE_MDN_3YR))

gender_salary_long <- gender_salary_data |>
  select(CIPDESC, EARN_MALE_NE_MDN_3YR, EARN_NOMALE_NE_MDN_3YR) |>
  pivot_longer(
    cols = c(EARN_MALE_NE_MDN_3YR, EARN_NOMALE_NE_MDN_3YR),
    names_to = "Gender",
    values_to = "Median_Earnings"
  ) |>
  mutate(Gender = ifelse(Gender == "EARN_MALE_NE_MDN_3YR", "Male", "Female"))

top_fields <- gender_salary_long |>
  group_by(CIPDESC) |>
  summarize(Average_Earnings = mean(Median_Earnings, na.rm = TRUE)) |>
  arrange(desc(Average_Earnings)) |>
  slice_head(n = 10) |>
  pull(CIPDESC)

# data for the top fields only
filtered_gender_salary <- gender_salary_long |>
  filter(CIPDESC %in% top_fields)


# bar plot
ggplot(filtered_gender_salary, aes(x = reorder(CIPDESC, -Median_Earnings), y = Median_Earnings, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("Male" = "royalblue3", "Female" = "plum1") 
  ) +
  labs(
    title = "Median Salary by Gender and Field of Study (Top 10 Fields, 3 Years After Graduation)",
    x = "Field of Study",
    y = "Median Earnings ($)",
    fill = "Gender"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# save plot
ggsave("figures/med_salary_by_gender_field_of_study.png", width = 12, height = 8)
