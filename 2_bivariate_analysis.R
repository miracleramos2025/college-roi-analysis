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
# replace "PS" with NA and ensure numeric
filtered_field_of_study <- filtered_field_of_study |>
  mutate(
    EARN_NE_MDN_3YR = as.numeric(ifelse(EARN_NE_MDN_3YR == "PS", NA, EARN_NE_MDN_3YR))
  )

# med earnings by credential level
median_earnings_by_credential <- filtered_field_of_study |>
  filter(CREDLEV %in% c(3, 5, 6)) |>
  group_by(CREDLEV) |>
  summarize(Median_Earnings = median(EARN_NE_MDN_3YR, na.rm = TRUE)) |>
  mutate(CREDLEV = factor(CREDLEV, levels = c(3, 5, 6), labels = c("Bachelor's Degree", "Master's Degree", "Doctoral Degree")))


# colors :) 
custom_colors <- c("Bachelor's Degree" = "lightskyblue", 
                   "Master's Degree" = "mediumturquoise", 
                   "Doctoral Degree" = "lightslateblue")

# bar plot
ggplot(median_earnings_by_credential, aes(x = CREDLEV, y = Median_Earnings, fill = CREDLEV)) +
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
    values = c("Male" = "slategray1", "Female" = "mediumslateblue") 
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
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

# save plot
ggsave("figures/med_salary_by_gender_field_of_study.png", width = 12, height = 8)



#### Impact of Pell Grants on Earnings ----

# replace "PS" with NA in the relevant columns
filtered_field_of_study$EARN_PELL_NE_MDN_3YR <- as.numeric(
  ifelse(filtered_field_of_study$EARN_PELL_NE_MDN_3YR == "PS", NA, 
         filtered_field_of_study$EARN_PELL_NE_MDN_3YR)
)

filtered_field_of_study$EARN_NOPELL_NE_MDN_3YR <- as.numeric(
  ifelse(filtered_field_of_study$EARN_NOPELL_NE_MDN_3YR == "PS", NA, 
         filtered_field_of_study$EARN_NOPELL_NE_MDN_3YR)
)

pell_earnings_long <- filtered_field_of_study |>
  select(CIPDESC, EARN_PELL_NE_MDN_3YR, EARN_NOPELL_NE_MDN_3YR) |>
  pivot_longer(
    cols = c(EARN_PELL_NE_MDN_3YR, EARN_NOPELL_NE_MDN_3YR),
    names_to = "Pell_Status",
    values_to = "Median_Earnings"
  ) |>
  mutate(Pell_Status = ifelse(Pell_Status == "EARN_PELL_NE_MDN_3YR", "Received Pell Grant", "No Pell Grant"))

# boxplot
ggplot(pell_earnings_long, aes(x = Pell_Status, y = Median_Earnings, fill = Pell_Status)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(
    values = c("Received Pell Grant" = "mediumturquoise", "No Pell Grant" = "royalblue")
  ) +
  labs(
    title = "Impact of Pell Grants on Earnings (3 Years After Graduation)",
    x = "Pell Grant Status",
    y = "Median Earnings ($)",
    fill = "Pell Grant Status"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

ggsave("figures/pell_grants_vs_earnings_boxplot.png", width = 8, height = 6)




#### Earnings vs. Debt Scatterplot ----
# replace "PS" with NA and convert columns to numeric
filtered_field_of_study <- filtered_field_of_study |>
  mutate(
    EARN_NE_MDN_3YR = as.numeric(ifelse(EARN_NE_MDN_3YR == "PS", NA, EARN_NE_MDN_3YR)),
    DEBT_ALL_STGP_ANY_MDN = as.numeric(ifelse(DEBT_ALL_STGP_ANY_MDN == "PS", NA, DEBT_ALL_STGP_ANY_MDN))
  )

earnings_vs_debt_data <- filtered_field_of_study |>
  filter(!is.na(EARN_NE_MDN_3YR), !is.na(DEBT_ALL_STGP_ANY_MDN)) |>
  left_join(salaries_by_college, by = c("INSTNM" = "School Name")) |>
  filter(!is.na(`School Type`))

# all School types
earnings_vs_debt_data <- earnings_vs_debt_data |>
  mutate(`School Type` = factor(
    `School Type`,
    levels = c("Ivy League", "Engineering", "Liberal Arts", "Party", "State")
  ))

# colors
custom_colors <- c(
  "Ivy League" = "blue",
  "Engineering" = "mediumpurple1",
  "Liberal Arts" = "lightseagreen",
  "Party" = "navy",
  "State" = "slategray1"
)

# scatterplot
ggplot(earnings_vs_debt_data, aes(x = DEBT_ALL_STGP_ANY_MDN, y = EARN_NE_MDN_3YR, color = `School Type`)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Earnings vs. Debt by Institution Type",
    x = "Median Debt ($)",
    y = "Median Earnings (3 Years After Graduation) ($)",
    color = "Institution Type"
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

ggsave("figures/earnings_vs_debt_scatterplot.png", width = 10, height = 6)



#### Earnings Growth by Credential Level ----

# replace "PS" with NA and ensure numeric data
filtered_field_of_study <- filtered_field_of_study |>
  mutate(
    EARN_MDN_HI_1YR = as.numeric(ifelse(EARN_MDN_HI_1YR == "PS", NA, EARN_MDN_HI_1YR)),
    EARN_NE_MDN_3YR = as.numeric(ifelse(EARN_NE_MDN_3YR == "PS", NA, EARN_NE_MDN_3YR))
  )

earnings_growth_credential <- filtered_field_of_study |>
  filter(CREDLEV %in% c(3, 5, 6), !is.na(EARN_MDN_HI_1YR), !is.na(EARN_NE_MDN_3YR)) |>
  group_by(CREDLEV) |>
  summarize(
    Median_Earnings_1YR = median(EARN_MDN_HI_1YR, na.rm = TRUE),
    Median_Earnings_3YR = median(EARN_NE_MDN_3YR, na.rm = TRUE),
    Growth = round(((Median_Earnings_3YR - Median_Earnings_1YR) / Median_Earnings_1YR) * 100, 2)  # Growth as %
  )

# labels for credential levels
earnings_growth_credential <- earnings_growth_credential |>
  mutate(CREDLEV = case_when(
    CREDLEV == 3 ~ "Bachelor's Degree",
    CREDLEV == 5 ~ "Master's Degree",
    CREDLEV == 6 ~ "Doctoral Degree",
    TRUE ~ "Other"
  ))

# table
kable(
  earnings_growth_credential,
  col.names = c("Credential Level", "Median Earnings (1st Year)", "Median Earnings (3rd Year)", "Growth (%)"),
  caption = "Earnings Growth by Credential Level (1st to 3rd Year After Graduation)",
  format = "html"
) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#### Median Earnings by Field of Study and Pell Grant Status ----
# randomly select 10 fields of study
set.seed(789)  
random_fields <- pell_earnings_long |>
  select(CIPDESC) |>
  distinct() |>
  slice_sample(n = 7)

filtered_pell_earnings <- pell_earnings_long |>
  filter(CIPDESC %in% random_fields$CIPDESC)

# scatterplot
random_scatter_pell_grants_plot <- ggplot(filtered_pell_earnings, 
                                          aes(x = CIPDESC, y = Median_Earnings, color = Pell_Status)) +
  geom_point(size = 3, alpha = 0.7) +  # Scatter points
  scale_color_manual(
    values = c("Received Pell Grant" = "mediumturquoise", "No Pell Grant" = "royalblue")
  ) +
  labs(
    title = "Random Field of Study Impact of Pell Grants on Earnings",
    x = "Field of Study",
    y = "Median Earnings ($)",
    color = "Pell Grant Status"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    legend.position = "top"
  )

print(random_scatter_pell_grants_plot)

ggsave("figures/random_scatter_pell_grants_plot.png", width = 10, height = 6)


