# multivariate_analysis ----
# Multivariate analysis involves three or more variables

## load packages & data ----
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

# selected columns from dataset
filtered_field_of_study <- field_of_study_data |>
  select(
    CIPDESC, CREDLEV, DEBT_ALL_STGP_ANY_MDN, DEBT_ALL_STGP_EVAL_MDN,
    EARN_MDN_HI_1YR, EARN_MDN_HI_2YR, EARN_NE_MDN_3YR,
    EARN_COUNT_WNE_HI_1YR, EARN_GT_THRESHOLD_1YR, EARN_IN_STATE_1YR,
    EARN_MALE_NE_MDN_3YR, EARN_NOMALE_NE_MDN_3YR, 
    EARN_PELL_NE_MDN_3YR, EARN_NOPELL_NE_MDN_3YR
  )

#### Earnings Growth Over Time by Institution Type ----
# joining datasets 
filtered_field_of_study_with_type <- filtered_field_of_study |>
  left_join(salaries_by_college, by = c("INSTNM" = "School Name"))

# filter out NA values 
filtered_field_of_study_with_type <- filtered_field_of_study_with_type |>
  filter(!is.na(`School Type`))

# school type grouping
school_type_growth <- filtered_field_of_study_with_type |>
  group_by(`School Type`) |>
  summarize(
    Avg_Earnings_1YR = mean(EARN_MDN_HI_1YR, na.rm = TRUE),
    Avg_Earnings_2YR = mean(EARN_MDN_HI_2YR, na.rm = TRUE),
    Avg_Earnings_3YR = mean(EARN_NE_MDN_3YR, na.rm = TRUE)
  )

school_type_growth_long <- school_type_growth |>
  pivot_longer(cols = starts_with("Avg_Earnings"), 
               names_to = "Year", 
               values_to = "Earnings") |>
  mutate(Year = case_when(
    Year == "Avg_Earnings_1YR" ~ 1,
    Year == "Avg_Earnings_2YR" ~ 2,
    Year == "Avg_Earnings_3YR" ~ 3
  ))

# line plot
earnings_growth_by_institution_type <- ggplot(
  school_type_growth_long, 
  aes(x = Year, y = Earnings, color = `School Type`)) +
  geom_line(size = 1.5) +  
  geom_point(size = 3) +  
  scale_color_manual(
    values = c(
      "Engineering" = "lightslateblue",
      "Ivy League" = "purple3",
      "Liberal Arts" = "lightskyblue",
      "Party" = "blue2",
      "State" = "royalblue"
    )
  ) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("1 Year", "2 Years", "3 Years")) +  
  scale_y_continuous(labels = scales::dollar_format()) +  
  labs(
    title = "Earnings Growth Over Time by Institution Type",
    x = "Years After Graduation",
    y = "Median Earnings ($)",
    color = "Institution Type"
  ) +
  theme_minimal()

earnings_growth_by_institution_type

# save plot
ggsave("figures/earnings_growth_by_institution_type.png", width = 10, height = 7)


#### Top Schools for Return of Investment on Bachelor's Degrees ----
bachelors_data <- filtered_field_of_study_with_type |>
  filter(CREDLEV == 3)

# calculate ROI 
bachelors_roi <- bachelors_data |>
  group_by(INSTNM) |>
  summarize(
    Median_Earnings_3YR = median(EARN_NE_MDN_3YR, na.rm = TRUE),  
    Median_Debt = median(DEBT_ALL_STGP_ANY_MDN, na.rm = TRUE),     
    ROI = Median_Earnings_3YR - Median_Debt                   
  ) |>
  arrange(desc(ROI))

# top 10 schools
top_bachelors_roi <- bachelors_roi |>
  slice_head(n = 10)

kable(
  top_bachelors_roi,
  format = "html",
  col.names = c("Institution", "Median Earnings (3rd Year)", "Median Debt", "ROI ($)"),
  caption = "Top Schools for Return of Investment on Bachelor's Degrees"
) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
