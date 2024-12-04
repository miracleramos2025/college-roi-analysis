# univariate_analysis ----
# Univariate analysis examines one variable at a time

## load packages & data ----
library(tidyverse)

# load Kaggle dataset 
degrees_that_pay_back <- read_csv("data/degrees-that-pay-back.csv")
salaries_by_college <- read_csv("data/salaries-by-college-type.csv")
salaries_by_region <- read_csv("data/salaries-by-region.csv")

# load College Scorecard dataset
codebook <- read_csv("data/codebook.csv")

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

# This bar chart visualizes the mean earnings for each credential level, 
# making it a univariate analysis






