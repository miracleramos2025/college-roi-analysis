## Basic repo setup for final project

This project focuses on analyzing college salary data to understand how different factors, 
such as school type, region, and major, influence starting and mid-career salaries. The goal is to 
provide insights that can guide educational and career planning by showing potential earnings outcomes 
based on various educational choices.

# load the tidyverse package
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)


# load only a small portion of the dataset to view column names
degrees_that_pay_back <- read_csv("data/degrees-that-pay-back.csv")
salaries_by_college <- read_csv("data/salaries-by-college-type.csv")
salaries_by_region <- read_csv("data/salaries-by-region.csv")

# display the column names
colnames(degrees_that_pay_back)
colnames(salaries_by_college)
colnames(salaries_by_region)

start viewing or plotting the data!



