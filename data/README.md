# Data Directory

This folder contains the datasets used in the final project. It includes both raw and cleaned datasets.

## Contents

1. **`degrees-that-pay-back.csv`**  
   - Kaggle dataset with starting and mid-career salary data for various undergraduate majors.

2. **`salaries-by-college-type.csv`**  
   - Dataset with salary data categorized by institution type.

3. **`salaries-by-region.csv`**  
   - Dataset with salary data segmented by region.

4. **`Most-Recent-Cohorts-Field-of-Study.csv`**  
   - College Scorecard dataset with detailed earnings and debt data by field of study.
   - This file is too large to display on GitHub due to `.gitignore` settings but is crucial for the analysis conducted in the project. Ensure you download it locally to replicate the results.

## Notes

- Missing values in the College Scorecard dataset marked as "PS" were replaced with `NA`.
- For more details on the cleaning process, refer to the `4_data_collection.R` script.
