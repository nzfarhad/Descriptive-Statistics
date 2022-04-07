# Date:           July, 2020
# Script:         Descriptive statistics function for tabular datasets 
# Author:         Sayed Farhad Nabizada - nzfarhad@gmail.com

# Load / Install package
if(!require(dplyr)) install.packages("dplyr")
if(!require(readxl)) install.packages("readxl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(reshape2)) install.packages("reshape2")

# Source Analysis Function
source("R/functions/Analysis_function.R")

# Load Dataset
# Source:       data.humdata.org
# Contributor:  REACH Initiatives
# License:      Creative Commons Attribution International
survey_data <- read_excel("input/data/reach_afg_dataset_wash_dry_spell_assessment_june2018.xlsx", sheet = "WASH_Dry_Spell_Data")

# Load Analysis Plan
analysis_plan <- read_excel("input/analysisplan/analysis_plan.xlsx")

# Run Analysis function
analysis_results <- analysis_func(df = survey_data, ap = analysis_plan)

# Export the results
write.xlsx(analysis_results, "output/Analysis_Results.xlsx")



# Long format
# analysis_results <- analysis_results %>% filter(is.na(repeat_for))
# analysis_results_long <- pivot_wider(analysis_results, 
#                                  id_cols = c("Disaggregation","Disaggregation_level"),
#                                  values_from = "Result",
#                                  names_from = c("Question", "Response", "Aggregation_method") )

