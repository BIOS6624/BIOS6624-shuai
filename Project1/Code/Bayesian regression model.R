### loading data and library
library(openxlsx)
library(tidyverse)
working_directory <- 'C:/Users/zhu-s/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project1'
df_cleaned <- read.xlsx(paste0(working_directory, '/DataProcessed/cleaned data.xlsx'))
df_hiv_bays_model <- df_cleaned

### variable name
outcome <- c('VLOAD_year2_log', 'LEU3N_year2', 'AGG_PHYS_year2', 'AGG_MENT_year2')
predictors <- c('Hard_drugs', 'BMI', 'Age', 'Edu', 'Adh')
baseline_outcome <- c('VLOAD_year0_log', 'LEU3N_year0', 'AGG_PHYS_year0','AGG_MENT_year0')

### set up outcome
N <- nrow(df_hiv_bays_model)
K <- 10
Y <- df_hiv_bays_model%>%select(all_of(outcome)) # N by 4 data

### set up data

bmi_dummy <- model.matrix(~  BMI- 1, data = df_hiv_bays_model)[,2:4]
edu_dummy <- model.matrix(~  Edu- 1, data = df_hiv_bays_model)[,2:3]
### a list of 4 matrix
intercept <- rep(1, N)
X <- list()
for(i in 1:4){
  X[[i]] <- cbind(intercept, df_hiv_bays_model$Hard_drugs, bmi_dummy, 
                  df_hiv_bays_model$Age, edu_dummy, df_hiv_bays_model$Adh, 
                  df_hiv_bays_model%>%select(baseline_outcome[i]) )
}


