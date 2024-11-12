
### set working directory

working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project3'
setwd(working_directory)
### load library
library(tidyverse)

## read raw data

df <- readRDS('DataRaw/BIOS6624_proj2_data.rds')

## subset data

df_cleaned <- df%>%select(SEQN,age_years_interview, n_good_days, permth_exm, diabetes, CHF, CHD, BMI_cat, gender, TMIMS_mean, L5_MIMS_mean, RA_MIMS_mean,mortstat)%>%
  filter(age_years_interview>=40, age_years_interview<=80, n_good_days>=3)%>%drop_na()

df_cleaned%>%saveRDS('DataProcessed/cleaned data.RDS')


