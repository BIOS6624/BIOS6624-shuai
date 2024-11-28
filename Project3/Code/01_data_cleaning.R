
### set working directory

working_directory <- 'C:/Users/zhu-s/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project3'
setwd(working_directory)
### load library
library(tidyverse)
library(table1)
## read raw data

df <- readRDS('DataRaw/BIOS6624_proj2_data.rds')

## subset data

df_cleaned <- df%>%select(SEQN,age_years_interview, n_good_days, permth_exm, diabetes, CHF, CHD, BMI_cat, gender, TMIMS_mean, L5_MIMS_mean, RA_MIMS_mean,mortstat)%>%
  filter(age_years_interview>=40, age_years_interview<=80, n_good_days>=3)%>%
  mutate(mortstat = as.factor(mortstat))%>%drop_na()
keep_id <- df_cleaned$SEQN
df_table1 <- df%>%filter(age_years_interview>=40, age_years_interview<=80)%>%
  mutate(drop = case_when(SEQN%in% keep_id~'Include',
                                     .default = 'Exclude'),
                         drop = factor(drop),
                         drop = relevel(drop, ref = 'Include'))%>%
  select(SEQN,age_years_interview, n_good_days, permth_exm, diabetes, CHF, CHD, BMI_cat, gender, TMIMS_mean, L5_MIMS_mean, RA_MIMS_mean,mortstat,drop)

label(df_table1$age_years_interview) <- "Age"
label(df_table1$n_good_days) <- "Number of good days "
label(df_table1$permth_exm) <- "Months of follow-up"
label(df_table1$diabetes) <- "Diabetes"
label(df_table1$CHF) <- "congestive heart failure"
label(df_table1$CHD) <- "coronary heart disease"
label(df_table1$BMI_cat) <- "Body mass index"
label(df_table1$TMIMS_mean) <- "Total MIMS"
label(df_table1$L5_MIMS_mean) <- "L5 MIMS"
label(df_table1$RA_MIMS_mean) <- "RA MIMS"
label(df_table1$mortstat) <- "Mortality rate"

table1(~TMIMS_mean+L5_MIMS_mean+RA_MIMS_mean+age_years_interview+n_good_days+permth_exm+diabetes+CHF+CHD+BMI_cat+mortstat|drop*gender, 
       data = df_table1, overall = F, caption = 'Summary of NHANES dataset')%>%saveRDS(file = 'Figure/table1.RDS')


df_cleaned%>%saveRDS('DataProcessed/cleaned data.RDS')
