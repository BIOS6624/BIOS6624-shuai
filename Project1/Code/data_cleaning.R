
library(tidyverse)
library(gtsummary)
library(table1) 
library(openxlsx)
n = 430
working_directory <- 'C:/Users/zhu-s/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project1'
df_hiv <- read.csv(paste0(working_directory, '/DataRaw/hiv_6624_final.csv'))%>%as_tibble()

### convert long format to wide format
df_hiv_wider <- df_hiv%>%
  select(newid, age, years, hard_drugs ,VLOAD, LEU3N, AGG_PHYS, AGG_MENT, BMI, EDUCBAS, ADH)%>%
  filter(years == 0|years == 2)%>%
  pivot_wider(names_from = years, names_glue = "{.value}_year{years}",
              values_from = c("VLOAD", "LEU3N", "AGG_PHYS",
                              "AGG_MENT",'age', 'BMI','EDUCBAS',
                              'hard_drugs','ADH'))%>% 
  ### set up adherence 
  mutate(adh = case_when(ADH_year2<=2~1,.default = 0))%>%
  
  select(-c(ADH_year2,ADH_year0))

### drop missing value
df_hiv_lm_model <- df_hiv_wider%>%drop_na()%>%
  filter(BMI_year0<=200, BMI_year0>0)

### draw histogram 

hist(df_hiv_lm_model$BMI_year0)
hist(df_hiv_lm_model$LEU3N_year0)

### transform vload and leu3n 
df_hiv_lm_model <- df_hiv_lm_model%>%
  mutate(VLOAD_year0_log =log(VLOAD_year0),
         VLOAD_year2_log =log(VLOAD_year2),
         LEU3N_year0_log =log(LEU3N_year0),
         LEU3N_year2_log =log(LEU3N_year2))%>%
  ### categorize education variable
  mutate(edu = factor(EDUCBAS_year2, levels = 1:7, 
                    labels = c(rep('High school', 3),'some college', 'some college', 'Graduate, Post Graduate','Graduate, Post Graduate')))%>%
  ### categroize bmi variable
  mutate(BMI = case_when(BMI_year0 < 18.5 ~ "Underweight",
                         BMI_year0 > 18.5 & BMI_year0 < 25 ~ "Healthy",
                         BMI_year0 > 25 & BMI_year0 < 30 ~ "Overweight",
                         BMI_year0 > 30 ~ "Obsese"))%>%
  select(-c('BMI_year0', 'BMI_year2', 'EDUCBAS_year2', 'hard_drugs_year2', 'age_year2', 'EDUCBAS_year0'))%>%
  rename(Age = 'age_year0', Hard_drugs = 'hard_drugs_year0', Edu = edu, Adh = adh)

write.xlsx(df_hiv_lm_model, paste0(working_directory, '/DataProcessed/cleaned data.xlsx'))





