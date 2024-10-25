## load library
library(tidyverse)
library(table1)
## set the working directory
working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project2'
setwd(working_directory)
## read raw data
df_actup <- readRDS(paste0(working_directory,'/DataRaw/BIOS6624_proj2_data.rds'))%>%as_tibble()

# filter data

df_filtered_age <- df_actup%>%
  filter(age_years_interview >=60 & age_years_interview <= 75)

first_quantile <- quantile(df_filtered_age$TMIMS_mean,0.25, na.rm = T)
df_filtered <- df_filtered_age%>%filter(TMIMS_mean<= first_quantile)%>%
  select(c("SEQN", "TMIMS_mean", "gender", "mortstat", "permth_exm"))%>%
  filter(!(mortstat == 0 & permth_exm < 7*12))%>%
  mutate(mortstat = case_when(mortstat= 1 & permth_exm > 7*12 ~ 0,
                              .default = mortstat))


# Making table1
# Rename variables for better labeling in the table
#df_filtered$mortstat <- factor(df_filtered$mortstat, levels = c(0,1), labels = c("Alive", "Deceased"))
df_filtered%>%saveRDS(paste0(working_directory, '/DataProcessed/cleaned data.RDS'))
# Define labels for variables
label(df_filtered$TMIMS_mean) <- "Mean Time in Minutes"
label(df_filtered$gender) <- "Gender"
label(df_filtered$mortstat) <- "Mortality Status"
label(df_filtered$permth_exm) <- "Months of Follow-Up"

# Create the table1 summary
table1(~ TMIMS_mean + mortstat + permth_exm|gender, data = df_filtered)

