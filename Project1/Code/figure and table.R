library(tidyverse)
library(table1) 
library(openxlsx)

working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project1'
df_cleaned <- read.xlsx(paste0(working_directory, '/DataProcessed/cleaned data.xlsx'))
df_cleaned %>%
  summarise_all(~ mean(is.na(.)) * 100) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "MissingPercentage")

