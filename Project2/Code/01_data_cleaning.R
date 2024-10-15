## load library
library(tidyverse)


working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project2'

df_actup <- readRDS(paste0(working_directory,'/DataRaw/BIOS6624_proj2_data.rds'))%>%as_tibble()


df_filtered<- df_actup%>%filter(age_years_interview >=60 & age_years_interview <= 75)




