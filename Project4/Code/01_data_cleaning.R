rm(list = ls())

library(tidyverse);library(lme4);library(parallel)


## set working directory
#working_directory <- '/home/shuai/BIOS6624-shuai/Project4'
working_directory <- 'C:/Users/zhu-s/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project4'
setwd(working_directory)

df <- read.csv('DataRaw/MVPA_seasonal.csv')%>%as_tibble()
df_lmer <- df%>%
  mutate(sind = day/365,
         sin_s = sin(2*pi*sind),
         cos_s = cos(4*pi*sind))






