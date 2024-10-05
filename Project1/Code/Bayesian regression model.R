### loading data and library
library(openxlsx)
library(tidyverse)
library(rstan)
set.seed(12345)
working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project1'
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

bays_noninfo_data <- list()
bays_noninfo_model <- list()
bays_vague_data <- list()
bays_vague_model <- list()


for (i in 1:4 ){
  ### non informative priors
  bays_noninfo_data[[i]] <- list(N = N, Y = Y[,i], X = X[[i]], K = K,
                                 priors_beta = matrix(c(rep(0,K), rep(10^7,K)), nrow = K),  prior_e = c(0, 2.5) )
  bays_noninfo_model[[i]] <- stan(file = paste0(working_directory, '/Code/Bayesian regression.stan'),
                                  data = bays_noninfo_data[[i]],
                                  iter = 2000,             # Number of iterations
                                  warmup = 1000,
                                  chains = 4,              # Number of MCMC chains
                                  seed = 123)
  ### vague priors
  bays_vague_data[[i]] <- list(N = N, Y = Y[,i], X = X[[i]], K = K,
                               priors_beta = matrix(c(rep(0,K), rep(10^6,K)), nrow = K),prior_e = c(0, 2.5))
  bays_vague_model[[i]] <- stan(file = paste0(working_directory, '/Code/Bayesian regression.stan'),
                                  data = bays_vague_data[[i]],
                                  iter = 2000,             # Number of iterations
                                  warmup = 1000,
                                  chains = 4,              # Number of MCMC chains
                                  seed = 123)
  
}

bays_noninfo_model%>%saveRDS(paste0(working_directory,'/DataProcessed/bays_noninfo_model.RDS'))
bays_vague_model%>%saveRDS(paste0(working_directory, '/DataProcessed/bays_vague_model.RDS'))

traceplot(bays_noninfo_model[[1]], pars= c('beta', 'sigma_e'))
traceplot(bays_noninfo_model[[1]], pars= c('beta', 'sigma_e'))


bays_noninfo_model <- readRDS(paste0(working_directory,'/DataProcessed/bays_noninfo_model.RDS'))
bays_vague_model <- readRDS(paste0(working_directory, '/DataProcessed/bays_vague_model.RDS'))

df_noninfo_estimate <- data.frame()
df_vague_estimate <- data.frame()
for(i in 1:4){
  df_noninfo_estimate <- rbind(df_noninfo_estimate, summary(bays_noninfo_model[[i]])$summary[2,c('mean', '2.5%', '97.5%')])
  df_vague_estimate <- rbind(df_vague_estimate, summary(bays_vague_model[[i]])$summary[2,c('mean', '2.5%', '97.5%')])
}
colnames(df_noninfo_estimate) <- c('Estimate', '2.5%', '97.5%')
colnames(df_vague_estimate) <- c('Estimate', '2.5%', '97.5%')
df_noninfo_estimate%>%saveRDS(paste0(working_directory,'/DataProcessed/df_noninfo_estimate.RDS'))
df_vague_estimate%>%saveRDS(paste0(working_directory,'/DataProcessed/df_vague_estimate.RDS'))

