

library(tidyverse)
library(MASS)
library(mvtnorm)
working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project0'
## read data
df_fmri <- read.csv('BIOS6624-shuai/Project0/DataRaw/fmri_phys_func.csv')

## create beta and residual value data frame 
df_residual <- matrix(NA, nrow=300, ncol=15)
df_beta <- matrix(NA, nrow = 31, ncol = 15)
for(i in 1:15){
  f <- paste0('Y.',i,'~', paste(colnames(df_fmri)[17:46], collapse = '+'))
  fit <- lm(f, data = df_fmri)
  df_residual[,i] <- as.vector(residuals(fit))
  
}
for(i in 1:15){
  f <- paste0('Y.',i,'~', paste(colnames(df_fmri)[17:46], collapse = '+'))
  fit <- lm(f, data = df_fmri)
  df_beta[,i] <- as.vector(summary(fit)$coefficient[,1][1:31])
  
}

df_residual <- data.frame(df_residual)

df_significant <- df_residual<0.000111
df_cleaned <- df_significant[rowSums(df_significant == FALSE) != ncol(df_significant), ]

df_beta <- data.frame(df_beta)
colnames(df_beta) <- paste0('Y.',1:15)
rownames(df_beta) <- paste0('X.',0:30)

## print a table of significant predictors without any adjusted
get_estimate <- function(i){
  f <- paste0('Y.',i,'~', paste(colnames(df_fmri)[17:46], collapse = '+'))
  fit <- lm(f, data = df_fmri)
  df_estimate <- as.data.frame(summary(fit)$coefficients)%>%filter(`Pr(>|t|)`<0.05)
  df_estimate <- df_estimate%>%mutate('Lower confident interval' = Estimate-1.96*`Std. Error`,
                                      'Upper confident interval'= Estimate-1.96*`Std. Error`,
                                      Outcome = paste0('Y.',i))
  
  df_estimate$Predicator <- rownames(df_estimate)
  rownames(df_estimate) <- NULL
  df_estimate <- df_estimate[,c(7,8,1:6)]
  df_estimate <- df_estimate%>%filter(Predicator!='(Intercept)')
  return(df_estimate)
}
list_dfestimate <- lapply(1:15, get_estimate)
combined_df_unadjusted <- do.call(rbind, list_dfestimate)
combined_df_unadjusted[,-c(4:6)]%>%
  saveRDS(., paste0(working_directory, '/DataProcessed/estimate table without adjustement.RDS'))


## print a table of significant predictors with Bonferroni correlation
Bonferroni_pvalue <- 0.05/450
get_estimate <- function(i){
  f <- paste0('Y.',i,'~', paste(colnames(df_fmri)[17:46], collapse = '+'))
  fit <- lm(f, data = df_fmri)
  df_estimate <- as.data.frame(summary(fit)$coefficients)%>%filter(`Pr(>|t|)`<Bonferroni_pvalue)
  df_estimate <- df_estimate%>%mutate('Lower confident interval' = Estimate-1.96*`Std. Error`,
                                      'Upper confident interval'= Estimate-1.96*`Std. Error`,
                                      Outcome = paste0('Y.',i))
  
  df_estimate$Predicator <- rownames(df_estimate)
  rownames(df_estimate) <- NULL
  df_estimate <- df_estimate[,c(7,8,1:6)]
  df_estimate <- df_estimate%>%filter(Predicator!='(Intercept)')
  return(df_estimate)
}
list_dfestimate <- lapply(1:15, get_estimate)
combined_df_adjusted <- do.call(rbind, list_dfestimate)
combined_df_adjusted[,-c(4:6)]%>%saveRDS(., paste0(working_directory, '/DataProcessed/estimate table Bonferroni adjustement.RDS'))

## print a table of significant predictors with alternative approach
set.seed(123)
M_outcomes <- 15
N_obs <- 300
n_simulation <- 1000
mean_beta <- as.matrix(stack(df_beta)[,1])
### matrix calculation
X_mat <- as.matrix(cbind(X.0 = rep(1, 300), df_fmri[, 17:46]))
Y_mat <-  as.matrix(stack(df_fmri%>%dplyr::select(starts_with('Y')))[,1])
XTX_inXT <-  solve(t(X_mat)%*%X_mat)%*%t(X_mat)
cov_mat_residuals <- cov(df_residual)
Z <-  Matrix::bdiag(replicate(15, XTX_inXT, simplify = FALSE)) 
beta_hat <- data.frame(df_beta)%>%unlist()%>%as.vector()
cov_Y <- matrix(0, nrow = M_outcomes * N_obs, ncol = M_outcomes * N_obs)
for (j in 1:M_outcomes) {
  for (i in 1:N_obs) {
    index_j <- i + N_obs * (j - 1)
    cov_Y[index_j, index_j] <- cov_mat_residuals[j, j]
    for (k in 1:M_outcomes) {
      if (k != j) {
        index_k <- i + N_obs * (k - 1)
        cov_Y[index_j, index_k] <- cov_mat_residuals[j, k]
        cov_Y[index_k, index_j] <- cov_mat_residuals[j, k]  # symmetric
      }
    }
  }
}
cov_betas <- as.matrix(Z %*% cov_Y %*% t(as.matrix(Z)))

simulate_beta <- rmvnorm(n_simulation, mean=beta_hat, sigma=cov_betas) 

### remove the intercept 
simulate_beta_noint <- simulate_beta[, -c(seq(1, 465, by = 31))]
se_betas_noint <- sqrt(diag(cov_betas)[-c(seq(1, 465, by = 31))])
beta_hat_noint <- beta_hat[-c(seq(1, 465, by = 31))]
### define tau
critical <- rep(NA, n_simulation)
for(i in 1:n_simulation){
  critical[i] <- max((abs(simulate_beta_noint[i,] - beta_hat_noint))/se_betas_noint)
}
critical_value <- quantile(critical, 0.95 )
LB <-  beta_hat_noint - critical_value*se_betas_noint
UB <-  beta_hat_noint + critical_value*se_betas_noint
signif_ind <- which(LB > 0 | UB < 0)




df_with_bootstrap <- tibble(outcome = floor(signif_ind/30)+1,
       predictor = signif_ind%%30,
       estimat = beta_hat_noint[signif_ind],
       'Lower confident interval' = LB[signif_ind], 
       'Upper confident interval'= UB[signif_ind],
       )
colnames(df_with_bootstrap) <- colnames(combined_df_unadjusted)[-c(4:6)]
df_with_bootstrap%>%saveRDS(., paste0(working_directory, '/DataProcessed/estimate table with alternative adjustment.RDS'))

