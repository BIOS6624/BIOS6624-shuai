library(tidyverse)
library(plyr)
library(parallel)

## read cleaned data
df_filtered <- readRDS(paste0(working_directory, '/DataProcessed/cleaned data.RDS'))
df_filtered$gender <- relevel(df_filtered$gender, ref = 'Female')
## fit a logistic model
model <- glm( mortstat~gender, data = df_filtered, family = binomial(link = 'logit'))
summary(model)
coef <- coef(model)
b0 <- coef[1]
b1 <- coef[2]
p_male <- 1/(1+exp(-(b0+b1)))
p_female <- 1/(1+exp(-(b0)))
p_control <- (p_male+p_female)/2
p_control <- unname(p_control)
## grid search to find b2
calculate_p_diff <- function(b2){
  p_male <- 1/(1+exp(-(b0+b1+b2)))
  p_female <- 1/(1+exp(-(b0+b2)))
  p_tret <- (p_male+p_female)/2
  return(unname(p_control-p_tret))
}
b2_vec <- seq(-1,1,by = 0.00001)
b2_p_diff <- sapply(b2_vec, calculate_p_diff)
b2 <- b2_vec[which.min(abs(b2_p_diff-0.07))]


## aim 1 simulation
simulate_power <- function(sample_size, iterations = 10000, alpha = 0.05) {
  significant_results <- 0
  for (i in 1:iterations) {
    # simulate data
    gender = sample(rep(c(0, 1), each = sample_size/2), sample_size, replace = T)
    treatment = sample(rep(c(0, 1), each = sample_size/2), sample_size, replace = T)
    xbeta <- b0 + b1*gender + b2*treatment
    prob <- 1/(1+exp(-xbeta))
    mortstat <- rbinom(sample_size,1,prob = prob)
    data <- data.frame(mortstat, gender, treatment)
    # fit model
    model <- glm( mortstat~gender+treatment,data = data,  family = binomial(link = 'logit'))
    p_value <- summary(model)$coefficients[3, 4]
    if (p_value < alpha) significant_results <- significant_results + 1
  }
  
  # Calculate power as proportion of significant results
  power <- significant_results / iterations
  return(power)
}
## multiple thread running
cl <- makeCluster(detectCores())
clusterEvalQ(cl,{
  set.seed(1234)
  NULL
})
clusterExport(cl, "simulate_power")
clusterExport(cl, "b0")
clusterExport(cl, "b1")
clusterExport(cl, "b2")
Sample_Size <- seq(1450,1550, by = 10)
power <- parSapply(cl, Sample_Size, simulate_power)
results <- tibble(Sample_Size, power)
stopCluster(cl)
## plot sample size and power line chart

results%>%
  ggplot(aes(x = Sample_Size, y = power))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.8)+
  ggtitle("Aim 1: line chart of sample size and power")+xlab('Sample size')
ggsave('Figure/Aim 1 line chart of sample size and power.png',dpi = 900, height = 10, width = 10, unit = 'in')
results%>%saveRDS('DataProcessed/simulation results aim1.RDS')
## aim 2 simulation start here

sample_size_aim2 <- 1510
## function of simulating aim2
simulate_power_aim2 <- function(b3, iterations = 10000, alpha = 0.05) {
  significant_results <- 0
  for (i in 1:iterations) {
    # simulate data
    gender <- sample(rep(c(0, 1), each = sample_size_aim2/2), sample_size_aim2, replace = T)
    treatment <-  sample(rep(c(0, 1), each = sample_size_aim2/2), sample_size_aim2, replace = T)
    gender_treatment <- gender*treatment
    xbeta <- b0 + b1*gender + b2*treatment+b3*gender_treatment
    prob <- 1/(1+exp(-xbeta))
    mortstat <- rbinom(sample_size_aim2,1,prob = prob)
    data <- data.frame(mortstat, gender, treatment, gender_treatment)
    # fit model
    model <- glm( mortstat~gender+treatment+gender_treatment,data = data,  family = binomial(link = 'logit'))
    p_value <- summary(model)$coefficients[4, 4]
    if (p_value < alpha) significant_results <- significant_results + 1
  }
  
  # Calculate power as proportion of significant results
  power <- significant_results / iterations
  return(power)
}
## multitreading aim2
cl <- makeCluster(detectCores())
clusterEvalQ(cl,{
  set.seed(1234)
  NULL
})
clusterExport(cl, "b0")
clusterExport(cl, "b1")
clusterExport(cl, "b2")
clusterExport(cl, "simulate_power_aim2")
clusterExport(cl, "sample_size_aim2")
b3 = seq(-1,0, by =0.01)
power_aim2 <- parSapply(cl, b3, simulate_power_aim2)
results_aim2 <- tibble(b3, power_aim2)
stopCluster(cl)
results_aim2%>%saveRDS('DataProcessed/simulation results aim2.RDS')
colnames(results_aim2)[2] <- 'power' 
results_aim2%>%
  ggplot(aes(x = b3, y = power))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.8)+
  ggtitle("Aim 2: line chart of b3 and power")+xlab('Coefficient of interaction term')
ggsave('Figure/Aim 2 line chart of b3 and power.png',dpi = 900, height = 10, width = 10, unit = 'in')




