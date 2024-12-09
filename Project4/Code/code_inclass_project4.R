

library(tidyverse); library(geepack); library(lme4); library(ggplot2)

## read the data
df_Q1 <- 
  read.csv('DataRaw/MVPA_seasonal.csv')%>%as_tibble() %>% 
  mutate(sind = day/365)



## covariance
uid <- unique(df_Q1$ID)
nid <- length(uid)
Y_wide <- matrix(NA, nid, 122)
for(i in 1:nid){
  df_Q1_i <- df_Q1 %>% filter(ID == uid[i])
  Y_wide[i,1:nrow(df_Q1_i)] <- df_Q1_i$Y
}
cor_Y <- cor(Y_wide,use="pairwise.complete.obs")


uday <- unique(df_Q1$day)
fields::image.plot(uday, uday,cor_Y, xlab="Day of Year (u)", ylab="Day of Year (v)",
                   main=expression(Cov(Y[i](u), Y[i](v))))


## missing data
df_miss_plt_Q1 <- 
  df_Q1 %>% 
  complete(ID,day) %>% 
  group_by(ID) %>% 
  mutate(trt=trt[1],
         first_miss = ifelse(any(is.na(Y)), day[min(which(is.na(Y)))],365),
         Y_lag = c(NA, Y[-n()])) %>% 
  ungroup() %>% 
  mutate(is_missing = as.numeric(day == first_miss)) %>%
  filter(day <= first_miss | is_missing == 1) 

glm_miss_Q1 <- glm(is_missing ~ Y_lag + trt + day, data=df_miss_plt_Q1, family=binomial)
summary(glm_miss_Q1)


df_Q1 <- 
  df_Q1 %>% 
  mutate(sin_s = sin(2*pi*sind))

## fit LMM and GEE
library(geepack)
fit_gee_exc <- geeglm(Y ~ trt + sin_s + trt:sin_s, data=df_Q1, family=poisson, 
                      corstr="exchangeable", id=ID)
fit_gee_ar1 <- geeglm(Y ~ trt + sin_s + trt:sin_s, data=df_Q1, family=poisson, 
                      corstr="ar1", id=ID)
fit_gee_ind <- geeglm(Y ~ trt + sin_s + trt:sin_s, data=df_Q1, family=poisson, 
                      corstr="independence", id=ID)

summary(fit_gee_exc)
summary(fit_gee_ar1)
summary(fit_gee_ind)




library(lme4)
df_Q1 <- 
  df_Q1 %>% 
  mutate(sin_s = sin(2*pi*sind),
         cos_s = cos(4*pi*sind))

fit_ri <- glmer(Y ~ trt + (1|ID), data=df_Q1, family=poisson)

fit_ri_rs <- glmer(Y ~ trt + sin_s + sin_s:trt + (1+sind|ID), 
                   data=df_Q1, family=poisson)

fit_correct <- glmer(Y ~ trt + sin_s + sin_s:trt + 
                       (sin_s-1|ID) + (cos_s-1|ID), 
                   data=df_Q1, family=poisson)
summary(fit_correct)

summary(fit_ri_rs)




## fit the model to the data
df_Q1 <- 
  df_Q1 %>% 
  mutate(cos_s = cos(4*pi*sind))
time_st <- Sys.time()
fit_glmm <- glmer(Y ~ trt*sin_s + (sin_s - 1|ID) + (cos_s - 1|ID), 
                  family=poisson, data=df_Q1)
time_end <- Sys.time()
difftime(time_end,time_st, units="mins")
summary(fit_glmm)



VarCorr(fit_glmm)

## extract parameters used for simulation
s2_b1 <- data.frame(VarCorr(fit_glmm))$vcov[1]
s2_b2 <- data.frame(VarCorr(fit_glmm))$vcov[2]
beta_vec <- fit_glmm@beta

## population parameters
N <- length(unique(df_Q1$ID))
sind <- sort(unique(df_Q1$sind))

## define the true effect on a fixed grid
# grid to evaluate effect
n_spred <- 1000
sind_pred <- seq(0,1,len=n_spred)
# true effect
true_effect <- beta_vec[2] + beta_vec[4]*sin(2*pi*sind_pred)


## set up container for results
nsim <- 500
results_arr <- 
  array(NA_real_, dim=c(nsim, n_spred, 3, 3),
        dimnames=list("simulation"=1:nsim,
                   "sind"=sind_pred,
                   "model"=c("correct","misspecified_re","misspecified_mean_re"),
                   "metric"=c("MSE","bias","coverage")))

## design matrix for all simulations
df_sim <- expand.grid(sind=sind,
                      ID=1:N)
df_sim <- 
  df_sim %>% 
  mutate(sin_s = sin(2*pi*sind),
         cos_s = cos(4*pi*sind))

## progress bar
pb <- txtProgressBar(0, nsim, style=3)
for(i in 1:nsim){
  ## simulate data
  bi1_i <- rnorm(N, mean=0, sd=sqrt(s2_b1))
  bi2_i <- rnorm(N, mean=0, sd=sqrt(s2_b2))
  df_re_trt_i <- data.frame("ID"=1:N, "bi1" = bi1_i,"bi2" = bi2_i,"trt"=rep(c(0,1),each=N/2))
  df_i <- 
    df_sim %>% 
    left_join(df_re_trt_i, by="ID") %>% 
    mutate(eta_ij = beta_vec[1] + beta_vec[2]*trt + beta_vec[3]*sin_s + beta_vec[4]*sin_s*trt + 
                    bi1*sin_s + bi2*cos_s)
  df_i$Y <- vapply(df_i$eta_ij, function(x) rpois(1, exp(x)),numeric(1))  
    
  ## fit the models
  fit_correct_i <- glmer(Y ~ trt*sin_s + (sin_s - 1|ID) + (cos_s - 1|ID), 
                         family=poisson, data=df_i)
  # fit_misspecified_re_i <- glmer()
  # fit_misspecified_mean_re_i <- glmer()
  
  ## get estimated coefficients
  # \hat{treatment effect}(s)
  eta_hat_correct_i <- fit_correct_i@beta[2] + fit_correct_i@beta[4]*sin(2*pi*sind_pred)
  
  # test something
  Xmat <- cbind(rep(0, n_spred), 
                       rep(1, n_spred),
                       rep(0, n_spred),
                       sin(2*pi*sind_pred))
  vbeta_hat_correct_i <- vcov(fit_correct_i)
  SE_correct_i <- sqrt(diag(Xmat %*% vbeta_hat_correct_i %*% t(Xmat))  )
  LB_correct_i <- eta_hat_correct_i - qnorm(0.975)*SE_correct_i
  UB_correct_i <- eta_hat_correct_i + qnorm(0.975)*SE_correct_i
  
  ## store results
  results_arr[i,,"correct",1] <- (eta_hat_correct_i - true_effect)^2
  results_arr[i,,"correct",2] <- eta_hat_correct_i - true_effect
  results_arr[i,,"correct",3] <- as.numeric(LB_correct_i < true_effect & UB_correct_i > true_effect)
                                  
  setTxtProgressBar(pb,i)
}



