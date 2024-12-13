df_Q1 <- df_lmer
## fit the correct glmer model with random slope
if(file.exists('DataProcessed/fit_glmm.RDS')){
  fit_glmm <- readRDS('DataProcessed/fit_glmm.RDS')
}else{
  fit_glmm <- glmer(Y ~ trt*sin_s + (sin_s - 1|ID) + (cos_s - 1|ID), family=poisson, data=df_Q1)
  saveRDS(fit_glmm,'DataProcessed/fit_glmm.RDS')
}


## save the random effect and beta
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

### define a function to store model result to results_arr
store_results <- function(results_arr,fit, model_name,i){
  if(model_name == 'misspecified_mean_re'){
    eta_hat <- fit_mis_all_i@beta[2] + fit_mis_all_i@beta[4]
    Xmat <- cbind(rep(0, n_spred), 
                   rep(1, n_spred),
                   rep(0, n_spred),
                   sind_pred)
  }else{
    Xmat <- cbind(rep(0, n_spred), 
                  rep(1, n_spred),
                  rep(0, n_spred),
                  sin(2*pi*sind_pred))
    eta_hat <- fit@beta[2] + fit@beta[4]*sin(2*pi*sind_pred)
  }
  vbeta_hat <- vcov(fit)
  SE_correct_i <- sqrt(diag(Xmat %*% vbeta_hat %*% t(Xmat)) )
  LB_correct_i <- eta_hat - qnorm(0.975)*SE_correct_i
  UB_correct_i <- eta_hat + qnorm(0.975)*SE_correct_i
  
  ## store results
  results_arr[i,,model_name,1] <- (eta_hat - true_effect)^2
  results_arr[i,,model_name,2] <- eta_hat - true_effect
  results_arr[i,,model_name,3] <- as.numeric(LB_correct_i < true_effect & UB_correct_i > true_effect)
  results_arr
}

## progress bar
set.seed(123)
start <- Sys.time()
start
pb <- txtProgressBar(0, nsim, style=3)

#for(i in 1:nsim){
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
  fit_correct_i <- glmer(Y ~ trt*sin_s + (sin_s - 1|ID) + (cos_s - 1|ID), family=poisson, data=df_i)
  fit_misspecified_re_i <- glmer(Y ~ trt*sin_s + (1+sind|ID), family=poisson, data=df_i)
  fit_misspecified_mean_re_i <- glmer(Y ~ trt*sind + (1+sind|ID), family=poisson, data=df_i)
  ## define Xmat

  ## get estimated coefficients
  # \hat{treatment effect}(s)

  # test something
  ### correct model
  results_arr <- store_results(results_arr, fit_correct_i, 'correct', i)
  results_arr <- store_results(results_arr, fit_misspecified_re_i,"misspecified_re",i)
  results_arr <- store_results(results_arr, fit_misspecified_mean_re_i, "misspecified_mean_re",i)

  setTxtProgressBar(pb,i)
}
end <- Sys.time()
end-start
saveRDS(results_arr, 'DataProcessed/results_arr.RDS')
results_arr <- readRDS('DataProcessed/results_arr.RDS')
res_df <- as.data.frame.table(results_arr)%>%
  group_by(model,metric,sind)%>%
  summarise(value = mean(Freq))%>%ungroup()%>%
  mutate(sind = as.numeric(as.character(sind)))


p1 <- res_df%>%filter( metric == 'MSE')%>%ggplot()+
  geom_line(aes(x = sind, y = value, group = model, color = model))+ggtitle("MSE") 
p2 <- res_df%>%filter( metric == 'bias')%>%ggplot()+
  geom_line(aes(x = sind, y = value, group = model, color = model))+ggtitle("Bias") 
p3 <- res_df%>%filter( metric == 'coverage')%>%ggplot()+
  geom_line(aes(x = sind, y = value, group = model, color = model))+ggtitle("95 % coverage") 

png('Figure/result plot.png', width = 6, height = 8, units = 'in',res = 300)
gridExtra::grid.arrange(p1,p2,p3)
dev.off()
