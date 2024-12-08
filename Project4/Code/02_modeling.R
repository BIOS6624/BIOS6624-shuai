
## fit the correct glmer model with random slope
fit.glmm <- glmer(Y ~ trt*sin_s + (sin_s - 1|ID) + (cos_s - 1|ID), 
                  family=poisson, data=df_Q1)
summary(fit.glmm)

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
nsim <- 5
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


store_results <- function(Xmat, vbeta_hat, model_name){
  SE_correct_i <- sqrt(diag(Xmat %*% vbeta_hat %*% t(Xmat)) )
  LB_correct_i <- eta_hat_correct_i - qnorm(0.975)*SE_correct_i
  UB_correct_i <- eta_hat_correct_i + qnorm(0.975)*SE_correct_i
  
  ## store results
  results_arr[i,,model_name,1] <- (eta_hat_correct_i - true_effect)^2
  results_arr[i,,model_name,2] <- eta_hat_correct_i - true_effect
  results_arr[i,,model_name,3] <- as.numeric(LB_correct_i < true_effect & UB_correct_i > true_effect)
}

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
  fit_misspecified_re_i <- glmer(Y ~ trt*sin_s + (1+sin_s|ID) + (1+cos_s|ID), 
                                 family=poisson, data=df_i)
  fit_misspecified_mean_re_i <- glmer(Y ~ trt+sin_s + (sin_s|ID) + (cos_s|ID), 
                                      family=poisson, data=df_i)
  
  ## get estimated coefficients
  # \hat{treatment effect}(s)
  eta_hat_correct_i <- fit_correct_i@beta[2] + fit_correct_i@beta[4]*sin(2*pi*sind_pred)
  
  # test something
  ### correct model
  Xmat <- cbind(rep(0, n_spred), 
                rep(1, n_spred),
                rep(0, n_spred),
                sin(2*pi*sind_pred))
  vbeta_hat_correct_i <- vcov(fit_correct_i)
  store_results(Xmat, vbeta_hat_correct_i, "correct")
  ### misspecified random effect model

  vbeta_hat_misspecified_re_i <- vcov(fit_misspecified_re_i)
  store_results(Xmat, vbeta_hat_misspecified_re_i, "misspecified_re")
  ### misspecified mean random effect model
  Xmat_no_inter <- cbind(rep(0, n_spred), 
                rep(1, n_spred),
                rep(0, n_spred))
  vbeta_hat_misspecified_mean_re_i <- vcov(fit_misspecified_mean_re_i)
  store_results(Xmat_no_inter, vbeta_hat_misspecified_re_i, "misspecified_mean_re")
  
  setTxtProgressBar(pb,i)
}



