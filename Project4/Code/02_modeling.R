
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
