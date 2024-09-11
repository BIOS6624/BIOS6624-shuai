library(MASS)
N <- 100
X <- mvrnorm(N, mu = c(0,0), Sigma = matrix(c(1,0.95,0.95,1),2,2))
beta <- c(0.5,1)
sig2_e <- 1
set.seed(100)
alpha <- 0.05
Y <- X%*%beta+rnorm(N,0,sqrt(sig2_e))
fit_obs <- lm(Y~X)

## bootstrap
beta_hat <- coef(fit_obs)
vbeta_hat <- vcov(fit_obs)
B <- 10000
nu_vec <- rep(NA,B)

for (b in 1:B){
  beta_hat_b <- mvrnorm(n = 1, mu = beta_hat, Sigma = vbeta_hat)
  nu_vec[b] <- max(abs(beta_hat_b[-1]-beta_hat[-1])/sqrt(diag(vbeta_hat)))
}

hist(nu_vec)
tau <- quantile(nu_vec, 1-alpha)
LB <- round(beta_hat[-1]-tau*sqrt(diag(vbeta_hat))[-1],2)
UB <- round(beta_hat[-1]+tau*sqrt(diag(vbeta_hat))[-1],2)
