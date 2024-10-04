data{
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of parameter
  vector[N] Y; // outcome variables
  matrix[N, K] X; // data matrix
  matrix[N_pre,2] priors_beta; // prios of beta
  real prior_e[2]; // prior for residuals
}
parameters {
  vector[K] beta; // parameters
  real<lower=0> sigma_e; // variance of residuals
  
}


model{
  // priors
  for(i in K){
    beta[i]~normal(priors_beta[i,1], priors_beta[i,2])
  }
  sigme_e~cauchy(prior_e[1], prior_e[2])
  
  // likelihood
  Y ~ normal(X*beta, sigma_e);
  
}