
data {
  int<lower=0> N;
  int<lower=0> M;
  int<lower=0> L;
  matrix[N,M] X;
  matrix[L,M] X_pred;
  vector[N] y;
}

parameters {
  real alpha;
  vector[M] beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] mu;
  mu = X*beta + alpha; 
}

model {
  alpha ~ normal(0,1);
  y ~ normal(mu, sigma);
}

generated quantities {
  real sd_mu;
  real Rsq;
  vector[L] mu_pred;
  
  sd_mu = sd(mu);
  Rsq = sd_mu/(sd_mu + sigma);
  
  mu_pred = X_pred*beta + alpha;
}
