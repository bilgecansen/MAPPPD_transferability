
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
  vector[L+1] lz;
  
  sd_mu = sd(mu);
  Rsq = sd_mu/(sd_mu + sigma);
  
  lz[1] = X_pred[1,4];
  
  for (i in 1:L) {
    mu_pred[i] = X_pred[i,1:3]*beta[1:3] + lz[i]*beta[4] + alpha;
    lz[i+1] = lz[i] + mu_pred[i];
  }
}
