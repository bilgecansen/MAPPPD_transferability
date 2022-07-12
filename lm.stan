
data {
  int<lower=0> N;
  int<lower=0> L;
  vector[N] X;
  vector[L] X_pred;
  vector[N] y;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] mu;
  mu = X*beta + alpha;
}

model {
  y ~ normal(mu, sigma);
}

generated quantities {
  real sd_mu;
  real Rsq;
  //vector[N] y_new;
  vector[L] mu_pred;
  sd_mu = sd(mu);
  Rsq = sd_mu/(sd_mu + sigma);
  //for (i in 1:N) {
    //y_new[i] = normal_rng(mu[i], sigma);
  //}
  mu_pred = X_pred*beta + alpha;
}