// moddified from: SSF analysis of fisher using Stan 
// "S. Muff, J. Signer, J. Fieberg"


data {
  
  int<lower=1> N; // no data points
  int<lower=1> I; // no steps (over all individuals)
  int<lower=1> J; // no individuals
  
  int<lower=0> y[N]; // response
  int K; // number of predictors (without intercept)
  matrix[N, K] x; // matrix of predictors
  //real forest[N]; // covariate
  int<lower=1, upper=I> stepid[N]; // step id
  int<lower=1, upper=J> indid[N]; // individual id
  
}

parameters {
  vector[I] a_re; // RE for steps
  vector[K] beta_p[J]; // slope coefficients by individual
  vector<lower=0>[K] sigma_p; // sd for slopes
  vector[K] beta; // slopes hyper-priors
  corr_matrix[K] Omega; // correlation matrix
}


model {
  vector[N] mu;
  
  // priors
  a_re ~ normal(0, 1000000);
  beta ~ normal(0, 1);
  Omega ~ lkj_corr(2);
  sigma_p ~ exponential(1);

  beta_p ~ multi_normal(beta, quad_form_diag(Omega, sigma_p));
  
  
  //likelihood
  for (i in 1:N) {
    mu[i] =  a_re[stepid[i]] + x[i] * (beta_p[indid[i]]);
    y[i] ~ poisson_log(mu);
  }
}