// moddified from: SSF analysis of fisher using Stan 
// "S. Muff, J. Signer, J. Fieberg"


data {
  int<lower=1> N;          // number data points
  int<lower=1> n_steps;    // number strata 
  int<lower=1> n_coef;             // number of predictors (without intercept)
  array[N] int<lower=1, upper=n_steps> step_id; // step id
  array[N] int<lower=0, upper=1> y;   // array of 0/1 outcomes
  matrix[N, n_coef] x;    // matrix of predictors
//  int<lower=1> J;          // number individuals
  int<lower=1> n_group;    // number of observations in each group
//  array[N] int<lower=1, upper=J> indid; // individual id
}

parameters {
  vector[n_steps] a_re; // RE for steps
//  array[J] vector[n_coef] beta_p; // slope coefficients by individual
//  vector<lower=0>[n_coef] sigma_p; // sd for slopes
  vector[n_coef] beta; // slopes hyper-priors
//  corr_matrix[n_coef] Omega; // correlation matrix
}


model {
  vector[N] mu;
  
  // priors
  a_re ~ normal(0, 1000000);
  beta ~ normal(0, 1);
//  Omega ~ lkj_corr(2);
//  sigma_p ~ exponential(1);

//  beta_p ~ multi_normal(beta, quad_form_diag(Omega, sigma_p));
  
  
  //likelihood
  for (i in 1:N) {
    mu[i] =  a_re[step_id[i]] + x[i] * beta;
    //mu[i] =  a_re[step_id[i]] + x[i] * (beta_p[indid[i]]);
    y[i] ~ poisson_log(mu[i]);
  }
}
