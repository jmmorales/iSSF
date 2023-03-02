data {
  int<lower=0> N;        // Number of observations
  int<lower=1> n_steps; // Number of strata 
  int<lower=1> n_coef;   // Number of coefficients (log odds ratios) to estimate
  array[N] int<lower=1, upper=n_steps> step_id; // stratum identifier
  array[N] int<lower=0, upper=1> y;   // array of 0/1 outcomes
  matrix[N, n_coef] x;     // Matrix of regressors
  int<lower=1> n_group;             // number of observations in each group
  int<lower=1> J;          // number individuals
  array[N] int<lower=1, upper=J> ind_id; // individual id
}

parameters {
  vector[n_coef] beta;       // slopes hyper-priors
  //vector[n_coef * J] b; // slope coefficients by individual
  vector[n_coef] b[J];
  vector<lower=0>[n_coef] sigma_p; // sd for slopes
  corr_matrix[n_coef] Omega; // correlation matrix
}

// transformed parameters{
//   matrix[J, n_coef] b_m = to_matrix(b, J, n_coef);  // coeffs potrero+estb  
// }

model {
  matrix[n_group, n_coef] xg;
  vector[n_coef] bg;
  vector[n_group] xb_g; // linear predictor
  int ix;       // incrementing index
 
  beta ~ normal(0, 1);
  Omega ~ lkj_corr(2);
  sigma_p ~ exponential(1);
  b ~ multi_normal(beta, quad_form_diag(Omega, sigma_p));

  ix = 1;
  for (i in 1 : n_steps) {
    xg = block(x, ix, 1, n_group, n_coef);
//    bg = to_vector(b_m[ind_id[i]]);
    bg = b[ind_id[i]];
    xb_g = xg * bg;

    target +=  bernoulli_lpmf(1|softmax(xb_g)[1]);
    ix = ix + n_group;
  }
}
