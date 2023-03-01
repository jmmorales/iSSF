// a test

data {
  int<lower=0> N;        // Number of observations
  int<lower=1> n_steps; // Number of strata 
  int<lower=1> n_coef;   // Number of coefficients (log odds ratios) to estimate
  array[N] int<lower=1, upper=n_steps> step_id; // stratum identifier
  array[N] int<lower=0, upper=1> y;   // array of 0/1 outcomes
  matrix[N, n_coef] x;     // Matrix of regressors
  int n_group;             // number of observations in each group
  //int n_case[n_steps];   // number of cases in each group
}

parameters {
  vector[n_coef] b;
}

model {
  vector[N] xb; // linear predictor
  int ix;       // incrementing index
  
  // diffuse normal prior for log odds ratios
  b ~ normal(0, 1);
  xb = x * b;
  // log likelihood is a sum over each group
  ix = 1;
  for (ii in 1 : n_steps) {
    array[n_group] int y_g;
    vector[n_group] xb_g;

    y_g = segment(y, ix, n_group);
    xb_g = segment(xb, ix, n_group);
    
    //target += bernoulli_lpmf(1|log(xb_g[1]) - log_sum_exp(xb_g));
    target +=  bernoulli_lpmf(1|softmax(xb_g)[1]);
    ix = ix + n_group;
  }
}
