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

model {
  vector[N] xb; // linear predictor
  real ll; // log likelihood
  int pos; // incrementing index
  
  // diffuse normal prior for log odds ratios
  b ~ normal(0, 1);
  xb = x * b;
  // log likelihood is a sum over each group
  pos = 1;
  for (ii in 1 : n_steps) {
    array[n_group] int y_g;
    vector[n_group] xb_g;
    //real tmp;
    
    y_g = segment(y, pos, n_group);
    xb_g = segment(xb, pos, n_group);

    ll = multinomial_lpmf(y_g | softmax(xb_g));
    target += ll;
    pos = pos + n_group;
  }
}
