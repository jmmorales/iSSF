// clogit.stan
// Conditional logistic regression
// (the same likelihood calculation as used by survival::clogit in R and
// clogit in Stata)
// David C Muller

functions {
  // recursive function to evaluate the denominator of the conditional likelihood
  real cl_denom(int N_g, int D_g, vector xb);
  real cl_denom(int N_g, int D_g, vector xb) {
    real res;
    if (N_g < D_g) {
      return 0;
    }
    if (D_g == 0) {
      return 1;
    }
    res = cl_denom(N_g-1, D_g, xb) + exp(log(cl_denom(N_g-1, D_g-1, xb)) + xb[N_g]);
    return res;
  }
}

data {
  int<lower=0> N;      // Number of observations
  int<lower=1> n_grp;  // Number of groups 
  int<lower=1> n_coef; // Number of coefficients (log odds ratios) to estimate
  int<lower=1, upper=n_grp> grp[N]; // stratum/group identifier
  int<lower=0, upper=1> y[N]; // array of 0/1 outcomes
  matrix[N, n_coef] x; // Matrix of regressors
  int n_group;  // number of observations in each group
  int n_case;   // number of cases in each group
}

parameters {
  vector[n_coef] b;
}

// transformed parameters {
//   vector[n_coef] oddsratio;
//   oddsratio = exp(b);// }

// }

model {
  vector[N] xb; // linear predictor
  real ll;      // log likelihood
  int pos;      // incrementing index
  
// diffuse normal prior for log odds ratios
  b ~ normal(0, 1);
  xb = x * b;
  // log likelihood is a sum over each group
  pos = 1;
  for (ii in 1:n_grp) {
    int y_g[n_group];
    vector[n_group] xb_g;
    y_g = segment(y, pos, n_group);
    xb_g = segment(xb, pos, n_group);
    ll = dot_product(to_vector(y_g), xb_g) - log(cl_denom(n_group, n_case, xb_g));
    target += ll;
    pos = pos + n_group;
  }
}