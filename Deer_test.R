# https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html

library(lubridate)
library(amt)
library(cmdstanr)
data("deer")
deer

summarize_sampling_rate(deer)

data("sh_forest")

ssf1 <- deer %>% steps_by_burst()

ssf1 <- ssf1 %>% random_steps(n_control = 10)

ssf1 <- ssf1 %>% extract_covariates(sh_forest) 

ssf1 <- ssf1 %>% 
  mutate(forest = factor(sh.forest, levels = 1:2, labels = c("forest", "non-forest")), 
         cos_ta = cos(ta_), 
         log_sl = log(sl_)) 

head(ssf1)

stan_dat <- list(N = nrow(ssf1), 
                 n_events = length(unique(ssf1$step_id_)),
                 n_coef = 1,
                 step_id = ssf1$step_id_,
                 y = as.numeric(ssf1$case_), 
                 x = matrix(as.numeric(ssf1$forest), 
                            nrow = nrow(ssf1), 1),
                 n_group = 11
)

mod <- cmdstan_model('issf_test.stan')

fit <- mod$sample(
  data = stan_dat,
  #seed = 123,
  chains = 4,
  parallel_chains = 4,
  #output_dir = "C:\\Users\\jm361n\\uruguay",
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200 # print update every 500 iters
)

#-------------------

stan_dat <- list(N = nrow(ssf1), 
                 n_grp = length(unique(ssf1$step_id_)),
                 n_coef = 1,
                 grp = ssf1$step_id_,
                 y = as.numeric(ssf1$case_), 
                 x = matrix(as.numeric(ssf1$forest), 
                            nrow = nrow(ssf1), 1),
                 n_group = 11,
                 n_case = 1
)

mod <- cmdstan_model('clogit.stan')

fit <- mod$sample(
  data = stan_dat,
  #seed = 123,
  chains = 4,
  parallel_chains = 4,
  #output_dir = "C:\\Users\\jm361n\\uruguay",
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200 # print update every 500 iters
)






matrix[N, n_coef] x;    // Matrix of regressors
int n_group[n_events];     // number of observations in each group
int n_case[n_events];      // number of cases in each group


m0 <- ssf1 %>% fit_clogit(case_ ~ forest + strata(step_id_))
m1 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl * cos_ta + strata(step_id_))
m2 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl + cos_ta + strata(step_id_))
summary(m0)
