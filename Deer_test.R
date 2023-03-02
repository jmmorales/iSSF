# https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html

library(lubridate)
library(amt)
library(cmdstanr)

data("deer")
deer

summarize_sampling_rate(deer)

data("sh_forest")

ssf1 <- deer %>% steps_by_burst()

ncontrol = 9
ssf1 <- ssf1 %>% random_steps(n_control = ncontrol)
ssf1 <- ssf1 %>% extract_covariates(sh_forest) 
ssf1 <- ssf1 %>% 
  mutate(forest = factor(sh.forest, levels = 1:2, labels = c("forest", "non-forest")), 
         cos_ta = cos(ta_), 
         log_sl = log(sl_)) 

head(ssf1)

# simple model -----------------------------------------------------------------

m0 <- ssf1 %>% fit_clogit(case_ ~ forest + strata(step_id_))

# Stan clogit version
stan_dat0 <- list(N = nrow(ssf1), 
                 n_steps = length(unique(ssf1$step_id_)),
                 n_coef = 1,
                 step_id = ssf1$step_id_,
                 y = as.numeric(ssf1$case_), 
                 x = matrix(as.numeric(ssf1$forest), 
                            nrow = nrow(ssf1), 1),
                 n_group = ncontrol + 1
)

mod <- cmdstan_model('issf_test.stan')

fit0 <- mod$sample(
  data = stan_dat0,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200
)

modclogit <- cmdstan_model('clogit.stan')

fit0cl <- modclogit$sample(
  data = stan_dat0,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200
)


# Stan multinomial version
modmulti <- cmdstan_model('issf_multi.stan')

fit0multi <- modmulti$sample(
  data = stan_dat0,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200
)

coef(m0)

fit0cl$time()$total
fit0cl$summary()$mean[2]

fit0multi$time()$total
fit0multi$summary()$mean[2]

#-------------------------------------------------------------------------------

m1 <- fit_issf(case_ ~ forest + 
             sl_ + log_sl + 
             strata(step_id_), model = TRUE, data = ssf1)


x = model.matrix(~ forest + sl_ +  log_sl , data = ssf1)
x = x[, 2:ncol(x)]

stan_dat1 <- list(N = nrow(ssf1), 
                 n_steps = length(unique(ssf1$step_id_)),
                 n_coef = ncol(x),
                 step_id = ssf1$step_id_,
                 y = as.numeric(ssf1$case_), 
                 x = x,
                 n_group = ncontrol + 1
)


fit1cl <- modclogit$sample(
  data = stan_dat1,
  #seed = 123,
  chains = 4,
  parallel_chains = 4,
  #output_dir = "C:\\Users\\jm361n\\uruguay",
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200 # print update every 500 iters
)

fit1multi <- modmulti$sample(
  data = stan_dat1,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200
)

fit1 <- mod$sample(
  data = stan_dat1,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  thin = 1,
  refresh = 200
)

coef(m1)

fit1cl$time()$total
fit1cl$summary()$mean[2:4]

fit1multi$time()$total
fit1multi$summary()$mean[2:4]

fit1$time()$total
fit1$summary()$mean[2:4]



m0 <- ssf1 %>% fit_clogit(case_ ~ forest + strata(step_id_))
m1 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl * cos_ta + strata(step_id_))
m2 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl + cos_ta + strata(step_id_))
summary(m0)
