### 0. Load Required Packages and Utility Functions
suppressPackageStartupMessages({
  library(rstan)
  options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
  
  library(brms)
  library(lme4)
  library(here)
  library(tidyverse)
  library(tidybayes)
  library(bayesplot)
  library(lubridate)
  library(zoo)
})

source("Packages/Utility_Functions_MMM.R")



### 1. Use Pre-Processed Media Mix Data
load("RData/pp_MMM_ts_comp_xy995.RData")



### 2. Train Models by Pooling Type
#------------------------------------------------------------#
# MCMC Conditions                                            #
#------------------------------------------------------------#
num_warmups     <- 1000
num_iterations  <- 2000
num_chains      <- 4
num_cores       <- 4
num_refresh     <- num_iterations/10
thin_num        <- 1
seed_num        <- 12345
num_treedepth   <- 15
acceptance_rate <- 0.95



#------------------------------------------------------------#
# Target:              True Gross Adds                       #
# Media Normalization: across dma per media normalization    #
#------------------------------------------------------------#
var_target      = 'gross_add_true'
var_control        = c('seasonality_gross_add_true','AR1', 'price_hybrid','competition','inflation','weather1','weather3','competitive_offer','istart_offer','financial_offer','price_for_life','covid_emergency','big_holiday','summer_promo_end','snow','covid_daily_confirmed_cases')
pp_MMM_ls = pp_MMM_ts_comp_xy995

# Print media variables
print_media_names(pp_MMM_ls = pp_MMM_ls)



#------------------------------------------------------------#
# Partial Pooling                                            #
#------------------------------------------------------------#
# One can choose one of ("partial", "no", "complete") pooling types.
pooling_type   = "partial"

# Choose the beginning of test date.
# test_date      = "2019-07-01"
# test_date      = NULL
test_date      = NULL      # 70% Train vs 30% Test

# Specify prior values for media parameters
# Provide (1) median/mean and (2) standard deviation of each of the media parameters
media_priors   = list(median = rep(0.10, 10), std = rep(1, 10))


### Create stan_data from `make_stan_data_MMM`.
stan_data_NP      = make_stan_data_MMM(pp_MMM_ls          = pp_MMM_ls,
                                       target_norm_method = 4,
                                       media_norm_method  = 4,
                                       var_target         = var_target,
                                       var_control        = var_control,
                                       df_geo             = NULL,
                                       test_date          = test_date,
                                       pooling_type       = pooling_type,
                                       media_priors       = media_priors)


### Fit the MMM.
t0 <- Sys.time()
fit_PP_44_10_C19_xy995 = stan(file    = here::here("stanmodels", "stan_PP_44_10_C19_xy995.stan"),
                 data    = stan_data_NP$stan_data$train,
                 chains  = num_chains,
                 warmup  = num_warmups,
                 iter    = num_iterations,
                 cores   = num_cores,
                 refresh = num_refresh,
                 thin    = thin_num,
                 seed    = seed_num,
                 control = list(max_treedepth = num_treedepth,
                                adapt_delta   = acceptance_rate))
t1 <- Sys.time()
Runtime <- t1 - t0
print(Runtime)
# Time difference of 3.910124 hours

# print(fit_NP_22, pars = c("beta_media"))
# print(fit_NP_22, pars = c("beta_base", "beta_media", "beta_control"))
# print(fit_NP_22, pars = c("alpha", "S", "K"))

save(fit_PP_44_10_C19_xy995, file = "RData/fit_PP_44_10_C19_xy995.RData")

# library(googleCloudStorageR)
# gcs_auth("alticeusa-am-b639e404289b.json")
# gcs_upload(segment_filename, bucket = "alticeusa-am", name = paste('cloud_abc_data/manoj/',segment_filename, sep = ''),predefinedAcl ="default")
