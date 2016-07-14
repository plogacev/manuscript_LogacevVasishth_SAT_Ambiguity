
library(rstan)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(brms)
stopifnot(packageVersion("brms") >= '0.10.0')

library(doMC)
doMC::registerDoMC(4)
n_cores <- 4

options(echo = TRUE)

### Set path for saving models
models_path <- "models"
samples_path <- "samples"

###########################
### Estimation function ###
###########################

estimate <- function(base_fname, iter, ...) {
  base_fname_models <- base_fname %>% file.path(models_path, .)
  fname_model_code <- base_fname_models %>% paste0(., "_model_code.stan")
  fname_params <- base_fname_models %>% paste0(., "_params.rda")
  
  base_fname_samples <- base_fname %>% file.path(models_path, .)
  fname_samples <- base_fname_samples %>% paste0(., sprintf("_iter%d.rda", iter))

  (load(fname_params))
  
  # re-compile the model if it has been changed
  if ( file.info(fname_params)$atime < file.info(fname_model_code)$atime ) {
      model <- rstan::stan_model(file = fname_model_code)
      brmsfit$fit@stanmodel <- model
      brmsfit$model <- model@model_code
  }
  
  brmsfit_samples <- brm(formula = params$formula,
                         nonlinear = params$nonlinear,
                         prior = params$prior,
                         data = params$data,
                         family = params$family,
                         fit = brmsfit,
                         iter = iter,
                         ...)

  save(brmsfit_samples, file = fname_samples)
}

#########################################################################################
### Generate multi-level model with condition contrasts and by-subject random effects ###
#########################################################################################
cat("Running rc_exp_mlm_subj: test\n")
estimate(base_fname = "rc_exp_mlm_subj", iter = 10, chains = 1, cores = 1) 
cat("Running rc_exp_mlm_subj: real\n")
estimate(base_fname = "rc_exp_mlm_subj", iter = 4000, chains = n_cores, cores = n_cores) 


#############################################################################################
### Multi-level model with condition contrasts and by-subject and by-items random effects ###
#############################################################################################

estimate(base_fname = "rc_exp_mlm_subj_item", iter = 10, chains = 1, cores = 1) 
estimate(base_fname = "rc_exp_mlm_subj_item", iter = 2000, chains = n_cores, cores = n_cores) 

##################################################################################################################
### Multi-level model with condition contrasts and by-subject and by-items random effects, and autocorrelation ###
##################################################################################################################

estimate(base_fname = "rc_exp_mlm_subj_item_cor", iter = 10, chains = 1, cores = 1) 
estimate(base_fname = "rc_exp_mlm_subj_item_cor", iter = 2000, chains = n_cores, cores = n_cores) 
