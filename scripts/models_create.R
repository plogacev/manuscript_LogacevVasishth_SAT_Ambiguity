
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

### Load data
(load("../data/Experiment1_RCs_preprocessed.rda"))

### Set path for saving models
models_path <- "models"


####################
### Prepare data ###
####################

data.rc$time %<>% multiply_by(1000) # convert time to ms
data.rc %<>% rename(trial_id = trial.id)
data.rc$condition %<>% factor(levels = c("none", "low", "high", "amb") )
contrasts(data.rc$condition)

data.rc$clTrialId <- ( log(data.rc$trial_id) / max(log(data.rc$trial_id)) ) - 0.5

data.rc %<>% within({
  cAttachmentNone  <- as.integer(condition == "none" )
  cAttachmentAmb  <- as.integer(condition == "amb" )
  cAttachmentHigh <- as.integer(condition == "high" )
  cAttachmentLow  <- as.integer(condition == "low" )
  cAttachmentAcceptable  <- as.integer(condition %in% c("amb", "high", "low") )
})



###############################
### Take a look at the data ###
###############################

# plot the percentages of 'acceptable' responses
data.rc %>% dplyr::group_by(condition, interval) %>% dplyr::summarize(p_yes = mean(responseGrammatical)) %>% ggplot(aes(interval, p_yes, color = condition)) + geom_point() + geom_line()

# plot d' by condition
data.rc %>% ddply(.(interval), function(d) {
  coef(glm(responseGrammatical ~ condition, family = "binomial", data = d)) %>% as.data.frame() %>% t()
}) %>% tidyr::gather(condition, dprime, -interval, -`(Intercept)`) %>% 
  ggplot(aes(interval, dprime, color = condition)) + geom_point() + geom_line()

fname_diagnostic <- function(fname) NULL #fname %>% gsub(".rda", "diagnostic.txt", .)

###################################
### Set priors and satf formula ###
###################################


stan_funs <- "
real dprime_fn(real time, real disabled, real asymptote_unconstrained, real invrate_unconstrained, real intercept)
{
  if (disabled == 0.0 && time >= intercept) {
  real asymptote; 
  real invrate; 
  
  asymptote = 10 * inv_logit(asymptote_unconstrained);
  invrate = exp(invrate_unconstrained) + 10;
  //asymptote = asymptote_unconstrained;
  //invrate = invrate_unconstrained;
  
  return asymptote * (1 - exp(-1/invrate * (time - intercept)) );
  }
  return 0;
}

real criterion_fn(real time, real init_unconstrained, real asymptote_unconstrained, real invrate_unconstrained, real intercept)
{
  real criterion; 
  real init; 
  
  init = 10 * (inv_logit(init_unconstrained) - .5);
  //init = init_unconstrained;
  criterion = init;
  
  if (time >= intercept) {
  real asymptote; 
  real invrate; 
  
  asymptote = 10 * (inv_logit(asymptote_unconstrained) - .5) - init;
  invrate = exp(invrate_unconstrained) + 10;
  //asymptote = asymptote_unconstrained - init;
  //invrate = invrate_unconstrained;
  
  criterion = criterion + (asymptote-init) * (1 - exp(-1/invrate * (time - intercept)) );
  }
  return criterion;
}
"

contrasts_priors <- c(set_prior(nlpar = "asymptote", "normal(-1, 2)", class = "b"), # for exp: normal(1, .5), for inv_logit: "normal(-1, 2)";
                      set_prior(nlpar = "intercept", "normal(500, 500)", class = "b", lb = 10, ub = 5000),
                      set_prior(nlpar = "invrate", "normal(6, 1)", class = "b"), # for exp: normal(6, 1), for inv_logit: "normal(-1, 2)"
                      
                      set_prior(nlpar = "critinvrate", "normal(6, 1)", class = "b"), # for exp: normal(6, 1), for inv_logit: "normal(-1, 2)"
                      set_prior(nlpar = "critinit", "normal(-1, 2)", class = "b"),
                      set_prior(nlpar = "critasymptote", "normal(-1, 2)", class = "b"), # for inv_logit: "normal(-1, 2)"
                      set_prior(nlpar = "critintercept", "normal(500, 500)", class = "b", lb = 10, ub = 5000))

satf_formula <- responseGrammatical ~ criterion_fn(time, critinit, critasymptote, critinvrate, critintercept) + 
                                      dprime_fn(time, cAttachmentNone, asymptote, invrate, intercept) 

###################################################################################################
### Test model, no multi-level structure, but using the same contrasts, and the entire data set. ###
###################################################################################################

if (FALSE) { # Run manually

  contrasts <- list(asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                    invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                    intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                    critasymptote ~ 1,
                    critintercept ~ 1,
                    critinvrate ~ 1,
                    critinit ~ 1
                    )
  
  
  rc_exp_pre_mlm_test <- brm(formula = satf_formula,
                             nonlinear = contrasts,
                             prior = contrasts_priors,
                             silent = FALSE, verbose = TRUE,
                             data = data.rc, iter = 1000, chains = 1, cores = 1, family = "bernoulli",
                             diagnostic_file = fname_diagnostic(fname)
                            )
  
  rc_exp_pre_mlm_test
  # launch_shiny(rc_exp_simple)
  
  
  model_code_rc_exp_pre_mlm_test <- make_stancode(formula = satf_formula,
                                                  nonlinear = contrasts,
                                                  prior = contrasts_priors,
                                                  silent = FALSE, verbose = TRUE,
                                                  data = data.rc, iter = 1000, chains = 1, cores = 1, family = "bernoulli",
                                                  diagnostic_file = fname_diagnostic(fname)
                                                  )
  data_rc_exp_pre_mlm_test <- make_standata(formula = satf_formula,
                                            nonlinear = contrasts,
                                            prior = contrasts_priors,
                                            silent = FALSE, verbose = TRUE,
                                            data = data.rc, iter = 1000, chains = 1, cores = 1, family = "bernoulli",
                                            diagnostic_file = fname_diagnostic(fname)
                                            )
  
  model_rc_exp_pre_mlm_test <- rstan::stan_model(model_code = model_rc_exp_pre_mlm_test)    
  
  samples_rc_exp_pre_mlm_test <- rstan::sampling(model_rc_exp_pre_mlm_test, data = data_rc_exp_pre_mlm_test, verbose = TRUE,
                                                 iter = 1000, chains = 1, cores = 1)    
}

#########################################################
### Function for generating models and data structure ###
#########################################################

generate_model <- function(satf_formula, contrasts, contrasts_priors, data, fname_model_code, fname_params, ...)
{
  params <- list(formula = satf_formula,
                 nonlinear = contrasts,
                 prior = contrasts_priors,
                 data = data, ...)

  zeroiter_params <- params
  zeroiter_params$iter <- 0
  zeroiter_params$chains <- 1

  brmsfit_call <- paste0(names(zeroiter_params), " = zeroiter_params$", names(zeroiter_params), collapse = ", ") %>% paste("brms::brm(", ., ", stan_funs = stan_funs)")
  brmsfit <- tryCatch({ eval(parse(text = brmsfit_call)) }, silent = TRUE)
  
  model_code <- brmsfit$model
  
  cat(model_code, file = fname_model_code)
  save(brmsfit, data, params, file = fname_params)
}


#########################################################################################
### Generate multi-level model with condition contrasts and by-subject random effects ###
#########################################################################################

base_fname <- file.path(models_path, "rc_exp_mlm_subj")
fname_model_code <- base_fname %>% paste0(., "_model_code.stan")
fname_params <- base_fname %>% paste0(., "_params.rda")
if (!file.exists(fname_model_code))
{
  contrasts <- list(asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject),
                    invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject),
                    intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject),
                    critasymptote ~ 1 + (1|subject),
                    critintercept ~ 1 + (1|subject), 
                    critinvrate ~ 1 + (1|subject), 
                    critinit ~ 1 + (1|subject)
                   )

  generate_model(satf_formula, contrasts, contrasts_priors,  data = data.rc, fname_model_code = fname_model_code, fname_params = fname_params, family = "bernoulli")
}




#############################################################################################
### Multi-level model with condition contrasts and by-subject and by-items random effects ###
#############################################################################################

base_fname <- file.path(models_path, "rc_exp_mlm_subj_item")
fname_model_code <- base_fname %>% paste0(., "_model_code.stan")
fname_params <- base_fname %>% paste0(., "_params.rda")
if (!file.exists(fname_model_code))
{
  contrasts <- list(asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject) +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|item),
                    invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + 
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject) +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|item),
                    intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + 
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject) +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|item),
                    critasymptote ~ 1 + (1|subject) + (1|item),
                    critintercept ~ 1 + (1|subject) + (1|item), 
                    critinvrate ~ 1 + (1|subject) + (1|item), 
                    critinit ~ 1 + (1|subject) + (1|item)
                    )
  
  generate_model(satf_formula, contrasts, contrasts_priors, data = data.rc, fname_model_code = fname_model_code, fname_params = fname_params, family = "bernoulli")
}


##################################################################################################################
### Multi-level model with condition contrasts and by-subject and by-items random effects, and autocorrelation ###
##################################################################################################################

base_fname <- file.path(models_path, "rc_exp_mlm_subj_item_cor")
fname_model_code <- base_fname %>% paste0(., "_model_code.stan")
fname_params <- base_fname %>% paste0(., "_params.rda")
if (!file.exists(fname_model_code))
{
  contrasts <- list(asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject) +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|item),
                    invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + 
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject) +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|item),
                    intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1 + 
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject) +
                                    (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|item),
                    critasymptote ~ 1 + (1|subject) + (1|item),
                    critintercept ~ 1 + (1|subject) + (1|item), 
                    critinvrate ~ 1 + (1|subject) + (1|item), 
                    critinit ~ 1 + (1|subject) + (1|item)
                  )
  
  generate_model(satf_formula, contrasts, contrasts_priors,  data = data.rc, fname_model_code = fname_model_code, fname_params = fname_params, 
                 family = "binomial",
                 autocor = cor_bsts(~interval|trial_id:subject))
}
