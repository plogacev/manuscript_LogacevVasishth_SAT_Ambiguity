
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

rstan_options(auto_write = TRUE)
options(mc.cores = n_cores)

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

###################################
### Set priors and satf formula ###
###################################

contrasts_priors <- c(set_prior(nlpar = "asymptote", "normal(-1, 2)", class = "b"), # for exp: normal(1, .5), for inv_logit: "normal(-1, 2)"; exp() doesn't restrict the range of values sufficiently - there are divergent transistions -- not with inv_logit, however
                      set_prior(nlpar = "intercept", "normal(500, 500)", class = "b", lb = 10, ub = 5000),
                      set_prior(nlpar = "invrate", "normal(6, 1)", class = "b"), # for exp: normal(6, 1), for inv_logit: "normal(-1, 2)"
                      
                      set_prior(nlpar = "critinit", "normal(-1, 2)", class = "b"),
                      set_prior(nlpar = "critasymptote", "normal(-1, 2)", class = "b"), # for inv_logit: "normal(-1, 2)"
                      set_prior(nlpar = "critintercept", "normal(500, 500)", class = "b", lb = 0, ub = 5000))

satf_formula <- responseGrammatical ~ ( ( inv_logit(asymptote)*10 + inv_logit(critasymptote)*10-5 - inv_logit(critinit)*10-5 ) * # constrained asymptotes: d' = (0; 10), criterion = (-10; +10)
                                          (1 - exp( -1/( exp(invrate)+10 ) *                                                     # constrained 1/rate (10ms; inf)
                                                      (time - (intercept + critintercept) ))) ) *                                # (unconstrained) intercepts
  ((time - (intercept + critintercept) ) > 0) +                                          # set d' to 0 before the intercept
  inv_logit(critinit)*10-5                                                               # constrained criterion before the intercept, (-5;5)


#satf_formula <- responseGrammatical ~ inv_logit(asymptote)*10

contrasts <- list( intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, 
                   invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow,
                   asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                   critasymptote ~ 1, 
                   critintercept ~ 1, 
                   critinit ~ 1
                   )

model_code_rc_exp_pre_mlm_test <- make_stancode(formula = satf_formula,
                                                nonlinear = contrasts,
                                                prior = contrasts_priors,
                                                family = "bernoulli",
                                                data = data.rc)


#data_rc_exp_pre_mlm_test <- make_standata(formula = satf_formula,
#                                          nonlinear = contrasts,
#                                          prior = contrasts_priors,
#                                          data = data.rc[1:87395/80,], family = "bernoulli") #87395/80
#
#samples_rc_exp_pre_mlm_test <- rstan::sampling(model_rc_exp_pre_mlm_test, data = data_rc_exp_pre_mlm_test, verbose = TRUE,
#                                               iter = 1000, chains = 1, cores = 1)    
#model_rc_exp_pre_mlm_test <- rstan::stan(model_code = model_code_rc_exp_pre_mlm_test, data = data_rc_exp_pre_mlm_test, verbose = TRUE,
#                                         iter = 1000, chains = 1, cores = 1)    

brmsfit <- getFromNamespace("brmsfit", "brms")

fit <- brmsfit(formula = satf_formula, family = "bernoulli", #link = family$link, 
             data = data, data.name = data.name, prior = prior, 
             autocor = autocor, nonlinear = nonlinear, 
             cov_ranef = cov_ranef, threshold = threshold, 
             algorithm = algorithm)

model_rc_exp_pre_mlm_test <- rstan::stan_model(model_code = model_code_rc_exp_pre_mlm_test)    

m <- brms::brm(formula = satf_formula,
               nonlinear = contrasts,
               prior = contrasts_priors,
               family = "bernoulli",
               data = data.rc, 
               iter = 0, chains = 1)
# m$model <- NA
m$model <- NA
x$fit <- rstan::stan_model(stanc_ret = x$model)



m2 <- brms::brm(formula = satf_formula,
               nonlinear = contrasts,
               prior = contrasts_priors,
               family = "bernoulli",
               data = data.rc, 
               iter = 10, chains = 1,
               fit = m)



m$fit@

x <- stan(fit = m$fit, iter = 10)

x <- sampling(m$fit, iter = 10)


