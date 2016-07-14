
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

# relevant papers:
# - http://www.columbia.edu/~ld208/psymeth98.pdf
# - http://www.columbia.edu/~ld208/jmp11.pdf
# - http://www.columbia.edu/~ld208/jeplmc08.pdf


map <- function(vec, from, to) {
  newVec <- vec
  for( i in 1:length(from) ) {
    newVec[vec == from[i]] <- to[i]
  }
  return(newVec)
}

nmap <- function(vec, fromto, cast=I) cast(map(vec, names(fromto), fromto))


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


##############################################################################################
### Test the model parameterization on one participant: using only the simplest predictors ###
##############################################################################################

data_tmp <- subset(data.rc, subject %in% c(1009))

# plot the percentages of 'acceptable' responses
data_tmp %>% dplyr::group_by(condition, interval) %>% dplyr::summarize(p_yes = mean(responseGrammatical)) %>% ggplot(aes(interval, p_yes, color = condition)) + geom_point() + geom_line()

# plot d' by condition
data_tmp %>% ddply(.(interval), function(d) {
  coef(glm(responseGrammatical ~ condition, family = "binomial", data = d)) %>% as.data.frame() %>% t()
}) %>% tidyr::gather(condition, dprime, -interval, -`(Intercept)`) %>% 
  ggplot(aes(interval, dprime, color = condition)) + geom_point() + geom_line()


##################################################################################
### Single-participant model with condition contrasts only: Parameterization 1 ###
##################################################################################

rc_exp_simple <- brm(formula = responseGrammatical ~ ( (asymptote + (critasymptote - critinit)) * (1 - exp( -1/(invrate + critinvrate) * (time - (intercept + critintercept) ))) ) * ((time - (intercept + critintercept) ) > 0) + critinit,
                     nonlinear = list( intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, 
                                       invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                                       asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                                       critasymptote ~ 1, critinvrate ~ 1, 
                                       critintercept ~ 1, critinit ~ 1),
                     prior = c(set_prior("normal(3, 5)", nlpar = "asymptote", class = "b", lb = 0, ub = 10),
                               set_prior("normal(200, 500)", nlpar = "invrate", class = "b", lb = 10, ub = 5000),
                               set_prior("normal(500, 500)", nlpar = "intercept", class = "b", lb = 10, ub = 5000),
                               
                               set_prior("normal(0, 5)", nlpar = "critinit", class = "b", lb = -5, ub = 5),
                               set_prior("normal(0, 5)", nlpar = "critasymptote", class = "b", lb = -5, ub = 5),
                               set_prior("normal(200, 500)", nlpar = "critinvrate", class = "b", lb = 10, ub = 5000),
                               set_prior("normal(500, 500)", nlpar = "critintercept", class = "b", lb = 0, ub = 5000)),
                     data = data_tmp, iter = 1000, chains = 4, cores = n_cores, family = "bernoulli"
                    )

rc_exp_simple
# pairs(rc_exp_simple)
# launch_shiny(rc_exp_simple)

# NOTE: This parameterization is suceptible to all sorts of instabilities because each condition's parameters are estimated separately



##################################################################################
### Single-participant model with condition contrasts only: Parameterization 2 ###
##################################################################################

# Visualization of the asymptote prior in constrained space:
if (FALSE) {
  constrain <- function(x) plogis(x) * 10
  unconstrain <- function(x) qlogis( x / 10 )
  x <- seq(-10, 100, by = .001)
  plot(constrain(x), dnorm(x, -1, 2), xlim = c(-10, 15), type = "l" )
}

# Visualization of the invrate prior in constrained space:
if (FALSE) {
  constrain <- function(x) 10 + plogis(x) * 5000
  unconstrain <- function(x) qlogis( (y - 10)/5000 )
  x <- seq(-10, 100, by = .001)
  plot(constrain(x), dnorm(x, -1, 2), xlim = c(-10, 5000), type = "l" )
}

# Visualization of the intercept prior in constrained space:
if (FALSE) {
  constrain <- function(x) exp(x)
  unconstrain <- function(x) log(y)
  x <- seq(-10, 100, by = .001)
  plot(constrain(x), dnorm(x, 1, .5), xlim = c(-1, 20), type = "l" )
  plot(constrain(x), dnorm(x, 6, 1), xlim = c(-1, 5000), type = "l" )
}

contrasts_priors <- c(set_prior(nlpar = "asymptote", "normal(-1, 2)", class = "b"), # for exp: normal(1, .5), for inv_logit: "normal(-1, 2)"; exp() doesn't restrict the range of values sufficiently - there are divergent transistions -- not with inv_logit, however
                      set_prior(nlpar = "intercept", "normal(500, 500)", class = "b", lb = 10, ub = 5000),
                      set_prior(nlpar = "invrate", "normal(6, 1)", class = "b"), # for exp: normal(6, 1), for inv_logit: "normal(-1, 2)"
                      
                      set_prior(nlpar = "critinit", "normal(-1, 2)", class = "b"),
                      set_prior(nlpar = "critasymptote", "normal(-1, 2)", class = "b"), # for inv_logit: "normal(-1, 2)"
                      set_prior(nlpar = "critintercept", "normal(500, 500)", class = "b", lb = 0, ub = 5000))

satf_formula <- responseGrammatical ~ ( ( inv_logit(asymptote)*10 + inv_logit(critasymptote)*10-5 - inv_logit(critinit)*10-5 ) * 
                                          (1 - exp( -1/( exp(invrate)+10 ) * 
                                                      (time - (intercept + critintercept) ))) ) * 
                                          ((time - (intercept + critintercept) ) > 0) + 
                                          inv_logit(critinit)*10-5

rc_exp_simple <- brm(formula = satf_formula,
                     nonlinear = list( intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, 
                                       invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow,
                                       asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                                       critasymptote ~ 1,
                                       critintercept ~ 1, critinit ~ 1),
                     
                     prior = contrasts_priors,
                     data = data_tmp, iter = 1000, chains = 1, cores = 1, family = "bernoulli",
                     silent = FALSE, verbose = TRUE
)

rc_exp_simple
# pairs(rc_exp_simple)
# launch_shiny(rc_exp_simple)

# NOTE: This parameterization is suceptible to all sorts of instabilities because each condition's parameters are estimated separately


##################################################################################
### Single-participant model with condition contrasts only: Parameterization 3 ###
##################################################################################

# Visualization of the asymptote prior in constrained space:
if (FALSE) {
  constrain <- function(x) plogis(x) * 10
  unconstrain <- function(x) qlogis( x / 10 )
  x <- seq(-10, 100, by = .001)
  plot(constrain(x), dnorm(x, -1, 2), xlim = c(-10, 15), type = "l" )
}

# Visualization of the invrate prior in constrained space:
if (FALSE) {
  constrain <- function(x) 10 + plogis(x) * 5000
  unconstrain <- function(x) qlogis( (y - 10)/5000 )
  x <- seq(-10, 100, by = .001)
  plot(constrain(x), dnorm(x, -1, 2), xlim = c(-10, 5000), type = "l" )
}

# Visualization of the intercept prior in constrained space:
if (FALSE) {
  constrain <- function(x) exp(x)
  unconstrain <- function(x) log(y)
  x <- seq(-10, 100, by = .001)
  plot(constrain(x), dnorm(x, 1, .5), xlim = c(-1, 20), type = "l" )
  plot(constrain(x), dnorm(x, 6, 1), xlim = c(-1, 5000), type = "l" )
}

data_tmp <- subset(data.rc, subject %in% c(1009))

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

satf_formula <- responseGrammatical ~ criterion_fn(time, critinit, critasymptote, critinvrate, critintercept) + dprime_fn(time, cAttachmentNone, asymptote, invrate, intercept) 

contrasts_priors <- c(set_prior(nlpar = "asymptote", "normal(-1, 2)", class = "b"), # for exp: normal(1, .5), for inv_logit: "normal(-1, 2)"; exp() doesn't restrict the range of values sufficiently - there are divergent transistions -- not with inv_logit, however
                      set_prior(nlpar = "intercept", "normal(500, 500)", class = "b", lb = 10, ub = 5000),
                      set_prior(nlpar = "invrate", "normal(6, 1)", class = "b"), # for exp: normal(6, 1), for inv_logit: "normal(-1, 2)"
                      
                      set_prior(nlpar = "critinvrate", "normal(6, 1)", class = "b"), # for exp: normal(6, 1), for inv_logit: "normal(-1, 2)"
                      set_prior(nlpar = "critinit", "normal(-1, 2)", class = "b"),
                      set_prior(nlpar = "critasymptote", "normal(-1, 2)", class = "b"), # for inv_logit: "normal(-1, 2)"
                      set_prior(nlpar = "critintercept", "normal(500, 500)", class = "b", lb = 0, ub = 5000))

contrasts <- list(asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, #+ (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject),
                  invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, # + (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject),
                  intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, # + (cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1|subject),
                  critasymptote ~ 1, # + (1|subject),
                  critintercept ~ 1, # + (1|subject), 
                  critinvrate ~ 1, # + (1|subject), 
                  critinit ~ 1 # + (1|subject)
                  )

rc_exp_simple <- brm(formula = satf_formula,
                     nonlinear = contrasts,
                     prior = contrasts_priors, stan_funs = stan_funs,
                     data = data_tmp, iter = 100, chains = n_cores, cores = n_cores, 
                     family = "bernoulli",
                     silent = FALSE, verbose = TRUE)

rc_exp_simple
# pairs(rc_exp_simple)
# launch_shiny(rc_exp_simple)

# NOTE: This parameterization is suceptible to all sorts of instabilities because each condition's parameters are estimated separately

##################################################################################
### Single-participant model with condition contrasts only: Parameterization 4 ###
##################################################################################

data_tmp %<>% within({
  cAttachmentAcceptableAvg  <- nmap(as.character(condition), c("none" = 0, "amb" = 1, "high" = 1, "low" = 1 ), as.integer) # use an 'acceptable'-intercept
  cAttachmentAcceptableAvgVsHigh <- nmap(as.character(condition), c("none" = 0, "amb" = -1, "high" = 0, "low" = 1 ), as.integer) # sum contrast for avg. vs. low attachment
  cAttachmentAcceptableAvgVsLow  <- nmap(as.character(condition), c("none" = 0, "amb" = -1, "high" = 1, "low" = 0 ), as.integer) # sum contrast for avg. vs. high attachment
})

rc_exp_simple2 <- brm(formula = responseGrammatical ~ ( (asymptoteavg + asymptotecontr) + (critasymptote - critinit)) * 
                                                     (1 - exp( -1/(invrateavg + invratecontr + critinvrate)^2 * (time - ( (interceptavg + interceptcontr) + critintercept) ))) * 
                                                     ((time - ((interceptavg + interceptcontr) + critintercept) ) > 0) + 
                                                     critinit,
                     nonlinear = list( interceptavg ~ -1 + cAttachmentAcceptableAvg,
                                       interceptcontr ~ -1 + cAttachmentAcceptableAvgVsHigh + cAttachmentAcceptableAvgVsLow,
                                       
                                       invrateavg ~ -1 + cAttachmentAcceptableAvg,
                                       invratecontr ~ -1 + cAttachmentAcceptableAvgVsHigh + cAttachmentAcceptableAvgVsLow,
                                       
                                       asymptoteavg ~ -1 + cAttachmentAcceptableAvg,
                                       asymptotecontr ~ -1 + cAttachmentAcceptableAvgVsHigh + cAttachmentAcceptableAvgVsLow,
                                       
                                       critasymptote ~ 1, critinvrate ~ 1, 
                                       critintercept ~ 1, critinit ~ 1),
                     prior = c(set_prior("normal(1000, 5000)", nlpar = "invrateavg", class = "b", lb = 10, ub = 5000),
                               set_prior("normal(0, 500)", nlpar = "invratecontr", class = "b"), #, lb = 10, ub = 5000),

                               set_prior("normal(3, 5)", nlpar = "asymptoteavg", class = "b", lb = 0, ub = 10),
                               set_prior("normal(0, 5)", nlpar = "asymptotecontr", class = "b"),

                               set_prior("normal(500, 5000)", nlpar = "interceptavg", class = "b", lb = 10, ub = 5000),
                               set_prior("normal(0, 500)", nlpar = "interceptcontr", class = "b"),
                               
                               set_prior("normal(0, 10)", nlpar = "critinit", class = "b", lb = -5, ub = 5),
                               set_prior("normal(0, 10)", nlpar = "critasymptote", class = "b", lb = -5, ub = 5),
                               set_prior("normal(500, 5000)", nlpar = "critinvrate", class = "b", lb = 10, ub = 5000),
                               set_prior("normal(500, 5000)", nlpar = "critintercept", class = "b", lb = 0, ub = 5000)),
                     data = data_tmp, iter = 2000, #chains = 4, cores = n_cores, 
                     chains = 1, cores = 1,
                     family = "bernoulli"
                    )

rc_exp_simple2
# pairs(rc_exp_simple2)
# launch_shiny(rc_exp_simple2)

# TODO: This model is utterly useless because the lack of constraints causes a lot of 'divergent transitions'.


#############################################################################
### Single-participant model with condition contrasts and autocorrelation ###
#############################################################################

rc_exp_cor <- brm(formula = responseGrammatical ~ ( (asymptote + (critasymptote - critinit)) * (1 - exp( -1/(invrate + critinvrate) * (time - (intercept + critintercept) ))) ) * ((time - (intercept + critintercept) ) > 0) + critinit,
                  nonlinear = list( intercept ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1, 
                                    invrate ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                                    asymptote ~ cAttachmentAmb + cAttachmentHigh + cAttachmentLow - 1,
                                    critasymptote ~ 1, critinvrate ~ 1, 
                                    critintercept ~ 1, critinit ~ 1),
                  prior = c(set_prior("normal(3, 5)", nlpar = "asymptote", class = "b", lb = 0, ub = 10),
                            set_prior("normal(200, 500)", nlpar = "invrate", class = "b", lb = 10, ub = 5000),
                            set_prior("normal(500, 500)", nlpar = "intercept", class = "b", lb = 10, ub = 5000),
                            
                            set_prior("normal(0, 5)", nlpar = "critinit", class = "b", lb = -5, ub = 5),
                            set_prior("normal(0, 5)", nlpar = "critasymptote", class = "b", lb = -5, ub = 5),
                            set_prior("normal(200, 500)", nlpar = "critinvrate", class = "b", lb = 10, ub = 5000),
                            set_prior("normal(500, 500)", nlpar = "critintercept", class = "b", lb = 0, ub = 5000)),
                  data = data_tmp, iter = 1000, chains = 4, cores = n_cores, 
                  family = "binomial", # using 'binomial' instead of 'bernoulli', because cor_bsts() isn't implemented for 'bernoulli'
                  autocor = cor_bsts(~interval|trial_id)
)


rc_exp_cor
# launch_shiny(rc_exp_cor)

