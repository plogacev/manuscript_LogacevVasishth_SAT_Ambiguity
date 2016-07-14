
# <REMOVE>

FitSerial(NA, params, data=subset(d.resp, subject==1008 & experiment=="RACE.RC" & !is.na(stim.time)), print.probs=F)

AssignParameters(res$par, params)

FitSerial(NA, params, data=subset(d.resp, subject==1020 & experiment=="RACE.RC" & !is.na(stim.time) & interval>1))


att1.params <- c(shape=1, scale=1000, shift=0, p.errWhenAvailable=.1,
                 p.errWhenUnavailable=.1)
params <- list(att1.params=att1.params, att2.params=att1.params, p.GuessYes=.5,
               p.1first=.5, p.try.next=1)

# 'att1-p.errWhenAvailable'=.1, 'att1-p.errWhenUnavailable'=.1, p.GuessYes=.5,  p.1first=.5, p.try.next=.9
  

FitSerial(NA, new.params, data=subset(d.resp, subject==1019 & experiment=="RACE.RC" & !is.na(stim.time) & interval>1), print.probs=F)

new.params$att1.params[['shift']] <-  0
new.params$att2.params[['shift']] <-  new.params$att1.params[['shift']] 
prob.yes.serial(1000, 0, 0, new.params)

new.params$att1.params[['shift']] <-  -10^100
new.params$att2.params[['shift']] <-  new.params$att1.params[['shift']] 
prob.yes.serial.one.branch(1000, .9, .9,
                            new.params$att1.params, new.params$att2.params,
                            new.params$p.try.next)

plot(function(t) prob.yes.serial(t, 1, 1, new.params), xlim=c(0,3000))

amb.dprime <- function(t) qnorm(prob.yes.serial(t, 1, 1, new.params))-qnorm(prob.yes.serial(t, 0, 0, new.params))
high.dprime <- function(t) qnorm(prob.yes.serial(t, 1, 0, new.params))-qnorm(prob.yes.serial(t, 0, 0, new.params))
low.dprime <- function(t) qnorm(prob.yes.serial(t, 0, 1, new.params))-qnorm(prob.yes.serial(t, 0, 0, new.params))
upper <- 2
plot(amb.dprime, xlim=c(0,5000), ylim=c(0,upper), col="red"); par(new=T);
plot(high.dprime, xlim=c(0,5000), ylim=c(0,upper), col="green"); par(new=T);
plot(low.dprime, xlim=c(0,5000), ylim=c(0,upper), col="blue")



prob.yes.serial(t=230, 1, 1, params)

# </REMOVE>


att1.params <- c(shape=2, scale=100, shift=0, p.errWhenAvailable=.1,
                 p.errWhenUnavailable=.1)
params <- list(att1.params=att1.params, att2.params=att1.params,
               p.GuessYes=.5, p.1first=0, p.try.next=.5)

data.serial <- simulate.discrimination(params, one.trial.serial, times.eval=0:20*100, n.trials=2000)

library(ggplot2)
qplot(data=data.serial, x=time, y=d, col=condition)+geom_path() +
  stat_function(fun=function(t) qnorm(prob.yes.serial(t, att1=1, att2=1, params))-qnorm(prob.yes.serial(t, att1=0, att2=0, params)), color="red") +
  stat_function(fun=function(t) qnorm(prob.yes.serial(t, att1=1, att2=0, params))-qnorm(prob.yes.serial(t, att1=0, att2=0, params)), color="green") +
  stat_function(fun=function(t) qnorm(prob.yes.serial(t, att1=0, att2=1, params))-qnorm(prob.yes.serial(t, att1=0, att2=0, params)), color="blue")

x <- ldply(seq(1,200,20), function(shape) {
  ldply(seq(1,200,20), function(scale) {
    ldply(seq(0,1,.1), function(p.errWhenAvailable) {
      ldply(seq(0,1,.1), function(p.errWhenUnavailable) {
        params$att1.params <- c(shape=shape, scale=scale, shift=300, p.errWhenAvailable=p.errWhenAvailable,
                 p.errWhenUnavailable=p.errWhenUnavailable)
        params$att2.params <- params$att1.params
        data.serial <- simulate.discrimination(params, one.trial.serial, times.eval=0:20*100, n.trials=1000)
        #par(ask=T)
        x <- qplot(data=data.serial, x=time, y=d, col=condition)+geom_path() +
  stat_function(fun=function(t) qnorm(prob.yes.serial(t, att1=1, att2=1, params))-qnorm(prob.yes.serial(t, att1=0, att2=0, params)), color="red") +
  stat_function(fun=function(t) qnorm(prob.yes.serial(t, att1=1, att2=0, params))-qnorm(prob.yes.serial(t, att1=0, att2=0, params)), color="green") +
  stat_function(fun=function(t) qnorm(prob.yes.serial(t, att1=0, att2=1, params))-qnorm(prob.yes.serial(t, att1=0, att2=0, params)), color="blue")
        print(x)
        print(params)
        1
      })
    })
  })
})

plot(x$low, x$high)

params <- list()
params$att1.params <- c(shape=2, scale=10, shift=200, p.errWhenAvailable=.22, p.errWhenUnavailable=.22)
params$att2.params <- params$att1.params
params$p.GuessYes <- .1
#params$p.1first <- 0
#params$p.try.next <- 0

data.parallel <- simulate.discrimination(params, one.trial.parallel, times.eval=0:20*100, n.trials=1000)

library(ggplot2)
qplot(data=data.parallel, x=interval, y=d, col=condition)+geom_path()


data.parallel$mechanism <- "parallel"
data.serial$mechanism <- "serial"
qplot(data=rbind(data.parallel,data.serial), x=interval, y=d, group=paste(mechanism,condition),
      col=condition, linetype=mechanism)+geom_path()
