
# TODO: remove all times which equal -1
# TODO: check why responses before the first signal have positive RTs


(load("RaceRC-SAT.dat"))
data <- subset(data, interval != 14)
data$time <- data$stim.time
data$full.condition <- data$condition
data$gender <- sapply(strsplit(data$condition, "-"), function(x) if(length(x) ==2) x[1] else NA)
data$condition <- sapply(strsplit(data$condition, "-"), function(x) if(length(x) ==1) x[1] else x[2])
data$responseCorrect <- as.integer(map(data$responseCorrect, c("True", "False"), c(1,0)))
data$responseGrammatical <- as.integer(map(data$responseGrammatical, c("Y", "N"), c(1,0)))
data.rc <- subset(data, experiment=="RACE.RC")

# LOOK AT SWITCHING BEHAVIOR
switching <- ddply(data, .(subject,trial.id), function(d) {
  c(switch.cnt=sum(d$responseGrammatical[-nrow(d)]!=d$responseGrammatical[-1]))
})
ddply(switching, .(subject), function(d) {
  c(zero=length(d$switch.cnt[d$switch.cnt==0]),
    one=length(d$switch.cnt[d$switch.cnt==1]),
    more.than.one=length(d$switch.cnt[d$switch.cnt>1]))
})

# FIND OUT BY HOW MUCH THE RESPONSE LAGS BEHIND THE SIGNAL
round(with(data, tapply(stim.time, list(subject,interval), mean, na.rm=T)),2)
with(data, tapply(time, list(subject, interval), function(x) length(x[!is.na(x)]) ))

method
# look at final responses by-item
round(sort( with(subset(d.resp, interval==13 & experiment=="RACE.RC"),
                 tapply(responseCorrect, item, mean, na.rm=T))),2)


round(sort( with(subset(d.resp, interval==13 & experiment=="LOCALITY"),
                 tapply(responseCorrect, item, mean, na.rm=T))),2)

# look at accuracy by block number
round(with(subset(d.resp, interval>12),
                 tapply(responseCorrect, list(subject,round(trial.id/36)), mean, na.rm=T)),2)

# ACCURACY
library(ggplot2)
library(plyr)
source("ExplicitModels_Base.R")

d.resp.accuracy <- 
ddply(subset(data,  experiment=="RACE.RC"), .(subject, experiment), function(d) {
  d$time <- d$stim.time
  summarize.accuracy(d, "accuracy")
})

qplot(data=subset(d.resp.accuracy, experiment=="RACE.RC" & subject==1026),
      x=time, y=accuracy, col=condition)+geom_path()


# SDT

library(ggplot2)
library(sdtalt)
library(plyr)
source("ExplicitModels_Base.R")

data.rc.dprime <- 
ddply(subset(data, experiment=="RACE.RC"), .(subject, experiment), function(d) {
  d$time <- d$stim.time
  summarize.accuracy(d, "dprime")
})

cur.subject <- 1008
qplot(data=subset(data.rc.dprime, subject==cur.subject),
      x=time, y=dprime, col=condition, group=condition, main=cur.subject)+geom_path()



# LOCALITY

d.resp$respGram <- as.factor(d.resp$responseGrammatical)
d.resp.locality <- ddply(subset(d.resp, experiment=="LOCALITY" & !is.na(time) & !is.na(interval)), .(subject, interval), function(d) {
  d <- subset(d, !is.na(time))
  resp <- with(d, xtabs(~full.condition+respGram))
  print(resp)
  flat  <- 1; meas <- c('d','A')
  long  <- sdt(resp['pl-pl-long','1']+resp['sg-sg-long','1'],
                  resp['sg-pl-long','1']+resp['pl-sg-long','1'],
                  resp['pl-pl-long','0']+resp['sg-sg-long','0'],
                  resp['sg-pl-long','0']+resp['pl-sg-long','0'], flat=flat, meas=meas)
  short  <- sdt(resp['pl-pl-short','1']+resp['sg-sg-short','1'],
                   resp['sg-pl-short','1']+resp['pl-sg-short','1'],
                   resp['pl-pl-short','0']+resp['sg-sg-short','0'],
                   resp['sg-pl-short','0']+resp['pl-sg-short','0'], flat=flat, meas=meas)
  res <- rbind(long, short)
  res$condition <- c('long', 'short')
  time <- with(d, tapply(stim.time, full.condition, mean)) #, na.rm=T
  res$time <- c(mean(time['pl-pl-long'], time['sg-pl-long'], time['sg-sg-long'], time['pl-sg-long']),
                mean(time['sg-sg-long'], time['pl-sg-long'], time['pl-pl-short'], time['sg-pl-short']))
  #print(summary(d$signal.time[d$condition=="amb"]))
  #print(summary(d$signal.time[d$condition=="high"]))
  #print(summary(d$signal.time[d$condition=="low"]))
  res
})

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1023),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1009),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1010),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1011),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1012),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1013),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1014),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1015),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1016),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1017),
      x=time, y=d, col=condition)+geom_path()

qplot(data=subset(d.resp.locality, !is.na(interval) & subject==1018),
      x=time, y=d, col=condition)+geom_path()
