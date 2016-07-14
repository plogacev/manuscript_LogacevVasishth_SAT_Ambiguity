library(plyr)

# Notes:
# - Until participant 7: There were typos, and in some items conditions were mislabeled
# - Starting participant 16: Added 5 target structures to the practice sentences (3 high-attachment sentences, 2 low-attachment sentences)

PrepSATData <- function(d) {
  d <- subset(d, !experiment%in%c("practice", "PracticeArrows"))
  d$time[d$time==-1] <- NA
  d$RT[d$RT==-1] <- NA

  d$phraseNr <- NULL
  d$phrase <- NULL
  # Besides the following 5 colums, there are also: responseButton, responseGrammatical, and responseCorrect
  # TODO: There seems to be quite some redundancy.
  d$button <- NULL
  d$answerLeft <- NULL
  d$answerRight <- NULL
  d$answerLeft <- NULL
  d$answerCorrect <- NULL


# label trials consecutively under the assumption that sentences may be repeated
  sentence.id <- as.factor(with(d, paste(subject,experiment,item,condition)))
  last.id <- sentence.id[1]; trial.id <- rep(0, length(sentence.id)); trial.id[1] <- 1
  for(i in 2:length(sentence.id)) {
    trial.id[i] <- trial.id[i-1]+(sentence.id[i-1]!=sentence.id[i])
  }
  d$trial.id <- trial.id
  d <- ddply(d, .(subject), function(d) {
    d$trial.id <- d$trial.id-min(d$trial.id)+1
    d
  })

  # determine timing relative to the first signal tone
  library(plyr)
  d <- ddply(d, .(subject, trial.id), function(d) {
    signal.start.evt <- subset(d, event=='signalStart')
    presentation.stop.evt <- subset(d, event=='displayUpdateStart')
    d$trial.time <- d$time-min(d$time, na.rm=T)
    d$signal.time <- d$time-min(signal.start.evt$time)
    d$stim.time <- d$time-max(presentation.stop.evt$time, na.rm=T)
    stopifnot(all( !is.na(c(max(presentation.stop.evt$time), min(signal.start.evt$time),min(d$time, na.rm=T))) ))
    d
  })

  d
}


AssignNewIntervals <- function(data, boundaries) {
  selection <- data$event=="satResponse" & data$signal.time > 0
  selection[is.na(selection)] <- F
  library(intervals)
  stopifnot(length(boundaries)==15)
  boundaries <- sort(boundaries)
  int <- Intervals(cbind(boundaries[-length(boundaries)], boundaries[-1]), closed=c(T,F))
  in.intervals <- interval_overlap(int, data$signal.time)
  data$new.interval <- NA
  for(i in 1:length(in.intervals)) {
    data[in.intervals[[i]], 'new.interval'] <- i
  }
  data[!selection, 'new.interval'] <- NA
  with(data[selection,], plot(signal.time, trial.id))
  abline(v=boundaries, col="red")
  data
}





ReadParticipant <- function(part.num, order) {
  if(order=="forward") {
    fname1 <- paste(data.dir, part.num, '.all_final1.dat', sep='')
    fname2 <- paste(data.dir, part.num, '.all_final2.dat', sep='')
    fname3 <- paste(data.dir, part.num, '.all_final3.dat', sep='')
  } else {
    fname1 <- paste(data.dir, part.num, '.all_final1.dat', sep='')
    fname2 <- paste(data.dir, part.num, '.all_final3_rev.dat', sep='')
    fname3 <- paste(data.dir, part.num, '.all_final2_rev.dat', sep='')
  }
  d.a <- read.table(fname1, header=T, as.is=T)
  d.b <- read.table(fname2, header=T, as.is=T)
  d.c <- read.table(fname3, header=T, as.is=T)
  d <- rbind(d.a, d.b, d.c)
  PrepSATData(rbind(d.a, d.b, d.c))
}


fn.determine.intervals <- function(data, trials.num=512, trial.effect=NULL, stretch=NULL) {
  xlim <- c(0, 6); ylim <- c(0,trials.num)
  data.x <- ddply(subset(data, !is.na(RT)), .(trial.id), function(d) {
    valid.button.presses <- 1:14 %in% d$interval
    d$signals <- sum(valid.button.presses)
    d$index <- 1:nrow(d)
    d
  })
  (res1 <- optim(0, fn=FitStepFn, trials.num=trials.num,
                 trial.effect=trial.effect, stretch=stretch,
                 data=data.x, method="Brent", upper=0, lower=-2))
  p <- c(shift=res1$par, period=.4)
  data.x <- FitStepFn(p, trials.num=trials.num, data=data.x,
                      trial.effect=trial.effect, stretch=stretch,
                      return.data=T)
  if(is.null(trial.effect)) {
    with(data.x, plot(signal.time, trial.id,
                      col=new.interval-min(new.interval)+1,
                      xlim=xlim, ylim=ylim));
  } else {
    with(data.x, plot(time.for.classification, trial.id,
                      col=new.interval-min(new.interval)+1,
                      xlim=xlim, ylim=ylim));
  } 
  data.x$new.interval <- data.x$new.interval-min(data.x$new.interval)+1
  print( with(data.x, tapply(trial.id, new.interval, function(x) length(x))) )
  print( with(data.x, tapply(trial.id, new.interval, function(x) length(unique(x)))) )
  #ddply(data.x, .(subject,new.interval), function(d) d[1,])
  data.x
}

step.fn <- function(x, period=.4, shift=0) {x <- ((x-shift)/period); floor(x)*(x>0)}
FitStepFn <- function(p, data, categories.num=14, trials.num=512, trial.effect=NULL, stretch=NULL, return.data=F) {
  if(is.null( names(p) )) p <- c(shift=p)
  if(!('period' %in% names(p)))  p['period'] <- .4
  if(is.null(stretch))
    data$time.for.classification <- data$signal.time
  else
    data$time.for.classification <- data$signal.time^stretch
  if(is.null(trial.effect))
    data$time.for.classification <- data$time.for.classification
  else
    data$time.for.classification <- data$time.for.classification +
                                    data$trial.id^2/exp(trial.effect)
  
  data$new.interval <- step.fn(data$time.for.classification,
                               period=p[['period']],
                               shift=p[['shift']])
  myvar <- function(x) ifelse(length(x)==1, 0, sum((x-mean(x))^2) )
  within.cat.var <- with(data, tapply(index, new.interval, myvar))
  within.cat.var <- sum(within.cat.var)
  trials.with.responses <- with(unique(data[,c('trial.id','new.interval')]),
                                tapply(trial.id, new.interval, length))
  #print(trials.with.responses)
  trials.with.responses <- sum(abs(trials.num-trials.with.responses))
  #print(c(trials.with.responses, within.cat.var))
  interval.lens <- with(data, tapply(index, new.interval, length))
  excess.responses <- (interval.lens-trials.num)
  excess.responses <- sum((excess.responses*(excess.responses>0)))
  categories.num.dev <- ((length(intervals)-categories.num)^2+1)
  #print(trials.with.responses^40)
  if(!return.data) {
    return(within.cat.var*trials.with.responses*excess.responses)
  } else {
    return(data)
  }
}

#########################################################################
#########################################################################
#########################################################################


data.dir <- '../Experimente/Results/'

d.forward <- NULL
for(part.num in c(1008, 1009, 1010, 1021, 1022, 1023, 1024, 1025, 1026, 1027)) {
  d.cur <- ReadParticipant(part.num, order="forward")
  d.forward <- rbind(d.forward, d.cur)
}

d.backward <- read.table(paste(data.dir, "1011.all_final132_rev.dat", sep=""), header=T, as.is=T)
d.backward <- PrepSATData(d.backward)
for(part.num in c(1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020)) {
  d.cur <- ReadParticipant(part.num, order="backward")
  d.backward <- rbind(d.backward, d.cur)
}

d.all <- rbind(d.forward, d.backward)
d.all$time <- d.all$time-min(d.all$time, na.rm=T)

#d.resp <- ddply(subset(d.all, event=="satResponse" & signal.time > 0),
#                .(subject, trial.id), function(d) {
#  selection <- !is.na(d$responseButton) & !is.na(d$interval)
#  d$valid.presses <- length(d$responseButton[])
#})



###############################################################

data.resp <- subset(d.all, event=="satResponse" & !is.na(signal.time) & signal.time>.1)

d <- fn.determine.intervals(subset(data.resp, subject==1008))
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d8 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1009), stretch=.99)
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d9 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1010), stretch=1)
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d10 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1011), stretch=1)
d[d$new.interval==15,'new.interval'] <- 14
d11 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1012 & signal.time<5.6))
d12 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1013))
d[d$new.interval==15,'new.interval'] <- 14
d13 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1014))
d[d$new.interval==15,'new.interval'] <- 14
d14 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1015))
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d15 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1016), stretch=1.03)
d[d$new.interval==15,'new.interval'] <- 14
d16 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1017))
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d17 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1018) )
d18 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1019) )
d19 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1020) )
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d20 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1021), stretch=.99)
d[d$new.interval==15,'new.interval'] <- 14
d21 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1022), trial.effect=2000)
d22 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1023), stretch=1)
d[d$new.interval==1,'new.interval'] <- 2
d$new.interval <- d$new.interval-1 
d23 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1024), stretch=0.99)
d24 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1025), stretch=0.99)
d[d$new.interval==14,'new.interval'] <- 13
d25 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1026), stretch=1.02)
d[d$new.interval==15,'new.interval'] <- 14
d26 <- d

d <- fn.determine.intervals(subset(data.resp, subject==1027), stretch=1)
d[d$new.interval==15,'new.interval'] <- 14
d27 <- d


data <- rbind(d8, d9, d10, d11, d12, d13, d14,
              d15, d16, d17, d18, d19, d20,
              d21, d22, d23, d24, d25, d26, d27)
data$interval <- data$new.interval

data$new.interval <- NULL
data$time.for.classification <- NULL
data$index <- NULL
data$event <- NULL
data$RT <- NULL


new.data <- 
ddply(data, .(subject), function(d) {
  print(paste("subject", unique(d$subject)))
  (medians <- with(d, tapply(signal.time, interval, median)))
  new.data <- ddply(d, .(trial.id, interval), function(d) {
                         interval <- d$interval[1]
                         distances <- abs(d$signal.time-medians[interval])
                         selection <- which.min( distances )
                         d[selection,]
                       } )
})

data <- new.data

save(data, file="RaceRC-SAT.dat")
