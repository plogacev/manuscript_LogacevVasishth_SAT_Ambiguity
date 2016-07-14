library(plyr)

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






ReadParticipant <- function(data.dir, part.num) {
  fnames <- system(sprintf("ls %s/%d.RaceArg*", data.dir, part.num), intern=T)
  basenames <- basename(fnames)
  labels <- gsub(".*\\.RaceArg_(.*)Block(.*)\\.dat", "\\1\\2", basename(fnames))
  d.a <- read.table(fnames[1], header=T, as.is=T)
  d.a$block <- labels[1]
  d.b <- read.table(fnames[2], header=T, as.is=T)
  d.b$block <- labels[2]
  d.c <- read.table(fnames[3], header=T, as.is=T)
  d.c$block <- labels[3]
  d.d <- read.table(fnames[4],header=T, as.is=T)
  d.d$block <- labels[4]
  d <- rbind(d.a, d.b, d.c, d.d)
  #PrepSATData(d)
}


fn.determine.intervals <- function(data, trials.num=640, trial.effect=NULL, stretch=NULL) {
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
FitStepFn <- function(p, data, categories.num=14, trials.num=640, trial.effect=NULL, stretch=NULL, return.data=F) {
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
  categories.num.dev <- ((length(interval.lens)-categories.num)^2+1)
  #print(trials.with.responses^40)
  if(!return.data) {
    return(within.cat.var*trials.with.responses*excess.responses)
  } else {
    return(data)
  }
}
