system("ls")

load.participant <- function(fnames) {
  library(plyr)
  d1 <- read.table(fnames[1], header=T)
  d2 <- read.table(fnames[2], header=T)
  d3 <- read.table(fnames[3], header=T)
  d4 <- read.table(fnames[4], header=T)
  d <- rbind(d1, d2, d3, d4)
  x <- ddply(subset(d, time!=-1 & experiment != "practice"), # & !condition%in%c('i','j')
           .(experiment, item, condition), function(d) {
             displayUpdateStart <- subset(d, event=="displayUpdateStart")
             displayUpdateEnd <- subset(d, event=="displayUpdateEnd")
             signalStart <- subset(d, event=="signalStart")
             signalEnd <- subset(d, event=="signalEnd")
             satResponse <- subset(d, event=="satResponse")
             satResponse$time <- satResponse$time-max(displayUpdateEnd$time)
             satResponse$lastResponseCorrect <- tail(satResponse$responseCorrect,1)
             satResponse
           } )
  x
}

d2001 <- load.participant(paste('1001', '.RaceArg_Block',1:4,'a.dat', sep=""))
d2002 <- load.participant(paste('2002', '.RaceArg_Block',1:4,'a.dat', sep=""))
d2003 <- load.participant(paste('2003', '.RaceArg_Block',1:4,'b.dat', sep=""))
d2004 <- load.participant(paste('2004', '.RaceArg_Block',1:4,'b.dat', sep=""))
d2005 <- load.participant(paste('2005', '.RaceArg_Block',1:4,'a.dat', sep=""))
d2006 <- load.participant(paste('2006', '.RaceArg_Block',1:4,'a.dat', sep=""))
d2007 <- load.participant(paste('2007', '.RaceArg_Block',1:4,'b.dat', sep=""))
d2008 <- load.participant(paste('2008', '.RaceArg_Block',1:4,'b.dat', sep=""))
d2009 <- load.participant(paste('2009', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))

# useless data
d2010 <- load.participant(paste('2010', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))


d2011 <- load.participant(paste('2011', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2012 <- load.participant(paste('2012', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2013 <- load.participant(paste('2013', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))
d2014 <- load.participant(paste('2014', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))
d2015 <- load.participant(paste('2015', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2016 <- load.participant(paste('2016', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2017 <- load.participant(paste('2017', '.RaceArg_Block',1:4,'a.dat', sep=""))
d2018 <- load.participant(paste('2018', '.RaceArg_Block',1:4,'a.dat', sep=""))
d2019 <- load.participant(paste('2019', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2020 <- load.participant(paste('2020', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2021 <- load.participant(paste('2021', '.RaceArg_Block',1:4,'b.dat', sep=""))
d2022 <- load.participant(paste('2022', '.RaceArg_Block',1:4,'b.dat', sep=""))
#d2023 <- load.participant(paste('2023', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))
d2024 <- load.participant(paste('2024', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))

d2110 <- load.participant(paste('2110', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))
d2113 <- load.participant(paste('2113', '.RaceArg_rev_Block',1:4,'a.dat', sep=""))
d2116 <- load.participant(paste('2116', '.RaceArg_rev_Block',1:4,'b.dat', sep=""))

head(x)

plot(subset(x, T)$time, ylim=c(-.5,5), col=(x$interval))

#x <- subset(x, experiment=="RACE.ARG")

with(x, tapply(time, list(condition,interval), length))

with(x, tapply(time, interval, mean))

cbind( with(x, tapply(time, interval, min)),
       with(x, tapply(time, interval, max)))

x$item.num <- asi(x$item)
unique(x$item.num)
head(subset(x, item.num=="5"))
# 5a, 8a, 11i

# subset(x, item.num%in%12:20)
x$new.interval <- round(x$time*10/4)*4/10



# PLOT: Overall accuracy
y <- ddply(subset(d2024, time > -0.5), .(interval), function(d) {
  c(t=mean(d$time), gram=mean(d$responseCorrect=="True"))
})
subset(y, interval==14)

library(ggplot2)
ggplot(subset(y, !is.na(interval)), aes(x=t, y=gram))+geom_line()+geom_point()

# 2001: 94, # 2002: 92
# 2003: 88, # 2004: 87
# 2005: 90, # 2006: 80
# 2007: 92, # 2008: 87
# 2009: 83, # 2010: 59*** (2110: 93)
# 2011: 80, # 2012: 97
# 2013: 69*** (2113: 79)
# 2014: 91, # 2015: 92,
# 2016: 78*** (2116: 80)
# 2017: 82, # 2018: 94
# 2019: 92, # 2020: 95
# 2021: 84, # 2022: 95
# 2024: 96


# PLOT: Accuracy by condition
y <- ddply(subset(d2024, time > -0.5), .(condition, interval), function(d) {
  c(t=mean(d$time), gram=mean(d$responseGrammatical=="Y"))
})
library(ggplot2)
ggplot(subset(y, !is.na(interval)), aes(x=t, y=gram, color=condition))+geom_line()+geom_point()


dim(subset(x, interval==5)


dprime.conds <- function(d, cond1, cond2) {
  qnorm(min(mean(subset(d, condition==cond1)$responseGrammatical=="Y"),.99))-
  qnorm(max(mean(subset(d, condition==cond2)$responseGrammatical=="Y"),.01))
}

y2 <- ddply(subset(d2113, time>-2 & !is.na(interval)), .(interval), function(x) {
  d <- c(dprime.conds(x, "a", "d"),
    dprime.conds(x, "b", "e"),
    dprime.conds(x, "c", "f") )
  cond <- c("amb","dat","gen")
  data.frame(t=mean(x$time), d, condition=cond)
})
ggplot(y2, aes(x=t, y=d, color=condition))+geom_line()+geom_point()

+scale_y_continuous(limits=c(-1,6))


with(x, tapply(time, interval, function(x) quantile(x, .2)))


dim( unique(x[,c('experiment','item','condition')]) )

subset(x, time < -10)

hist(x$time)


unique( subset(x, lastResponseCorrect=="False")[,c('experiment','item','condition')] )

subset(x, item=="37-39-1" & condition=="c")

unique(d$event)
