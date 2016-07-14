setwd("~/Desktop/SAT/RTExp_SAT/SAT/RTExp/Experimente/EXPT.RACERC.SAT/Results/")

fnames <- c("2000.block1.dat","2000.block2.dat","2000.block3.dat","2000.block4.dat")
library(plyr)
d <- ldply(fnames, function(fn) {
  read.table(fn, header=T)
})

d <- subset(d, event=="satResponse")

head(d)

cond <- ldply(strsplit(asc(d$condition), "-"), function(v) c('gender'=v[1], 'att'=v[2]))
d <- cbind(d, cond)

library(sdtalt)

x <- ddply(d, .(interval), function(d) {
if(TRUE)  {
  res <- with(d, tapply(responseGrammatical, list(att, responseGrammatical), length ))
  meas <- c('all')
  flat <- .5
  ldply(c('amb','high','low'), function(att) {
    res <- sdt(hits=res[att,'Y'], fas=res['none','Y'], misses=res[att,'N'], cr=res['none','N'], meas=meas, flat=flat, confl=.9)
    c(att=att, d=res$d)
  }) 
} else {
  x <- with(d, aggregate(responseCorrect=="True", list(att=att), mean, na.rm=T))
  y <- with(d, aggregate(responseCorrect=="True", list(att=att), se))
  merge(x, y, by=c('att'))
}
})
x


library(ggplot2)

qplot(data=x, x=interval*400, y=asdc(d), color=att)+geom_path()

qplot(data=x, x=interval*400, y=x.x, color=att)+geom_path()+
geom_errorbar(aes(ymin=x.x-x.y*2, ymax=x.x+x.y*2), width=.1)

