library(plyr)

# Notes:
#

source("./PrepDataFunctions.R")

#########################################################################
#########################################################################
#########################################################################


data.dir <- '../0_data_raw/'
d <- NULL
for(part.num in c(1001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
                  2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2110, 2113,
                  2019, 2116, 2020, 2021, 2022, 2023, 2024)) # 2025, 2026, 2027
{
  cat("participant", part.num)
  d_cur <- ReadParticipant(data.dir, part.num)
  d_cur <- PrepSATData(d_cur)
  d <- rbind(d, d_cur)
}

d$time <- d$time-min(d$time, na.rm=T)
save(d, file="allsubj_raw.rda")
# (load("allsubj_raw.rda"))

library(plyr)

# determine the average time required for the experiment
exp_time_bysubj <- dlply(d, .(subject), function(d) { diff(range(d$time, na.rm=T))/(60) })
mean(unlist(exp_time_bysubj))

tapply(d$item, d$subject, function(x) length(unique(x)))

d.resp <- subset(d, event=="satResponse" & !is.na(signal.time) & signal.time>.1)
save(d.resp, file="allsubj_resp.rda")

(load("allsubj_resp.rda"))

###############################################################
# Length of practice and of the experimental session.
###############################################################
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1001))
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d1 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==2002))
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d2 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==2003))
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d3 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==2004))
# d[d$new.interval==15,'new.interval'] <- 14
# d4 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1012 & signal.time<5.6))
# d12 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1013))
# d[d$new.interval==15,'new.interval'] <- 14
# d13 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1014))
# d[d$new.interval==15,'new.interval'] <- 14
# d14 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1015))
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d15 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1016), stretch=1.03)
# d[d$new.interval==15,'new.interval'] <- 14
# d16 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1017))
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d17 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1018) )
# d18 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1019) )
# d19 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1020) )
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d20 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1021), stretch=.99)
# d[d$new.interval==15,'new.interval'] <- 14
# d21 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1022), trial.effect=2000)
# d22 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1023), stretch=1)
# d[d$new.interval==1,'new.interval'] <- 2
# d$new.interval <- d$new.interval-1 
# d23 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1024), stretch=0.99)
# d24 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1025), stretch=0.99)
# d[d$new.interval==14,'new.interval'] <- 13
# d25 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1026), stretch=1.02)
# d[d$new.interval==15,'new.interval'] <- 14
# d26 <- d
# 
# d <- fn.determine.intervals(subset(d.resp, subject==1027), stretch=1)
# d[d$new.interval==15,'new.interval'] <- 14
# d27 <- d
# 
# data <- rbind(d8, d9, d10, d11, d12, d13, d14,
#               d15, d16, d17, d18, d19, d20,
#               d21, d22, d23, d24, d25, d26, d27)


data <- d.resp
data$interval <- data$new.interval
data$new.interval <- NULL
data$time.for.classification <- NULL
data$index <- NULL
data$event <- NULL
data$RT <- NULL

# new.data <- 
# ddply(data, .(subject), function(d) {
#   print(paste("subject", unique(d$subject)))
#   (medians <- with(d, tapply(signal.time, interval, median)))
#   new.data <- ddply(d, .(trial.id, interval), function(d) {
#                          interval <- d$interval[1]
#                          distances <- abs(d$signal.time-medians[interval])
#                          selection <- which.min( distances )
#                          d[selection,]
#                        } )
# })


# 
# data <- new.data
# 
# data <- subset(data, interval != 14)
# data$time <- data$stim.time
# data$full.condition <- data$condition
# 
# data.rc <- subset(data, experiment=="RACE.RC")
# data.loc <- subset(data, experiment=="LOCALITY")
# 
# data.rc <- within(data.rc, {
#   gender <- sapply(strsplit(full.condition, "-"), function(x) x[1])
#   condition <- sapply(strsplit(full.condition, "-"), function(x) x[2])
#   responseCorrect <- nmap(responseCorrect, c("True"=1, "False"=0), as.integer)
#   responseGrammatical <- nmap(responseGrammatical, c("Y"=1, "N"=0), as.integer )
#   stimulusGrammatical <- nmap(condition, c("none"=0, "amb"=1, "high"=1,"low"=1), as.integer)
# })
# data.loc <- within(data.loc, {
#   cond.num.np1 <- sapply(strsplit(full.condition, "-"), function(x) x[1])
#   cond.num.np2 <- sapply(strsplit(full.condition, "-"), function(x) x[2])
#   condition <- sapply(strsplit(full.condition, "-"), function(x) x[3])
#   responseCorrect <- nmap(responseCorrect, c("True"=1, "False"=0), as.integer)
#   responseGrammatical <- nmap(responseGrammatical, c("Y"=1, "N"=0), as.integer )
# })

data.arg <- data

# default button
c(1a='-', 2a='-', 3a='+', 4a='+', 
  1b='+', 2b='+', 3b='-', 4b='-',
  rev_1a='+', rev_2a='+', rev_3a='-', rev_4a='-', 
  rev_1b='-', rev_2b='-', rev_3b='+', rev_4b='+')

# order of block presentation
c(1a=1, 2a=2, 3a=3, 4a=4,
  1b=1, 2b=2, 3b=3, 4b=4,
  rev_1a=1, rev_2a=2, rev_3a=3, rev_4a=4,
  rev_1b=1, rev_2b=2, rev_3b=3, rev_4b=4)

data.arg$time <- data.arg$stim.time
data.arg$response_yes <- as.integer(data.arg$responseGrammatical=="Y")



### Conditions:
# 1-11-5 a  Was befahl damals der_Chef $ der_Bäckerin?
# 1-11-5 d *Wem befahl das der_Chef damals $ der_Bäckerin?

# 1-11-5 b  Was befahl der_Chef damals $ dem_Bäcker?
# 1-11-5 e *Wem befahl das damals der_Chef $ dem_Bäcker?

# 1-11-5 c  Wem befahl das damals der_Chef $ des_Bäckers?
# 1-11-5 f *Was befahl der_Chef damals $ des_Bäckers?

# 1-11-5 i  Wem befahl das damals $ der_General?
# 1-11-5 j *Was befahl damals $ der_Gedanke?



data.arg$last_phrase <- with(data.arg, ifelse(condition %in% c('a','d'), "fem_amb", 
                                              ifelse(condition %in% c('b','e'), "masc_dat", 
                                                     ifelse(condition %in% c('c','f'), "masc_gen", "other")
                                              )))


data.arg$grammatical <- with(data.arg, ifelse(condition %in% c('a','b','c','i'), 1, 0) )

data.arg$block <- gsub(".*\\.RaceArg_(.*)Block(.*)\\.dat", "\\1\\2", data.arg$block)

# split up block info into block id and presentation order
block_info <- strsplit(data.arg$block, "_")
data.arg$block_id <- sapply(block_info, function(x) x[length(x)])
data.arg$block_order <- sapply(block_info, function(x) if(length(x)==1) "forw" else x[length(x)-1])

# let's get an overview of the numbers of participants for different presentation orders, etc.
data_meta_info <- unique( data.arg[,c('subject', 'block', 'block_id', 'block_order')] )
with(data_meta_info, xtabs(~block_id + block_order))
# there are two extra participants in reverse A, and one extra in reverse B:
# -> these are the participants with an accuracy of almost 0 for the genetive modifier condition,
#    so I recorded replacement participants for those

# exclude participants with a d' of (nearly) 0 at the latest lag in the genitive conditions
subj_no_genitive <- c(2010, 2013, 2016) # there are two more suspicious subjects: 2011 and 2116
data.arg <- subset(data.arg, !(subject %in% subj_no_genitive))

# another an overview of the numbers of participants for different presentation orders, etc.
data_meta_info <- unique( data.arg[,c('subject', 'block', 'block_id', 'block_order')] )
with(data_meta_info, xtabs(~block_id + block_order))
# everything is balanced now

save(data.arg, file="../../Experiment2_Arg_preprocessed.rda")
