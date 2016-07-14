

check.c1 <- function(d, do.print) {
  gram.n <- asc(d$gram)
  gram.nplus1 <- c(gram.n[-1], NA)   
  gram.nplus2 <- c(gram.nplus1[-1], NA)

  block <- rep(1:16, each=25)

  (n.nplus1 <- with(d, xtabs(~gram.n+gram.nplus1)))
  x1 <- sum( n.nplus1^10 )
  (n.nplus2 <- with(d, xtabs(~gram.n+gram.nplus2)))
  x2 <- sum( n.nplus2^8 )
#  (n.nplus1.byblock <- with(d, xtabs(~gram.n+gram.nplus1+block)))
  x3 <- 0#var(as.vector(n.nplus1.byblock/sum(n.nplus1.byblock)))
  if(do.print) {
    print("c1: (un)grammatical-(un)grammatical-(un)grammatical sequence")
    print(n.nplus1)
    print(n.nplus2)
    print(c(x1, x2))
  }
  x1+x2+x3
}


check.c2 <- function(d, do.print) {
  library(plyr)
  rownames(d) <- NULL
  distances <- dlply(d, .(item,exp), function(d) {
    diff(asic(rownames(d)))
  })
  distances <- unlist(distances)
  min <- min(distances)
  num.min <- sum(distances==min)
  if(do.print) {
    print("c2: distance between instances of the same item")
    print(summary(distances))
    print(c(min, num.min))
  }
  (nrow(d)-min)*nrow(d)+num.min
}

check.c3 <- function(d, do.print) {
  cond.n <- asc(d$exp.cond)
  cond.nplus1 <- c(cond.n[-1], NA)   
  cond.nplus2 <- c(cond.nplus1[-1], NA)   
  
  (n.nplus1 <- with(d, xtabs(~cond.n+cond.nplus1)))
  x1 <-  sum( n.nplus1^10 ) #var(as.vector(n.nplus1))

  (n.nplus2 <- with(d, xtabs(~cond.n+cond.nplus2)))
  x2 <-  sum( n.nplus2^10 ) # var(as.vector( n.nplus1))
  if(do.print) {
    print(n.nplus1)
    print(n.nplus2)
    print(c(x1, x2))
  }
  x1+x2/2
}

check.c4 <- function(d, do.print) {
  library(plyr)
  conditions <- ddply(d, .(exp,item), function(d) {
    last <- nrow(d)
    n.plus1 <- c(NA,d$cond[-last])
    n.plus2 <- c(NA,NA,d$cond[-last+c(+1,0)])
    data.frame(n=d$cond, n.plus1=n.plus1, n.plus2=n.plus2)
  })

  n.nplus1 <- with(conditions, xtabs(~n+n.plus1))
  x1 <-  sum( n.nplus1^10 )
  n.nplus2 <- with(conditions, xtabs(~n+n.plus2))
  x2 <- sum( n.nplus2^10 )

  if(do.print) {
    print(n.nplus1)
    print(n.nplus2)
    print(c(x1, x2))
  }
  x1+x2/2
}

check.c5 <- function(d, do.print) {
  item.nplus1 <- c(d$exp.item[-1], NA)
  item.nplus2 <- c(item.nplus1[-1], NA)
  item.nplus3 <- c(item.nplus2[-1], NA)
  n.nplus1 <- xtabs(~d$exp.item+item.nplus1)
  n.nplus2 <- xtabs(~d$exp.item+item.nplus2)
  n.nplus3 <- xtabs(~d$exp.item+item.nplus3)
  x1<-sum( n.nplus1^10 ) #pmax(n.nplus1-1, 0)
  x2<-sum( n.nplus2^8 ) #pmax(n.nplus2-1, 0)
  x3<-sum( n.nplus3^6 ) #pmax(n.nplus3-1, 0)
  if(do.print) {
    print("constraint c5")
    print(n.nplus1)
    print(n.nplus2)
    print(n.nplus3)
    print(summary(as.vector(n.nplus1)))
    print(summary(as.vector(n.nplus2)))
    print(summary(as.vector(n.nplus3)))
  }
  sum(c(x1,x2,x3)^10)
}


check.c6 <- function(d, do.print) {
  exp.n <- asc(d$exp)
  exp.nplus1 <- c(exp.n[-1], NA)   
  exp.nplus2 <- c(exp.nplus1[-1], NA)

#  block <- rep(1:16, each=25)

  (n.nplus1 <- with(d, xtabs(~exp.n+exp.nplus1)))
  x1 <- sum( n.nplus1^10 )
  (n.nplus2 <- with(d, xtabs(~exp.n+exp.nplus2)))
  x2 <- sum( n.nplus2^8 )
  #x2 <- var(as.vector(n.nplus2/sum(n.nplus2)))
#  (n.nplus1.byblock <- with(d, xtabs(~exp.n+exp.nplus1+block)))
  x3 <- 0#var(as.vector(n.nplus1.byblock/sum(n.nplus1.byblock)))
  if(do.print) {
    print("c6: experiment order")
    print(n.nplus1)
    print(n.nplus2)
    print(c(x1, x2))
  }
  x1+x2+x3
}


check.c7 <- function(d, do.print) {
  library(plyr)
  conditions <- ddply(d, .(exp), function(d) {
    last <- nrow(d)
    n.plus1 <- c(NA,d$cond[-last])
    n.plus2 <- c(NA,NA,d$cond[-last+c(+1,0)])
    data.frame(n=d$cond, n.plus1=n.plus1, n.plus2=n.plus2)
  })

  n.nplus1 <- with(conditions, xtabs(~n+n.plus1))
  x1 <-  sum( n.nplus1^10 ) #var(as.vector( n.n.plus1 ))

  n.nplus2 <- with(conditions, xtabs(~n+n.plus2))
  x2 <- sum( n.nplus2^8 ) #var(as.vector( n.plus1.n.plus2 ))

  if(do.print) {
    print(n.nplus1)
    print(n.nplus2)
    print(c(x1, x2))
  }
  c(x1, x2)
}

check.constraints <- function(d, enabled, do.print=F) {
  if(enabled[1]) c1 <- check.c1(d, do.print)
  else           c1 <- 0
  if(enabled[2]) c2 <- check.c2(d, do.print)
  else           c2 <- 0
  if(enabled[3]) c3 <- check.c3(d, do.print)
  else           c3 <- 0
  if(enabled[4]) c4 <- check.c4(d, do.print)
  else           c4 <- 0
  if(enabled[5]) c5 <- check.c5(d, do.print)
  else           c5 <- 0
  if(enabled[6]) c6 <- check.c6(d, do.print)
  else           c6 <- 0
  if(enabled[7]) c7 <- check.c7(d, do.print)
  else           c7 <- 0
  c(c1, c2, c3, c4, c5, c6, c7)
}

generate.new.candidate <- function(d, constraints=c(), num=c(1.5, 5.49)) {
  if(length(num) > 1)
    num <- round(runif(1, min=num[1], max=num[2]))
  if(length(constraints)==0) {
    pos <- sample(1:nrow(d), num)
    new.pos <- sample(pos)
  } else {
    repeat {
      first.pos <- sample(1:nrow(d), 1)
      selection <- rep(T, nrow(d))
      for(i in 1:length(constraints)) {
        name <- names(constraints[i])
        selection <- selection & (d[,name]==d[first.pos,name])==constraints[i]
        selection[first.pos] <- FALSE
      }
      possible.pos <- asic(rownames(d[selection,]))
      other.pos <-sample(possible.pos, num-1)
      pos <- c(first.pos,other.pos)
      new.pos <- sample(pos)
      for(i in 1:length(constraints)) {
        name <- names(constraints[i])
        if(constraints[i] == FALSE)
          if( length(d[new.pos,name]) > length(unique(d[new.pos,name])) ) {
            next;
          }
      }
      break;
    }
  }
  d[new.pos,] <- d[pos,]
  d
}



#cond <- sapply(strsplit(asc(d$cond), "-"), function(x) {
#  if(length(x)==1) { x } else { x[length(x)] }})
#d$cond <- cond
#d$exp.cond <- paste(d$exp, d$cond)
#d.both <- d
#d.arg <- subset(d, exp=="race.arg")
#d.rc <- subset(d, exp=="race.rc")


d <- read.table("items_all_almost_optimized", header=F, as.is=T)

map(d$V1, c("{$RACERC_UNGRAM,","{$RACERC_GRAM,","{$LOCALITY_GRAM,","{$LOCALITY_UNGRAM,"), c("RACERC","RACERC","LOCALITY","LOCALITY")) -> d$exp
map(d$V1, c("{$RACERC_UNGRAM,","{$RACERC_GRAM,","{$LOCALITY_GRAM,","{$LOCALITY_UNGRAM,"), c("ungram","gram","ungram","gram")) -> d$gram
d$cond <- d$V7
d$item <- d$V4

d$exp.item <- paste(d$exp, d$item)
d$exp.cond <- paste(d$exp, d$cond)


d.loc <- subset(d, exp=="LOCALITY")
d.rc <- subset(d, exp=="RACERC")

d <- d.loc

do.plot <- T
enabled.constraints <- c(1, 1, 1, 1, 1, 1, 0)
violations <- t(data.frame(check.constraints(d, enabled.constraints)))
nums <- c()
while(TRUE) {
 for(i in 1:1000) {
  d.new <- generate.new.candidate(d, c(), num=c(2.5,6.49))
  violation <- check.constraints(d.new, enabled.constraints)
  if(all( violation <= violations[nrow(violations),] )) {
    violations <- rbind(violations, violation)
    d <- d.new
    if(do.plot==T) {
      par(mfrow=c(3,3))
      plot(violations[,1]); plot(nrow(d)-violations[,2]/nrow(d));
      plot(violations[,3]); plot(violations[,4])
      plot(violations[,5]); plot(violations[,6]);
      plot(violations[,7], main=violations[nrow(violations),7]); plot(violations[,8]); plot(violations[,9]);
    }
  }
 }
 write.table(d, file="LOCALITY/items_all_almost_optimized")
 print("wrote to disk")
}
write.table(d, file="LOCALITY/items_all_almost_optimized")



d <- d.rc

do.plot <- T
enabled.constraints <- c(1, 1, 1, 1, 1, 1, 0)
violations <- t(data.frame(check.constraints(d, enabled.constraints)))
nums <- c()
while(TRUE) {
 for(i in 1:1000) {
  d.new <- generate.new.candidate(d, c(), num=3)#c(2.5,6.49))
  violation <- check.constraints(d.new, enabled.constraints)
  if(all( violation <= violations[nrow(violations),] )) {
    violations <- rbind(violations, violation)
    d <- d.new
    if(do.plot==T) {
      par(mfrow=c(3,3))
      plot(violations[,1]); plot(nrow(d)-violations[,2]/nrow(d));
      plot(violations[,3]); plot(violations[,4])
      plot(violations[,5]); plot(violations[,6]);
      plot(violations[,7], main=violations[nrow(violations),7]); plot(violations[,8]); plot(violations[,9]);
    }
  }
 }
 write.table(d, file="RACE.RC.SAT/items_all_almost_optimized")
 print("wrote to disk")
}
write.table(d, file="RACE.RC.SAT/items_all_almost_optimized")


check.c3(d, T)


d1 <- read.table("RACE.RC.SAT/items_all_almost_optimized", header=T, as.is=T)
d2 <- read.table("LOCALITY/items_all_almost_optimized", header=T, as.is=T)

idx <- sample(1:(nrow(d1)+nrow(d2)))
idx1 <- sort(idx[1:nrow(d1)])
idx2 <- sort(idx[nrow(d1)+c(1:nrow(d2))])

d12 <- rbind(d1, d2)
d12[idx1,] <- d1
d12[idx2,] <- d2


d <- d12

do.plot <- T
enabled.constraints <- c(1, 1, 1, 1, 1, 1, 1)
violations <- t(data.frame(check.constraints(d, enabled.constraints)))
nums <- c()
while(TRUE) {
 for(i in 1:1000) {
  d.new <- generate.new.candidate(d, c(), num=c(1.5,4.49))
  violation <- check.constraints(d.new, enabled.constraints)
  if(all( violation <= violations[nrow(violations),] )) {
    violations <- rbind(violations, violation)
    d <- d.new
    if(do.plot==T) {
      par(mfrow=c(3,3))
      plot(violations[,1]); plot(nrow(d)-violations[,2]/nrow(d));
      plot(violations[,3]); plot(violations[,4])
      plot(violations[,5]); plot(violations[,6]);
      plot(violations[,7]); 
    }
  }
 }
 write.table(d, file="items_all_almost_optimized")
 print("wrote to disk")
}

check.c3(d,T)





# DIVIDE into 16 blocks with 32 sentences each
nrow(d)/(2^4)

d$V4 <- as.character(d$V4)
d$item <- as.character(d$item)

d.new <- data.frame()
row1 <- c('---', rep('', ncol(d)-1))
row3 <- c('---', rep('', ncol(d)-1))
for(i in 1:16) { #seq(1, nrow(d), 49)
  j <- (i-1)*32;
  idx <- (1+j:(j+31))
  row2 <- c(paste('Pause', i, 'von', 16) , rep('', ncol(d)-1))
  print(row2)
  pause <- data.frame(rbind(row1, row2, row3), row.names=NULL)
  colnames(pause) <- colnames(d)
  d.new <- rbind(d.new, pause, d[idx,])
}

d.new <- d.new[, paste('V',1:11,sep="")]

#map(asc(d.new$V1), c('{$RACE_ARG_UNGRAM', '{$RACE_ARG_GRAM','{$RACE_ARG_UNGRAM,','{$RACE_ARG_GRAM,'), c('\"{$RACE_ARG_UNGRAM', '{$RACE_ARG_GRAM','{$RACE_ARG_UNGRAM,','{$RACE_ARG_GRAM,'), ) -> d.new$V1
#map(d.new$V11, c('}\"'), c('}'), ) -> d.new$V11

d.new <- within(d.new, {V2 <- paste("'",V2,"'",sep="")
                        V4 <- paste("'",V4,"'",sep="")
                        V7 <- paste("'",V7,"'",sep="")
                        V10 <- paste("'",V10,"'",sep="")
                      })


write.table(d.new, file="items_all_final", row.names=F, col.names=F)
