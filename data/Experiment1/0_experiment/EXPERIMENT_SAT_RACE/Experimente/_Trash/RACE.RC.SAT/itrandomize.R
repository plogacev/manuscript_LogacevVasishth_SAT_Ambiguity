
d <- read.table("items_all_blocks", quote="'", header=F)

x <- unlist(strsplit(asc(d$V7), "-"))
d$cond <- x[1:(length(x)/2)*2]
d$gender <- x[1:(length(d)/2)*2-1]
d$item <- d$V4


nrow(d)/6

check.c1 <- function(d, do.print) {
  gram.n <- asc(d$V1)
  gram.nplus1 <- c(gram.n[-1], NA)   
  gram.nplus2 <- c(gram.nplus1[-1], NA)

  block <- rep(1:16, each=24)

  (n.nplus1 <- with(d, xtabs(~gram.n+gram.nplus1)))
  x1 <- var(as.vector(n.nplus1/sum(n.nplus1)))
  (n.nplus2 <- with(d, xtabs(~gram.n+gram.nplus2)))
  x2 <- var(as.vector(n.nplus2/sum(n.nplus2)))
  (n.nplus1.byblock <- with(d, xtabs(~gram.n+gram.nplus1+block)))
  x3 <- var(as.vector(n.nplus1.byblock/sum(n.nplus1.byblock)))
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
  distances <- dlply(d, .(item), function(d) {
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
  cond.n <- asc(d$cond)
  cond.nplus1 <- c(cond.n[-1], NA)   
  cond.nplus2 <- c(cond.nplus1[-1], NA)   
  
  (n.nplus1 <- with(d, xtabs(~cond.n+cond.nplus1)))
  x1a <- var(as.vector( n.nplus1[c('amb','high','low'),c('amb','high','low')] ))
  x1b <- var( n.nplus1[c('none'),c('amb','high','low')] )
  x1c <- var( n.nplus1[c('amb','high','low'), c('none')] )

  (n.nplus2 <- with(d, xtabs(~cond.n+cond.nplus2)))
  x2a <- var(as.vector( n.nplus1[c('amb','high','low'),c('amb','high','low')] ))
  x2b <- var( n.nplus1[c('none'),c('amb','high','low')] )
  x2c <- var( n.nplus1[c('amb','high','low'), c('none')] )
  if(do.print) {
    print(n.nplus1)
    print(n.nplus2)
    print(c(x1a, x1b, x1c, x2a, x2b, x2c))
  }
  c(sum( c(x1a, x2a) ), sum( c( x1b, x1c, x2b, x2c) )) 
}

check.c4 <- function(d, do.print) {
  library(plyr)
  conditions <- ddply(d, .(item), function(d) {
    last <- nrow(d)
    n.plus1 <- c(NA,d$cond[-last])
    n.plus2 <- c(NA,NA,d$cond[-last+c(+1,0)])
    data.frame(n=d$cond, n.plus1=n.plus1, n.plus2=n.plus2)
  })

  n.n.plus1 <- with(conditions, xtabs(~n+n.plus1))
  x1a <- var(as.vector( n.n.plus1[c('amb','high','low'),c('amb','high','low')] ))
  x1b <- var( n.n.plus1[c('none'),c('amb','high','low')] )
  x1c <- var( n.n.plus1[c('amb','high','low'), c('none')] )

  n.plus1.n.plus2 <- with(conditions, xtabs(~n.plus1+n.plus2))
  x2a <- var(as.vector( n.plus1.n.plus2[c('amb','high','low'),c('amb','high','low')] ))
  x2b <- var( n.plus1.n.plus2[c('none'),c('amb','high','low')] )
  x2c <- var( n.plus1.n.plus2[c('amb','high','low'), c('none')] )

  if(do.print) {
    print(n.n.plus1)
    print(n.plus1.n.plus2)
    print(c(x1a, x1b, x1c, x2a, x2b, x2c))
  }
  
  c(sum(x1a,x1b,x1c), sum(x2a,x2b,x2c))
}
check.c5 <- function(d, do.print) {
  item.nplus1 <- c(d$item[-1], NA)
  item.nplus2 <- c(item.nplus1[-1], NA)
  item.nplus3 <- c(item.nplus2[-1], NA)
  x1<-pmax(xtabs(~d$item+item.nplus1)-1, 0)
  x2<-pmax(xtabs(~d$item+item.nplus2)-1, 0)
  x3<-pmax(xtabs(~d$item+item.nplus3)-1, 0)
  sum(c(x1,x2,x3)^10)
}

check.constraints <- function(d, enabled, do.print=F) {
  if(enabled[1]) c1 <- check.c1(d, do.print)
  else           c1 <- 0
  if(enabled[2]) c2 <- check.c2(d, do.print)
  else           c2 <- 0
  if(enabled[3]) c3 <- check.c3(d, do.print)
  else           c3 <- c(0,0)
  if(enabled[4]) c4 <- check.c4(d, do.print)
  else           c4 <- c(0,0)
  if(enabled[5]) c5 <- check.c5(d, do.print)
  else           c5 <- 0
  c(c1, c2, c3[1], c3[2], c4[1], c4[2], c5)
}

generate.new.candidate <- function(d, constraints=c()) {
  num <- 5#round(runif(1, min=1.5, max=10.49))
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

d.new <- generate.new.candidate(d, c(V4=FALSE))

check.constraints(d, enabled.constraints, do.print=T)

do.plot <- T
enabled.constraints <- c(1, 1, 1, 1, 1)
violations <- t(data.frame(check.constraints(d, enabled.constraints)))
while(TRUE) {
  d.new <- generate.new.candidate(d, c())#, c(V4=FALSE))
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


check.c4(d, T)


write.table(d, file="items_all_almost_optimized")

append(list(), list(c(1,2)))

d.saved <- d
