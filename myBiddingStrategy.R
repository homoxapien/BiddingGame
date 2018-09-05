##bidding strategy
#dumbs' bids depend on each of their budget(left)
d_BID <- function(budget){
  return(runif(1, min=0, max=budget))
}
#my bid depends on the value of the current item, the total value left and the number of items I got so far
m_BID <- function(value, past_value, MM){
  if(MM$collect < 8){
    if(value > (20000-past_value)/2){
      return(MM$budget)
    }else if(value > (20000-past_value)/(10-MM$collect)){
      return(value*MM$budget/(20000-past_value))
    }else{
      return(0)
    }
  }else if(MM$collect == 8){
    if(value > (20000-past_value)/2){
      return(value*MM$budget/(20000-past_value))
    }else{
      return(0)
    }
  }else{
    if(value > (20000-past_value)/2){
      return(MM$budget)
    }else{
      return(0)
    }
  }
}
#smart guy's bid depends on his budget and my bid
s_BID <- function(budget, master){
  if(budget>=master){
    sbid <- rnorm(1, master+1, (budget-master)/3)
  }else{
    sbid <- 0 #incapable s
  }
  
  if(sbid<=0){
    return(0)
  }else if(sbid>budget){
    return(budget)
  }else{
    return(sbid)
  }
}

##Simulation_0
play0 <- function(){
  
  ###players
  DD <- data.frame(role=rep('d',9), budget=rep(2000,9), collect=rep(0,9))
  MM <- data.frame(role='m', budget=2000, collect=0, profit=0)
  
  ###items
  temp1 <- sort(c(sample(1:19999, 99), 20000))
  temp0 <- c(0, temp1[-length(temp1)])
  stock <- temp1 - temp0
  
  ###game starts
  for(item in 1:100){
    value <- stock[item]
    past_value <- temp0[item]
    if(dim(DD)[1]!=0){
      dbidList <- round(sapply(DD$budget, d_BID),0)
      dbid <- max(dbidList)
    }else{
      dbid <- 0
    }
    mbid <- m_BID(value, past_value, MM)
    
    if(mbid >= dbid){
      MM$collect <- MM$collect + 1
      MM$budget <- MM$budget - mbid
      MM$profit <- MM$profit + (value-mbid)
    }else{
      DD$collect[which.max(dbidList)] <- DD$collect[which.max(dbidList)] + 1
      DD$budget[which.max(dbidList)] <- DD$budget[which.max(dbidList)] - dbid
    }
    DD <- DD[DD$collect<10,]
    DD <- DD[DD$budget>0,]
    if(MM$collect == 10 | MM$budget == 0){
      break
    }
  }
  return(MM$profit)
}

##Simulation_1
play1 <- function(){
  
  ###players
  DD <- data.frame(role=rep('d',8), budget=rep(2000,8), collect=rep(0,8))
  SS <- data.frame(role=rep('s',1), budget=rep(2000,1), collect=rep(0,1))
  MM <- data.frame(role='m', budget=2000, collect=0, profit=0)
  
  ###items
  temp1 <- sort(c(sample(1:19999, 99), 20000))
  temp0 <- c(0, temp1[-length(temp1)])
  stock <- temp1 - temp0
  
  ###game starts
  for(item in 1:100){
    value <- stock[item]
    past_value <- temp0[item]
    ###d's bidding
    if(dim(DD)[1]!=0){
      dbidList <- round(sapply(DD$budget, d_BID),0)
      dbid <- max(dbidList)
    }else{
      dbid <- 0
    }
    ###m's bidding
    mbid <- m_BID(value, past_value, MM)
    ###s's bidding
    if(dim(SS)[1]!=0){
      sbid <- round(s_BID(SS$budget,mbid),0)
    }else{
      sbid <- 0
    }
    
    if(sbid >= dbid){
      if(mbid > sbid){ #s aims at m
        MM$collect <- MM$collect + 1
        MM$budget <- MM$budget - mbid
        MM$profit <- MM$profit + (value-mbid)
      }else{
        SS$collect <- SS$collect + 1
        SS$budget <- SS$budget - sbid
      }
      SS <- SS[SS$collect<10,]
      SS <- SS[SS$budget>0,]
    }else{
      if(mbid >= dbid){ #m is faster than d
        MM$collect <- MM$collect + 1
        MM$budget <- MM$budget - mbid
        MM$profit <- MM$profit + (value-mbid)
      }else{
        DD$collect[which.max(dbidList)] <- DD$collect[which.max(dbidList)] + 1
        DD$budget[which.max(dbidList)] <- DD$budget[which.max(dbidList)] - dbid
      }
      DD <- DD[DD$collect<10,]
      DD <- DD[DD$budget>0,]
    }
    
    if(MM$collect == 10 | MM$budget == 0){
      break
    }
  }
  return(MM$profit)
}

##Simulation_2
play2 <- function(){
  
  ###players
  DD <- data.frame(role=rep('d',7), budget=rep(2000,7), collect=rep(0,7))
  SS <- data.frame(role=rep('s',2), budget=rep(2000,2), collect=rep(0,2))
  MM <- data.frame(role='m', budget=2000, collect=0, profit=0)
  
  ###items
  temp1 <- sort(c(sample(1:19999, 99), 20000))
  temp0 <- c(0, temp1[-length(temp1)])
  stock <- temp1 - temp0
  
  ###game starts
  for(item in 1:100){
    value <- stock[item]
    past_value <- temp0[item]
    ###d's bidding
    if(dim(DD)[1]!=0){
      dbidList <- round(sapply(DD$budget, d_BID),0)
      dbid <- max(dbidList)
    }else{
      dbid <- 0
    }
    ###m's bidding
    mbid <- m_BID(value, past_value, MM)
    ###s's bidding
    if(dim(SS)[1]!=0){
      sbidList <- round(sapply(SS$budget, s_BID, master=mbid),0)
      sbid <- max(sbidList)
    }else{
      sbid <- 0
    }
    
    if(sbid >= dbid){
      if(mbid > sbid){ #s aims at m
        MM$collect <- MM$collect + 1
        MM$budget <- MM$budget - mbid
        MM$profit <- MM$profit + (value-mbid)
      }else{
        SS$collect[which.max(sbidList)] <- SS$collect[which.max(sbidList)] + 1
        SS$budget[which.max(sbidList)] <- SS$budget[which.max(sbidList)] - sbid
      }
      SS <- SS[SS$collect<10,]
      SS <- SS[SS$budget>0,]
    }else{
      if(mbid >= dbid){ #m is faster than d
        MM$collect <- MM$collect + 1
        MM$budget <- MM$budget - mbid
        MM$profit <- MM$profit + (value-mbid)
      }else{
        DD$collect[which.max(dbidList)] <- DD$collect[which.max(dbidList)] + 1
        DD$budget[which.max(dbidList)] <- DD$budget[which.max(dbidList)] - dbid
      }
      DD <- DD[DD$collect<10,]
      DD <- DD[DD$budget>0,]
    }
    
    if(MM$collect == 10 | MM$budget == 0){
      break
    }
  }
  return(MM$profit)
}

##Simulation Generalized
play <- function(ns){
  
  ###players
  DD <- data.frame(role=rep('d',9-ns), budget=rep(2000,9-ns), collect=rep(0,9-ns))
  SS <- data.frame(role=rep('s',ns), budget=rep(2000,ns), collect=rep(0,ns))
  MM <- data.frame(role='m', budget=2000, collect=0, profit=0)
  
  ###items
  temp1 <- sort(c(sample(1:19999, 99), 20000))
  temp0 <- c(0, temp1[-length(temp1)])
  stock <- temp1 - temp0
  
  ###game starts
  for(item in 1:100){
    value <- stock[item]
    past_value <- temp0[item]
    ###d's bidding
    if(dim(DD)[1]!=0){
      dbidList <- round(sapply(DD$budget, d_BID),0)
      dbid <- max(dbidList)
    }else{
      dbid <- 0
    }
    ###m's bidding
    mbid <- m_BID(value, past_value, MM)
    ###s's bidding
    if(dim(SS)[1]!=0){
      sbidList <- round(sapply(SS$budget, s_BID, master=mbid),0)
      sbid <- max(sbidList)
    }else{
      sbid <- 0
    }
    
    if(sbid >= dbid){
      if(mbid > sbid){ #s aims at m
        MM$collect <- MM$collect + 1
        MM$budget <- MM$budget - mbid
        MM$profit <- MM$profit + (value-mbid)
      }else{
        SS$collect[which.max(sbidList)] <- SS$collect[which.max(sbidList)] + 1
        SS$budget[which.max(sbidList)] <- SS$budget[which.max(sbidList)] - sbid
      }
      SS <- SS[SS$collect<10,]
      SS <- SS[SS$budget>0,]
    }else{
      if(mbid >= dbid){ #m is faster than d
        MM$collect <- MM$collect + 1
        MM$budget <- MM$budget - mbid
        MM$profit <- MM$profit + (value-mbid)
      }else{
        DD$collect[which.max(dbidList)] <- DD$collect[which.max(dbidList)] + 1
        DD$budget[which.max(dbidList)] <- DD$budget[which.max(dbidList)] - dbid
      }
      DD <- DD[DD$collect<10,]
      DD <- DD[DD$budget>0,]
    }
    DD
    SS
    MM
    if(MM$collect == 10 | MM$budget == 0){
      break
    }
  }
  return(MM$profit)
}

##MonteCarlo for (d,s)=(9,0)
mp <- c()
for(time in 1:1000){
  mp <- c(mp, play0())
}
#mean(mp)
#hist(mp)

##Bootstrap
bs <- c()
for(time in 1:1000){
  bs <- c(bs, mean(sample(mp, size=1000, replace=TRUE)))
}
#sd(bs)

(expect_profit_95 <- c(mean(mp)+2*sd(bs), mean(mp)-2*sd(bs)))

##MonteCarlo for (d,s)=(8,1)
mp <- c()
for(time in 1:1000){
  mp <- c(mp, play1())
}
#mean(mp)
#hist(mp)

##Bootstrap
bs <- c()
for(time in 1:1000){
  bs <- c(bs, mean(sample(mp, size=1000, replace=TRUE)))
}
#sd(bs)

(expect_profit_95 <- c(mean(mp)+2*sd(bs), mean(mp)-2*sd(bs)))

##MonteCarlo for (d,s)=(7,2)
mp <- c()
for(time in 1:100){
  mp <- c(mp, play2())
}
#mean(mp)
#hist(mp)

##Bootstrap
bs <- c()
for(time in 1:100){
  bs <- c(bs, mean(sample(mp, size=100, replace=TRUE)))
}
#sd(bs)

(expect_profit_95 <- c(mean(mp)+2*sd(bs), mean(mp)-2*sd(bs)))
