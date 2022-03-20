# minerva.R
# Sean Conway
# Last modified March 2022

# setup ============================================================================
# clear environment
rm(list=ls())

# libraries
library(dplyr)
library(ggplot2)

# functions ============================================================================

# function to simulate minerva2 model 
# This is a slow function!
# should be rewritten with matrix algebra to improve performance
rminerva <- function(m, l, nfeat, nsim, crit){
  # number of criteria
  ncrit <- length(crit)
  
  # init matrices of hits, false alarms
  hits <- matrix(c(crit, rep(0, ncrit)),ncol=2)
  falarms <- matrix(c(crit, rep(0, ncrit)),ncol=2)
  # misses <- matrix(c(crit, rep(0, ncrit)),ncol=2)
  # corr_rej <- matrix(c(crit, rep(0, ncrit)),ncol=2)
  
  # potential feature values
  vals <- c(1,0,-1)
  
  # memory traces
  traces <- matrix(sample(vals, m*nfeat, replace=T), nrow=m, ncol=nfeat, byrow=T)
  
  # loop through number of runs
  for(sim in 1:nsim){
    
    # half the time it's a studied word, half the time it's a lure
    # even trials are studied, odd trials are lures
    if(sim %% 2 == 0){
      # studied trial
      # randomly pick a studied word to test
      probe <- traces[sample(1:m,1), ]
      trial_type <- "studied"
    }else{
      # lure trial
      # sample random feature vector to test
      probe <- sample(vals, nfeat, replace=T)
      trial_type <- "lure"
    }
    
    # similarity
    # Hintzman (1988) eq. 1
    S <- numeric(nfeat)
    for(i in 1:m){
      S[i] <- sum ( ( probe * ( traces[i,]*l ) ) / sum( probe!=0 | traces[i,]!=0 ) )
    }
    
    # activation
    # Hintzman (1988) eq. 2
    A <- S^3
    
    # intensity
    # Hintzman (1988) eq. 3
    I <- sum(A)
    
    # get a response for each criterion
    for(i in 1:ncrit){
      if(I > crit[i]){
        if(trial_type=="studied"){
          hits[i,2] <- hits[i,2]+1
        }else if(trial_type=="lure"){
          falarms[i,2] <- falarms[i,2]+1
        }
    # }else{
    #   if(trial_type=="studied"){
    #     misses[i,2] <- misses[i,2]+1
    #   }else if(trial_type=="lure"){
    #     corr_rej[i,2] <- corr_rej[i,2]+1
    #   }
    # }
      }
    }
  }
  
  # calculating hit rates and false alarm rates
  # nsim/2 is the denominator because 1/2 the time it's studied and 1/2 the time it's a lure
  hr <- hits[,2]/(nsim/2)
  far <- falarms[,2]/(nsim/2)
  
  return(tibble(crit,hr,far))
}

# wrapper function to simulate minerva and plot ROC
roc_minerva <- function(m, l, nfeat=10, nsim = 5000, crit=seq(-1,1,by=.01), return_dat=F){
  # data frame of criteria, hit rate, false alarm rate
  dat <- rminerva(m,l,nfeat,nsim,crit)
  
  # plot ROC
  pl <- dat %>% 
    arrange(desc(crit)) %>% # just to be safe
    ggplot(aes(far,hr))+
    geom_path()+
    coord_fixed(xlim=c(0,1),ylim=c(0,1))+ # square the plot
    ggthemes::theme_few()
  
  # if I want the data returned
  if(return_dat){
    return(list(dat,pl))
  }else{
    return(pl)
  }
}

# testing ============================================================================
# number of traces 
m <- 50

# learning rate 
l <- .6

roc_minerva(m,l,return_dat = T)

