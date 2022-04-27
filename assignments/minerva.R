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
  # error handling
  stopifnot((l>=0 & l<=1) & (m>0) & (nfeat>0))
  
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
  trace_init <- matrix(sample(vals, m*nfeat, replace=T), nrow=m, ncol=nfeat, byrow=T)
  traces <- matrix(NA, nrow=m, ncol=nfeat, byrow=T)
  for(i in 1:m){
    for(j in 1:nfeat){
      traces[i,j] <- sample(c(trace_init[i,j],0),size=1,prob=c(l,1-l))
    }
  }
  
  # loop through number of runs
  for(sim in 1:nsim){
    
    # half the time it's a studied word, half the time it's a lure
    # even trials are studied, odd trials are lures
    if(sim %% 2 == 0){
      # studied trial
      # randomly pick a studied word to test
      probe <- trace_init[sample(x=1:m,size=1), ]
      trial_type <- "studied"
    }else{
      # lure trial
      # create random feature vector to test
      probe <- sample(vals, nfeat, replace=T)
      trial_type <- "lure"
    }
    
    # similarity
    # Hintzman (1988) eq. 1
    S <- numeric(nfeat)
    for(i in 1:m){
      S[i] <- sum ( ( probe * ( traces[i,]) ) / sum( probe!=0 | trace_init[i,]!=0 ) )
    }

    # activation
    # Hintzman (1988) eq. 2
    A <- S^3
    
    # intensity
    # Hintzman (1988) eq. 3
    intens <- sum(A)
    
    # get a response for each criterion
    for(i in 1:ncrit){
      if(intens > crit[i]){
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
roc_minerva <- function(m, l, nfeat=10, nsim = 5000, crit=seq(-1,1,by=.01), return_dat=F, norm=F){
  # data frame of criteria, hit rate, false alarm rate
  if(length(l)>1){
    dat <- tibble()
    for(rate in 1:length(l)){
      dat <- bind_rows(
        dat,
        mutate(rminerva(m,l[rate],nfeat,nsim,crit),
               l=l[rate])
      )
    }
    if(norm){
      dat_norm <- dat %>%
        mutate(across(c(hr,far),qnorm))
      pl <- dat_norm %>% 
        arrange(desc(crit)) %>% # just to be safe
        ggplot(aes(far,hr,col=as.factor(l)))+
        geom_path()+
        geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.6)+
        labs(title="zROC")+
        scale_color_discrete(name="learning rate")+
        coord_fixed(xlim=c(-5,5),ylim=c(-5,5))+ # square the plot
        ggthemes::theme_few()+
        theme(plot.title=element_text(hjust=0.5))
      if(return_dat){
        return(list(dat_norm,pl))
      }else{
        return(pl)
      }
    }else{
      pl <- ggplot(dat, aes(far,hr,col=as.factor(l)))+
        geom_path()+
        geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.6)+
        labs(title="ROC")+
        scale_color_discrete(name="learning rate")+
        coord_fixed(xlim=c(0,1),ylim=c(0,1))+ # square the plot
        ggthemes::theme_few()+
        theme(plot.title=element_text(hjust=0.5))
      # if I want the data returned
      if(return_dat){
        return(list(dat,pl))
      }else{
        return(pl)
      }
    }
  }else{
    dat <- rminerva(m,l,nfeat,nsim,crit)
    if(norm){
      dat_norm <- dat %>%
        mutate(across(c(hr,far),qnorm))
      pl <- dat_norm %>% 
        arrange(desc(crit)) %>% # just to be safe
        ggplot(aes(far,hr))+
        geom_path()+
        geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.6)+
        labs(title="zROC")+
        coord_fixed(xlim=c(-5,5),ylim=c(-5,5))+ # square the plot
        ggthemes::theme_few()+
        theme(plot.title=element_text(hjust=0.5))
      if(return_dat){
        return(list(dat,pl))
      }else{
        return(pl)
      }
    }else{
      pl <- dat %>% 
        arrange(desc(crit)) %>% # just to be safe
        ggplot(aes(far,hr))+
        geom_path()+
        geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.6)+
        labs(title="ROC")+
        coord_fixed(xlim=c(0,1),ylim=c(0,1))+ # square the plot
        ggthemes::theme_few()+
        theme(plot.title=element_text(hjust=0.5))
      # if I want the data returned
      if(return_dat){
        return(list(dat,pl))
      }else{
        return(pl)
      }
    }
  }
}

# testing ============================================================================
# list length 
m <- 50

# learning rate 
l <- c(.1,.3,.5,.7,.9)

# n features
nfeat <- 50

# number of trials
nsim <- 100000

# criteria 
crit <- seq(-1,1,.01)

# ROC
roc_minerva(m=m,l=l,crit=crit,nsim=nsim)

rminerva(m,l,nfeat,nsim,crit)

# zROC
roc_minerva(m,l,crit=crit,nsim=nsim,return_dat=T,norm=T)

