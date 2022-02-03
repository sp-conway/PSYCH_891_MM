# sdt_functions.R
# functions for working with signal detection theory
# Sean Conway
# last modified Feb. 2022

# libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(glue)

# functions for calculating dprime and criterion from equal variance signal detection model
# assumes noise ~ N(mu=0, sigma=1) and signal ~ N(mu=dprime, sigma=1)
dprime <- function(hr,far) qnorm(hr) - qnorm(far)
crit <- function(far) qnorm(far, lower.tail=F)
crit_cent <- function(hr,far) crit(far) - .5*dprime(hr,far)
beta <- function(hr,far,log=F) {
  ds <- dnorm(x=crit(far),mu=dprime(hr,far),sigma=1) # density for signal
  dn <- dnorm(x=crit(far),mu=0,sigma=1) # density for noise
  beta <- ds/dn # likelihood ratio of signal/noise
  if(log){ # use log to scale it
    beta <- log(beta)
  }
  return(beta)
}

# randomly draw from signal detection model (from noise and signal)
# returns draws[[1]], rates[[2]]
rsdt <- function(mu_s, sig_s, n, lambda=NULL, calc_rates=F){
  xn <- rnorm(n)
  xs <- rnorm(n, mu_s, sig_s)
  draws <- tibble(xn=xn,xs=xs)
  if(calc_rates){
    stopifnot(!is.null(lambda))
    rates <- tibble(hr=sum(xs>lambda)/n,
                    far=sum(xn>lambda)/n)
    return(list(draws,rates))
  }else{
    return(draws)
  }
}

# visualize signal detection theory distributions
vis_dists <- function(mu_s, sig_s, lambda, return_plot=F){
  range <- c(-5, mu_s+5) # range of x axis. # semi-arbitrary
  x <- seq(range[1],range[2],by=.01) # x axis points
  d <- tibble( # get density for noise and signal
    x=x,
    noise=dnorm(x,0,1),
    signal=dnorm(x,mu_s,sig_s)
  ) %>%
    pivot_longer(cols=c(signal,noise), # pivoting data for easier plotting
                 names_to="distribution",
                 values_to="density") %>%
    mutate(distribution=factor(distribution,levels=c("signal","noise")))
  pl <- ggplot(d, aes(x,density,col=distribution))+
    geom_line(size=1.25,alpha=.55)+
    geom_vline(xintercept=lambda,col='black',linetype='dashed')+
    scale_color_manual(values=c("green4","red2"))+
    scale_x_continuous(limits=c(range[1],range[2]))+
    labs(y="probability density",title="signal detection distributions")+
    ggthemes::theme_few()+
    theme(plot.title=element_text(hjust=0.5))
  if(return_plot){
    return(pl)
  }else{
    pl
  }
}

# wrapper function to draw samples from sdt model and visualize results
vis_draws <- function(mu_s,sig_s,n,lambda=NULL,calc_rates=F,return_plot=F){
  sim <- rsdt(mu_s=mu_s, sig_s=sig_s, n=n, lambda=lambda,calc_rates=calc_rates)
  draws <- sim[[1]] %>%
    rename("signal"="xs","noise"="xn") %>%
    pivot_longer(cols=everything(),
                 values_to="x",
                 names_to="trial")  %>%
    mutate(trial=factor(trial,levels=c("signal","noise")))
  pl <- ggplot(draws,aes(x,fill=trial))+
    geom_histogram(binwidth=.25,alpha=.55,boundary=-5)+
    facet_grid(trial~.)+
    scale_x_continuous(limits=c(-5,mu_s+5))+
    scale_fill_manual(values=c("green4","red2"))+
    labs(title=glue("N={n} trials"))+
    ggthemes::theme_few()+
    theme(strip.text=element_text(size=14),
          plot.title=element_text(size=14,hjust=0.5))
  if(calc_rates){
    pl <- pl+
      labs(subtitle=glue("\nhit rate={round(sim[[2]]$hr[1],digits=2)}, ",
      "false alarm rate={round(sim[[2]]$far[1],digits=2)}"))+
      theme(plot.subtitle=element_text(hjust=0.5))
  }
  if(!is.null(lambda)){
    pl <- pl +
      geom_vline(aes(xintercept=lambda),linetype="dashed") 
  }
  if(return_plot){
    return(pl)
  }else{
    suppressMessages(pl)
  }
}


