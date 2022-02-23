# sdt_dp_functions.R
# functions for working with signal detection theory & dual process model
# Sean Conway
# last modified Feb. 2022

# libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(glue)
library(purrr)
library(patchwork)

# functions for calculating dprime and criterion from equal variance signal detection model
# assumes noise ~ N(mu=0, sigma=1) and signal ~ N(mu=dprime, sigma=1)
dprime <- function(hr,far) qnorm(hr) - qnorm(far)
crit <- function(far) qnorm(far, lower.tail=F)
crit_cent <- function(hr,far) crit(far) - .5*dprime(hr,far)
beta <- function(hr,far,scale=F) {
  ds <- dnorm(x=crit(far),mu=dprime(hr,far),sigma=1) # density for signal
  dn <- dnorm(x=crit(far),mu=0,sigma=1) # density for noise
  beta <- ds/dn # likelihood ratio of signal/noise
  if(scale){ # use log to scale it
    beta <- log(beta)
  }
  return(beta)
}

# randomly draw from signal detection model (from noise and signal)
# returns draws[[1]], rates[[2]]
rsdt <- function(n, mu_s, sig_s, crit=NULL, calc_rates=F){
  xn <- rnorm(n)
  xs <- rnorm(n, mu_s, sig_s)
  draws <- tibble(xn=xn,xs=xs)
  if(calc_rates){
    stopifnot(!is.null(crit))
    rates <- tibble(crit=crit,
                    hr=sum(xs>crit)/n,
                    far=sum(xn>crit)/n)
    return(list(draws,rates))
  }else{
    return(draws)
  }
}

# visualize signal detection theory distributions
vis_dists <- function(mu_s, sig_s, crit){
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
    geom_path(size=1.25,alpha=.55)+
    geom_vline(xintercept=crit,col='black',linetype='dashed')+
    scale_color_manual(values=c("green4","red2"))+
    scale_x_continuous(limits=c(range[1],range[2]))+
    labs(y="probability density",title="signal detection distributions")+
    ggthemes::theme_few()+
    theme(plot.title=element_text(hjust=0.5))
  return(pl)
}

# wrapper function to draw samples from sdt model and visualize results
vis_draws <- function(mu_s,sig_s,n,crit=NULL,calc_rates=F){
  sim <- rsdt(mu_s=mu_s, sig_s=sig_s, n=n, crit=crit,calc_rates=calc_rates)
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
    labs(title=glue("N={n*2} trials"))+
    ggthemes::theme_few()+
    theme(strip.text=element_text(size=14),
          plot.title=element_text(size=14,hjust=0.5))
  if(calc_rates){
    pl <- pl+
      labs(subtitle=glue("\nhit rate={round(sim[[2]]$hr[1],digits=2)}, ",
                         "false alarm rate={round(sim[[2]]$far[1],digits=2)}"))+
      theme(plot.subtitle=element_text(hjust=0.5))
  }
  if(!is.null(crit)){
    pl <- pl +
      geom_vline(aes(xintercept=crit),linetype="dashed") 
  }
  return(pl)
}

# function to plot ROC curve or zROC curve
# really only works with the rsdt_roc and rdp_roc functions here
# assumes specific column names
# maybe rewrite in tidy eval framework?
plot_roc <- function(dat, ztrans=F, title=""){
  if(ztrans){
    dat_trans <- dat %>% 
      mutate(across(c(hr, far),qnorm))
    stopifnot(apply(dat_trans, 2, is.finite))
    min_lim <- floor(min(c(dat_trans$far, dat_trans$hr)))
    max_lim <- ceiling(max(c(dat_trans$far, dat_trans$hr)))
    roc <-  dat_trans %>%
      ggplot(aes(far, hr))+
      geom_point(size=3,alpha=.5)+
      geom_path()+
      coord_fixed(xlim=c(min_lim,max_lim), 
                  ylim=c(min_lim,max_lim))+
      labs(x="zFAR",y="zHR",title=title)+
      ggthemes::theme_few()+
      theme(plot.title=element_text(hjust=0.5))
  }else{
    roc <- ggplot(dat, aes(far, hr))+
      geom_point(size=3,alpha=.5)+
      geom_path()+
      coord_fixed(xlim=c(0,1),
                  ylim=c(0,1))+
      labs(x="FAR",y="HR",title=title)+
      ggthemes::theme_few()+
      theme(plot.title=element_text(hjust=0.5))
  }
  return(roc)
}

# draw from equal variance SDT model and plot ROC, ZROC
rsdt_roc <- function(n, mu_s, sig_s, crit, show_plots=F){
  trials <- rsdt(n, mu_s, sig_s, .x, F)
  hr <- map_dbl(crit, ~sum(trials$xs>.x)/n)
  far <- map_dbl(crit, ~sum(trials$xn>.x)/n)
  dat <- tibble(
    crit=crit,
    hr=hr,
    far=far
  )
  if(show_plots){
    roc <- plot_roc(dat, ztrans=F, "SDT ROC")
    zroc <- plot_roc(dat, ztrans=T, "SDT zROC")
    all_plots <- (roc|zroc)+
      plot_annotation(title="SDT ROC Plots",
                      theme=theme(plot.title=element_text(hjust=0.5,size=18)))
    return(list(dat, all_plots))
  }else{
    return(dat)
  }
}

# draw from dual process model and plot ROC, zROC
rdp_roc <- function(n, mu_s, R, crit, show_plots=F){
  sig_s <- 1
  rec <- sample(x=c(0,1),size=n,prob=c(1-R, R),replace=T)
  trials <- rsdt(n, mu_s, sig_s, .x, F)
  hr <- map_dbl(crit, ~sum(trials$xs>.x|rec==1)/n)
  far <- map_dbl(crit, ~sum(trials$xn>.x)/n)
  dat <- tibble(
    crit=crit,
    hr=hr,
    far=far
  )
  if(show_plots){
    roc <- plot_roc(dat, ztrans=F, title="DP ROC")
    zroc <- plot_roc(dat, ztrans=T, title="DP zROC")
    all_plots <- (roc|zroc)+
      plot_annotation(title="Dual Process ROC Plots",
                      theme=theme(plot.title=element_text(hjust=0.5,size=18)))
    return(list(dat, all_plots))
  }else{
    return(dat)
  }
}

