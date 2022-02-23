# sdt_tests.R
# testing out functions for working with signal detection theory & dual process model
# Sean Conway
# last modified Feb. 2022

# Setup ====================================================================================
# clear environment
rm(list=ls())

# libraries
library(here)

# source sdt functions
source(here("assignments","sdt_dp_functions.R"))

# sdt parameters ( noise ~ N(0,1) )
#mu_s <- 1
#sig_s <- 1
# crit <- .5
#crit <- .5 # define crit in relation to mu_s. 

# sampling parameters
#n <- 10000

# SDT ====================================================================================
#rsdt(n, mu_s, sig_s, crit, T)[[2]]

# see distributions
#dists_pl <- vis_dists(mu_s=mu_s,sig_s=sig_s,crit=crit)

# do sampling and visualize results
#draws_pl <- vis_draws(mu_s=mu_s,sig_s=sig_s,crit=crit,n=n,calc_rates=T)

# combine distributions plot with sampling results plot
#draws_pl/dists_pl

# SDT ROC ===================================================================================
n <- 100
mu_s <- 1
sig_s <- 1
crit <- c(.5,.75,1,1.25,1.5)
res <- rsdt_roc(n, mu_s, sig_s, crit, show_plots=F) 
sdt_plot <- rsdt_roc(n, mu_s, sig_s, crit = crit, show_plots=T)[[2]]

# DP ROC ====================================================================================
R <- .4 # probability of recollecting
rdp_roc(n, mu_s, R, crit, show_plots=F) 
dp_plot <- rdp_roc(n, mu_s, R, crit, show_plots=T)[[2]]
sdt_plot/dp_plot
