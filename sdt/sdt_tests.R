# sdt_tests.R
# testing out functions for working with signal detection theory
# Sean Conway
# last modified Feb. 2022

# Setup ====================================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(patchwork)

# source sdt functions
source(here("sdt","sdt_functions.R"))

# sdt parameters ( noise ~ N(0,1) )
mu_s <- 2
sig_s <- 2
# lambda <- .5
lambda <- mu_s*.5 # define lambda in relation to mu_s. m

# sampling parameters
n <- 10000

# SDT ====================================================================================
# see distributions
dists_pl <- vis_dists(mu_s=mu_s,sig_s=sig_s,lambda=lambda)

# do sampling and visualize results
draws_pl <- vis_draws(mu_s=mu_s,sig_s=sig_s,lambda=lambda,n=n,calc_rates=T)

# combine distributions plot with sampling results plot
draws_pl/dists_pl

