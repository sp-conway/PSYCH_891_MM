library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
rm(list=ls())

temp <- tibble(
  dist=6:2,
  log=log(6:2)
)

ggplot(temp,aes(dist,log))+
  geom_point()+
  ggthemes::theme_few()


