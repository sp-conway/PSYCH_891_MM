library(ggplot2)
library(dplyr)
library(here)
rm(list=ls())
time <- tibble(
  t=seq(1,60,.01),
  lt=log(t)
)

tfig <- ggplot(time,aes(t,lt))+
  geom_path()+
  geom_point(aes(4,log(4)),col="red",size=4)+
  geom_point(aes(12,log(12)),col="red",size=4)+
  geom_point(aes(40,log(40)),col="blue",size=4)+
  geom_point(aes(48,log(48)),col="blue",size=4)+
  ggthemes::theme_few()+
  labs(x="time since study",
       y="ln time since study")

ggsave(plot=tfig,filename=here("project","time_compression_fig.pdf"))

#similarity <- function(c,dist
