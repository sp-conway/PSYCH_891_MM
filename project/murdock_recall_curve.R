library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(glue)
library(purrr)
rm(list=ls())
source(here("project","simple.R"))

get_items <- function(n_items,pres_time,wait_time){
  init_time <- n_items*pres_time
  items <- seq(init_time,pres_time,-pres_time)+wait_time
  return(items)
}

conds <- tribble(
  ~n_items, ~pres_time, ~wait_time,
  10,        2,          15,
  15,        2,          20,
  20,        2,          25,
  20,        1,          10,
  30,        1,          15,
  40,        1,          20
) 

items <- map(seq_len(nrow(conds)),~get_items(conds$n_items[.x], conds$pres_time[.x], conds$wait_time[.x]))

#pos <- seq_along(items)
c <- c(12.1, 10.82, 9.69, 10.45, 9.41, 12.03)
t <- c(.53, .42, .35, .40, .30, .30)
s <- c(8.98, 12.09, 16.19, 16.01, 21.12, 22.83)

preds <- map_dfr(seq_len(nrow(conds)),
        ~pos_pred(items[[.x]],
                  c[.x],
                  s[.x],
                  t[.x]) %>%
            mutate(n_items=conds$n_items[.x],
                   pres_time=conds$pres_time[.x])
        ) %>%
  mutate(
    condition=glue("{n_items} items, {pres_time} sec/item")
  )
preds

murdock <- ggplot(preds,aes(pos,prob,col=condition,shape=condition))+
  geom_point(alpha=.5,size=3)+
  geom_path()+
  labs(x="Serial Position",
       y="Recall Proportion Correct")+
       #title="Murdock (1962) Serial Position Free Recall Data\nSIMPLE Model Predictions",
       #subtitle="Parameters from Brown et al (2007)")+
  coord_cartesian(xlim=c(0,40),ylim=c(0,1))+
  ggthemes::theme_few()+
  theme(plot.title=element_text(hjust=0.5,size=18),
        plot.subtitle=element_text(hjust=0.5,size=16),
        axis.title=element_text(hjust=0.5,size=16),
        legend.title = element_text(hjust=0.5,size=16))
ggsave(here("project","murdock_curve.pdf"),plot=murdock,
       width=5,height=3)

# stim <- items[[1]]
# pos_pred(stim, c[1],s[1],t[1])$prob
# pos_pred(stim,c[1],s[1],t[1],type="serial")
# 
# rsimple(1000,stim, c[1],s[1],t[1])
