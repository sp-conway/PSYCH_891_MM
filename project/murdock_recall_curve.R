library(here)
library(dplyr)
library(tidyr)
library(glue)
library(purrr)
rm(list=ls())
source(here("project","simple.R"))
source(here("murdock_stim.R"))
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
