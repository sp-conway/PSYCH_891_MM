# simple_fc.R
# modifying the SIMPLE model from Brown et al (2007) to do forced choice

# Setup ===========================================================================================
# clear environment
rm(list=ls())

# libraries
library(here)
library(ggplot2)
library(purrr)
library(stringr)

# source simple functions
source(here("project","simple.R"))

# function to get items
get_items <- function(n_items,pres_time,wait_time){
  init_time <- n_items*pres_time
  items <- seq(init_time,pres_time,-pres_time)+wait_time
  return(items)
}

# Murdock (1962) stimuli
conds <- tribble(
  ~n_items, ~pres_time, ~wait_time,
  10,        2,          15,
  15,        2,          20,
  20,        2,          25,
  20,        1,          10,
  30,        1,          15,
  40,        1,          20
) 

# list of items
items <- map(seq_len(nrow(conds)),
             ~get_items(conds$n_items[.x], conds$pres_time[.x], conds$wait_time[.x])
             )

# only doing first set for now
test <- items[[1]]
n_per_set <- 2 

# Brown et al params
c <- c(12.1, 10.82, 9.69, 10.45, 9.41, 12.03)
t <- c(.53, .42, .35, .40, .30, .30)
s <- c(8.98, 12.09, 16.19, 16.01, 21.12, 22.83)

# Functions ====================================================================================
# function for prediction 
choice_pred <- function(test,sets,c,pos=NULL){
  if(is.null(pos)) pos <- 1:length(sets)
  preds <- map2_dfr(pos,
                    1:length(sets),
                   ~tibble(
                     prob=simple(test,c[1],type="fc",choice_set = sets[[.y]],test_pos=.x),
                     test_items=sets[[.y]],
                     test_val=test[sets[[.y]]],
                     test_pos=.x,
                     test_set=paste0(sets[[.y]],collapse=","),
                     set_size=length(sets[[.y]])
                   )
   )
  return(preds)
}

# function for plotting
plot_fun <- function(dat){
  pl <- ggplot(dat,aes(test_pos,prob,col=as.factor(test_set)))+
    geom_text(aes(label=test_items),size=6,alpha=.8)+
    scale_x_continuous(breaks=seq(1,10,1))+
    scale_y_continuous(limits=c(0,1))+
    scale_color_discrete(name="choice set")+
    labs(x="test position",
         y="choice probability")+
    ggthemes::theme_few()+
    theme(axis.title=element_text(size=15),
          axis.text=element_text(size=13),
          legend.title=element_text(size=15))
  return(pl)
}

# Predictions ====================================================================================
# asking about a particular option and the stimulus that appeared after it (except for last stim)
preds_2_after <- tibble()
sets_2_after <- list(c(1,2),
               c(2,3),
               c(3,4),
               c(4,5),
               c(5,6),
               c(6,7),
               c(7,8),
               c(8,9),
               c(9,10),
               c(10,9))
preds_2_after <- choice_pred(test,sets_2_after,c[1])
preds_2_after_pl <- plot_fun(preds_2_after)

# asking about a particular option and the stimulus that appeared before it (except for first stim)
preds_2_before <- tibble()
sets_2_before <- list(c(1,2),
                     c(2,1),
                     c(3,2),
                     c(4,3),
                     c(5,4),
                     c(6,5),
                     c(7,6),
                     c(8,7),
                     c(9,8),
                     c(10,9))
preds_2_before <- choice_pred(test,sets_2_before,c[1])
preds_2_before_pl <- plot_fun(preds_2_before)

# asking about an option and the two after it
preds_3_after <- tibble()
sets_3_after <- list(c(1,2,3),
             c(2,3,4),
             c(3,4,5),
             c(4,5,6),
             c(5,6,7),
             c(6,7,8),
             c(7,8,9),
             c(8,9,10),
             c(8,9,10),
             c(8,9,10))
preds_3_after <- choice_pred(test,sets_3_after,c[1])
preds_3_after_pl <- plot_fun(preds_3_after)

# asking about an option and the two before it
preds_3_before <- tibble()
sets_3_before <- list(c(1,2,3),
                     c(1,2,3),
                     c(1,2,3),
                     c(2,3,4),
                     c(3,4,5),
                     c(4,5,6),
                     c(5,6,7),
                     c(6,7,8),
                     c(7,8,9),
                     c(8,9,10))
preds_3_before <- choice_pred(test,sets_3_before,c[1])
preds_3_before_pl <- plot_fun(preds_3_before)

# asking about an option and the options before ANd after it
preds_3_middle <- tibble()
sets_3_middle <- list(c(1,2,3),
                      c(1,2,3),
                      c(2,3,4),
                      c(3,4,5),
                      c(4,5,6),
                      c(5,6,7),
                      c(6,7,8),
                      c(7,8,9),
                      c(8,9,10),
                      c(8,9,10))
preds_3_middle <- choice_pred(test,sets_3_middle,c[1])
preds_3_middle_pl <- plot_fun(preds_3_middle)

# examining independence of irrelevant alternatives
sets_iia <- list(c(1,2),
                   c(1,2,5),
                   c(9,10),
                   c(9,10,5))
preds_iia <- choice_pred(test,sets_iia,c[1],pos=c(1,1,10,10)) %>%
  rename(`Set size`=set_size)
preds_iia_pl <- ggplot(preds_iia,aes(test_pos,prob,col=test_set))+
  geom_text(size=8,alpha=.85,aes(label=test_items))+
  facet_wrap(vars(`Set size`),labeller=label_both)+
  scale_x_continuous(breaks=c(1,10),limits=c(1,12))+
  scale_y_continuous(limits=c(0,1))+
  labs(x="test position",
       y="choice probability")+
  ggthemes::theme_few()+
  scale_color_tron(name="choice set")+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=13),
        legend.title=element_text(size=15),
        strip.text=element_text(size=15))

# sets primacy/recency
prim <- map(2:10,~c(1,.x))
rec <- map(1:9,~c(10,.x))
prim_test_pos <- rep(1,9)
rec_test_pos <- rep(10,9)
primacy <- choice_pred(test,prim,c[1],prim_test_pos) %>%
  mutate(type="primacy",
         option_type=case_when(
           test_items==test_pos~"focal",
           test_items!=test_pos~"comparison"
         ))
recency <- choice_pred(test,rec,c[1],rec_test_pos) %>%
  mutate(type="recency",
         option_type=case_when(
           test_items==test_pos~"focal",
           test_items!=test_pos~"comparison"
         ))
prim_rec <- bind_rows(primacy,
                      recency) %>%
  mutate(comparison_option=as.numeric(str_extract(test_set,"(?<=,)[:digit:]{1,2}")),
         correct_option=test_pos,
         type=factor(type,levels=c("recency","primacy"))) 
prim_rec_plot <- prim_rec %>%
  filter(option_type=="focal") %>%
  ggplot(aes(comparison_option,prob,col=as.factor(test_items)))+
  geom_text(alpha=.9,size=8,aes(label=test_items))+
  geom_hline(yintercept=.5,linetype="dashed")+
  scale_x_continuous(n.breaks=9)+
  scale_fill_tron(name="option type")+
  labs(x="comparison option",
       y="choice probability")+
  scale_color_tron(name="option")+
  ggthemes::theme_few()+
  theme(axis.title=element_text(size=17),
        axis.text=element_text(size=16),
        legend.title=element_text(size=16),
        strip.text=element_text(size=16))

prim_rec_distSim <- prim_rec %>%
  group_by(type,test_set) %>%
  mutate(sim=similarity(c[1],test_val[1],test_val[2]),
         log_dist=abs(log(test_val[1])-log(test_val[2]))) %>%
  ungroup() %>%
  distinct(type,comparison_option,test_set,sim,log_dist,test_items) %>%
  mutate(focal=case_when(
    type=="primacy"~1,
    type=="recency"~2
  ))

prim_rec_dist_sim_pl <- ggplot(prim_rec_distSim,aes(comparison_option,sim))+
  scale_x_continuous(n.breaks=9)+
  geom_text(alpha=.8,size=8,aes(col=as.factor(focal),label=as.factor(focal)))+
  scale_color_tron(name="option")+
  labs(x="comparison option",
       y="similarity")+
  #facet_grid(vars(type))+
  ggthemes::theme_few()+
  theme(axis.title = element_text(size=17),
        axis.text=element_text(size=16),
        legend.title=element_text(size=17),
        strip.text=element_text(size=16))


# Plots explaining stimuli ============================================================
stim <- tibble(
  seconds=test,
  test_position=1:length(test),
)

stim_sec_pl <- ggplot(stim,aes(test_position,seconds))+
  geom_point(size=5,alpha=.75,color="dodgerblue1")+
  scale_x_continuous(n.breaks=length(test))+
  labs(x="Study Position",
       y="Seconds Since Study")+
  ggthemes::theme_few()+
  theme(axis.title = element_text(size=15),
        axis.text=element_text(size=13),
        legend.title=element_text(size=15))

stim_sec_log_pl <- stim %>%
  mutate(seconds=log(seconds)) %>%
  ggplot(aes(test_position,seconds))+
  geom_point(size=5,alpha=.75,color="dodgerblue1")+
  scale_x_continuous(n.breaks=length(test))+
  labs(x="Study Position",
       y="Log Seconds Since Study")+
  ggthemes::theme_few()+
  theme(axis.title = element_text(size=15),
        axis.text=element_text(size=13),
        legend.title=element_text(size=15))

# save plots ====================================================================================
ggsave(preds_2_after_pl, filename = here("project","preds_2_after_pl.pdf"),
       width=5,height=5)
ggsave(preds_2_before_pl, filename = here("project","preds_2_before_pl.pdf"),
       width=5,height=5)
ggsave(preds_3_after_pl, filename = here("project","preds_3_after_pl.pdf"),
       width=5,height=5)
ggsave(preds_3_before_pl, filename = here("project","preds_3_before_pl.pdf"),
       width=5,height=5)
ggsave(preds_3_middle_pl, filename = here("project","preds_3_middle_pl.pdf"),
       width=5,height=5)
ggsave(preds_iia_pl, filename=here("project","preds_iia_pl.pdf"),
       width=8,height=8)
ggsave(prim_rec_plot,filename=here("project","prim_rec_pl.pdf"),
       width=7,height=5)
ggsave(prim_rec_dist_sim_pl,filename=here("project","prim_rec_dist_sim_pl.pdf"),
       width=7,height=5)
ggsave(stim_sec_pl,filename=here("project","stim_sec_pl.pdf"),
       width=5,height=5)
ggsave(stim_sec_log_pl,filename=here("project","stim_sec_log_pl.pdf"),
       width=5,height=5)



