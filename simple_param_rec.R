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
#stim <- items[[1]]
c <- seq(.01,10,length.out=100)
t <- seq(.01,1,length.out=100)
s <- seq(0.01,25,length.out=100)
n <- 1000

res <- vector("list",length(c))

for(i in seq_along(c)){
  res[[i]] <- tryCatch(rsimple(n,items[[1]],c[i],s[i],t[i]),
                       error=function(e) print(i))
}

cost <- function(items, params, obs){
  c <- params[1]
  s <- params[2]
  t <- params[3]
  pred <- pos_pred(items,c,s,t)$prob
  sse <- sum((obs-pred)^2)
  return(sse)
}

lower <- c(0,0,0)
upper <- c(15,1,25)
start_par <- function() runif(3,0,1)
fits <- vector("list",6)
for(i in seq_along(c)){
  fits[[i]] <- optim(
    start_par(),
    fn=cost,
    method="L-BFGS-B",
    lower=lower,
    upper=upper,
    items=items[[1]],
    obs=res[[i]]
  )
}

best_params <- tibble()
for(j in seq_along(c)){
  best_params <- bind_rows(
    best_params,
    tibble(
      n=j,
      c=fits[[j]]$par[1],
      s=fits[[j]]$par[2],
      t=fits[[j]]$par[3]
    )
  )
}

best_params <- best_params %>%
  mutate(source="rec")

orig_params <- tibble(
  n=seq_along(c),
  c=c,
  s=s,
  t=t,
  source="gen"
)

all_params <- bind_rows(
  best_params,
  orig_params
) %>%
  pivot_longer(c(c,s,t),
               values_to="value",
               names_to="param") %>%
  pivot_wider(names_from=source,
              values_from=value)

p  <- ggplot(all_params,aes(gen,rec))+
  geom_point()+
  facet_wrap(vars(param),scales="free_x")+
  ggthemes::theme_few()

ggsave(here("project","param_recov.pdf"),p)


