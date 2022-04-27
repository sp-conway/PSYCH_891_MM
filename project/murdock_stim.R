library(here)
library(dplyr)
library(purrr)

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
