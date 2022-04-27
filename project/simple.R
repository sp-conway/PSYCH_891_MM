similarity <- function(c,t1,t2,alpha=1) exp(-c* ( abs(log(t1)-log(t2) )^alpha ) )


simple <- function(items,c,s=NULL,t=NULL,type="free",alpha=1,return_discrim=F,choice_set=NULL,test_pos=NULL,lambda=1){
  n_items <- length(items)
  n_pos <- length(items)
  eta <- array(NA_real_,dim=c(n_pos,n_items))
  for(i in 1:n_items){
    for(j in 1:n_pos){
      eta[i,j] <- similarity(c,items[i],items[j])
    }
  }
  if(type=="free"){
      d <- eta/rowSums(eta)
      p <- array(NA_real_, dim=c(n_pos,n_items))
      for(i in 1:n_pos){
        for(j in 1:n_items){
          p[i,j] <- 1/(1+exp(-s*(d[i,j]-t)))
        }
      }
    }else if(type=="serial"){
      p <- numeric(n_items)
      for(i in 1:n_pos){
        for(j in 1:n_items){
          if(i!=j){
            next
          }else{
            p[j] <- eta[i,j]/sum(eta[i,])
          }
        }
      }
    }else if(type=="fc"){
      #pos_items <- items[test_set]
      v <- eta[choice_set,test_pos]
      p <- v^lambda / sum(v^lambda)
    }
  return(p)
}

pos_pred <- function(items,c,s,t,type="free",alpha=1){
  p <- simple(items,c,s,t,type=type)
  if(type=="free"){
    prob <- purrr::map_dbl(colSums(p),~min(c(.x,1)))
  }else{
    prob <- p
  }
  pp <- tibble::tibble(
    pos=seq_along(items),
    prob=prob
  )
  return(pp)
}

rsimple <- function(n, stim, c, s, t, alpha=1){
  studied <- seq_along(stim)
  n_pos <- length(stim)
  n_stim <- length(stim)
  p <- pos_pred(stim,c,s,t)$prob
  res <- matrix(NA_integer_, n, n_pos)
  for(i in 1:n){
    for(j in 1:n_stim){
      res[i,j] <- sample(c(j,0),size=1,prob=c(p[j],1-p[j]))
    }
  }
  prob <- map_dbl(1:n_stim, ~sum(res[,.x]!=0)/n)
  return(prob)
}

