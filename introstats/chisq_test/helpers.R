draw.obs.rect <- function(obs, cts){
  nrow <- nrow(obs)
  ncol <- ncol(obs)
  rowtot <- apply(obs, 1, sum)
  coltot <- apply(obs, 2, sum)
  total <- sum(obs)
  obs.xleft <- obs.xright <- obs.ytop <- obs.ylow <- array(dim=c(nrow, ncol))
  obs.ytop[1,] <- 1
  obs.xleft[,1] <- 0
  for(i in 1:nrow){
    obs.ylow[i,] <- 1 - sum(rowtot[1:i])/total
    if(i < nrow) obs.ytop[i+1,] <- obs.ylow[i,]
    for(j in 1:ncol){
      obs.xright[i,j] <- sum(obs[i, 1:j]) / rowtot[i]
      if(j < ncol) obs.xleft[i,j+1] <- obs.xright[i,j]
      rect(obs.xleft[i,j], obs.ylow[i,j], obs.xright[i,j], obs.ytop[i,j], 
           border=1)
      if(cts==T) 
        text(x=mean(c(obs.xleft[i,j], obs.xright[i,j])),
           y=mean(c(obs.ylow[i,j], obs.ytop[i,j])),
           labels=obs[i,j], col=1)
    }
  }
}

draw.exp.rect <- function(obs, rv, cts){
  nrow <- nrow(obs)
  ncol <- ncol(obs)
  rowtot <- apply(obs, 1, sum)
  coltot <- apply(obs, 2, sum)
  total <- sum(obs)
  exp.xleft <- exp.xright <- exp.ytop <- exp.ylow <- array(dim=c(nrow, ncol))
  exp.xleft[,1] <- 0
  for(i in 1:ncol){
    exp.xright[,i] <- sum(coltot[1:i])/total
    if(i<ncol) exp.xleft[,i+1] <- exp.xright[,i]
  }
  exp.ytop[1,] <- 1
  for(i in 1:nrow){
    exp.ylow[i,] <- 1 - sum(rowtot[1:i])/total
    if(i<nrow) exp.ytop[i+1,] <- exp.ylow[i,]
  }
  for(i in 1:nrow){
    for(j in 1:ncol){
      rect(exp.xleft[i,j], exp.ylow[i,j], exp.xright[i,j], exp.ytop[i,j], 
           border=2)
      if(cts==T)
        text(x=mean(c(exp.xleft[i,j], exp.xright[i,j])),
           y=mean(c(exp.ylow[i,j], exp.ytop[i,j])),
           labels=round(rv$exp[i,j], 1), col=2)
    }
  }
}
