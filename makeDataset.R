make.dataset <- function(beta = c(-2,1,0.5,3, rep(0,6)), nObs = 200, f_NonLin = NULL, se = 1){
  X <- matrix(runif(length(beta)*nObs, -0.5, 0.5), nrow = nObs)
  cNamesX <- paste0("x", 1:length(beta))
  if(!is.null(f_NonLin)){
    Z <- matrix(runif(length(f_NonLin)*nObs, -0.5, 0.5), nrow = nObs)
    cNamesZ <- paste0("z", 1:length(f_NonLin))
    y_z <- rep(0, nObs)
    for (i in 1:length(f_NonLin)) {
      y_z <- y_z + f_NonLin[[i]](Z[,i])
    }
    y <- X%*%beta + y_z + rnorm(nObs, mean = 0, sd = se)
    dat <- cbind(y,X,Z)
    colnames(dat) <- c("y", cNamesX, cNamesZ)
    dat <- as.data.frame(scale(dat, scale = F))
  }else{
    y <- X%*%beta + rnorm(nObs, mean = 0, sd = se)
    dat <- cbind(y,X)
    colnames(dat) <- c("y", cNamesX)
    dat <- as.data.frame(scale(dat, scale = F))
  }
  return(dat)
}

make.formula <- function(dat){
  varNames <- colnames(dat)[-1]
  fm1 <- paste0(paste0("bols(", varNames, collapse = ", intercept = F) + "), ", intercept = F) + ")
  fm2 <-paste0(paste0("bbs(", varNames, collapse = ", df = 1, center = TRUE) + "), ", df = 1, center = TRUE)")
  y <- as.formula(paste0("y ~ ", fm1, fm2))
  return(y)
}