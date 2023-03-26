simulate.performance <- function(nSlopes = 10, beta = c(-2,-1,0.5,3), nObs_fit = 200, 
                                 nObs_test = 1000, funs = NULL, se = 1, nDatasets = 100,
                                 maxIter = 500, filename, currProc = currProc, totProc = totProc){
  # Setting the number of maximum boosting iterations
  ctr_options <- boost_control(mstop = maxIter)
  # Creating a vector containing the slopes for all informative linear predictors and zeros for all uninformative predictors
  beta <- c(beta, rep(0, nSlopes-length(beta)))
  
  # Creating the formula for the boosting algorithm
  fmData <- make.dataset(beta = beta, nObs = nObs_fit, f_NonLin = funs, se = se)
  fm <- make.formula(fmData)
  
  # Creating the data frames used for saving all results
  results1 <- as.data.frame(matrix(rep(0, 2*(length(beta)+length(funs))*nDatasets+nDatasets*2), nrow = nDatasets))
  colnames(results1) <- c(paste0("Lin_", colnames(fmData)[-1]), paste0("Spl_", colnames(fmData)[-1]), "RMSE")
  results2 <- as.data.frame(matrix(rep(0, 2*(length(beta)+length(funs))*nDatasets+nDatasets*2), nrow = nDatasets))
  colnames(results2) <- c(paste0("Lin_", colnames(fmData)[-1]), paste0("Spl_", colnames(fmData)[-1]), "RMSE")
  results3 <- as.data.frame(matrix(rep(0, 2*(length(beta)+length(funs))*nDatasets+nDatasets*2), nrow = nDatasets))
  colnames(results3) <- c(paste0("Lin_", colnames(fmData)[-1]), paste0("Spl_", colnames(fmData)[-1]), "RMSE")
  
  # Creating a function to correctly link each base-learner with its respective column in the results data frame
  findIndex <- function(x) return(which(baseLearners == x))
  # Determining the names of all base-learners
  baseLearners <- unlist(strsplit(as.character(fm)[3], split = "[+]"))
  baseLearners <- gsub("[\t\n]", "", baseLearners)
  baseLearners <- gsub(" ", "", baseLearners)
  baseLearners <- gsub("center=TRUE,df=1", "df=1,center=TRUE", baseLearners)
  
  if(!exists("funs")) funs <- NULL
  
  # Creating a process bar to let the user know about the current status of the simulation
  cat("Process[", currProc, "/", totProc, "] for Dataset ", filename, "\n", sep = "")
  pb <- txtProgressBar(min = 0, max = nDatasets, initial = 0, style = 3)
  
  for(i in 1:nDatasets){
    set.seed(i)
    setTxtProgressBar(pb,i)
    # Creating a training dataset for each simulation
    dat <- make.dataset(beta = beta, nObs = nObs_fit, f_NonLin = funs, se = se)
    # Fitting a boosting model
    gam <- gamboost(formula = fm, data = dat, control = ctr_options)
    # Estimating the number of optimal boosting iterations
    cvm <- cvrisk(gam)
    gam[mstop(cvm)]
    # Creating the testing dataset
    dat_test <- make.dataset(beta = beta, nObs = nObs_test, f_NonLin = funs, se = se)
    
    # Saving the results after default boosting
    freq <- as.data.frame(table(selected(gam)))
    
    results1[i,as.numeric(levels(freq$Var1))[freq$Var1]] <- freq$Freq
    results1[i,ncol(results1)-1] <- suppressWarnings(rmse(dat[,1], predict(gam, dat[,-1])))
    results1[i,ncol(results1)] <- suppressWarnings(rmse(dat_test[,1], predict(gam, dat_test[,-1])))
    
    # Variable selection step
    gam_deselect <- DeselectBoost2(gam, fam = Gaussian(), data = dat)
    # Saving the results after the variable selection step
    freq <- as.data.frame(table(selected(gam_deselect)))
    freq$Var1 <- names(coef(gam_deselect))
    freq$Var1 <- gsub(" ", "", freq$Var1)
    
    freq$Var1 <- apply(as.matrix(freq$Var1), 1, findIndex)
    
    results2[i,freq$Var1] <- freq$Freq
    results2[i,ncol(results2)-1] <- suppressWarnings(rmse(dat[,1], predict(gam_deselect, dat[,-1])))
    results2[i,ncol(results2)] <- suppressWarnings(rmse(dat_test[,1], predict(gam_deselect, dat_test[,-1])))
    
    # Proposed model choice step
    gam_linChoice <- linChoiceBoost(gam_deselect, data = dat, fam = Gaussian(), eta = 0.5)
    # Saving the results after the model choice step
    freq <- as.data.frame(table(selected(gam_linChoice)))
    freq$Var1 <- names(coef(gam_linChoice))
    freq$Var1 <- gsub(" ", "", freq$Var1)
    
    freq$Var1 <- apply(as.matrix(freq$Var1), 1, findIndex)
    
    results3[i,freq$Var1] <- freq$Freq
    results3[i,ncol(results3)-1] <- suppressWarnings(rmse(dat[,1], predict(gam_linChoice, dat[,-1])))
    results3[i,ncol(results3)] <- suppressWarnings(rmse(dat_test[,1], predict(gam_linChoice, dat_test[,-1])))
  }
  close(pb)
  # Saving the results for the simulations of the respective setup
  save(list = c("results1"), file = paste0("Results/", paste0(filename, "_Standard"), ".RData"))
  save(list = c("results2"), file = paste0("Results/", paste0(filename, "_Deselect"), ".RData"))
  save(list = c("results3"), file = paste0("Results/", paste0(filename, "_ModelChoice"), ".RData"))
}

rmse <- function(y, y_hat){return(sqrt(mean((y-y_hat)^2)))}


