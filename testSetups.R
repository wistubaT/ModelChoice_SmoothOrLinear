library(mboost)
library(plyr)

source("Methods/DeselectBoost2.R")
source("Methods/linChoiceBoost.R")
source("Methods/makeDataset.R")
source("Methods/simulatePerformance.R")
options(mboost_dftraceS = FALSE)

# Creating a vector that contains the names of all setup files
fileNames <- list.files("Setups/")
# Removing the default setup file for creating new setups from the vector
fileNames <- fileNames[-1]

currProc <- 1
totProcesses <- 0

# Determining the number of setups that still need to be evaluated
for(k in fileNames){
  if(!file.exists(paste0("Results/", substring(k, 1, nchar(k)-4), "_Standard.RData"))){
    totProcesses <- totProcesses + 1
  }
}

for(i in 1:length(fileNames)){
  k <- fileNames[i]
  # Only running the simulations for which no results are available in the folder "Results" yet
  if(!file.exists(paste0("Results/", substring(k, 1, nchar(k)-4), "_Standard.RData"))){
    # Reading and processing of the information in the setup files
    tempSetup <- read.csv2(paste0("Setups/", k), header = T)
    nSlopes <- as.numeric(tempSetup[1,1])
    beta <- as.numeric(tempSetup[,2])[!is.na(as.numeric(tempSetup[,2]))]
    nObs_fit <- as.numeric(tempSetup[1,3])
    nObs_test <- as.numeric(tempSetup[1,4])
    funs <- list()
    funs_char <- tempSetup[,5][which(tempSetup[,5] != "")]
    se <- as.numeric(tempSetup[1,6])
    nDatasets <- as.numeric(tempSetup[1,7])
    maxIter <- as.numeric(tempSetup[1,8])
    
    if(length(funs_char)!=0){
      for (j in 1:length(funs_char)) {
        eval(parse(text = paste('funs[[j]] <- function(x) { return(' , funs_char[j] , ')}', sep='')))
      }
    }else{
      funs <- NULL
    }
    
    # Calling the function simulate.performance to run the simulations and save the results
    simulate.performance(nSlopes = nSlopes, beta = beta, nObs_fit = nObs_fit,
                                  nObs_test = nObs_test, funs = funs, se = se, nDatasets = nDatasets,
                                  maxIter = maxIter, filename = substring(k, 1, nchar(k)-4),
                                  currProc = currProc, totProc = totProcesses)
    currProc <- currProc+1
    
  }else{
    print(paste0("File ", substring(k, 1, nchar(k)-4), ".RData does already exist!"))
  }
}
