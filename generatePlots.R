library(grid)
library(gridBase)
library(gridExtra)

fileNames <- list.files("Setups/")
fileNames <- fileNames[-1]
plotRow <- 2
plotCol <- 2
sufs <- c("_Standard.RData","_Deselect.RData","_ModelChoice.RData")

resList <- list()

for(i in 1:length(fileNames)){
  for(suf in 1:3){
    k <- paste0(strtrim(fileNames[i], nchar(fileNames[i])-4), sufs[suf])
    tempSetup <- read.csv2(paste0("Setups/", fileNames[i]), header = T)
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
    
    load(paste0("Results/", k))
    
    eval(parse(text = paste0('results <- results', suf)))
    
    binResults <- (results[,-c(ncol(results)-1,ncol(results))] > 0) * 1
    relCounts <- as.data.frame(colMeans(binResults))
    rmseTest <- results[,ncol(results)]
    legFuns <- NULL
    if(length(funs_char) != 0) for(j in 1:length(funs_char)) legFuns[j] <- paste0("f_", j, "(x) = ", funs_char[j])
    
    tpfp <- c(sum(colSums(binResults[,which(beta != 0)]))/(length(which(beta != 0))*nDatasets),
              sum(colSums(binResults[,which(beta != 0)+length(beta)+length(funs_char)]))/(length(which(beta != 0))*nDatasets))
    
    estEff <- NULL
    estEff <- data.frame(matrix(0, nrow = length(which(beta != 0))+length(funs)+2, ncol = 2))
    colnames(estEff) <- c("modelLin", "modelNonLin")
    if(length(funs) != 0){
      rownames(estEff) <- c(paste0("x", 1:length(which(beta != 0))), paste0("z", 1:length(funs)), "meanLin", "meanNonLin")
    }else{
      rownames(estEff) <- c(paste0("x", 1:length(which(beta != 0))), "meanLin", "meanNonLin")
    }
    
    for(j in 1:length(which(beta != 0))){
      estEff[j,1] <- length(which(which(binResults[,j] > 0) %in% which(binResults[,(length(beta)+length(funs)+j)] == 0)))/nDatasets
      estEff[j,2] <- length(which(binResults[,(length(beta)+length(funs)+j)] > 0))/nDatasets
    }
    if(length(funs) != 0){
      for(j in 1:length(funs)){
        estEff[length(which(beta != 0))+j,1] <- length(which(which(binResults[,length(beta)+j] > 0) %in% which(binResults[,(2*length(beta)+length(funs)+j)] == 0)))/nDatasets
        estEff[length(which(beta != 0))+j,2] <- length(which(binResults[,(2*length(beta)+length(funs)+j)] > 0))/nDatasets
      }
    }
    estEff[nrow(estEff)-1,] <- round(colMeans(estEff[1:(nrow(estEff)-(length(funs)+2)),]),3)
    if(length(funs) != 0)estEff[nrow(estEff),] <- round(colMeans(estEff[(length(which(beta != 0))+1):(nrow(estEff)-2),]),3)
    
    resList[[(i-1)*3+suf]] <- list(results, binResults, relCounts, rmseTrain, rmseTest, legFuns, tpfp, k, estEff)
  }
}

pdf("Plots/tables.pdf", pointsize = 6, width = 10, height = 9)
for(k_each in seq(1,length(resList), by = 3)){
  estEff1 <- resList[[k_each]][[9]]
  estEff2 <- resList[[k_each+1]][[9]]
  estEff3 <- resList[[k_each+2]][[9]]
  tabTitle <- substr(fileNames[floor(k_each/3)+1], 13, nchar(fileNames[floor(k_each/3)+1])-4)
  subTitle1 <- "Standard Boosting"
  subTitle2 <- "Deselection Step"
  subTitle3 <- "Model Choice Step"
  layoutMat <- matrix(c(1,1,1,1,1,1,1,1,1,2,2,2,4,4,4,6,6,6), ncol = 9, byrow = TRUE)
  layoutMat <- rbind(layoutMat, matrix(rep(c(3,3,3,5,5,5,7,7,7), floor(nrow(resList[[k_each]][[9]])/3)), ncol = 9, byrow = TRUE))
  layout(layoutMat)
  
  if(!is.null(estEff)){
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  textGrob(tabTitle, gp=gpar(fontsize=12))
    grid.draw(grob)
    popViewport(3)
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  textGrob(subTitle1, gp=gpar(fontsize=12))
    grid.draw(grob)
    popViewport(3)
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  tableGrob(estEff1)
    grid.draw(grob)
    popViewport(3)
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  textGrob(subTitle2, gp=gpar(fontsize=12))
    grid.draw(grob)
    popViewport(3)
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  tableGrob(estEff2)
    grid.draw(grob)
    popViewport(3)
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  textGrob(subTitle3, gp=gpar(fontsize=12))
    grid.draw(grob)
    popViewport(3)
    frame()
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    grob <-  tableGrob(estEff3)
    grid.draw(grob)
    popViewport(3)
  }
}
dev.off()

pdf("Plots/boxplots_RMSE.pdf", width = 7, height = 4, pointsize = 0.2)
par(mar = c(7.5,6,2,2))
for(k_each in seq(1,length(resList), by = 3)){
  rmse1Train <- resList[[k_each]][[4]]
  rmse2Train <- resList[[k_each+1]][[4]]
  rmse3Train <- resList[[k_each+2]][[4]]
  rmse1Test <- resList[[k_each]][[5]]
  rmse2Test <- resList[[k_each+1]][[5]]
  rmse3Test <- resList[[k_each+2]][[5]]
  df <- data.frame(rmse1Train,rmse2Train,rmse3Train,rmse1Test,rmse2Test,rmse3Test)
  
  boxplot(df, axes = F)
  corns <- par("usr")
  title(ylab = "Root Mean Square Error (RMSE)", cex.lab = 1.5, line = 4)
  axis(2, pretty(c(min(df), max(df))), las = 1, cex.axis = 1.5)
  axis(1, at = 0:7, c("","","","","","","",""))
  text(1:6, rep(corns[3]-0.125*(corns[4]-corns[3])), labels = c("Training RMSE\nStandard Boosting","Training RMSE Boosting\n with Deselection","Training RMSE Boosting\n with Model Choice",
                                                                "Test RMSE\nStandard Boosting","Test RMSE Boosting\n with Deselection","Test RMSE Boosting\n with Model Choice"), xpd = NA, srt = 30, cex = 1.2)
}
dev.off()
