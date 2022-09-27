linChoiceBoost <- function(object, data = NULL, fam, eta = NULL){
  require('plyr')
  
  if(is.null(data) && class(object$model.frame()) == 'list'){return(stop("Please enter the data."))
  } else if(!is.null(data)){
    data = data
  }else{
    data <- object$model.frame()
  }
  
  eta <- ifelse(is.null(eta), 0.2, eta)
  
  which.response <- which(sapply(1:(dim(data)[2]), function(x){identical(as.numeric(data[,x]), as.numeric(object$response))}))
  name.response <- colnames(data)[which.response]
  
  nameVar <- gsub(" ", "", names(coef(object)), fixed = TRUE)
  preds <- unique(gsub("\\,.*", "", gsub(".*\\(", "", nameVar)))
  isNonLin <- as.numeric(lapply(preds, function(j){ifelse(length(grep(paste0("bbs\\(", j, ","), nameVar))==1,TRUE,FALSE)}))
  isLin <- as.numeric(lapply(preds, function(j){ifelse(length(grep(paste0("bols\\(", j, ","), nameVar))==1,TRUE,FALSE)}))
  cand <- preds[which(isLin+isNonLin == 2)]
  if(length(cand)==0){
    out <- object
    object$eta  <- eta
    class(out) <- c(class(out))
  }else{
    candRisks <- data.frame(cand, 0, 0)
    colnames(candRisks) <- c("varName", "bols", "bbs")
    
    mstop <- object$mstop()
    RiskRed <- object$risk()
    totalRiskRed <- RiskRed[1] - RiskRed[mstop+1] 
    diffRiskRed = sapply(seq(1:(mstop)), function(k){RiskRed[k]-RiskRed[k+1]})
    
    select = selected(object)
    
    select = select[select != 0]
    Var = count(select)[[1]]
    Risk.Var <- lapply(1:length(Var),function(j){sum(diffRiskRed[which(count(select)[[1]][j] == select)])})
    
    n.parameter <- c(names(object$coef()))
    
    Risk.order <- data.frame(Var,n.parameter, as.numeric(Risk.Var))
    Risk.order <- Risk.order[order(Risk.order$as.numeric.Risk.Var.),]
    
    candRisks[,2] <- as.numeric(lapply(cand, function(j){Risk.order$as.numeric.Risk.Var.[grep(paste0("bols\\(", j, ","), Risk.order$n.parameter)]}))
    candRisks[,3] <- as.numeric(lapply(cand, function(j){Risk.order$as.numeric.Risk.Var.[grep(paste0("bbs\\(", j, ","), Risk.order$n.parameter)]}))
    candRisks$percentage <- candRisks[,3]/rowSums(candRisks[,2:3])
    
    
    if(length(which(candRisks$percentage < eta)) == 0){
      out <- object
      object$eta  <- eta
      class(out) <- c(class(out))
    }else{
      delInds <- lapply(1:length(which(candRisks$percentage < eta)), function(j){grep(paste0("bbs\\(", candRisks$varName[which(candRisks$percentage < eta)[j]], ","), nameVar)})
      nameVar2 <- names(coef(object))[-as.numeric(delInds)]
      form2 <-as.formula(paste(name.response, " ~ ", paste(nameVar2, collapse= "+")))
      model_after <- gamboost(formula = form2, data = data, weights = model.weights(object), family = fam, control = boost_control(mstop = mstop, nu = object$control$nu, risk = object$control$risk))
      out <- model_after
      out$eta  = eta
      class(out) <- c(class(out))
    }
  }
  return(out)
}
