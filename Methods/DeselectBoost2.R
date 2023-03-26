DeselectBoost2 <- function(object, data = NULL, fam, tau = NULL, method = c('attributable','cumulative')){
  require('plyr')
  tau = ifelse(is.null(tau), 0.01, tau)
  method <- method[1]
  
  if(is.null(data) && class(object$model.frame()) == 'list'){return(stop("Please enter the data."))
  } else if(!is.null(data)){
    data = data
  }else{
    data <- object$model.frame()
  }
  
  nameVar <- names(coef(object,which = ''))[-1]
  which.response <- which(sapply(1:(dim(data)[2]), function(x){identical(as.numeric(data[,x]), as.numeric(object$response))}))
  name.response <- colnames(data)[which.response]
  
  mstop <- object$mstop()
  RiskRed <- object$risk()
  totalRiskRed <- RiskRed[1] - RiskRed[mstop+1] 
  diffRiskRed = sapply(seq(1:(mstop)), function(k){RiskRed[k]-RiskRed[k+1]})
  
  if(any(class(object) %in% "glmboost")){
    select = selected(object) - 1
    diffRiskRed = diffRiskRed[selected(object)-1 != 0]
  }else{
    select = selected(object)
  }
  
  select = select[select != 0]
  Var = count(select)[[1]]
  Risk.Var <- lapply(1:length(Var),function(j){sum(diffRiskRed[which(count(select)[[1]][j] == select)])})
  
  n.parameter <- c(names(object$coef()))
  if('(Intercept)' %in% n.parameter) n.parameter <- n.parameter[-which(n.parameter == '(Intercept)')]
  
  
  Risk.order <- data.frame(Var,n.parameter, as.numeric(Risk.Var), 0)
  colnames(Risk.order) <- c( 'Var', 'VarName', 'Risk', 'CumRisk')
  
  perc <- ifelse(is.null(tau), 0.01, tau) 
  percRiskRed <- totalRiskRed * perc
  
  riskTemp <- Risk.order$Risk
  for(k in 1:nrow(Risk.order)){
    cand <-  regmatches(Risk.order$VarName[k], regexec("\\(\\s*(.*?)\\s*, ", Risk.order$VarName[k]))[[1]][2]
    if(sum(1*grepl(paste0("\\(", cand, ","), Risk.order$VarName))==2){
      #print(cand)
      candInds <- which(1*grepl(paste0("\\(", cand, ","), Risk.order$VarName) == 1)
      Risk.order$Risk[k] <- Risk.order$Risk[k] + riskTemp[candInds[which(candInds != k)]]
    }
  }
  
  Risk.order <- Risk.order[order(Risk.order$Risk),]
  Risk.order$CumRisk <- cumsum(Risk.order$Risk)
  
  if(method[1] == 'attributable'){RiskRedOver <- Risk.order[which(Risk.order$Risk > percRiskRed),]
  }else{RiskRedOver <- Risk.order[which(Risk.order$CumRisk > percRiskRed),]}
  
  if(empty(RiskRedOver)){form2 = as.formula(paste(name.response, "~ 1"))
  }else{form2 <-as.formula(paste(name.response, " ~ ", paste(RiskRedOver$VarName, collapse= "+")))
  if(!is.null(environment(environment(object[["fitted"]])[["RET"]][["baselearner"]][[1]][["model.frame"]])[["df"]])){
    dfbase = environment(environment(object[["fitted"]])[["RET"]][["baselearner"]][[1]][["model.frame"]])[["df"]]
  }}
  #if(is.null(object$call$family)){ fam <-  Gaussian()
  #}else fam <- eval(parse(text = object$call$family))
  
  if(any(class(object) %in% "glmboost")){
    model_after = glmboost(form2, data = data, weights = model.weights(object), family = fam, control = boost_control(mstop = mstop, nu = object$control$nu, risk = object$control$risk))
  }else{
    model_after = gamboost(form2, data = data, weights = model.weights(object), family = fam, control = boost_control(mstop = mstop, nu = object$control$nu, risk = object$control$risk))
  }
  
  out <- model_after
  out$tau  = 0.01
  out$deselectmethod = method[1] 
  class(out) <- c(class(out))
  
  return(out)
}
