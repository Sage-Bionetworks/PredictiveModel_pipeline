bootstrapPredictiveModel1 <- 
  function(featureData, responseData, model, numBootstrap = 100, core,...){
    
    bootIndices <- createResample(featureData[,1],times = numBootstrap,list = TRUE)
    
    bootCoeff <- function(k){
      bootModel<-model$copy()
      bootModel$customTrain(featureData[bootIndices[[k]], ], responseData[bootIndices[[k]]], ...)
      coeffs<-bootModel$getCoefficients()      
      return(coeffs)
    }
    
    bootResults<-mclapply(1:numBootstrap, function(x)bootCoeff(x), mc.cores= core)  
    
    return(bootResults)
  }
