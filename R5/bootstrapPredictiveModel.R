bootstrapPredictiveModel <- 
  function(featureData, responseData, model, numBootstrap = 100, ...){
    
    bootIndices <- createResample(featureData[,1],times = numBootstrap,list = TRUE)
    
    bootResults <- foreach (numBoot = bootIndices) %dopar% {
      bootModel<-model$copy()
      bootModel$customTrain(featureData[numBoot, ], responseData[numBoot], ...)
      coeffs<-bootModel$getCoefficients()      
      return(coeffs)
    }
    
    return(bootResults)
  }
