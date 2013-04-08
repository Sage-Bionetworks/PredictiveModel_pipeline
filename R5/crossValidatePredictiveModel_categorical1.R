require(ROCR)
require(ggplot2)
crossValidatePredictiveModel_categorical1 <- 
  function(featureData, responseData, model, numFolds = 5, thresholdMethod = NULL,...){
    
    require(modeest)
    if(is.element(thresholdMethod,"mode")){
      responseData <-factor(as.numeric(responseData>= mlv(responseData)$M))
    }
    if(is.element(thresholdMethod,"mean")){
      responseData <-factor(as.numeric(responseData>= mean(responseData)))
    }
    if(is.element(thresholdMethod,"mean_sd")){
      low <- responseData <= mean(responseData)-0.5*sd(responseData)
      high <- responseData >= mean(responseData)+0.5*sd(responseData)
      pos <- low|high
      threshold = mean(responseData)
      responseData<-responseData[pos]
      featureData<-featureData[pos,]
      responseData <-factor(as.numeric(responseData >= threshold))
    }
    if(is.element(thresholdMethod,"median")){
      responseData <-factor(as.numeric(responseData >= median(responseData)))
    }
    if(is.element(thresholdMethod,"median_mad")){
      low <- responseData <= median(responseData)-0.5*mad(responseData)
      high <- responseData >= median(responseData)+0.5*mad(responseData)
      pos <- low|high
      threshold<-median(responseData)
      responseData<-responseData[pos]
      featureData<-featureData[pos,]
      responseData <-factor(as.numeric(responseData >= threshold))      
    }
    if(is.null(thresholdMethod)){
      responseData <-factor(responseData)
    }
   
    #-----------------------------------------------------------------------
    # Split the data into training and test partitions
    # -----------------------------------------------------------------------
    
    set.seed(2)
    foldIndices <- createFolds(featureData[,1], k = numFolds, list = TRUE)
    
    message("Training partitions")
    
    foldResults <- foreach(fold = foldIndices) %dopar% {   
      
      model$customTrain(featureData[-fold,], responseData[-fold], ...)
      
      trainPredictions = model$customPredict(featureData[-fold,])
      trainObservations = responseData[-fold]
      testPredictions = model$customPredict(featureData[fold,])
      testObservations = responseData[fold]
      
      res <- list(trainPredictions = trainPredictions, 
                  trainObservations = trainObservations,
                  testPredictions = testPredictions,
                  testObservations = testObservations)
      return(res)           
    }   
    
    trPred <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$trainPredictions}
    tePred <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$testPredictions}
    trObsr <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$trainObservations}
    teObsr <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$testObservations}
    
    allTrPred<-do.call("c",trPred)
    allTePred<-do.call("c",tePred)
    allTrObsr<-do.call("c",trObsr)
    allTeObsr<-do.call("c",teObsr)
    
    
    # EVALUATE VALIDATION MODEL PERFORMANCE
    erPred <- prediction(allTePred,as.numeric(factor(allTeObsr))-1) # factor become c(1,2) from c(0,1) after concatenate
    erPerf <- performance(erPred, "tpr", "fpr")
    erAUC <- performance(erPred, "auc")
    
    # FIND YOUDEN'S J POINT AND OPTIMAL SENSITIVITY AND SPECIFICITY
    erRFPerf <- performance(erPred, "sens", "spec")
    youdensJ <- erRFPerf@x.values[[1]] + erRFPerf@y.values[[1]] - 1
    jMax <- which.max(youdensJ)
    optCut <- erPerf@alpha.values[[1]][jMax]
    
    optSens <- unlist(erRFPerf@x.values)[jMax]
    optSpec <- unlist(erRFPerf@y.values)[jMax]
    
    #     rankSum <- wilcox.test(validScoreHat[validScore == 0],validScoreHat[validScore == 1])
    
    dfPerf <- as.data.frame(cbind(unlist(erPerf@x.values), unlist(erPerf@y.values)))
    colnames(dfPerf) <- c("FalsePositiveRate", "TruePositiveRate")
    
    rocCurve <- ggplot(dfPerf, aes(FalsePositiveRate, TruePositiveRate)) +
      geom_line() + 
      geom_abline(slope = 1, colour = "red") +
      opts(title = "Cross Validation ROC Curve") +
      ylab("True Positive Rate") +
      xlab("False Positive Rate") +
      opts(plot.title = theme_text(size = 14))
    
    return(list("erPred"=erPred,
                "erPerf"=erPerf,
                "erAUC"=erAUC,
                "rocCurve" = rocCurve))
  }
