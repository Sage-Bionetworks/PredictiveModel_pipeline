require(ROCR)
require(ggplot2)
crossValidatePredictiveModel2<- 
  function(featureData, responseData, model, numFolds = 5, thresholdMethod = c("GMM","MEAN","MEDIAN","MEAN_SD","MEDIAN_MAD","ThirtySixty"),...){
    
    myGMM<-function(Y){  
      require(mixtools)
      a<-which(!is.na(Y))
      wait1 <- normalmixEM(as.numeric(Y[a]), lambda = .5, mu = c(-1,1), sigma = sd(Y[a]),maxit=100000)
      Y1<-matrix(NA,nrow=1,ncol = length(Y))
      Y1[a]<- 1*  (wait1$posterior[,1] <= wait1$posterior[,2])
      return(factor(Y1))
    }    
    
    myMEAN<-function(Y){
      Y <-factor(as.numeric(Y>= mean(Y,na.rm = T)))
      return(Y)
    }
    
    myMEAN_sd1<-function(Y){    
      low <- Y < (mean(Y,na.rm = T)-0.5*sd(Y,na.rm = T))
      high <- Y >= (mean(Y,na.rm = T)+0.5*sd(Y,na.rm = T))
      pos <- low|high
      threshold = mean(Y,na.rm = T)
      Y[low]<-0
      Y[high]<-1
      Y[-which(pos)]<-NA
      Y <-factor(Y)
      return(Y)      
    }
    myMEDIAN<-function(Y){
      Y <-factor(as.numeric(Y>= median(Y,na.rm = T)))
      return(Y)
    }
    
    myMEDIAN_mad1<-function(Y){          
      low <- Y <= (median(Y,na.rm = T)-0.5*mad(Y,na.rm = T))
      high <- Y >= (median(Y,na.rm = T)+0.5*mad(Y,na.rm = T))
      pos <- low|high
      Y[low]<-0
      Y[high]<-1
      Y[-which(pos)]<-NA
      Y <- factor(Y)
      return(Y)
    }
    myThirtySixty<-function(Y){
      Y<-as.numeric(Y)
      Y1<-Y[!is.na(Y)]
      
      X<-sort(Y1,index.return = T)
      a<-length(X$x)
      b1<-X$x[round(a/3)]
      b2<-X$x[round(a*2/3)]      
      low <- Y <= b1
      high <- Y >= b2
      pos <- low|high
      Y[low]<-0
      Y[high]<-1
      Y[-which(pos)]<-NA
      Y <- factor(Y)
      return(Y)
    }
    
    threshold.fun <- match.arg(thresholdMethod)
    switch(threshold.fun, 
           MEAN = (myfun = myMEAN),
           MEDIAN = (myfun = myMEDIAN),
           MEAN_SD = (myfun = myMEAN_sd1),
           MEDIAN_MAD = (myfun = myMEDIAN_mad1),
           GMM = (myfun = myGMM),
           ThirtySixty = (myfun = myThirtySixty))
    
    binResponseData <- myfun(responseData)
    #-----------------------------------------------------------------------
    # Split the data into training and test partitions
    # -----------------------------------------------------------------------
    
    set.seed(2)
    foldIndices <- createFolds(featureData[,1], k = numFolds, list = TRUE)
    
    message("Training partitions")
    
    foldResult<-function(k){      
      foldModel <- model$copy()      
      foldModel$customTrain(featureData[-foldIndices[[k]],], binResponseData[-foldIndices[[k]]], ...)
      
      trainPredictions = foldModel$customPredict(featureData[-foldIndices[[k]],])
      trainObservations = binResponseData[-foldIndices[[k]]]
      testPredictions = foldModel$customPredict(featureData[foldIndices[[k]],])
      testObservations = binResponseData[foldIndices[[k]]]
      
      res <- list(trainPredictions = trainPredictions, 
                  trainObservations = trainObservations,
                  testPredictions = testPredictions,
                  testObservations = testObservations)
      return(res)           
    }   
    require(multicore)
    
    foldResults<-mclapply(1:numFolds, function(x)foldResult(x),mc.cores = 5)
    return(foldResults)
  }
#     trPred <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$trainPredictions}
#     tePred <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$testPredictions}
#     trObsr <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$trainObservations}
#     teObsr <- foreach(k = 1:numFolds) %do%{foldResults[[k]]$testObservations}
#     
#     allTrPred<-do.call("c",trPred)
#     allTePred<-do.call("c",tePred)
#     allTrObsr<-do.call("c",trObsr)
#     allTeObsr<-do.call("c",teObsr)
#     
#     
#     # EVALUATE VALIDATION MODEL PERFORMANCE
#     erPred <- prediction(allTePred,as.numeric(factor(allTeObsr))-1) # factor become c(1,2) from c(0,1) after concatenate
#     erPerf <- performance(erPred, "tpr", "fpr")
#     erAUC <- performance(erPred, "auc")
#     
#     # FIND YOUDEN'S J POINT AND OPTIMAL SENSITIVITY AND SPECIFICITY
#     erRFPerf <- performance(erPred, "sens", "spec")
#     youdensJ <- erRFPerf@x.values[[1]] + erRFPerf@y.values[[1]] - 1
#     jMax <- which.max(youdensJ)
#     optCut <- erPerf@alpha.values[[1]][jMax]
#     
#     optSens <- unlist(erRFPerf@x.values)[jMax]
#     optSpec <- unlist(erRFPerf@y.values)[jMax]
#     
#     #     rankSum <- wilcox.test(validScoreHat[validScore == 0],validScoreHat[validScore == 1])
#     
#     dfPerf <- as.data.frame(cbind(unlist(erPerf@x.values), unlist(erPerf@y.values)))
#     colnames(dfPerf) <- c("FalsePositiveRate", "TruePositiveRate")
#     
#     rocCurve <- ggplot(dfPerf, aes(FalsePositiveRate, TruePositiveRate)) +
#       geom_line() + 
#       geom_abline(slope = 1, colour = "red") +
#       opts(title = "Cross Validation ROC Curve") +
#       ylab("True Positive Rate") +
#       xlab("False Positive Rate") +
#       opts(plot.title = theme_text(size = 14))
#     
#     return(list("erPred"=erPred,
#                 "erPerf"=erPerf,
#                 "erAUC"=erAUC,
#                 "rocCurve" = rocCurve))
#   }