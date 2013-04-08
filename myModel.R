myModel<-function(kk,
                  data.type = c("Mh","C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL"), 
                  drug.type = c("ActArea","IC50","EC50"), 
                  model.type = c("ENet","Lasso","Ridge","RF","PCR","PLS","SVM"), 
                  nfolds = 5){
  require(PredictiveModeling)
  require(synapseClient)
  synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")
  source("~/PredictiveModel_pipeline/R5/crossValidatePredictiveModel1.R")
  
  myENet<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel1.R")
    alphas =unique(createENetTuneGrid()[,1])    
    CV<-crossValidatePredictiveModel1(X, Y, model = myEnetModel1$new(), alpha = alphas, numFolds = nfolds)
    return(CV)
  }
  myLasso<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel1.R")    
    CV<-crossValidatePredictiveModel1(X, Y, model = myEnetModel1$new(), alpha = 1, numFolds = nfolds)
    return(CV)
  }
  myRidge<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel1.R")    
    CV<-crossValidatePredictiveModel1(X, Y, model = myEnetModel1$new(), alpha = 10^-10, numFolds = nfolds)
    return(CV)
  }
  myRF<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myRandomForestModel1.R")    
    CV<-crossValidatePredictiveModel1(X, Y, model = myRandomForestModel1$new(), ntree = 500)
    return(CV)
  }
  myPCR<-function(X,Y){
    require(pls)
    source("~/PredictiveModel_pipeline/R5/myPcrModel1.R")    
    CV<-crossValidatePredictiveModel1(X, Y, model = myPcrModel1$new(), ncomp=10)
    return(CV)
  }
  myPLS<-function(X,Y){
    require(pls)
    source("~/PredictiveModel_pipeline/R5/myPlsModel1.R")    
    CV<-crossValidatePredictiveModel1(X, Y, model = myPlsModel1$new(), ncomp=10)
    return(CV)
  }
  mySVM<-function(X,Y){    
    myTrControl=trainControl(method = "cv", number = 5, returnResamp = "all", verboseIter = TRUE)    
    CV<-crossValidatePredictiveModel1(X, Y, model = CaretModel$new(modelType = "svmLinear"), trControl = myTrControl, numFolds = nfolds)
    return(CV)
  }
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun = myENet),
         Lasso = (myfun = myLasso),
         Ridge = (myfun = myRidge),
         RF = (myfun = myRF),
         SVM = (myfun = mySVM),
         PCR = (myfun = myPCR),
         PLS = (myfun = myPLS))
  
  
  dataSet<-myData_CCLE(data.type,drug.type)
  
  # data preprocessing for preselecting features
  filteredData<-filterPredictiveModelData(dataSet$featureData,dataSet$responseData[,kk,drop=FALSE], featureVarianceThreshold = 0.01, corPValThresh = 0.1)
  
  # filtered feature and response data
  filteredFeatureData  <- filteredData$featureData
  filteredFeatureData  <- t(unique(t(filteredFeatureData)))
  filteredResponseData <- filteredData$responseData
  
  ## scale these data    
  filteredFeatureDataScaled <- scale(filteredFeatureData)
  filteredResponseDataScaled <- scale(filteredResponseData)  
  
  resultsScale<-myfun(filteredFeatureDataScaled,filteredResponseDataScaled)
  
  return(resultsScale) 
}
