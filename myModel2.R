myModel2<-function(kk,
                  data.set = c("CCLE","Sanger"),
                  data.type = c("Mh","C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL"), 
                  drug.type = c("ActArea","IC50","EC50"), 
                  model.type = c("ENet","Lasso","Ridge","RF","SVM","LDA","QDA"), 
                  nfolds = 5,
                  ThresholdMethod = NULL ){
  require(predictiveModeling)
  require(synapseClient)
  synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")
  source("~/PredictiveModel_pipeline/R5/crossValidatePredictiveModel2.R")
  
  myCCLE<-function(X,Y){
    dataSets<-myData_CCLE_new(X,Y)
    return(dataSets)
  }
  mySanger<-function(X,Y){
    dataSets<-myData_Sanger(X,Y)
    return(dataSets)
  }
  
  myENet<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel2.R")
    alphas =unique(createENetTuneGrid()[,1])    
    CV<-crossValidatePredictiveModel2(X, Y, model = myEnetModel2$new(), alpha = alphas, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myLasso<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel2.R")    
    CV<-crossValidatePredictiveModel2(X, Y, model = myEnetModel2$new(), alpha = 1, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myRidge<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel2.R")    
    CV<-crossValidatePredictiveModel2(X, Y, model = myEnetModel2$new(), alpha = 10^-10, numFolds = nfolds, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myRF<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myRandomForestModel2.R")    
    CV<-crossValidatePredictiveModel2(X, Y, model = myRandomForestModel2$new(), ntree = 500, thresholdMethod = ThresholdMethod)
    return(CV)
  }
  mySVM<-function(X,Y){
    require(pls)
    source("~/PredictiveModel_pipeline/R5/mySvmModel2.R")    
    CV<-crossValidatePredictiveModel2(X, Y, model = mySvmModel2$new(), thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myLDA<-function(X,Y){
    require(pls)
    source("~/PredictiveModel_pipeline/R5/myLDAModel2.R")    
    CV<-crossValidatePredictiveModel2(X, Y, model = myLDAModel2$new(), thresholdMethod = ThresholdMethod)
    return(CV)
  }
  myQDA<-function(X,Y){
    require(pls)
    source("~/PredictiveModel_pipeline/R5/myQDAModel2.R")    
    CV<-crossValidatePredictiveModel2(X, Y, model = myQDAModel2$new(), thresholdMethod = ThresholdMethod)
    return(CV)
  }
  
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun = myENet),
         Lasso = (myfun = myLasso),
         Ridge = (myfun = myRidge),
         RF = (myfun = myRF),
         SVM = (myfun = mySVM),
         LDA = (myfun = myLDA),
         QDA = (myfun = myQDA))
         
  
  set.fun <- match.arg(data.set)
  switch(set.fun, 
         CCLE = (myfun2 = myCCLE),
         Sanger = (myfun2 = mySanger))
  
  
  dataSet<-myfun2(data.type,drug.type)
  
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
