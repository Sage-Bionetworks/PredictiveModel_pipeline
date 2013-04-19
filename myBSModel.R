myBSModel<-function(kk,
                  data.set = c("CCLE","Sanger"),
                  data.type = c("Mh","C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL"), 
                  drug.type = c("ActArea","IC50","EC50"), 
                  model.type = c("ENet","Lasso"), 
                  numBS = NULL){
  require(predictiveModeling)
  require(synapseClient)
  synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")
  source("~/PredictiveModel_pipeline/R5/bootstrapPredictiveModel.R")
  
  myCCLE<-function(X,Y){
    dataSets<-myData_CCLE_new(X,Y)
    return(dataSets)
  }
  mySanger<-function(X,Y){
    dataSets<-myData_Sanger(X,Y)
    return(dataSets)
  }
  
  myENet<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel1.R")
    alphas =unique(createENetTuneGrid()[,1])    
    BS<-bootstrapPredictiveModel(X,Y, model = myEnetModel1$new(), numBootstrap= numBS, alpha=alphas)
    return(BS)
  }
  myLasso<-function(X,Y){
    source("~/PredictiveModel_pipeline/R5/myEnetModel1.R")    
    BS<-bootstrapPredictiveModel(X,Y, model = myEnetModel1$new(), numBootstrap= numBS, alpha=1)
    return(BS)
  }
  
  model.fun <- match.arg(model.type)
  switch(model.fun, 
         ENet = (myfun = myENet),
         Lasso = (myfun = myLasso))
  
  set.fun <- match.arg(data.set)
  switch(set.fun, 
         CCLE = (myfun2 = myCCLE),
         Sanger = (myfun2 = mySanger))
  
  
  dataSet<-myfun2(data.type,drug.type)
  
  # data preprocessing for preselecting features
  filteredData<-filterPredictiveModelData(dataSet$featureData,dataSet$responseData[,kk,drop=FALSE])
  
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
