require(randomForest)
myRandomForestModel1 <- setRefClass(Class = "myRandomForestModel1",
                                   contains="PredictiveModel",
                                   fields="model",
                                   methods = list(
                                     initialize = function(...){
                                       return(.self)
                                     },
                                     
                                     rawModel = function(){
                                       return(.self$model)
                                     },
                                     
                                     customTrain = function(featureData, responseData,ntree = 500,...){ 
                                       K<-which(!is.na(responseData))
                                       FeatureData<-featureData[K,]
                                       ResponseData<-responseData[K]
                                       
                                       .self$model <- randomForest(FeatureData, ResponseData, ntree = ntree,...)
                                     },
                                     
                                     customPredict = function(featureData){
                                       predictedResponse <- predict(.self$model, featureData, type = "response")
                                       return(predictedResponse)
                                     }
                                   )
)
