require(randomForest)
require(modeest)

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
                                     
                                     customTrain = function(featureData, responseData,ntree = 50,...){                                                                  
                                       .self$model <- randomForest(featureData, responseData, ntree = ntree, do.trace = 5,...)
                                     },
                                     
                                     customPredict = function(featureData){
                                       predictedResponse <- predict(.self$model, featureData, type = "response")
                                       return(predictedResponse)
                                     }
                                   )
)
