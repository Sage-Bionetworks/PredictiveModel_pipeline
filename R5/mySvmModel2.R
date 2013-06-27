require(e1071)
mySvmModel2 <- setRefClass(Class = "mySvmModel2",
                          contains="PredictiveModel",
                          fields="model",
                          methods = list(
                            initialize = function(...){
                              return(.self)
                            },
                            
                            rawCaretModel = function(){
                              return(.self$model)
                            },
                            
                            customTrain = function(featureData, responseData){
                              if(!is.factor(responseData)){                              
                                stop('Response variable should be two levels of factor')
                              }   
                              K<-which(!is.na(responseData))
                              FeatureData<-featureData[K,]
                              ResponseData<-responseData[K]
                              
                              .self$model<- svm(FeatureData, ResponseData,decision.values = TRUE)
                              
                            },
                            
                            customPredict = function(featureData){
                              predictedResponse <- predict(.self$model, featureData, decision.values = TRUE)
                              
                              return(attributes(predictedResponse)$decision.values)
                            }
                          )
)
