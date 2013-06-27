myLDAModel2 <- setRefClass(Class = "myLDAModel2",
                           contains="PredictiveModel",
                           fields="model",
                           methods = list(
                             initialize = function(...){
                               return(.self)
                             },
                             
                             rawCaretModel = function(){
                               return(.self$model)
                             },
                             
                             customTrain = function(featureData, responseData, ...){
                               if(!is.factor(responseData)){                              
                                 stop('Response variable should be two levels of factor')
                               }   
                               K<-which(!is.na(responseData))
                               FeatureData<-featureData[K,]
                               FeatureData  <- t(unique(t(FeatureData)))
                               A<-data.frame(response = responseData[K],FeatureData)                             
                               .self$model<- lda(response ~ ., data = A)                             
                             },
                             
                             customPredict = function(featureData){
                               if(!is(featureData,"data.frame")){
                                 featureData<-data.frame(featureData)
                               }
                               
                               predictedResponse <- predict(.self$model, newdata = featureData)
                               return(predictedResponse$class)
                             }
                           )
)
