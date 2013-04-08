#' Constructor for a class in the PredictiveModel class hierarchy that wraps models returned from caret
#'
#' @param model a raw model returned by caret train()
#' @param modelType
#' @return an instance of the class CaretModel
#' @seealso caret
#' @export
mySvmModel <- setRefClass(Class = "mySvmModel",
                          contains="PredictiveModel",
                          fields="model",
                          methods = list(
                            initialize = function(...){
                              return(.self)
                            },
                            
                            rawCaretModel = function(){
                              return(.self$model)
                            },
                            
                            train = function(featureData, responseData, trControl = defaultTrainControl(),
                                             filterData = TRUE, tuneGrid = NULL){
                              if(filterData == TRUE){
                                processedData <- filterPredictiveModelData(featureData, responseData)
                                featureData <- processedData$featureData
                                responseData <- processedData$responseData
                              }
                              .self$model<- caret::train(featureData, 
                                                         responseData, 
                                                         method = "svmLinear",
                                                         preProcess = NULL, # preProcess = c("center", "scale"),
                                                         tuneLength = 4,
                                                         trControl = trControl,
                                                         tuneGrid = tuneGrid
                                                         )
                              
                            },
                            
                            predict = function(featureData){
                              predictedResponse <- predict.train(.self$model, featureData)
                              return(predictedResponse)
                            }
                            )
                          )
