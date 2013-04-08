#' Constructor for a class in the PredictiveModel class hierarchy that wraps models returned from caret
#'
#' @param model a raw model returned by caret train()
#' @param modelType
#' @return an instance of the class CaretModel
#' @seealso caret
#' @export
require(spls)
mySplsModel1 <- setRefClass(Class = "mySplsModel1",
                           contains="PredictiveModel",
                           fields="model",
                           methods = list(
                             initialize = function(...){
                               return(.self)
                             },
                             
                             rawModel = function(){
                               return(.self$model)
                             },
                             
                             customTrain = function(featureData, responseData, nfolds = 5, K, eta, ...){
                               SD<-apply(featureData,2,sd)
                               a<-which(SD != 0)
                               featureData1<-featureData[,a]
                               cv<-cv.spls(featureData1, responseData, fold= nfolds, K, eta, kappa=0.5, select="pls2", fit="simpls")
                               
                               .self$model<-spls(featureData1,responseData,eta=cv$eta.opt,K=cv$K.opt)                                 
                             },
                             
                             customPredict = function(featureData){
                               predictedResponse <- predict(.self$model, featureData)
                               return(predictedResponse)
                             },
                             
                             getCoefficients = function(){
                               coeff<-predict(.self$model,type = "coefficient")
                               return(coeff)
                             }
                             
                           )
                           
)
