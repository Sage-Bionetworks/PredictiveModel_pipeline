workFlow_all<-function(kk){
  source("~/PredictiveModel_pipeline/myModel.R")
  source("~/PredictiveModel_pipeline/myData_CCLE_new.R")
  require(pls)  
  require(kernlab)
  require(synapseClient)
  Drug.type = c("ActArea")#,"IC50","EC50")
  Data.type = c("Mh","C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL")  
  Model.type = c("PCR","PLS","Ridge","Lasso","RF","SVM","ENet")
  
  k1 <- rep(rep(1:length(Model.type), each=4), length(Data.type))
  k2 <- rep(1:length(Data.type), each=length(Model.type)*4)
  k3 <- rep(1:4, length(Data.type)*length(Model.type))
  
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/CCLE/",Model.type[k1[kk]],"/cvDrug_",k3[kk],".Rdata",sep="")
  
  if(!file.exists(filename)){
    resultsScale <- myModel(k3[kk],data.set = "CCLE",data.type=Data.type[k2[kk]], drug.type = "ActArea", model.type = Model.type[k1[kk]], nfolds = 5)    
    save(resultsScale,file = filename)        
  }
  
}


library(multicore)
kk<-1:(20*7*4)
mat<-matrix(kk,nrow=4)
for(k in 1:ncol(mat)){
  mclapply(mat[,k], function(x)workFlow_all(x))  
}


