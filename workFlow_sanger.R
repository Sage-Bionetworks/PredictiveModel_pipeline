
Drug.type = c("ActArea","IC50")
Data.type = c("CL","CMoL","EL","EMoL","ECL","ECMoL")  
Model.type = c("PCR","PLS","Ridge","Lasso","RF","SVM","ENet")

k1 <- rep(rep(1:length(Model.type), each=10), length(Data.type))
k2 <- rep(1:length(Data.type), each=length(Model.type)*10)
k3 <- rep(11:20, length(Data.type)*length(Model.type))


workFlow_all<-function(kk){
  source("~/PredictiveModel_pipeline/myModel.R")
  source("~/PredictiveModel_pipeline/myData_Sanger.R")
  
  require(synapseClient)
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/Sanger/",Model.type[k1[kk]],"/cvDrug_",k3[kk],".Rdata",sep="")
  
  if(!file.exists(filename)){
    resultsScale <- myModel(k3[kk],data.set = "Sanger",data.type=Data.type[k2[kk]], drug.type = Drug.type, model.type = Model.type[k1[kk]], nfolds = 5)    
    save(resultsScale,file = filename)        
  }  
}


KK<-c()
for(kk in 1:length(k3)){  
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/Sanger/",Model.type[k1[kk]],"/cvDrug_",k3[kk],".Rdata",sep="")  
  if(!file.exists(filename)){
    KK<-c(KK,kk)    
  } 
}

library(multicore)
mclapply(KK, function(x)workFlow_all(x),mc.cores= 8)  

