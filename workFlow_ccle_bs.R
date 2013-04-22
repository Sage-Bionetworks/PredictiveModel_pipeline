
system("mkdir ~/newPredictiveModel_ActArea/ECMh/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/ECMh/CCLE/bsLasso")

system("mkdir ~/newPredictiveModel_ActArea/Mh/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/Mh/CCLE/bsLasso")

system("mkdir ~/newPredictiveModel_ActArea/E/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/E/CCLE/bsLasso")

system("mkdir ~/newPredictiveModel_ActArea/C/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/C/CCLE/bsLasso")

system("mkdir ~/newPredictiveModel_ActArea/EMh/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/EMh/CCLE/bsLasso")

system("mkdir ~/newPredictiveModel_ActArea/CMh/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/CMh/CCLE/bsLasso")

system("mkdir ~/newPredictiveModel_ActArea/EC/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/EC/CCLE/bsLasso")


Drug.type = c("ActArea")
Data.type = c("CMh")#,"EMh","EC","ECMh")#,"MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL")  
Model.type = c("ENet")

k1 <- rep(rep(1:length(Model.type), each=24), length(Data.type))
k2 <- rep(1:length(Data.type), each=length(Model.type)*24)
k3 <- rep(1:24, length(Data.type)*length(Model.type))


workFlow_all<-function(kk){
  source("~/PredictiveModel_pipeline/myBSModel.R")
  source("~/PredictiveModel_pipeline/myData_CCLE_new.R")
  
  require(synapseClient)
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/CCLE/bs",Model.type[k1[kk]],"/bsDrug_",k3[kk],".Rdata",sep="")
  
  if(!file.exists(filename)){
    resultsScale <- myBSModel(k3[kk],data.set = "CCLE",data.type=Data.type[k2[kk]], drug.type = "ActArea", model.type = Model.type[k1[kk]], numBS= 100)    
    save(resultsScale,file = filename)        
  }  
}


KK<-c()
for(kk in 1:(24*7)){  
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/CCLE/bs",Model.type[k1[kk]],"/bsDrug_",k3[kk],".Rdata",sep="")
  if(!file.exists(filename)){
    KK<-c(KK,kk)
  }  
}

library(multicore)
mclapply(KK, function(x)workFlow_all(x),mc.cores= 8)  


