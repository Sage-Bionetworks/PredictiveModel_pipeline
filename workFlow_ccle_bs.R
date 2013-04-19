
system("mkdir ~/newPredictiveModel_ActArea/ECMh/CCLE/bsENet")
system("mkdir ~/newPredictiveModel_ActArea/ECMh/CCLE/bsLasso")


system("mkdir ~/newPredictiveModel_ActArea/Mh/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/Mh/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/ECMhL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/ECMhL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/MhL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/MhL/CCLE/Lasso")


system("mkdir ~/newPredictiveModel_ActArea/E/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/E/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/C/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/C/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/EMo/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/EMo/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/CMo/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/CMo/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/EMh/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/EMh/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/CMh/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/CMh/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/EC/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/EC/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/ECMo/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/ECMo/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/EL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/EL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/CL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/CL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/EMoL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/EMoL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/CMoL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/CMoL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/EMhL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/EMhL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/CMhL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/CMhL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/ECL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/ECL/CCLE/Lasso")

system("mkdir ~/newPredictiveModel_ActArea/ECMoL/CCLE/ENet")
system("mkdir ~/newPredictiveModel_ActArea/ECMoL/CCLE/Lasso")




workFlow_all<-function(kk){
  source("~/PredictiveModel_pipeline/myBSModel.R")
  source("~/PredictiveModel_pipeline/myData_CCLE_new.R")
  
  require(synapseClient)
  Drug.type = c("ActArea")
  Data.type = c("Mh")#,"C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL")  
  Model.type = c("Lasso")#,"ENet")
  
  k1 <- rep(rep(1:length(Model.type), each=24), length(Data.type))
  k2 <- rep(1:length(Data.type), each=length(Model.type)*24)
  k3 <- rep(1:24, length(Data.type)*length(Model.type))
  
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/CCLE/bs",Model.type[k1[kk]],"/bsDrug_",k3[kk],".Rdata",sep="")
  
  if(!file.exists(filename)){
    resultsScale <- myBSModel(k3[kk],data.set = "CCLE",data.type=Data.type[k2[kk]], drug.type = "ActArea", model.type = Model.type[k1[kk]], numBS= 100)    
    save(resultsScale,file = filename)        
  }  
}


KK<-c()
for(kk in 1:24){
  Drug.type = c("ActArea")
  Data.type = c("Mh")#,"C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL")  
  Model.type = c("Lasso")#,"ENet")
  
  k1 <- rep(rep(1:length(Model.type), each=24), length(Data.type))
  k2 <- rep(1:length(Data.type), each=length(Model.type)*24)
  k3 <- rep(1:24, length(Data.type)*length(Model.type))
  
  filename = paste("~/newPredictiveModel_",Drug.type,"/",Data.type[k2[kk]],"/CCLE/bs",Model.type[k1[kk]],"/bsDrug_",k3[kk],".Rdata",sep="")
  if(!file.exists(filename)){
    KK<-c(KK,kk)
  }  
}

library(multicore)
mclapply(KK, function(x)workFlow_all(x),mc.cores= 8)  


