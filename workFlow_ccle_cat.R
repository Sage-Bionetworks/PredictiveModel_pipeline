Drug.type = c("EC50","ActArea","IC50")
Data.type = c("Mh","C","E","EC","CMo","EMo","EMh","CMh","ECMo","ECMh")#,"MhL","CL","EL","ECL","CMoL","EMoL","EMhL","CMhL","ECMoL","ECMhL")
Model.type = c("Lasso","Ridge","ENet","RF","SVM")#,"ENet","Ridge","RF"
Threshold.Method = "ThirtySixty"

k3 <- rep(1:24)
source("~/PredictiveModel_pipeline/myModel2.R")
source("~/PredictiveModel_pipeline/myData_CCLE_new.R")

for(model.Type in Model.type){  
  for(drug.Type in Drug.type){
    for(kk in k3){
      for(data.Type in Data.type){
        filename = paste("~/Result_predictiveModel_ver3/newPredictiveModel_",drug.Type,"/",data.Type,"/CCLE/", model.Type,"_cat/cvDrug_",Threshold.Method,"_",kk,".Rdata",sep="")
        if(!file.exists(filename)){        
          resultsScale <- myModel2(kk, data.set = "CCLE", data.type=data.Type, drug.type = drug.Type, model.type = model.Type, nfolds = 5,ThresholdMethod = Threshold.Method)    
          save(resultsScale,file = filename)        
        }  
      }  
    }
    
  }
}