Drug.type = c("ActArea","IC50")
Data.type = c("C","E","EC","CMo","EMo","ECMo")#,"MhL","CL","EL","ECL","CMoL","EMoL","EMhL","CMhL","ECMoL","ECMhL")
Model.type = c("Ridge","Lasso","ENet")

k3 <- rep(1:138)
source("~/PredictiveModel_pipeline/myBSModel1.R")
source("~/PredictiveModel_pipeline/myData_Sanger.R")

for(model.Type in Model.type){  
  for(drug.Type in Drug.type){
    for(kk in k3){
      for(data.Type in Data.type){
        filename = paste("~/newPredictiveModel_",drug.Type,"/",data.Type,"/Sanger/bs",model.Type,"/bsDrug_",kk,".Rdata",sep="")
        if(!file.exists(filename)){
          resultsScale <- myBSModel1(kk,data.set = "Sanger",data.type=data.Type, drug.type = drug.Type, model.type = model.Type, numBS= 100, numCore = 8)    
          save(resultsScale,file = filename)        
        }  
      }  
    }
  }
}
