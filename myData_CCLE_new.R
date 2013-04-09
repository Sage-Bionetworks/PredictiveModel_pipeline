myData_CCLE_new <- function(data.type = c("Mh","C","CMo","CMh","E","EMo","EMh","EC","ECMo","ECMh","MhL","CL","CMoL","CMhL","EL","EMoL","EMhL","ECL","ECMoL","ECMhL"),drug.type = c("ActArea","IC50","EC50")){
  
  library(predictiveModeling)
  library(synapseClient)
  synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")
  
  #### Load CCLE Molecular Feature Data from Synapse ####    
  id_exprLayer <- "syn1744779" 
  layer_expr <- loadEntity(id_exprLayer)
  eSet_expr <- layer_expr$objects$eSet_expr
  
  id_copyLayer <- "syn1744777"     
  layer_copy <- loadEntity(id_copyLayer)
  eSet_copy <- layer_copy$objects$eSet_copy
  
  id_hybridLayer <-  "syn1744781" 
  layer_hybrid <- loadEntity(id_hybridLayer)
  eSet_hybrid <- layer_hybrid$objects$eSet_hybrid
  
  id_oncomapLayer <- "syn1744844"  
  layer_oncomap <- loadEntity(id_oncomapLayer)
  eSet_oncomap <- layer_oncomap$objects$eSet_oncomap
  
  id_lineageLayer <- "syn1744846"  
  layer_lineage <- loadEntity(id_lineageLayer)
  eSet_lineage <- layer_lineage$objects$eSet_lineage
  
  myActArea<-function(){
    id_drugLayer <- "syn1744787" 
    layer_drug <- loadEntity(id_drugLayer)
    adf_drug <- layer_drug$objects$drugCCLE_ActArea
    return(adf_drug)
  }
  myIC50<-function(){
    id_drugLayer <- "syn1744783" 
    layer_drug <- loadEntity(id_drugLayer)
    adf_drug <- layer_drug$objects$drugCCLE_IC50
    return(adf_drug)
  }
  myEC50<-function(){
    id_drugLayer <- "syn1744785" 
    layer_drug <- loadEntity(id_drugLayer)
    adf_drug <- layer_drug$objects$drugCCLE_EC50
    return(adf_drug)
  }
  
  drug.fun <- match.arg(drug.type)
  switch(drug.fun, 
         ActArea = (myfun1 = myActArea),
         IC50 = (myfun1 = myIC50),
         EC50 = (myfun1 = myEC50))     
  adf_drug<-myfun1()
  
  myE <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myC <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEC <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMo <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_oncomap))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMo <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_oncomap))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMo <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_oncomap))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  myMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMh <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_hybrid))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  
  
  myEL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCL <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMoL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_oncomap,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMoL <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_oncomap,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMoL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_oncomap,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  myMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myEMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myCMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(copy = eSet_copy, mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  myECMhL <- function(){
    featureData <- createAggregateFeatureDataSet(list(expr = eSet_expr,copy = eSet_copy, mut = eSet_hybrid,line = eSet_lineage))    
    featureData_filtered <- filterNasFromMatrix(featureData, filterBy = "rows")
    dataSets <- createFeatureAndResponseDataList(t(featureData_filtered),adf_drug)
    return(dataSets)
  }
  
  data.fun <- match.arg(data.type)
  switch(data.fun, 
         Mh = (myfun = myMh),
         E = (myfun = myE),
         C = (myfun = myC),
         EMo = (myfun = myEMo),
         CMo = (myfun = myCMo),
         EMh = (myfun = myEMh),
         CMh = (myfun = myCMh),
         ECMo = (myfun = myECMo),
         ECMh = (myfun = myECMh),
         EC = (myfun = myEC),
         MhL = (myfun = myMhL),
         EL = (myfun = myEL),
         CL = (myfun = myCL),
         EMoL = (myfun = myEMoL),
         CMoL = (myfun = myCMoL),
         EMhL = (myfun = myEMhL),
         CMhL = (myfun = myCMhL),
         ECMoL = (myfun = myECMoL),
         ECMhL = (myfun = myECMhL),
         ECL = (myfun = myECL))     
  
  dataSets<-myfun()
  return(dataSets)
}