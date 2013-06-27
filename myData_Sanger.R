myData_Sanger <- function(data.type = c("C","CMo","E","EMo","EC","ECMo","CL","CMoL","EL","EMoL","ECL","ECMoL"),drug.type = c("ActArea","IC50","EC50")){
  
  library(predictiveModeling)
  library(synapseClient)
  synapseLogin("in.sock.jang@sagebase.org","tjsDUD@")
  
  ###################################################
  #### Load Sanger Molecular Feature Data from Synapse ####
  ###################################################
  id_copyLayer <- "1742880"     
  layer_copy <- loadEntity(id_copyLayer)
  eSet_copy <- layer_copy$objects$eSet_copy
  
  id_oncomapLayer <- "syn1742882"  
  layer_oncomap <- loadEntity(id_oncomapLayer)
  eSet_oncomap <- layer_oncomap$objects$eSet_oncomap
  
  id_exprLayer <- "1742878" 
  layer_expr <- loadEntity(id_exprLayer)
  eSet_expr <- layer_expr$objects$eSet_expr
  
  id_lineageLayer <- "1742890"  
  layer_lineage <- loadEntity(id_lineageLayer)
  eSet_lineage <- layer_lineage$objects$eSet_lineage
  
  
  myActArea<-function(){
    id_drugLayer <- "syn1807986" 
    layer_drug <- loadEntity(id_drugLayer)
    adf_drug <- layer_drug$objects$sangerAUC
    return(adf_drug)
  }
  myIC50<-function(){
    id_drugLayer <- "1742876" 
    layer_drug <- loadEntity(id_drugLayer)
    adf_drug <- layer_drug$objects$sangerIC50    
    return(adf_drug)
  }
  myEC50<-function(){
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
  
  
  data.fun <- match.arg(data.type)
  switch(data.fun, 
         E = (myfun = myE),
         C = (myfun = myC),
         EMo = (myfun = myEMo),
         CMo = (myfun = myCMo),
         ECMo = (myfun = myECMo),
         EC = (myfun = myEC),
         EL = (myfun = myEL),
         CL = (myfun = myCL),
         EMoL = (myfun = myEMoL),
         CMoL = (myfun = myCMoL),
         ECMoL = (myfun = myECMoL),
         ECL = (myfun = myECL))     
  
  dataSets<-myfun()
  return(dataSets)
}