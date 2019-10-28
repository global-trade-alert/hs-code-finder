gta_hs_classify_results<- function(processed.phrase=NULL,
                                   relevance.threshold=.5,
                                   path.to.cloud=NULL,
                                   source.data="17 Shiny/5 HS code finder/database/HS classifier.Rdata"){
 
  # variable.df="classifier.variables"
  # relevance.threshold=.5
  # path.to.cloud=NULL
  # source.data="17 Shiny/5 HS code finder/database/HS classifier.Rdata"
 
  library(gtalibrary)
  
  if(is.null(processed.phrase)){
    stop("gta_hs_classify_results: No ID for the processed phrase is specified.")
  }
  
  estimation.set=gta_hs_create_classifier_variables_phrase(phrase.ids=processed.phrase)
  
  agreed.parts=subset(estimation.set, selection.share %in% c(0,1))
  agreed.parts$probability=agreed.parts$selection.share
  agreed.parts$relevant=agreed.parts$selection.share
  
  estimation.set=subset(estimation.set, ! selection.share %in% c(0,1))
   
  if(is.null(path.to.cloud)==F){
    setwd(path.to.cloud)
  } else {
    if(grepl(" cloud", getwd())){
      gta_setwd()
    } else{
      stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
    }
  }
  
  ## estimating disagreements, if there are any
  if(nrow(estimation.set)>0){
    
    load(source.data)
    
    ## compare whether variables are the same as in estimator, if not re-estimate classifier
    if(any(! classifier.variables %in% names(estimation.set))){
      
      # SEND EMAIL TO JF
      gta_hs_estimate_classifier()
      
    } else {
      
      estimate=estimation.set[,classifier.variables]
      
    }
    
    ## estimating the unsure cases
    estimation.set$probability=round(predict(hs.classifier, estimate)$pred[,1],3)
    estimation.set$relevant=as.numeric(estimation.set$probability>=relevance.threshold)
    
    ## adding estimates & agreed cases
    estimation.set=rbind(estimation.set, agreed.parts)
    
    
  }
  
  
  ##  Updating database for processed suggestions
  for (suggestion in unique(estimation.set$suggestion.id)){
    sql <- paste0("UPDATE hs_code_suggested 
                  SET probability = ", 
                  min(estimation.set$probability[estimation.set$suggestion.id==suggestion]), "
                  WHERE suggestion_id = ", suggestion,";")
    query <- sqlInterpolate(pool,
                            sql)
    gta_sql_update_table(query)
    rm(query, sql)
  }
  
  
}
