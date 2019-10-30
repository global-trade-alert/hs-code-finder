gta_hs_classify_results<- function(processed.phrase=NULL,
                                   job.id=NULL,
                                   relevance.threshold=.5,
                                   path.to.cloud=NULL,
                                   source.data="17 Shiny/5 HS code finder/database/HS classifier.Rdata"){
 
  # variable.df="classifier.variables"
  # relevance.threshold=.5
  # path.to.cloud=NULL
  # source.data="17 Shiny/5 HS code finder/database/HS classifier.Rdata"
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  
  setwd("/home/rstudio/Dropbox/GTA cloud")
  
  gta_sql_pool_open(db.title="ricardomain",
                    db.host = gta_pwd("ricardomain")$host,
                    db.name = gta_pwd("ricardomain")$name,
                    db.user = gta_pwd("ricardomain")$user,
                    db.password = gta_pwd("ricardomain")$password,
                    table.prefix = "hs_")
  
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
      gta_sql_pool_close()
    }
  }
  
  ## estimating disagreements, if there are any
  if(nrow(estimation.set)>0){
    
    load(source.data)
    
    ## compare whether variables are the same as in estimator, if not re-estimate classifier
    if(any(! classifier.variables %in% names(estimation.set))){
      
      # SEND EMAIL TO JF
      sender = gta_pwd("mail")$mail  
      recipients = c("fritz.johannes@gmail.com","patrick.buess@student.unisg.ch")
      attachment=NULL
      
      sbjct=paste("[HS App] Classifier reestimation", sep="")
      message=paste0("Hello Johannes\n\n The HS App classifier is being reestimated. \n\nRegards\nGTA data team\n\n\n\n", sep="")
      send.mail(from = sender,
                to = recipients,
                subject=sbjct,
                body=message,
                html=F,
                attach.files = attachment,
                smtp = list(host.name = gta_pwd("mail")$host,
                            port=gta_pwd("mail")$port,
                            user.name=sender, 
                            passwd=gta_pwd("mail")$password,
                            tls=T),
                authenticate = T)
      
      rm(recipients, message, sbjct, sender)
      
      gta_hs_estimate_classifier()
      
      
    } else {
      
      estimate=estimation.set[,classifier.variables]
      
    }
    
    ## estimating the unsure cases
    estimation.set$probability=round(predict(hs.classifier, estimate)$pred[,1],3)
    estimation.set$relevant=as.numeric(estimation.set$probability>=relevance.threshold)
    
    ## adding estimates & agreed cases
    estimation.set=rbind(estimation.set, agreed.parts)
    
    
  } else {
    estimation.set=agreed.parts
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
  
  # GET MAX PROBABILITY AND DECIDE WHETHER PRHASE IS PROCESSED OR NOT
  for(this.phrase in unique(estimation.set$phrase.id)){
    
    max.prob=max(estimation.set$probability[estimation.set$phrase.id==this.phrase])
    
    if (max.prob > relevance.threshold) {
      
      sql <- "UPDATE hs_job_phrase SET processed = 1 WHERE (phrase_id = ?phraseID AND job_id = ?jobID);"
      query <- sqlInterpolate(pool,
                              sql,
                              phraseID = this.phrase,
                              jobID = job.id)
      gta_sql_update_table(query)
      
      sql <- "UPDATE hs_phrase_log SET exit_status = 2 WHERE phrase_id = ?phraseID;"
      query <- sqlInterpolate(pool,
                              sql,
                              phraseID = this.phrase)
      gta_sql_update_table(query)
      
    } else {
      
      sql <- "UPDATE hs_phrase_log SET processing_round = processing_round + 1 WHERE phrase_id = ?phraseID;"
      query <- sqlInterpolate(pool,
                              sql,
                              phraseID = this.phrase)
      gta_sql_update_table(query)
      
      sql <- "UPDATE hs_job_phrase SET processed = 0 WHERE (phrase_id = ?phraseID AND job_id = ?jobID);"
      query <- sqlInterpolate(pool,
                              sql,
                              phraseID = this.phrase,
                              jobID = job.id)
      gta_sql_update_table(query)
      
    }
    
  }
  print("Fully classified!")
  gta_sql_pool_close()
}
