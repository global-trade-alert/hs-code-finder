gta_hs_classify_results<- function(variable.df="classifier.input",
                                   relevance.threshold=.5,
                                   path.to.cloud=NULL,
                                   source.data="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"){
 
  library(gtalibrary)
  
  
  eval(parse(text=paste("estimation.set<<-",variable.df, sep="")))
  
  agreed.parts=subset(estimation.set, selection.share %in% c(0,1))
  agreed.parts$probability=1
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
  
  load(source.data)
  
  ## estimating the unsure cases
  estimate=estimation.set[,setdiff(names(estimation.set), c("phrase.id","suggestion.id","hs.code.6","nr.times.chosen","nr.of.checks","selection.share"))]
  
  estimate$train.id=NULL
  estimate$evaluation=NULL
  
  estimation.set$probability= round(predict(hs.classifier, estimate)$pred[,1],3)
  estimation.set$relevant=as.numeric(estimation.set$probability>=relevance.threshold)

  ## adding estimates & agreed cases
  estimation.set=rbind(estimation.set, agreed.parts)
  
  ##  Updating database for processed suggestions
  code.suggested <- gta_sql_load_table("code_suggested")
  code.suggested <<- code.suggested
  phrase.table <- gta_sql_load_table("phrase_table")
  phrase.table <<- phrase.table
  job.phrase <- gta_sql_load_table("job_phrase")
  job.phrase <<- job.phrase
  
  
  # code.suggested=rbind(subset(code.suggested, (! suggestion.id %in% estimation.set$suggestion.id)),
                       # unique(estimation.set[,c(names(code.suggested))]))
  code.suggested.update <- unique(estimation.set[,c(names(code.suggested))])
  
  for (i in 1:nrow(code.suggested.update)){
    sql <- paste0("UPDATE hs_code_suggested SET probability = ", code.suggested.update$probability[i], " WHERE suggestion_id = ", code.suggested.update$suggestion.id[i],";")
    query <- sqlInterpolate(pool,
                            sql)
    gta_sql_update_table(query)
  }
  
  
    
  estimation.set=merge(estimation.set, phrase.table[,c("phrase.id","phrase")], by="phrase.id")
  estimation.set=merge(estimation.set, job.phrase[,c("phrase.id","job.id")], by="phrase.id")
  classification.result=unique(estimation.set[,c("job.id","phrase","hs.code.6","probability","relevant")])
  return(classification.result)
}
