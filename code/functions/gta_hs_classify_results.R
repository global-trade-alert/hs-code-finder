gta_hs_classify_results<- function(variable.df="classifier.input",
                                   relevance.threshold=.5,
                                   path.to.cloud=NULL,
                                   source.data="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"){
 
  library(gtalibrary)
  
  
  eval(parse(text=paste("estimation.set<<-",variable.df, sep="")))
  
  
   
  if(is.null(path.to.cloud)==F){
    setwd(path.to.cloud)
  } else {
    if(grepl(" cloud", getwd())){
      gta_setwd()
    } else{
      stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
    }
  }
  
  load("17 Shiny/5 HS code finder/database/HS classifier.Rdata")
  
  ## estimating the unsure cases
  estimate=estimation.set[,setdiff(names(estimation.set), c("phrase.id","suggestion.id","hs.code.6","nr.times.chosen","nr.of.checks","selection.share"))]
  
  estimate$train.id=NULL
  estimate$evaluation=NULL
  
  estimation.set$probability= round(predict(hs.classifier, estimate)$pred[,1],3)
  estimation.set$relevant=as.numeric(estimation.set$probability>=relevance.threshold)

  ##  Updating database for processed suggestions
  load_all()
  
  code.suggested=rbind(subset(code.suggested, (! suggestion.id %in% estimation.set$suggestion.id)),
                       unique(estimation.set[,c(names(code.suggested))]))
  
  assign.global("code.suggested",code.suggested)
  save_all()
  
    
  estimation.set=merge(estimation.set, phrase.table[,c("phrase.id","phrase")], by="phrase.id")
  estimation.set=merge(estimation.set, job.phrase[,c("phrase.id","job.id")], by="phrase.id")
  classification.result=unique(estimation.set[,c("job.id","phrase","hs.code.6","probability","relevant")])
  return(classification.result)
}
