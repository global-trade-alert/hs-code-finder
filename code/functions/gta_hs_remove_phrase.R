gta_hs_remove_phrase<- function(remove.phrase.ids=NULL,
                             path.to.cloud=NULL,
                             source.data="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"){
 
  if(is.null(path.to.cloud)==F){
    setwd(path.to.cloud)
  } else {
    if(grepl(" cloud", getwd())){
      gta_setwd()
    } else{
      stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
    }
  }
  
 
  load_all(source.data)
  
  remove.phrases=remove.phrase.ids
  
  if(is.null(remove.phrases)){
    stop("Removal IDs are NULL.")
  }
  
  for(i in remove.phrases){
    
    if(! i  %in% phrase.table$phrase.id){
      stop(paste("Phrase",i,"is not in the job.log."))
    }
  }

  
  
  phrase.table=subset(phrase.table, ! phrase.id %in% remove.phrases)
  assign.global("phrase.table",phrase.table)
  
  job.phrase=subset(job.phrase, phrase.id %in% phrase.table$phrase.id)
  assign.global("job.phrase",job.phrase)
  
  job.log=subset(job.log, job.id %in% job.phrase$job.id)
  assign.global("job.log",job.log)
  
  check.phrases=subset(check.phrases, phrase.id %in% phrase.table$phrase.id)
  assign.global("check.phrases",check.phrases)
  
  check.log=subset(check.log, check.id %in% check.phrases$check.id)
  assign.global("check.log",check.log)
 
  phrases.to.import=subset(phrases.to.import, job.id %in% job.log$job.id)
  assign.global("phrases.to.import",phrases.to.import)
  
  code.suggested=subset(code.suggested, is.na(hs.code.6)==F & phrase.id %in% phrase.table$phrase.id)
  assign.global("code.suggested",code.suggested)
  
  code.selected=subset(code.selected, suggestion.id %in% code.suggested$suggestion.id)
  assign.global("code.selected",code.selected)
  
  code.source=subset(code.source, suggestion.id %in% code.suggested$suggestion.id)
  assign.global("code.source",code.source)
 
  additional.suggestions=subset(additional.suggestions, check.id %in% check.phrases$check.id)
  assign.global("additional.suggestions",additional.suggestions)
  
  save_all(source.data)
  
  print(paste("Removed phrase(s)",paste(remove.phrases, collapse=","), "successfully."))
}
