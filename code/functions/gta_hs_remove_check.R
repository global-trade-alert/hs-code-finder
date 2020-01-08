gta_hs_remove_check<- function(remove.check.ids=NULL,
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
  
  remove.checks=remove.check.ids
  
  if(is.null(remove.checks)){
    stop("Removal IDs are NULL.")
  }
  
  for(i in remove.checks){
    
    if(! i  %in% check.log$check.id){
      stop(paste("Check",i,"is not in the job.log."))
    }
  }

  
  
  check.log=subset(check.log, ! check.id %in% remove.checks)
  assign.global("check.log",check.log)
  
  check.phrases=subset(check.phrases, check.id %in% check.log$check.id)
  assign.global("check.phrases",check.phrases)
  
  phrase.table=subset(phrase.table, phrase.id %in% check.phrases$phrase.id)
  assign.global("phrase.table",phrase.table)
  
  job.phrase=subset(job.phrase, phrase.id %in% phrase.table$phrase.id)
  assign.global("job.phrase",job.phrase)
  
  job.log=subset(job.log, job.id %in% job.phrase$job.id)
  assign.global("job.log",job.log)
  
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
  
  print(paste("Removed phrase(s)",paste(remove.checks, collapse=","), "successfully."))
}
