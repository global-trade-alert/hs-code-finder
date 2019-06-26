gta_hs_remove_job<- function(remove.job.ids=NULL,
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
  
  remove.jobs=remove.job.ids
  
  if(is.null(remove.jobs)){
    stop("Removal IDs are NULL.")
  }
  
  for(i in remove.jobs){
    
    if(! i  %in% job.log$job.id){
      stop(paste("Job",i,"is not in the job.log."))
    }
  }
  
  job.phrase=subset(job.phrase, ! job.id %in% remove.jobs)
  assign.global("job.phrase",job.phrase)
  
  check.log=subset(check.log, ! job.id %in% remove.jobs)
  assign.global("check.log",check.log)
  
  phrase.table=subset(phrase.table, phrase.id %in% job.phrase$phrase.id)
  assign.global("phrase.table",phrase.table)
  
  code.suggested=subset(code.suggested, is.na(hs.code.6)==F & phrase.id %in% phrase.table$phrase.id)
  assign.global("code.suggested",code.suggested)
  
  code.selected=subset(code.selected, suggestion.id %in% code.suggested$suggestion.id)
  assign.global("code.selected",code.selected)
  
  code.source=subset(code.source, suggestion.id %in% code.suggested$suggestion.id)
  assign.global("code.source",code.source)
  
  check.phrases=subset(check.phrases, phrase.id %in% phrase.table$phrase.id)
  assign.global("check.phrases",check.phrases)
  
  job.log=subset(job.log, ! job.id %in% remove.jobs)
  assign.global("job.log",job.log)

  additional.suggestions=subset(additional.suggestions, check.id %in% check.phrases$check.id)
  assign.global("additional.suggestions",additional.suggestions)
  
  save_all(path)
  
  print(paste("Removed job(s)",paste(remove.jobs, collapse=","), "successfully."))
}
