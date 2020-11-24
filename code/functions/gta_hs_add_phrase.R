gta_hs_add_phrase<- function(add.job.id=NULL,
                             phrase.to.add=NULL,
                             phrase.source=NULL,
                             update.job.phrase=T){

  
  # if(is.null(phrase.source)){
  #   
  #   stop("Please specify the phrase.source e.g. 'XLSX import'.")
  # }
  
  # if(! is.null(path.to.cloud)){
  #   setwd(path.to.cloud)
  # } else {
  #   if(grepl(" cloud", getwd())){
  #     gta_setwd()
  #   } else{
  #     stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
  #   }
  # }
  
  # load_all(source.data)
  
  phrase.log <- change_encoding(gta_sql_load_table("phrase_log"))
  
  # Filter out adjusted phrases from phrase log table, to prevent adding an adjusted phrase to a new job id
  phrase.log <- subset(phrase.log, source != "adjusted")
  
  # Trim whitespace at the end and beginning of phrase log and phrase to import
  phrase.log$phrase <- trimws(phrase.log$phrase, which="both")
  phrase.to.add <- trimws(phrase.to.add, which = "both")
  
  phrase.log <<- phrase.log
  code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
  code.suggested <<- code.suggested
  
  if(tolower(phrase.to.add) %in% tolower(phrase.log$phrase)){
    
    pt.row=min(which(tolower(phrase.log$phrase)==tolower(phrase.to.add)))
    
    ## existing phrases
    is.new=F
    
    new.phrase.id=phrase.log$phrase.id[pt.row]
    
    # CHECK IF EXISTING PHRASE HAS ALREADY PROBABILITES ABOVE 0.5
    if(new.phrase.id %in% unique(subset(code.suggested, is.na(probability)==F)$phrase.id)){
      is.processed=max(subset(code.suggested, phrase.id %in% new.phrase.id)$probability, na.rm=T)>.5
    } else {
      is.processed=F
    }
    
    phrase.jobs=max(c(subset(phrase.log, tolower(phrase)==phrase.to.add)$nr.completed.jobs,0), na.rm=T)
    
  } else {
    ## new phrases
    is.new=T
    is.processed=F
    
    new.phrase.id=max(phrase.log$phrase.id, na.rm = T)+1
    
    phrase.jobs=0
    
    
    print(new.phrase.id)
    print(phrase.to.add)
    print(phrase.source)
    print(phrase.jobs)
    
    phrase.log.update = data.frame(phrase.id=new.phrase.id,
                                  phrase=phrase.to.add,
                                  source=phrase.source,
                                  exit.status=1,
                                  processing.round=1,
                                  stringsAsFactors = F)
    phrase.log.update <<- phrase.log.update
    
    gta_sql_append_table(append.table = "phrase.log",
                         append.by.df = "phrase.log.update")
    
    rm(phrase.log.update)
    
    }
  
  # assign.global("phrase.log",phrase.log)
  
  
  if(update.job.phrase){
    
    if(is.null(add.job.id)){
      
      stop("Please specify the job.id.")
      
    }
    
    #### update job.phrase
    job.phrase <- change_encoding(gta_sql_load_table("job_phrase"))
    job.phrase <<- job.phrase
    
    if(nrow(subset(job.phrase, job.id %in% add.job.id & phrase.id==new.phrase.id))>0){
      
 
      gta_sql_update_table(paste0("UPDATE hs_job_phrase 
                                  SET processed = ",is.processed,"
                                  WHERE (job_id IN (",paste(add.job.id, collapse=","),")
                                  AND phrase_id = ",new.phrase.id,");"))
      
      
    }else{
      
      job.phrase.update=data.frame(job.id=add.job.id,
                                  phrase.id=new.phrase.id,
                                  processed=is.processed,
                                  stringsAsFactors = F)
      job.phrase.update <<- job.phrase.update
      
      gta_sql_append_table(append.table = "job.phrase",
                           append.by.df = "job.phrase.update")
      
      rm(job.phrase.update)
    }

    
    # assign.global("job.phrase",job.phrase)
    
  }
  
  
  # save_all(source.data)
  
  print(paste("Added phrase",paste(phrase.to.add, collapse=","), "successfully."))
  
  
  
  output.list<- list("phrase"=phrase.to.add,
                     "phrase.id"=new.phrase.id,
                     "phrase.new"=is.new, 
                     "phrase.processed"=is.processed)
  return(output.list)

}
      
