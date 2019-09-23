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
  
  phrase.table <- change_encoding(gta_sql_load_table("phrase_table"))
  phrase.table <<- phrase.table
  code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
  code.suggested <<- code.suggested
  
  if(tolower(phrase.to.add) %in% tolower(phrase.table$phrase)){
    
    pt.row=min(which(tolower(phrase.table$phrase)==tolower(phrase.to.add)))
    
    ## existing phrases
    is.new=F
    
    new.phrase.id=phrase.table$phrase.id[pt.row]
    
    if(new.phrase.id %in% unique(subset(code.suggested, is.na(probability)==F)$phrase.id)){
      is.processed=max(subset(code.suggested, phrase.id %in% new.phrase.id)$probability, na.rm=T)>.5
    } else {
      is.processed=F
    }
    
    phrase.jobs=max(c(subset(phrase.table, tolower(phrase)==phrase.to.add)$nr.completed.jobs,0), na.rm=T)
    
  } else {
    ## new phrases
    is.new=T
    is.processed=F
    
    new.phrase.id=max(phrase.table$phrase.id, na.rm = T)+1
    
    phrase.jobs=0
    
    
    print(new.phrase.id)
    print(phrase.to.add)
    print(phrase.source)
    print(phrase.jobs)
    
    phrase.table.update = data.frame(phrase.id=new.phrase.id,
                                  phrase=phrase.to.add,
                                  source=phrase.source,
                                  nr.completed.jobs=phrase.jobs,
                                  stringsAsFactors = F)
    phrase.table.update <<- phrase.table.update
    
    gta_sql_append_table(append.table = "phrase.table",
                         append.by.df = "phrase.table.update")
    
    rm(phrase.table.update)
    
    }
  
  # assign.global("phrase.table",phrase.table)
  
  
  if(update.job.phrase){
    
    if(is.null(add.job.id)){
      
      stop("Please specify the job.id.")
      
    }
    
    #### update job.phrase
    job.phrase <- change_encoding(gta_sql_load_table("job_phrase"))
    job.phrase <<- job.phrase
    
    if(nrow(subset(job.phrase, job.id==add.job.id & phrase.id==new.phrase.id))>0){
      
      sql <- "UPDATE hs_job_phrase SET processed = ?newvalue WHERE (job_id = ?jobID AND phrase_id = ?phraseID);"
      query <- sqlInterpolate(pool, 
                              sql, 
                              newvalue = is.processed,
                              jobID = add.job.id,
                              phraseID = new.phrase.id)
      
      gta_sql_update_table(query)
      
      # job.phrase$processed[job.phrase$job.id==add.job.id & job.phrase$phrase.id==new.phrase.id]=is.processed
      
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
      
