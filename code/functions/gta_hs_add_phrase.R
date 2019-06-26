gta_hs_add_phrase<- function(add.job.id=NULL,
                             phrase.to.add=NULL,
                             phrase.source=NULL,
                             path.to.cloud=NULL,
                             update.job.phrase=T,
                             source.data="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"){
 
  
  if(is.null(phrase.source)){
    
    stop("Please specify the phrase.source e.g. 'XLSX import'.")
  }
  
  if(! is.null(path.to.cloud)){
    setwd(path.to.cloud)
  } else {
    if(grepl(" cloud", getwd())){
      gta_setwd()
    } else{
      stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
    }
  }
  
  load_all(source.data)
  
  if(tolower(this.phrase) %in% tolower(phrase.table$phrase)){
    ## existing phrases
    is.new=F
    
    new.phrase.id=unique(subset(phrase.table, tolower(phrase)==phrase.to.add)$phrase.id)
    
    if(new.phrase.id %in% unique(code.suggested$phrase.id)){
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
    }
  
  
  phrase.table=rbind(phrase.table,
                     data.frame(phrase.id=new.phrase.id,
                                phrase=phrase.to.add,
                                source=phrase.source,
                                nr.completed.jobs=phrase.jobs,
                                stringsAsFactors = F))
  
  assign.global("phrase.table",phrase.table)
  
  
  if(update.job.phrase){
    
    if(is.null(add.job.id)){
      
      stop("Please specify the job.id.")
      
    }
    
    #### update job.phrase
    job.phrase=rbind(job.phrase,
                     data.frame(job.id=add.job.id,
                                phrase.id=new.phrase.id,
                                processed=is.processed,
                                stringsAsFactors = F))
    
    assign.global("job.phrase",job.phrase)
    
  }
  
  
  save_all(source.data)
  
  print(paste("Added phrase",paste(phrase.to.add, collapse=","), "successfully."))
  
  
  
  output.list<- list("phrase"=phrase.to.add,
                     "phrase.id"=new.phrase.id,
                     "phrase.new"=is.new, 
                     "phrase.processed"=is.processed)
  return(output.list)

}
