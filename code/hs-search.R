library("httr")
library("splitstackshape")
library("foreign")
library("xlsx")
library("gtalibrary")
library("data.table")
library("ggplot2")
library(mailR)
rm(list = ls())
setwd("/home/rstudio/Dropbox/GTA cloud")


## check if a process is running on the server
running.processes=system("ps aux", intern=T)

hs.search.busy=sum(as.numeric(grepl("(hs-search.R)",running.processes, ignore.case = T)))

if(hs.search.busy>2){
  
  print(paste(Sys.time(), ": HS Search is busy"))
  print(running.processes[grepl("(hs-search.R)",running.processes, ignore.case = T)])
  
} else {
  
  ## setup
  path="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"
  for(fct in list.files("17 Shiny/5 HS code finder/code/functions", pattern = ".R", full.names=T)){
    source(fct)
  }
  
  
  ## updating phrases to import
  load_all(path)
  search.phrases=unique(subset(phrases.to.import, search.underway==F & search.concluded==F)$phrase)
  
  
  if(length(search.phrases)==0){
    print(paste(Sys.time(), ": no business", sep=""))
    
  }else{
    
    ## Search for the non-imported phrases, one-by-one
    while(length(search.phrases)>0){
      ## updating phrases to import
      load_all(path)
      search.phrases=unique(subset(phrases.to.import, search.underway==F & search.concluded==F)$phrase)
      
      
      
      ## Start search for a phrase
      this.phrase=search.phrases[1]
      this.phrase.jobs=unique(subset(phrases.to.import, phrase==this.phrase)$job.id)
      phrases.to.import$search.underway[phrases.to.import$phrase==this.phrase]=T
      save_all(path)
      
      ## initialise data collection
      error.message <- c(F)
      search.result=data.frame()
      
      
      tryCatch({
      
        print("FIRST ROUND")
        search.result=gta_hs_code_finder(products = this.phrase,
                                         sources = c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "eximguru", "cybex"),
                                         check.archive = T,
                                         archive.location = "17 Shiny/5 HS code finder/database/GTA HS code database.Rdata",
                                         wait.time = 15)
        
        
        
        if(! is.data.frame(search.result)){
          
          search.result=data.frame()
          
          print("SECOND ROUND")
          search.result=gta_hs_code_finder(products = this.phrase,
                                           sources = c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "eximguru", "cybex"),
                                           check.archive = T,
                                           archive.location = "17 Shiny/5 HS code finder/database/GTA HS code database.Rdata",
                                           wait.time = 15)
          
          
          
        }
        
        
        ## in any case: 
        #### update the phrase.table & job.phrase
        this.phrase=gta_hs_add_phrase(add.job.id=this.phrase.jobs,
                                      phrase.to.add=this.phrase,
                                      phrase.source="xlsx import",
                                      update.job.phrase=T,
                                      source.data=path)
        
        
        ## adding found HS codes for unprocessed phrases, if any
        if(! this.phrase$phrase.processed){
          
          if(is.data.frame(search.result) & nrow(search.result)>0){
            
            this.sug.id=(max(code.suggested$suggestion.id)+1):(max(code.suggested$suggestion.id)+nrow(search.result))
            search.result$suggestion.id=this.sug.id
            
            load_all(path)
            code.suggested=rbind(code.suggested,
                                 data.frame(suggestion.id=this.sug.id,
                                            phrase.id=this.phrase$phrase.id,
                                            hs.code.6=search.result$hs.code,
                                            probability=NA,
                                            stringsAsFactors = F))
            
            
            ## Updating code.source
            code.source.new=unique(cSplit(search.result[,c("suggestion.id","source.names")], 2, direction="long",sep=";"))
            names(code.source.new)=c("suggestion.id","source.name")
            code.source.new=merge(code.source.new, suggestion.sources, by="source.name", all.x=T)
            
            
            ## in case there is an unknown source (could be a parsing error)
            if(nrow(subset(code.source.new, is.na(source.id)))>0){
              new.src=unique(subset(code.source.new, is.na(source.id))$source.name)
              
              suggestion.sources=rbind(suggestion.sources,
                                       data.frame(source.id=(max(suggestion.sources$source.id)+1):(max(suggestion.sources$source.id)+length(new.src)),
                                                  source.name=new.src,
                                                  stringsAsFactors = F))
              suggestion.sources<<-suggestion.sources
              
              new.src=subset(code.source.new, is.na(source.id))
              new.src$source.id=NULL
              new.src=merge(new.src, suggestion.sources, by="source.name", all.x=T)
              
              code.source.new=rbind(subset(code.source.new, is.na(source.id)==F),
                                    new.src)
              
            }
            
            code.source=rbind(code.source,
                              code.source.new[,c("suggestion.id", "source.id")])
            save_all(path)
          }
            
        }
        
       
        
        ## updating phrase import status
        load_all(path)
        phrases.to.import$search.underway[phrases.to.import$phrase==this.phrase]=F
        phrases.to.import$search.concluded[phrases.to.import$phrase==this.phrase]=T
        save_all(path)
        
        
      },
      error = function(error.msg) {
        if(error.message[1]==T){
          error.message <<- c(T, stop.print)
        } else {
          error.message <<- c(T,error.msg$message)
        }
      })
      
      
      ## In case something went wrong
      
      if (error.message[1]) {
        # ERROR EMAIL
        sender = "data@globaltradealert.org"
        recipients = c("patrick.buess@student.unisg.ch", "fritz.johannes@gmail.com")
        sbjct=paste("[",kl$job.name,"] Import unsuccessful",sep="")
        message=paste0("Hello \n\n The job '",kl$job.name,"' ended with an error. The message is: \n\n",error.message[2],"\n\nRegards\nGTA data team")
        
        
        send.mail(from = sender,
                  to = recipients,
                  subject=sbjct,
                  body=message,
                  html=F,
                  smtp = list(host.name = "mail.infomaniak.com",
                              port=587,
                              user.name=sender,
                              passwd="B0d@nstrasse",
                              tls=T),
                  authenticate = T)
        
        rm(recipients, message, sbjct, sender)
      }
      
      
      
      
      
      
      
      ## Check whether a job is complete
      job.import.complete=nrow(subset(phrases.to.import, job.id %in% this.phrase.jobs & search.concluded==F))==0
      
      if(job.import.complete){
        
        # SEND AVAILABILITY EMAIL TO USER
        sender = "data@globaltradealert.org"
        recipients = kl$order.email
        sbjct=paste("[",kl$job.name,"] Import available in the app",sep="")
        message=paste0("Hello \n\nThank you for importing new terms. The job '",kl$job.name,"' is now processed and the terms can be reviewed online. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGTA data team")
        
        
        send.mail(from = sender,
                  to = recipients,
                  subject=sbjct,
                  body=message,
                  html=F,
                  smtp = list(host.name = "mail.infomaniak.com",
                              port=587,
                              user.name=sender,
                              passwd="B0d@nstrasse",
                              tls=T),
                  authenticate = T)
        
        rm(recipients, message, sbjct, sender)
        
        
        # SEND AVAILABILITY EMAIL TO UPWORK
        sender = "data@globaltradealert.org"
        sbjct=paste("GTA/UpWork HS code classification: App updated",sep="")
        
        nr.left=length(unique(subset(job.phrase, processed==F & job.id %in% subset(job.log, job.processed==F)$job.id)$phrase.id))
        message=paste0("Hello \n\nThank you for your patience. We have just updated the HS code app.\n\nThere are now ",nr.left," products awaiting classification.\n\nRegards\nJohannes\nhttp://hs.globaltradealert.org/")
        
        source("17 Shiny/5 HS code finder/setup/uw.R")
        
        if(nr.left>0){
          for(email.to in recipients){
            
            send.mail(from = sender,
                      to = email.to,
                      subject=sbjct,
                      body=message,
                      html=F,
                      smtp = list(host.name = "mail.infomaniak.com",
                                  port=587,
                                  user.name=sender,
                                  passwd="B0d@nstrasse",
                                  tls=T),
                      authenticate = T)
            }
          }
        }
      
      print(paste("Processed: ",this.phrase$phrase))
      }
  }
}
        
        
