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

# copying log over to the cloud
# file.copy("/home/rstudio/Dropbox/GTA cloud/17 Shiny/5 HS code finder/code/importer.log",
          # "/home/rstudio/Dropbox/GTA cloud/17 Shiny/5 HS finder/code/importer.log",overwrite = T)

## check if a process is running on the server
running.processes=system("ps aux", intern=T)

load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
importer.busy=sum(as.numeric(grepl("(importer.R)",running.processes, ignore.case = T)))

if(importer.busy>2){
  
  print(paste(Sys.time(), ": importer is busy with order #",min(importer.log$ticket.number[importer.log$under.preparation==1]), sep=""))
  print(running.processes[grepl("(importer.R)",running.processes, ignore.case = T)])
  
} else{
  
  
  ## setup
  # setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud/17 Shiny/5 HS code finder")
  # setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
  # source("17 Shiny/5 HS code finder/code/importer utensils.R")
  
  load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
  path="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"
  ## helpful functions
  for(fct in list.files("17 Shiny/5 HS code finder/code/functions", pattern = ".R", full.names=T)){
    source(fct)
  }
  
  
  
  if(sum(importer.log$under.preparation)==0){
    print(paste(Sys.time(), ": no business", sep=""))
    
  }else{
    
    max.rnds=sum(importer.log$under.preparation)
    rnd=0
    ## if nothing is running, then start the oven.
    while(sum(importer.log$under.preparation)>0 & rnd<max.rnds){
      rnd=rnd+1
      error.message <- c(F)
      
      tryCatch({
      # first come, first served.
      log.row=min(which(importer.log$under.preparation==1))
      
      logpath=paste("17 Shiny/5 HS code finder/log/",Sys.Date()," - HS code finder import #", importer.log$ticket.number[log.row],".txt",sep="")
      con <- file(logpath)
      sink(con, append=T)
      sink(con, append=T, type="message")
      
      
      ## IMPORTER
        importer.log$time.start[log.row]=Sys.time()
        class(importer.log$time.start)=c('POSIXt', 'POSIXct')
        save(importer.log, file="17 Shiny/5 HS code finder/log/importer-log.Rdata")
        
        ## extracting parameters
        kl=importer.log[log.row,]
        
        ## IMPORTING XLSX
        xlsx.path=paste0("17 Shiny/5 HS code finder/xlsx imports/",kl$xlsx.file)
        imported.phrases <- read.xlsx(file = xlsx.path, sheetIndex = 1, header = F)
        imported.phrases <- as.character(imported.phrases$X1)
        imported.phrases <- imported.phrases[is.na(imported.phrases)==F]
        
        
        # checking if we have at leat one phrase
        nr.of.phrases=length(imported.phrases)
        
        sender = "data@globaltradealert.org"  
        recipients = kl$order.email
        attachment=NULL
        
        if(nr.of.phrases>0){
          
          sbjct=paste("[",kl$job.name,"] Upload successful", sep="")
          message=paste0("Hello \n\nThank you for uploading new terms. The job ",kl$job.name," will now be processed.\n\nThis job includes ",nr.of.phrases," search phrases (=lines in the XLSX sheet). In case this number is lower than expected, consult the list of successfully imported phrases pasted at the bottom of this email.\n\nYou will receive a confirmation email as soons as it's finished. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGTA data team\n\n\n\n", paste(imported.phrases, collapse=";"), sep="")
          send.mail(from = sender,
                    to = recipients,
                    subject=sbjct,
                    body=message,
                    html=F,
                    attach.files = attachment,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender, 
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          rm(recipients, message, sbjct, sender)

        } else{
          
          recipients=c(recipients, "patrick.buess@student.unisg.ch","johannes.fritz@unsig.ch")
          sbjct=paste("[",kl$job.name,"] Upload unsuccessful", sep="")
          message=paste0("Hello \n\nThank you for uploading new terms. Unfortunately something went wrong as we could not import a single phrase from the XLSX you provided. The file we received is attached for reference.\n\nPatrick and Johannes are copied to this message. They should get back to you soon.\n\nRegards\nHS finder app")
          attachment=xlsx.path
          send.mail(from = sender,
                    to = recipients,
                    subject=sbjct,
                    body=message,
                    html=F,
                    attach.files = attachment,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender, 
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          rm(recipients, message, sbjct, sender)
        }
        
        
        ## Updating job.log
        load_all(path)
        job.id.import <- max(job.log$job.id)+1
        job.id.import <<- job.id.import
        
        job.log <- rbind(job.log,
                         data.frame(job.id = job.id.import,
                                    job.type = "import",
                                    job.name = kl$job.name,
                                    user.id = kl$user.id,
                                    nr.of.checks = max(1,kl$process.by.others),
                                    check.hierarchy = FALSE,
                                    is.priority = kl$is.priority,
                                    self.check = TRUE,
                                    related.state.act = kl$related.state.act,
                                    job.processed = TRUE, ## will be set to FALSE by HS searcher.
                                    submission.id = Sys.Date()))
        save_all(path)
        
        ## adding it to the range of HS code searches
        load_all(path)
        
        phrases.to.import=rbind(phrases.to.import, 
                                data.frame(job.id=job.id.import,
                                           phrase=imported.phrases,
                                           search.underway=F,
                                           search.concluded=F,
                                           run.time=NA,
                                           nr.attempts=0,
                                           stringsAsFactors = F))
        
        save_all(path)
        
        importer.log$under.preparation[log.row]=1
        save(importer.log, file="17 Shiny/5 HS code finder/log/importer-log.Rdata")
        
        
        
        
      },
      error = function(error.msg) {
        if(error.message[1]==T){
          error.message <<- c(T, stop.print)
        } else {
          error.message <<- c(T,error.msg$message)
        }
      })
      
      if (error.message[1]) {
        # SEND EMAIL
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
      
      print(paste("Importing complete for job ",kl$job.name, sep="" ))
      
      update_logs()
      
    }
    
    print("All imports from this round are processed")
    
  }
  
  
}
        
        

