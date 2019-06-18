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
    rnd=1
    ## if nothing is running, then start the oven.
    while(sum(importer.log$under.preparation)>0 & rnd<=max.rnds){
      
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
        importfile <- read.xlsx(file = xlsx.path, sheetIndex = 1, header = F)
        importfile <- as.character(importfile$X1)
        importfile <- importfile[is.na(importfile)==F]
        
        
        # checking if we have at leat one phrase
        nr.of.phrases=length(importfile)
        
        sender = "data@globaltradealert.org"  
        recipients = kl$order.email
        attachment=NULL
        
        if(nr.of.phrases>0){
          
          sbjct=paste("[",kl$job.name,"] Upload successful", sep="")
          message=paste0("Hello \n\nThank you for uploading new terms. The job ",kl$job.name," will now be processed.\n\nThis job includes ",nr.of.phrases," search phrases (=lines in the XLSX sheet). In case this number is lower than expected, consult the list of successfully imported phrases pasted at the bottom of this email.\n\nYou will receive a confirmation email as soons as it's finished. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGTA data team\n\n\n\n", paste(importfile, collapse=";"), sep="")
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
        
       
        
        
        
        ## SEARCHING THE CODES, if there are any
        import.collector <- data.frame(product.name = as.character(),
                                       hs.code = as.character(),
                                       nr.sources = as.numeric(),
                                       source.names = as.character())
        
        print("FIRST ROUND")
        for(term in importfile) {
          import.collector.temp <- gta_hs_code_finder(products = term,
                                                      sources <- c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "eximguru", "cybex"),
                                                      check.archive = T,
                                                      archive.location = "17 Shiny/5 HS code finder/database/GTA HS code database.Rdata")
          if (is.data.frame(import.collector.temp)==T) {
            if (nrow(import.collector.temp)>0) {
              import.collector <- rbind(import.collector, import.collector.temp)
            }
          } else {
            import.collector <- rbind(import.collector, data.frame(product.name = term,
                                                                   hs.code = NA,
                                                                   nr.sources = NA,
                                                                   source.names = NA))
          }
          rm(import.collector.temp)
        }
        
        print("SECOND ROUND")
        if (nrow(subset(import.collector, is.na(hs.code)==T))>0) {
          scd.terms <- unique(subset(import.collector, is.na(hs.code)==T)$product.name)
          for(term in scd.terms) {
            import.collector.temp <- gta_hs_code_finder(products = term,
                                                        sources <- c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "eximguru", "cybex"),
                                                        check.archive = T,
                                                        archive.location = "17 Shiny/5 HS code finder/database/GTA HS code database.Rdata")
            if (is.data.frame(import.collector.temp)==T) {
              if (nrow(import.collector.temp)>0) {
                import.collector <- subset(import.collector, product.name != term)
                import.collector <- rbind(import.collector, import.collector.temp)
              }
            }
            rm(import.collector.temp)
          }
        }
      
        write.xlsx(import.collector, file=paste0("17 Shiny/5 HS code finder/xlsx imports/",gsub("\\.xlsx","",kl$xlsx.file), " - found.xlsx"),row.names = F, col.names = F,sheetName = "found")
      
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
                                    job.processed = FALSE,
                                    submission.id = Sys.Date()))
        save_all(path)
        
        ## Updating phrase.table
        load_all(path)
        
        phrase.table$phrase.tl=tolower(phrase.table$phrase)
        import.collector$phrase.tl=tolower(import.collector$product.name)
        
        import.collector=merge(import.collector, phrase.table[,c("phrase.tl","phrase.id")], by="phrase.tl", all.x=T)
        phrase.table$phrase.tl=NULL
        
        phrase.table.temp=unique(import.collector[,c("product.name","phrase.id")])
        
        new.phrases=subset(phrase.table.temp, is.na(phrase.id))
        new.phrases$phrase.id=(max(phrase.table$phrase.id)+1):(max(phrase.table$phrase.id)+nrow(new.phrases))
        
        phrase.table=rbind(phrase.table,
                           data.frame(phrase.id=new.phrases$phrase.id,
                                      phrase=new.phrases$product.name,
                                      source="xlsx import",
                                      nr.completed.jobs=0,
                                      stringsAsFactors = F))
        
        save_all(path)
        
        
        ## checking whether existing phrases have been processed
        if(nrow(subset(phrase.table.temp, is.na(phrase.id)==F))>0){
          old.phrases=subset(phrase.table.temp, is.na(phrase.id)==F)
          
          op.procssed=logical(nrow(old.phrases))
          
          for(i in 1:nrow(old.phrases)){
            op.procssed[i]=T %in% subset(job.phrase, phrase.id %in% old.phrases$phrase.id[i])$processed
          }
                             
        }
        
        phrase.table.temp=rbind(subset(phrase.table.temp, is.na(phrase.id)==F), 
                               new.phrases)
        
        phrase.table.temp=phrase.table.temp[order(phrase.table.temp$phrase.id),]
        
        ## Updating job.phrase
        load_all(path)
        
        if(nrow(new.phrases)==nrow(phrase.table.temp)){
          
          job.phrase=rbind(job.phrase,
                           data.frame(job.id=job.id.import,
                                      phrase.id=phrase.table.temp$phrase.id,
                                      processed=F,
                                      stringsAsFactors = F))
          
        } else {
          
          job.phrase=rbind(job.phrase,
                           data.frame(job.id=job.id.import,
                                      phrase.id=old.phrases$phrase.id,
                                      processed=op.procssed,
                                      stringsAsFactors = F))
          
          job.phrase=rbind(job.phrase,
                           data.frame(job.id=job.id.import,
                                      phrase.id=new.phrases$phrase.id,
                                      processed=F,
                                      stringsAsFactors = F))
          
        }

        
        save_all(path)
        
        ## Updating code.suggested & code source for new phrases
        code.suggested.temp=merge(subset(phrase.table.temp, phrase.id %in% new.phrases$phrase.id), 
                                  import.collector[,c("product.name","hs.code","source.names")], by="product.name", all.x=T)
        code.suggested.temp=subset(code.suggested.temp, is.na(hs.code)==F)
        
        
        if(nrow(code.suggested.temp)>0){
          
          ## Updating code.suggested & code source
          load_all(path)
          
          ## code.suggested
          code.suggested.temp$suggestion.id=(max(code.suggested$suggestion.id)+1):(max(code.suggested$suggestion.id)+nrow(code.suggested.temp))
          
          code.suggested=rbind(code.suggested,
                               data.frame(suggestion.id=code.suggested.temp$suggestion.id,
                                          phrase.id=code.suggested.temp$phrase.id,
                                          hs.code.6=code.suggested.temp$hs.code,
                                          probability=NA,
                                          stringsAsFactors = F))
          
          ## code.source
          code.source.new=unique(cSplit(code.suggested.temp[,c("suggestion.id","source.names")], 2, direction="long",sep=";"))
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
      } else {
        
        
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
        } else {
          
          
          send.mail(from = sender,
                    to = "fritz.johannes@gmail.com",
                    subject="HS app: Job import did not update DB somewhow",
                    body="Hi\n\nWhile it says that the import of an XLSX into the DB was successful, there are now unprocessed phrases in job.phrase.\n\nPlease check, JF",
                    html=F,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
        }
        
        rm(recipients, message, sbjct, sender)
        
        
      }
      
      print(paste("Importing complete for job ",kl$job.name, sep="" ))
      
      update_logs()
      
    }
    
    print("All imports from this round are processed")
    
  }
  
  
}
        
        

