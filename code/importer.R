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
  save_all <- function() {
    print("SAVE_ALL()")
    save(check.certainty,
         check.log,
         check.phrases,
         code.selected,
         code.source,
         code.suggested,
         hs.codes,
         hs.descriptions,
         job.log,
         job.phrase,
         levels.of.certainty,
         phrase.table,
         suggestion.sources,
         users,
         words.removed,
         report.services,
         additional.suggestions,
         file = path)
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
      
      logpath=paste("17 Shiny/5 HS code finder/results/",Sys.Date()," - HS code finder import #", importer.log$ticket.number[log.row],".txt",sep="")
      con <- file(logpath)
      sink(con, append=T)
      sink(con, append=T, type="message")
      
      
      ## IMPORTER
        importer.log$time.start[log.row]=Sys.time()
        class(importer.log$time.start)=c('POSIXt', 'POSIXct')
        save(importer.log, file="17 Shiny/5 HS code finder/log/importer-log.Rdata")
        
        ## extracting parameters
        kl=importer.log[log.row,]
        
        # SEND CONFIRMATION EMAIL
        
        sender = "data@globaltradealert.org"  
        recipients = kl$order.email
        sbjct=paste("[",kl$job.name,"] Upload successful: ", sep="")
        message=paste0("Hello \n\nThank you for uploading new terms. The job ",kl$job.name," will now be processed. You will receive a confirmation email as soons as it's finished. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGlobal Trade Alert Data")
        
        
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
        
        importfile <- read.xlsx(file = paste0("17 Shiny/5 HS code finder/xlsx imports/",kl$xlsx.file), sheetIndex = 1, header = F)
        importfile <- as.character(importfile$X1)
        importfile <- importfile[is.na(importfile)==F]

        import.collector <- data.frame(product.name = as.character(),
                                       hs.code = as.character(),
                                       nr.sources = as.numeric(),
                                       source.names = as.character())
        
        print("FIRST ROUND")
        for(term in importfile) {
          import.collector.temp <- gta_hs_code_finder(products = term,
                                                      sources <- c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "hsbianma", "eximguru", "cybex"),
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
                                                        sources <- c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "hsbianma", "eximguru", "cybex"),
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
      
        if (nrow(subset(import.collector, is.na(hs.code)==F))>0) {
        
          load(file=path)
          
        import.collector.temp <- subset(import.collector, is.na(hs.code)==F)
        
        # STORE SEARCH SOURCES
        search.sources.import <- data.frame(hs.code.6 = character(),
                                            source.id = numeric(),
                                            product.name = character())
        
        search.sources.import.temp = import.collector[,c("hs.code","source.names","product.name")]
        search.sources.import.temp <- cSplit(search.sources.import.temp, which(colnames(search.sources.import.temp)=="source.names"), direction="long", sep=";")
        names(search.sources.import.temp) <- c("hs.code.6","source.name","product.name")
        search.sources.import.temp <- merge(search.sources.import.temp, suggestion.sources, by="source.name",all.x=T)
        
        search.sources.import <- unique(search.sources.import.temp[,c("hs.code.6","source.id","product.name")])
        search.sources.import <<- search.sources.import
        rm(search.sources.import.temp, import.collector.temp)
        
        importNull = F
        
      } else {
        importNull = T
      }
      
        load(file=path)
        
      if (all(! tolower(unique(import.collector$product.name)) %in% tolower(phrase.table$phrase)) & importNull == T){
        
        # NOTHING TO ADD IF NOTHING ALREADY IN PHRASE TABLE AND importNull == T
        
        sender = "data@globaltradealert.org"  
        recipients = kl$order.email
        sbjct=paste("[",kl$job.name,"] Import not successful: ", sep="")
        message=paste0("Hello \n\nThank you for importing new terms. The job ",kl$job.name," is now processed, unfortunately no new HS codes could be found for your suggested terms. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGlobal Trade Alert Data")
        
        
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
        
        
        # ADD PHRASE IDs
        import.names <- as.character(unique(import.collector$product.name))
        import.collector <- subset(import.collector, hs.code != 0)
        import.collector <- unique(import.collector[,c(1:2)])
        names(import.collector) <- c("phrase","hs.code.6")
        
        job.id.import <- max(job.log$job.id)+1
        job.id.import <<- job.id.import
        
        job.log <- rbind(job.log,
                         data.frame(job.id = job.id.import,
                                    job.type = "import",
                                    job.name = kl$job.name,
                                    user.id = kl$user.id,
                                    nr.of.checks = kl$process.by.others,
                                    check.hierarchy = FALSE,
                                    is.priority = kl$is.priority,
                                    self.check = TRUE,
                                    related.state.act = kl$related.state.act,
                                    job.processed = FALSE,
                                    submission.id = Sys.Date()))
        
        
        # job.log <- rbind(job.log,
        #                  data.frame(job.id = job.id.import,
        #                           job.type = "Import",
        #                           job.name = "input$import.job.name",
        #                           user.id = 23,
        #                           nr.of.checks = 4,
        #                           check.hierarchy = FALSE,
        #                           is.priority = TRUE,
        #                           self.check = TRUE,
        #                           related.state.act = 123121,
        #                           job.processed = FALSE,
        #                           submission.id = Sys.Date()))
        
        job.log <<- job.log
        
        
        for (term in unique(import.names)) {
          
          ic.subset <- subset(import.collector, tolower(phrase) == tolower(term))
          
          if (! tolower(term) %in% unique(tolower(phrase.table$phrase)) & nrow(ic.subset) > 0) {
            
            phrase.table <- rbind(phrase.table,
                                  data.frame(phrase.id = max(phrase.table$phrase.id)+1,
                                             phrase = term,
                                             source = "original"))
            phrase.table <<- phrase.table
            
            phr.id.import <- max(phrase.table$phrase.id)
            phr.id.import <<- phr.id.import
            
            
            # Add job.phrases
            job.phrase <- rbind(job.phrase, 
                                data.frame(job.id = job.id.import,
                                           phrase.id = phr.id.import,
                                           processed = FALSE))
            job.phrase <<- job.phrase
            
            
            # Add suggested codes
            suggested.new.import <- ic.subset
            suggested.new.import$phrase.id = phr.id.import
            suggested.new.import$suggestion.id <- seq((max(code.suggested$suggestion.id)+1),(max(code.suggested$suggestion.id))+nrow(suggested.new.import),1)
            code.suggested <- rbind(code.suggested, suggested.new.import[,c("suggestion.id","phrase.id","hs.code.6")])
            code.suggested <<- code.suggested
            
            
            if (exists("search.sources.import")) {
              print("Source 1")
              sources.temp <- subset(search.sources.import, product.name == term)
              sources.temp <- merge(sources.temp, suggested.new.import[,c("hs.code.6","suggestion.id")], by="hs.code.6", all.x=T)
              code.source <- rbind(code.source, sources.temp[,c("source.id","suggestion.id")])
              code.source <<- code.source
              rm(sources.temp)
            }
            
          } else {
            
            if (tolower(term) %in% unique(tolower(phrase.table$phrase))){
              
              phr.id.import <- phrase.table$phrase.id[tolower(phrase.table$phrase) == tolower(term)]
              phr.id.import <<- phr.id.import
              
              # Add job.phrases
              job.phrase <- rbind(job.phrase, 
                                  data.frame(job.id = job.id.import,
                                             phrase.id = phr.id.import,
                                             processed = FALSE))
              job.phrase <<- job.phrase
              
              if (nrow(ic.subset)>0) {
                suggested.new.import <- ic.subset
                suggested.new.import$phrase.id <- phr.id.import
                
                suggested.new.import$suggestion.id <- seq((max(code.suggested$suggestion.id)+1),(max(code.suggested$suggestion.id))+nrow(suggested.new.import),1)
                code.suggested <- rbind(code.suggested, suggested.new.import[,c("suggestion.id","phrase.id","hs.code.6")])
                code.suggested <<- code.suggested
                
                
                if (exists("search.sources.import")) {
                  sources.temp <- subset(search.sources.import, hs.code.6 %in% unique(suggested.new.import$hs.code.6))
                  sources.temp <- merge(sources.temp, suggested.new.import[,c("hs.code.6","suggestion.id")], by="hs.code.6", all.x=T)
                  code.source <- rbind(code.source, sources.temp[,c("source.id","suggestion.id")])
                  code.source <<- code.source
                  print(nrow(code.source))
                  rm(sources.temp)
                }
                
              }
              
            }
            
          }
          
        }
        
        save_all()
        
        # SEND EMAIL
        sender = "data@globaltradealert.org"
        recipients = kl$order.email
        sbjct=paste("[",kl$job.name,"] Import successful",sep="")
        message=paste0("Hello \n\nThank you for importing new terms. The job '",kl$job.name,"' is now processed and the terms can be reviewed online. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGlobal Trade Alert Data")
        
        
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
        
        print("BACKGROUND PROCESS IMPORTING ALL DONE")
        
      }

    print("ALL DONE IMPORTING")

      },
    error = function(error.msg) {
      if(error.message[1]==T){
        error.message <<- c(T, stop.print)
      } else {
        error.message <<- c(T,error.msg$message)
      }
    })
      
      if (error.message[1]==T) {
        # SEND EMAIL
        sender = "data@globaltradealert.org"
        recipients = c("patrick.buess@student.unisg.ch", "fritz.johannes@gmail.com")
        sbjct=paste("[",kl$job.name,"] Import unsuccessful",sep="")
        message=paste0("Hello \n\n The job '",kl$job.name,"' ended with an error. The message is: \n\n",error.message[2],"\n\nRegards\nGlobal Trade Alert Data")
        
        
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
    
      ## updating log
      load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
      importer.log$time.finish[log.row]=Sys.time()
      class(importer.log$time.finish)=c('POSIXt', 'POSIXct')
      importer.log$under.preparation[log.row]=0
        
      
      ## Storing updated log
      save(importer.log, file = "17 Shiny/5 HS code finder/log/importer-log.Rdata")
      load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
      sink()
      sink(type="message")
      rnd=rnd+1 
  
    }
    
  }
  
  
}


