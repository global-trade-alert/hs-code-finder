library("httr")
library("splitstackshape")
library("foreign")
library("xlsx")
library("gtalibrary")
library("gtasql")
library("pool")
library("RMariaDB")
library("data.table")
library("ggplot2")
library(mailR)
rm(list = ls())

# gta_setwd()
setwd("/home/rstudio/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

wdpath="17 Shiny/5 HS code finder/"
# wdpath="0 dev/hs-code-finder-pb/"

# copying log over to the cloud
# file.copy("/home/rstudio/Dropbox/GTA cloud/17 Shiny/5 HS code finder/code/importer.log",
          # "/home/rstudio/Dropbox/GTA cloud/17 Shiny/5 HS finder/code/importer.log",overwrite = T)
database = "ricardomain"
gta_sql_pool_open(db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "hs_")

## check if a process is running on the server
running.processes=system("ps aux", intern=T)

# load(paste0(wdpath,"/log/importer-log.Rdata"))
importer.busy=sum(as.numeric(grepl("(importer.R)",running.processes, ignore.case = T)))

if(importer.busy>2){
  
  print(paste(Sys.time(), ": importer is busy with order #",min(importer.log$ticket.number[importer.log$under.preparation==1]), sep=""))
  print(running.processes[grepl("(importer.R)",running.processes, ignore.case = T)])
  
} else{
  
  
  ## setup
  # setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud/17 Shiny/5 HS code finder")
  # setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
  # source("17 Shiny/5 HS code finder/code/importer utensils.R")
  
  # load(paste0(wdpath,"/log/importer-log.Rdata"))
  # path=paste0(wdpath,"/database/GTA HS code database.Rdata")
  
  ## helpful functions
  for(fct in list.files(paste0(wdpath,"code/functions"), pattern = ".R", full.names=T)){
    source(fct)
  }
  
  importer.log <- change_encoding(gta_sql_load_table("importer_log"))
  importer.log <<- importer.log
  
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
      log.row=min(importer.log$ticket.number[importer.log$under.preparation==1])
      
      logpath=paste0(wdpath,"log/",Sys.Date()," - HS code finder import #", importer.log$ticket.number[log.row],".txt")
      con <- file(logpath)
      sink(con, append=T)
      sink(con, append=T, type="message")
      
      
      
      ## IMPORTER
      sql <- "UPDATE hs_importer_log SET time_start = ?newvalue WHERE ticket_number = ?forwhom;"
      query <- sqlInterpolate(pool, 
                              sql, 
                              forwhom = log.row,
                              newvalue = as.character(Sys.time()))
      
      gta_sql_update_table(query)
        
        ## extracting parameters
        kl=importer.log[importer.log$ticket.number==log.row,]
        
        ## IMPORTING XLSX
        xlsx.path=paste0(wdpath,"xlsx imports/",kl$xlsx.file)
        imported.phrases <- xlsx::read.xlsx(file = xlsx.path, sheetIndex = 1, header = F)
        imported.phrases <- as.character(imported.phrases$X1)
        imported.phrases <- imported.phrases[is.na(imported.phrases)==F]
        
        
        # checking if we have at leat one phrase
        nr.of.phrases=length(imported.phrases)
        
        sender = gta_pwd("mail")$mail  
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
                    smtp = list(host.name = gta_pwd("mail")$host,
                                port=gta_pwd("mail")$port,
                                user.name=sender, 
                                passwd=gta_pwd("mail")$password,
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
                    smtp = list(host.name = gta_pwd("mail")$host,
                                port=gta_pwd("mail")$port,
                                user.name=sender, 
                                passwd=gta_pwd("mail")$password,
                                tls=T),
                    authenticate = T)
          
          rm(recipients, message, sbjct, sender)
        }
        
        
        ## Updating job.log
        
        job.id.import=gta_sql_get_value("SELECT MAX(job_id) FROM hs_job_log;")+1
        job.id.import <<- job.id.import
        
        job.log.update <- data.frame(job.id = job.id.import,
                                    job.type = "import",
                                    job.name = kl$job.name,
                                    user.id = kl$user.id,
                                    nr.of.checks = max(1,kl$process.by.others),
                                    check.hierarchy = FALSE,
                                    is.priority = kl$is.priority,
                                    self.check = TRUE,
                                    related.state.act = kl$related.state.act,
                                    job.processed = TRUE, ## will be set to FALSE by HS searcher.
                                    submission.id = Sys.Date())
        
        gta_sql_append_table(append.table = "job.log",
                             append.by.df = "job.log.update")
        
        rm(job.log.update)
        
        ## adding it to the range of HS code searches
        
        phrases.to.import.update = data.frame(job.id=job.id.import,
                                             phrase=trimws(imported.phrases, which="both"),
                                             search.underway=F,
                                             search.concluded=F,
                                             run.time=NA,
                                             nr.attempts=0,
                                             stringsAsFactors = F)
          
        gta_sql_append_table(append.table = "phrases.to.import",
                             append.by.df = "phrases.to.import.update")
        
        rm(phrases.to.import.update)
        
        sql <- "UPDATE hs_importer_log SET under_preparation = true WHERE ticket_number = ?forwhom;"
        query <- sqlInterpolate(pool, 
                                sql, 
                                forwhom = log.row)
        
        gta_sql_update_table(query)
        
        
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
        sender = gta_pwd("mail")$mail
        recipients = c("patrick.buess@student.unisg.ch", "fritz.johannes@gmail.com")
        sbjct=paste("[",kl$job.name,"] Import unsuccessful",sep="")
        message=paste0("Hello \n\n The job '",kl$job.name,"' ended with an error. The message is: \n\n",error.message[2],"\n\nRegards\nGTA data team")
        
        
        send.mail(from = sender,
                  to = recipients,
                  subject=sbjct,
                  body=message,
                  html=F,
                  smtp = list(host.name = gta_pwd("mail")$host,
                              port=gta_pwd("mail")$port,
                              user.name=sender,
                              passwd=gta_pwd("mail")$password,
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
        
gta_sql_pool_close()
