collection = function(code) {
  
  tryCatch(code,
           error = function(c) {
             print(c)
             error.log=readLines(logpath)
             error.txt=paste(error.log[(length(error.log)-10):(length(error.log))], collapse="\n")
             email.alert(run, error.txt)
             
             load("scrape_log/log_file.Rda")
             scrape.log.file$completed[scrape.log.file$code==run]=1
             save(scrape.log.file, file="scrape_log/log_file.Rda")
             rm(scrape.log.file)
             
           })
}

email.alert = function(bastiat.code, error.text){
  e.text=paste("Hi Johannes \n\n
               I have encountered an error. Please check.\n
               Merci\n
               Bastiat\n\n
               Log reads:\n",error.text,sep="")
  
  sender <- "data@globaltradealert.org"  
  alert.recipients <- c("johannes.fritz@unisg.ch")
  send.mail(from = sender,
            to = alert.recipients,
            subject=paste("Error in ",bastiat.code," / ",Sys.Date(), sep=""),
            body = e.text,
            html=F,
            smtp = list(host.name = "mail.infomaniak.com",
                        port=587,
                        user.name=sender, 
                        passwd="B0d@nstrasse",
                        tls=T),
            authenticate = T)
  problem=1
}