library("httr")
library("splitstackshape")
library("foreign")
library("xlsx")
library("gtalibrary")
library("data.table")
library("ggplot2")
library("gtasql")
library("pool")
library("RMariaDB")
library(stringr)
library(mailR)
rm(list = ls())

# gta_setwd()
setwd("/home/rstudio/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

wdpath="17 Shiny/5 HS code finder/"
# wdpath="0 dev/hs-code-finder-pb/"

database = "ricardomain"
gta_sql_pool_open(db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "hs_")

## check if a process is running on the server
search.time.allowance=5
nr.parallel.processes=50000
running.processes=system("ps aux", intern=T)

hs.search.busy=sum(as.numeric(grepl("(hs-search.R)",running.processes, ignore.case = T)))


## setup
for(fct in list.files(paste0(wdpath,"/code/functions"), pattern = ".R", full.names=T)){
  source(fct)
}

phrases.to.import <- gta_sql_load_table("phrases_to_import")
phrases.to.import <<- phrases.to.import

if(hs.search.busy>=nr.parallel.processes){

  ## looking for broken searches
  run.not.finished=subset(phrases.to.import, (search.underway==T & search.concluded==F & difftime(Sys.time(), as.POSIXct(phrases.to.import$run.time[1], origin="1970-01-01"), units="mins")>search.time.allowance & is.na(run.time)==F))
  
  if(nrow(run.not.finished)>0){
    
    for(i in 1:nrow(run.not.finished)){
      # run.not.finished$search.underway=F
      sql <- "UPDATE phrases_to_import SET search_underway = 0 WHERE phrase = ?forwhom;"
      query <- sqlInterpolate(pool,
                              sql,
                              forwhom = run.not.finished$phrase[i])
      
      gta_sql_update_table(query)

    }
    ## abort stuck process
    process.to.kill=as.numeric(gsub("\\D","",str_extract(running.processes[grepl("hs-search.R", running.processes)][1], "^rstudio +?\\d+")))
    system(paste("kill -9", process.to.kill), intern=T)
    

  }

  rm(run.not.finished)

  # phrases.to.import$search.underway[]=F
  sql <- "UPDATE phrases_to_import SET search_underway = 0"
  query <- sqlInterpolate(pool,
                          sql)

  gta_sql_update_table(query)

  print(paste(Sys.time(), ": HS Search is busy"))
  print(running.processes[grepl("(hs-search.R)",running.processes, ignore.case = T)])

} else {

  ## updating phrases to import
  search.phrases=unique(subset(phrases.to.import, search.underway==F & search.concluded==F)$phrase)


  if(length(search.phrases)==0){
    print(paste(Sys.time(), ": no business", sep=""))

  }else{

    ## Search for the non-imported phrases, one-by-one
    while(length(search.phrases)>0){
      ## updating phrases to import

      phrases.to.import <- change_encoding(gta_sql_load_table("phrases_to_import"))
      phrases.to.import <<- phrases.to.import
      job.log <- change_encoding(gta_sql_load_table("job_log"))
      job.log <<- job.log
      users <- change_encoding(gta_sql_load_table("user_log", table.prefix = "gta_"))
      users <<- users

      search.phrases=unique(subset(phrases.to.import, search.underway==F & search.concluded==F)$phrase)

      ## Start search for a phrase
      this.phrase=search.phrases[1]
      
      if (is.na(this.phrase)==F) {
        
        this.phrase.jobs=unique(subset(phrases.to.import, phrase==this.phrase)$job.id)
        this.job.name=paste(unique(subset(job.log, job.id %in% this.phrase.jobs)$job.name), collapse="; ")
        this.job.email=unique(subset(users, user.id %in% subset(job.log, job.id %in% this.phrase.jobs )$user.id)$user.email)
        pti.nr.attemps <- phrases.to.import$nr.attempts[phrases.to.import$phrase==this.phrase & phrases.to.import$search.concluded == F][1]+1
  
        sql <- "UPDATE hs_phrases_to_import SET search_underway = 1, run_time = ?when, nr_attempts = ?howmuch WHERE phrase = ?forwhom;"
        query <- sqlInterpolate(pool,
                                sql,
                                forwhom = this.phrase,
                                when = as.numeric(Sys.time()),
                                howmuch = pti.nr.attemps)
  
        gta_sql_update_table(query)
  
        ## initialise data collection
        error.message <- c(F)
        search.result=data.frame()
  
  
        tryCatch({
  
          print("FIRST ROUND")
          search.result=gta_hs_code_finder(products = this.phrase,
                                           sources = c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "eximguru", "cybex"),
                                           wait.time = 15)
  
  
  
          if(! is.data.frame(search.result)){
  
            search.result=data.frame()
  
            print("SECOND ROUND")
            search.result=gta_hs_code_finder(products = this.phrase,
                                             sources = c("eurostat", "eu.customs", "zauba", "e.to.china", "google", "eximguru", "cybex"),
                                             wait.time = 15)
  
  
  
          }
  
  
          ## in any case:
          #### update the phrase.table & job.phrase
          this.phrase=gta_hs_add_phrase(add.job.id=this.phrase.jobs,
                                        phrase.to.add=this.phrase,
                                        phrase.source="xlsx import",
                                        update.job.phrase=T)
  
  
  
          ## adding found HS codes for unprocessed phrases, if any
          if(! this.phrase$phrase.processed){
  
            ## mark job as unprocessed
  
            sql <- paste0("UPDATE hs_job_log SET job_processed = 0 WHERE job_id IN (",paste0(c(this.phrase.jobs), collapse=', '),");")
            query <- sqlInterpolate(pool,
                                    sql)
            gta_sql_update_table(query)
  
            ## check whether suggestions have been made in an earlier job for this phrase
  
            code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
            code.suggested <<- code.suggested
  
            search.result$hs.code=as.numeric(search.result$hs.code)
            if(this.phrase$phrase.id %in% unique(code.suggested$phrase.id)){
  
              hs.already.suggested=as.numeric(unique(subset(code.suggested, phrase.id==this.phrase$phrase.id)$hs.code.6))
              search.result=subset(search.result, ! hs.code %in% hs.already.suggested)
  
            }
  
            ## add what's left of the search result, if anything
            if(is.data.frame(search.result) & nrow(search.result)>0){
  
              this.sug.id=(max(code.suggested$suggestion.id)+1):(max(code.suggested$suggestion.id)+nrow(search.result))
              search.result$suggestion.id=this.sug.id
  
              code.suggested.update = data.frame(suggestion.id=this.sug.id,
                                              phrase.id=this.phrase$phrase.id,
                                              hs.code.6=search.result$hs.code,
                                              probability=NA,
                                              stringsAsFactors = F)
  
              gta_sql_append_table(append.table = "code.suggested",
                                   append.by.df = "code.suggested.update")
  
              # rm(code.suggested.update)
  
              # Get newly added suggestion.ids, because of primary key, they can differ from the ones in this environment
              sql <- "SELECT * FROM hs_code_suggested WHERE phrase_id = ?phraseID;"
              query <- sqlInterpolate(pool,
                                      sql,
                                      phraseID = this.phrase$phrase.id)
  
              new.codes=gta_sql_get_value(query)
              new.codes$hs.code.6 = as.numeric(new.codes$hs.code.6)
              new.codes = subset(new.codes, hs.code.6 %in% code.suggested.update$hs.code.6)
  
  
  
              ## Updating code.source
              suggestion.sources <- change_encoding(gta_sql_load_table("suggestion.sources"))
              suggestion.sources <<- suggestion.sources
  
              code.source.new=unique(cSplit(search.result[,c("suggestion.id","source.names","hs.code")], 2, direction="long",sep=";"))
              code.source.new <- merge(code.source.new[,c("source.names","hs.code")], new.codes[,c("hs.code.6","suggestion.id")], by.x="hs.code", by.y="hs.code.6")[,c("source.names","suggestion.id")]
              names(code.source.new)=c("source.name","suggestion.id")
              code.source.new=merge(code.source.new, suggestion.sources, by="source.name", all.x=T)
  
  
              ## in case there is an unknown source (could be a parsing error)
              if(nrow(subset(code.source.new, is.na(source.id)))>0){
                new.src=unique(subset(code.source.new, is.na(source.id))$source.name)
  
  
                suggestion.sources.update = data.frame(source.id=(max(suggestion.sources$source.id)+1):(max(suggestion.sources$source.id)+length(new.src)),
                                                    source.name=new.src,
                                                    stringsAsFactors = F)
  
                gta_sql_append_table(append.table = "suggestion.sources",
                                     append.by.df = "suggestion.sources.update")
  
                # suggestion.sources<<-suggestion.sources
  
                suggestion.sources <- change_encoding(gta_sql_load_table("suggestion.sources"))
                suggestion.sources <<- suggestion.sources
  
                new.src=subset(code.source.new, is.na(source.id))
                new.src$source.id=NULL
                new.src=merge(new.src, suggestion.sources, by="source.name", all.x=T)
  
                code.source.new=rbind(subset(code.source.new, is.na(source.id)==F),
                                      new.src)
  
              }
  
  
              code.source.update = code.source.new[,c("suggestion.id", "source.id")]
  
              gta_sql_append_table(append.table = "code.source",
                                   append.by.df = "code.source.update")
  
            }
  
          }
  
  
  
          ## updating phrase import status
  
  
          sql <- "UPDATE hs_phrases_to_import SET search_underway = 0, search_concluded = 1 WHERE phrase = ?forwhom;"
          query <- sqlInterpolate(pool,
                                  sql,
                                  forwhom = this.phrase$phrase)
          gta_sql_update_table(query)
  
  
        },
        error = function(error.msg) {
          if(error.message[1]==T){
            error.message <<- c(T, stop.print)
          } else {
            error.message <<- c(T,error.msg$message)
          }
          sql <- "UPDATE hs_phrases_to_import SET search_underway = 0, search_concluded = 0 WHERE phrase = ?forwhom;"
          query <- sqlInterpolate(pool,
                                  sql,
                                  forwhom = this.phrase$phrase)
          gta_sql_update_table(query)
          },
          warning = function(error.msg) {
            if(error.message[1]==T){
              error.message <<- c(T, stop.print)
            } else {
              error.message <<- c(T,error.msg$message)
            }
            sql <- "UPDATE hs_phrases_to_import SET search_underway = 0, search_concluded = 0 WHERE phrase = ?forwhom;"
            query <- sqlInterpolate(pool,
                                    sql,
                                    forwhom = this.phrase$phrase)
            gta_sql_update_table(query)
        })
  
  
        ## In case something went wrong
  
        if (error.message[1]) {
          # ERROR EMAIL
          sender = gta_pwd("mail")$mail
          recipients = c("patrick.buess@student.unisg.ch")
          sbjct=paste("[",this.job.name,"] HS search unsuccessful",sep="")
          message=paste0("Hello \n\n The HS search for '",this.phrase$phrase,"' in job '",this.job.name,"' ended with an error. The message is: \n\n",error.message[2],"\n\nRegards\nGTA data team")
  
  
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


      }




      ## Check whether a job is complete

      phrases.to.import <- change_encoding(gta_sql_load_table("phrases_to_import"))
      phrases.to.import <<- phrases.to.import

      job.import.complete=nrow(subset(phrases.to.import, job.id %in% this.phrase.jobs & search.concluded==F))==0

      if(job.import.complete){

        # SEND AVAILABILITY EMAIL TO USER
        sender = gta_pwd("mail")$mail
        recipients = this.job.email
        sbjct=paste("[",this.job.name,"] Import available in the app",sep="")
        message=paste0("Hello \n\nThank you for importing new terms. The job '",this.job.name,"' is now processed and the terms can be reviewed online. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGTA data team")


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


        # SEND AVAILABILITY EMAIL TO UPWORK
        sender = gta_pwd("mail")$mail
        sbjct=paste("GTA/UpWork HS code classification: App updated",sep="")

        nr.left=length(unique(subset(job.phrase, processed==F & job.id %in% subset(job.log, job.processed==F)$job.id)$phrase.id))
        message=paste0("Hello \n\nThank you for your patience. We have just updated the HS code app.\n\nThere are now ",nr.left," products awaiting classification.\n\nRegards\nJohannes\nhttp://hs.globaltradealert.org/")

        source("17 Shiny/5 HS code finder/setup/uw.R")

        if(nr.left>0){
          for(email.to in recipients){

            # send.mail(from = sender,
            #           to = email.to,
            #           subject=sbjct,
            #           body=message,
            #           html=F,
            #           smtp = list(host.name = gta_pwd("mail")$host,
            #                       port=gta_pwd("mail")$port,
            #                       user.name=sender,
            #                       passwd=gta_pwd("mail")$password,
            #                       tls=T),
            #           authenticate = T)
            }
          }
        }

      print(paste("Processed: ",this.phrase$phrase))
      }
  }
}

gta_sql_pool_close()

