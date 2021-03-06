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
library(webdriver)
library(XML)
library(gtabastiat)

rm(list = ls())

# gta_setwd()
setwd("/home/rstudio/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

wdpath="17 Shiny/5 HS code finder/"
# wdpath="0 dev/hs-code-finder-pb/"

gta_sql_pool_open(db.title="ricardomain",
                  db.host = gta_pwd("ricardomain")[['host']],
                  db.name = gta_pwd("ricardomain")[['name']],
                  db.user = gta_pwd("ricardomain")[['user']],
                  db.password = gta_pwd("ricardomain")[['password']],
                  table.prefix = "hs_")

## check if a process is running on the server
search.time.allowance=5
nr.parallel.processes=20
running.processes=system("ps aux", intern=T)

hs.search.busy=sum(as.numeric(grepl("(hs-search.R)",running.processes, ignore.case = T)))


## setup
for(fct in list.files(paste0(wdpath,"/code/functions"), pattern = ".R", full.names=T)){
  source(fct)
}

phrases.to.import <- gta_sql_load_table("phrases_to_import")
phrases.to.import <<- phrases.to.import

# stop("JOHANNES is working")

if(hs.search.busy>=nr.parallel.processes){

  ## looking for broken searches
  run.not.finished=subset(phrases.to.import, (search.underway==T & search.concluded==F & difftime(Sys.time(), as.POSIXct(phrases.to.import$run.time[1], origin="1970-01-01"), units="mins")>search.time.allowance & is.na(run.time)==F))
  
  if(nrow(run.not.finished)>0){
    
    for(i in 1:nrow(run.not.finished)){
      # run.not.finished$search.underway=F
      sql <- "UPDATE hs_phrases_to_import SET search_underway = 0 WHERE phrase = ?forwhom;"
      query <- sqlInterpolate(pool,
                              sql,
                              forwhom = run.not.finished$phrase[i])
      
      gta_sql_update_table(query)

    }
    ## abort stuck process
    process.to.kill=as.numeric(gsub("\\D","",str_extract(running.processes[grepl("hs-search.R", running.processes)][1], "^rstudio +?\\d+")))
    system(paste("kill -9", process.to.kill), intern=T)
    

    rm(run.not.finished)
    
    # phrases.to.import$search.underway[]=F
    sql <- "UPDATE hs_phrases_to_import SET search_underway = 0"
    query <- sqlInterpolate(pool,
                            sql)
    
    gta_sql_update_table(query)
    
    print(paste(Sys.time(), ": HS searches aborted"))
    
  } else {
    
    print(paste(Sys.time(), ": HS Search is busy"))
    print(running.processes[grepl("(hs-search.R)",running.processes, ignore.case = T)])
    
  }
  
} else {

  ## updating phrases to import
  search.phrases=unique(subset(phrases.to.import, search.underway==F & search.concluded==F)$phrase)


  if(length(search.phrases)==0){
    print(paste(Sys.time(), ": no business", sep=""))

  }else{

    ## Search for the non-imported phrases, one-by-one
    while(length(search.phrases)>0){
      ## updating phrases to import

      phrases.to.import <- gta_sql_load_table("phrases_to_import")
      phrases.to.import <<- phrases.to.import
      job.log <- gta_sql_load_table("job_log")
      job.log <<- job.log
      users <- gta_sql_load_table("user_log", table.prefix = "gta_")
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
        
        pjs <- run_phantomjs()
        remDr<<-Session$new(port=pjs$port)
        
        
  
        search.result=data.frame()
        wait.time=15
        
        
        print(this.phrase)
        
        attempt = pti.nr.attemps
        while(nrow(search.result)==0 & attempt<=2){
          attempt=attempt+1
          
          print(paste("ROUND", attempt))
          
          
          
          ### search code, horror show
          
          print("Checking Eurostat ...")
          
          remDr$go("https://eurostat.prod.3ceonline.com")
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          e=remDr$findElement(xpath="//textarea[@id='ccce-queryBox']")
          e$sendKeys(as.character(this.phrase))
          e$sendKeys('\ue007')
          
          b_load_site(xpath="//h2[contains(text(),'assumed characteristics')]",
                      wait=wait.time)
          
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          if(length(xpathSApply(html, "//div[@id='hscode']",xmlValue))>0){
            
            search.result=rbind(search.result,
                                data.frame(product.name=this.phrase,
                                           hs.code=as.character(paste(unlist(str_extract_all(xpathSApply(html, "//div[@id='hscode']",xmlValue), "\\d+")),collapse="")),
                                           hs.order=1,
                                           source="Eurostat",
                                           stringsAsFactors = F)
            )
          }
          
          rm(html)
          
          
          
          # 
          # print("Checking Eximguru ...")
          # 
          # remDr$go("http://www.eximguru.com/hs-codes/default.aspx")
          # html <- htmlParse(remDr$getSource()[[1]], asText=T)
          # 
          # e=remDr$findElement(css="#uxContentPlaceHolder_ucSearchBox1_ContentPanel1_uxValueToSearchTextBox")
          # e$sendKeys(as.character(this.phrase))
          # e$sendKeys('\ue007')
          # 
          # guru.path="//div[@class='Search']/descendant::table/descendant::tr/td[1]/a"
          # 
          # 
          # b_load_site(xpath=guru.path,
          #             wait=wait.time)
          # 
          # html <- htmlParse(remDr$getSource()[[1]], asText=T)
          # 
          # if(length(xpathSApply(html, guru.path,xmlValue))>0){
          #   
          #   guru.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, guru.path,xmlValue),"\\d+")),1,6))
          #   guru.hs=guru.hs[nchar(guru.hs)>=4]
          #   
          #   if(length(guru.hs)>0){
          #     
          #     search.result=rbind(search.result,
          #                         data.frame(product.name=this.phrase,
          #                                    hs.code=guru.hs,
          #                                    hs.order=1:length(guru.hs),
          #                                    source="Eximguru",
          #                                    stringsAsFactors = F)
          #     )
          #     
          #     
          #   }
          #   
          #   rm(guru.hs, html)
          #   
          # }
          # 
          # 
          
          
          print("Checking EU customs ...")
          
          remDr$go(paste("https://www.tariffnumber.com/2013/", gsub(" ","%20", this.phrase),sep=""))
          
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          if(length(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))>=6])>0){
            
            customs.found=unique(substr(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))>=6],1,6))
            
            search.result=rbind(search.result,
                                data.frame(product.name=this.phrase,
                                           hs.code=customs.found,
                                           hs.order=1:length(customs.found),
                                           source="EU Customs (2013 edition)",
                                           stringsAsFactors = F)
            )
            
            rm(customs.found, html)
          }
          
          
          
          print("Checking Google ...")
          
          remDr$go(paste("https://google.com/search?q=", paste("HS+Code",gsub(" ","+", this.phrase), sep="+"),sep=""))
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          google.txt=tolower(paste(xpathSApply(html, "//*[descendant-or-self::text()]", xmlValue), collapse=" "))
          
          google.hs=unique(substr(unlist(str_extract_all(str_extract_all(google.txt, "(hs ?)?codes? \\d+"), "\\d+")), 1,6))
          google.hs=google.hs[nchar(google.hs)>=4]
          
          if(sum(as.numeric(google.hs!=0))>0){
            
            search.result=rbind(search.result,
                                data.frame(product.name=this.phrase,
                                           hs.code=google.hs,
                                           hs.order=1:length(google.hs),
                                           source="Google",
                                           stringsAsFactors = F)
            )
            
          }
          
          rm(google.txt, google.hs, html)
          
          
          
          print("Checking Zauba ...")
          
          remDr$go(paste("https://www.zauba.com/USA-htscodes/", paste(gsub(" ","+", this.phrase), sep="-"),sep=""))
          
          zauba.path="//table/descendant::td/descendant::a"
          
          b_load_site(xpath=zauba.path,
                      wait=wait.time)
          
          
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          if(length(xpathSApply(html, zauba.path, xmlValue))>0){
            
            
            zauba.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, zauba.path, xmlValue), "\\d+")),1,6))
            zauba.hs=zauba.hs[nchar(zauba.hs)>=4]
            
            if(length(zauba.hs)>0){
              
              search.result=rbind(search.result,
                                  data.frame(product.name=this.phrase,
                                             hs.code=zauba.hs,
                                             hs.order=1:length(zauba.hs),
                                             source="Zauba",
                                             stringsAsFactors = F)
              )
              
              
            }
            rm(zauba.hs)
            
          }
          
          rm(html)
          
          
          
          
          print("Checking E-to-China ...")
          
          remDr$go(paste("http://hs.e-to-china.com/ks-", paste(gsub(" ","+", this.phrase), sep="+"),"-d_3-t_1.html",sep=""))
          
          etc.path="//a[@class='hs_tree']"
          
          b_load_site(xpath=etc.path,
                      wait=wait.time)
          
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          if(length(xpathSApply(html, etc.path, xmlGetAttr, "name"))>0){
            
            etc.hs=unique(substr(xpathSApply(html, etc.path, xmlGetAttr, "name"), 1,6))
            etc.hs=etc.hs[nchar(etc.hs)>=4]
            
            if(length(etc.hs)>0){
              
              search.result=rbind(search.result,
                                  data.frame(product.name=this.phrase,
                                             hs.code=etc.hs,
                                             hs.order=1:length(etc.hs),
                                             source="E-to-China",
                                             stringsAsFactors = F)
              )
              
              
            }
            
            rm(etc.hs)
            
          }
          
          
          rm(html)
          
          
          
          
          
          
          print("Checking HSbianma ...")
          
          remDr$go(paste("https://hsbianma.com/Search?keywords=",gsub(" ","%20",this.phrase),"&filterFailureCode=true", sep=""))
          
          bianma.path="//table[@class='result']//descendant::tr/td[1]/a"
          
          b_load_site(xpath=bianma.path,
                      wait=wait.time)
          
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          if(length(xpathSApply(html, bianma.path, xmlValue))>0){
            
            bianma.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, bianma.path, xmlValue),"\\d+")),1,6))
            bianma.hs=bianma.hs[nchar(bianma.hs)>=4]
            
            if(length(bianma.hs)>0){
              
              search.result=rbind(search.result,
                                  data.frame(product.name=this.phrase,
                                             hs.code=bianma.hs,
                                             hs.order=1:length(bianma.hs),
                                             source="HSbianma",
                                             stringsAsFactors = F)
              )
              
              
            }
            
            rm(bianma.hs)
            
          }
          rm(html)
          
          
          
          
          
          
          
          print("Checking Cybex ...")
          
          remDr$go(paste("http://www.cybex.in/HS-Codes/of-",gsub(" ","-",this.phrase),".aspx", sep=""))
          
          cybex.path="//span[contains(text(),'Hs Code')]/following-sibling::a[1]"
          
          b_load_site(xpath=cybex.path,
                      wait=wait.time)
          
          html <- htmlParse(remDr$getSource()[[1]], asText=T)
          
          if(length(xpathSApply(html, cybex.path, xmlValue))>0){
            
            cybex.hs=unique(substr(unlist(str_extract_all(xpathSApply(html, cybex.path, xmlValue),"\\d+")),1,6))
            cybex.hs=cybex.hs[nchar(cybex.hs)>=4]
            
            if(length(cybex.hs)>0){
              
              search.result=rbind(search.result,
                                  data.frame(product.name=this.phrase,
                                             hs.code=cybex.hs,
                                             hs.order=1:length(cybex.hs),
                                             source="Cybex",
                                             stringsAsFactors = F)
              )
              
              
            }
            
            rm(cybex.hs, html)
            
          }
          
          
          
          
          
        }
        
        
        ## formatting the findings
        if (nrow(search.result)>0) {
          ## expanding HS codes
          if(nrow(subset(search.result, nchar(hs.code)<=4))>0){
            
            short.hs=subset(search.result, nchar(hs.code)<=4)
            
            for(i in 1:nrow(short.hs)){
              hs=gta_hs_code_check(as.integer(short.hs$hs.code[i]))
              
              if(is.null(hs)==F){
                short.hs$hs.code[i]=paste(gta_hs_code_check(as.integer(short.hs$hs.code[i])), collapse=";")
              }else{
                short.hs$hs.code[i]=999999
              }
            }
            short.hs=cSplit(short.hs, which(names(short.hs)=="hs.code"), sep=";", direction="long")
            short.hs=subset(short.hs, hs.code!=999999)
            
            search.result=rbind(subset(search.result, nchar(hs.code)>4), short.hs)
            
          }
        }
          
          
        if (nrow(search.result)>0){
          nr.hits=aggregate(source ~ product.name + hs.code, search.result, function(x) length(unique(x)))
          names(nr.hits)=c("product.name","hs.code","nr.sources")
          
          search.result=merge(nr.hits, aggregate(source ~ product.name + hs.code, search.result, function(x) paste(unique(x), collapse="; ")), by=c("product.name","hs.code"))
          names(search.result)=c("product.name","hs.code","nr.sources", "source.names")
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
          
          gta_sql_update_table(paste0("UPDATE hs_job_log SET job_processed = 0 WHERE job_id IN (",paste0(c(this.phrase.jobs), collapse=', '),");"))
          
          ## check whether suggestions have been made in an earlier job for this phrase
          
          code.suggested <- gta_sql_load_table("code_suggested")
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
            suggestion.sources <- gta_sql_load_table("suggestion.sources")
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
              
              suggestion.sources <- gta_sql_load_table("suggestion.sources")
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
  
        remDr$delete()

      }

  
      
      # Check whether all phrases are new phrases are imported correctly
      # If some phrases are not imported correcty (process broke down) delete unfinished data and mark phrase as needing to be imported again
      
      phrases.log <- gta_sql_get_value("SELECT * FROM hs_phrase_log WHERE phrase_id >= 3818 AND phrase_id NOT IN (SELECT phrase_id FROM hs_job_phrase);")
      
      if (nrow(phrases.log)>0) {
        gta_sql_multiple_queries("DELETE FROM hs_code_source WHERE suggestion_id IN (SELECT suggestion_id FROM hs_code_suggested WHERE phrase_id IN (SELECT phrase_id FROM hs_phrase_log WHERE phrase_id >= 3818 AND phrase_id NOT IN (SELECT phrase_id FROM hs_job_phrase)));
                                 DELETE FROM hs_code_suggested WHERE phrase_id IN (SELECT phrase_id FROM hs_phrase_log WHERE phrase_id >= 3818 AND phrase_id NOT IN (SELECT phrase_id FROM hs_job_phrase));
                                 UPDATE hs_phrases_to_import SET search_concluded = 0, nr_attempts = 0, run_time = NULL WHERE LCASE(phrase) IN (SELECT phrase FROM hs_phrase_log WHERE phrase_id >= 3818 AND phrase_id NOT IN (SELECT phrase_id FROM hs_job_phrase));
                                 DELETE FROM hs_phrase_log WHERE phrase_id >= 3818 AND phrase_id NOT IN (SELECT phrase_id FROM hs_job_phrase);")
      } else {
      
        ## Check whether a job is complete
  
        phrases.to.import <- gta_sql_load_table("phrases_to_import")
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
      }

      print(paste("Processed: ",this.phrase$phrase))
      
      }
  }
}

gta_sql_pool_close()

