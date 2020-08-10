gta_hs_process_completed_job <- function(processed.job=NULL,
                                         open.pool=F){
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(gtabastiat)
  
  setwd("/home/rstudio/Dropbox/GTA cloud")
  # gta_setwd()
  
  result.path <- "17 Shiny/5 HS code finder/results/"
  
  
  if(open.pool){
    gta_sql_pool_open(db.title="ricardomainclone",
                      db.host = gta_pwd("ricardodev")[['host']],
                      db.name = 'ricardomainclone',
                      db.user = gta_pwd("ricardodev")[['user']],
                      db.password = gta_pwd("ricardodev")[['password']],
                      table.prefix = "hs_")
    
  }
  
  
  if(is.null(processed.job)){
    stop("gta_hs_process_completed_job: No ID for the processed job is specified.")
  }
  
  job.result=gta_hs_check_job_results(processed.job)$all
  job.result=subset(job.result, is.na(hs.code.6)==F)
  
  related.intervention=as.numeric(gta_sql_get_value(paste0("SELECT related_intervention FROM hs_job_log WHERE job_id =",processed.job)))
  
  
  
  
  if(is.na(related.intervention)){
    xlsx.name=paste0("HS classification result for job ",processed.job,".xlsx")
  }else{
    xlsx.name=paste0("HS classification result for intervention ",related.intervention," (job ",processed.job,").xlsx")
  }
  
  ## (1) create XLSX with cover sheet, sheet per outcome
  cover.stats=c("All HS codes with relevance probability exceeding 90%:",
                "All HS codes with relevance probability exceeding 80%:",
                "All HS codes with relevance probability exceeding 70%:",
                "All HS codes with relevance probability exceeding 60%:",
                "All HS codes with relevance probability exceeding 50%:",
                "",
                "Total number of phrases in this job: ",
                "Number of phrases processed successfully:",
                "Number of phrases deemed 'not a product':",
                "Number of phrases for which no HS codes where found:",
                "Number of phrases for which no agreement was reached:")
  cover.values=c(paste(unique(job.result$hs.code.6[job.result$probability>=.9 & is.na(job.result$probability)==F]),collapse=","),
                 paste(unique(job.result$hs.code.6[job.result$probability>=.8 & is.na(job.result$probability)==F]),collapse=","),
                 paste(unique(job.result$hs.code.6[job.result$probability>=.7 & is.na(job.result$probability)==F]),collapse=","),
                 paste(unique(job.result$hs.code.6[job.result$probability>=.6 & is.na(job.result$probability)==F]),collapse=","),
                 paste(unique(job.result$hs.code.6[job.result$probability>=.5 & is.na(job.result$probability)==F]),collapse=","),
                 "",
                 length(unique(job.result$phrase.id)),
                 length(unique(job.result$phrase.id[job.result$exit.status==2])),
                 length(unique(job.result$phrase.id[job.result$exit.status==3])),
                 length(unique(job.result$phrase.id[job.result$exit.status==4])),
                 length(unique(job.result$phrase.id[job.result$exit.status==5])))
  
  cover.sheet=data.frame(statistic=cover.stats,
                         value=cover.values,
                         stringsAsFactors = F)
  
  
  xlsx::write.xlsx(cover.sheet, file=paste0(result.path, xlsx.name), sheetName = "Result Summary", row.names = F, col.names = F)
  
  
  ## phrase-probability matrix
  
  
  matrix=data.frame()
  for(p.id in unique(subset(job.result, is.na(probability)==F)$phrase.id)){
    
    matrix=rbind(matrix,
                 data.frame(phrase.id=p.id,
                            phrase=unique(job.result$phrase[job.result$phrase.id==p.id]),
                            p.5=paste(subset(job.result, phrase.id==p.id & probability>=.5)$hs.code.6, collapse=","),
                            p.6=paste(subset(job.result, phrase.id==p.id & probability>=.6)$hs.code.6, collapse=","),
                            p.7=paste(subset(job.result, phrase.id==p.id & probability>=.7)$hs.code.6, collapse=","),
                            p.8=paste(subset(job.result, phrase.id==p.id & probability>=.8)$hs.code.6, collapse=","),
                            p.9=paste(subset(job.result, phrase.id==p.id & probability>=.9)$hs.code.6, collapse=","),
                            stringsAsFactors = F))
    
    
  }
  
  matrix[is.na(matrix)]="-"
  names(matrix)=c("phrase.id","phrase","probability >= 50%","probability >= 60%","probability >= 70%","probability >= 80%","probability >= 90%")
  
  if(nrow(matrix)>0){
    xlsx::write.xlsx(matrix, file=paste0(result.path, xlsx.name), sheetName = "Phrase-HS-Probability matrix", append = T, row.names = F)
  }
  
  # PER EXIT STATUS
  # 2 (PROCESSED) IF CODE SELECTED AND CODE SUGGESTED ARE AVAILABLE
  sheet1=subset(job.result, exit.status==2)
  sheet1$job.id=NULL
  sheet1$exit.status=NULL
  sheet1=subset(sheet1, is.na(probability)==F)
  
  sheet1 = sheet1[with(sheet1, order(phrase.id, -probability)),]
  
  if(nrow(sheet1)>0){
    xlsx::write.xlsx(sheet1, file=paste0(result.path, xlsx.name), sheetName = "Processed successfully", append = T, row.names = F)
  }
  
  # 3 (NOT A PRODUCT) IF MAJORITY OF CHECKS LABEL AS "NOT A PRODUCT"
  # 4 (NO CODES) IF CHECKED ENOUGH TIMES BUT NO CODES HAVE BEEN SELECTED FOR THIS PHRASE

  
  sheet2=subset(job.result, exit.status %in% c(3,4))
  sheet2$job.id=NULL
  sheet2 = sheet2[with(sheet2, order(exit.status, phrase.id)),]
  
  
  sheet2$exit.status[sheet2$exit.status==3]="Not recognised as a product"
  sheet2$exit.status[sheet2$exit.status==4]="No HS codes found for this product"
  
  sheet2=unique(sheet2[,c("phrase.id","phrase","exit.status")])
  
  if(nrow(sheet2)>0){
    xlsx::write.xlsx(sheet2, file=paste0(result.path, xlsx.name), sheetName = "No codes found", append = T, row.names = F)
  }  
  # 5 (ROUND LIMIT) IF NROUND >=4

  sheet3=subset(job.result, exit.status ==5)
  sheet3$job.id=NULL
  sheet3$exit.status=NULL
  sheet3 = sheet3[with(sheet3, order(phrase.id)),]
  
  if(nrow(sheet3)>0){
    xlsx::write.xlsx(sheet3, file=paste0(result.path, xlsx.name), sheetName = "No agreement", append = T, row.names = F)
  }
  
  

  ## (2) If there is no state_act_id or intervention_id, 
  ##     email it out:
  if(is.na(related.intervention)){
    
    
    recipients = gta_sql_get_value(paste0("SELECT user_email
                                           FROM gta_user_log
                                           WHERE user_id = (SELECT user_id FROM hs_job_log WHERE job_id =",processed.job,");"))
    if(is.na(recipients)){
      
      recipients="johannes.fritz@unisg.ch"
      
    }
    
    sender = gta_pwd("mail")$mail  
    sbjct=paste0("[HS app] Job #",processed.job,": Processing results.")
    message=paste0("Hello \n\nPlease find the results for your request attached to this message.\n\nIf you have any questions, please reply to this email. \n\nRegards\nGTA data team", sep="")
    attachment=paste0(result.path, xlsx.name)
    
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
      
    
  } else {
    ## (3) If there is a state_act_id or intervention_id, 
    ##     attach to that state act, 
    ##     leave a comment and 
    ##     change status to "under review" if not there or in progress already.
    
    remote.file=gta_upload_to_aws(upload.file.name = xlsx.name,
                                  upload.file.path = result.path)
    
    gta_sql_pool_open(pool.name = "hs.connect",
                      db.title="gtamain",
                      db.host = gta_pwd("gtamain")$host,
                      db.name = gta_pwd("gtamain")$name,
                      db.user = gta_pwd("gtamain")$user,
                      db.password = gta_pwd("gtamain")$password,
                      table.prefix = "gta_")
    
    related.sa=gta_sql_get_value(paste0("SELECT measure_id FROM gta_intervention WHERE id=",related.intervention,";"),
                                 db.connection = "hs.connect")
    
    if(is.na(related.sa)){
      
      
      recipients = gta_sql_get_value(paste0("SELECT user_email
                                           FROM gta_user_log
                                           WHERE user_id = (SELECT user_id FROM hs_job_log WHERE job_id =",processed.job,");"))
      if(is.na(recipients)){
        
        recipients="johannes.fritz@unisg.ch"
        
      }
      
      sender = gta_pwd("mail")$mail  
      sbjct=paste0("[HS app] Job #",processed.job,": Processing results.")
      message=paste0("Hello \n\nPlease find the results for your request attached to this message.\n\nIf you have any questions, please reply to this email. \n\nRegards\nGTA data team", sep="")
      attachment=paste0(result.path, xlsx.name)
      
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
      
      stop("Cannot find the state act.")
    } else {
     
    
    ## attach file to state act sources
    gta_sql_update_table(paste0("INSERT INTO gta_files (field_id, field_type, file_url, file_name, is_deleted)
                                 VALUES (",related.sa,",'measure','",remote.file,"','",xlsx.name,"',0)"),
                         db.connection = "hs.connect")
      
      
    ## leave a comment that you did.
      author.name=gta_sql_get_value(paste0("SELECT f_name FROM fw_users WHERE id = (SELECT author_id FROM gta_measure WHERE id =",related.sa,");"),
                                    db.connection = "hs.connect")
      
      comment=paste0("Hi ",author.name,"\n\nThe product names have been processed by the HS code app. You can find the results in the file --",xlsx.name,"-- attached to the sources.\n\nHope this helps.")
      
      gta_sql_update_table(paste0("INSERT INTO gta_comment (author_id, measure_id, comment_value, creation_time)
                                 VALUES (52,",related.sa,",'",comment,"', CURRENT_TIMESTAMP)"),
                           db.connection = "hs.connect") 
         
      } 
    
    # put back into review IF IN A PROCESSING STATE
    ## TBA
    
    gta_sql_pool_close("hs.connect")
    
  }
  
  
 
  
  
  
  if(open.pool){
    gta_sql_pool_close()
    gta_sql_kill_connections()
    
    }
}
