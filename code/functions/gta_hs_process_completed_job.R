gta_hs_process_completed_job <- function(processed.job=NULL,
                                         open.pool=F){
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  
  setwd("/home/rstudio/Dropbox/GTA cloud")
  # gta_setwd()
  
  
  if(open.pool){
    gta_sql_kill_connections()
    database = "ricardomain"
    gta_sql_pool_open(db.title=database,
                      db.host = gta_pwd(database)$host,
                      db.name = gta_pwd(database)$name,
                      db.user = gta_pwd(database)$user,
                      db.password = gta_pwd(database)$password,
                      table.prefix = "hs_")
    
  }
  
  
  if(is.null(processed.job)){
    stop("gta_hs_process_completed_job: No ID for the processed job is specified.")
  }
  
  
  ## What needs to happen in this code
  ## You want to communicate the result the job result to the user. 
  ## Right now this goes via XLSX & email, but keep in mind this moves to the RICARDO dashboard eventually. Think a bit about how this is communicated on that dashboard. 
  ## My hunch is in tabs equivalent to the XLSX sheets, so you don't need to worry too much about code structure now. 
  ## Maybe divide the code into producing the sheets first, then storing them at the end of the code/separate function. Then edit section/function eventually to switch from XLSX & email to RICARD dashboard.
  
  ## The user needs to see
  ## (1) The phrase itself plus the matched HS codes, one sheet per exit status.
  ## (optional) a cover sheet giving the number of phrases by exit status, plus one CSV-chain for all unique matched HS codes across all phrases in this job.
  
 
  ## This is legacy code that seems to have been used for this sofar:
  # 
  # ## XLSX with one sheet for those phrase with zero results, and those with 1 or more results.
  # xlsx::write.xlsx(subset(classification.result, relevant==1)[,c("job.id","phrase","hs.code.6")], file=paste(result.path, ".xlsx", sep=""), sheetName = "Classified phrases", row.names=F)
  # 
  # none.found=subset(classification.result, ! phrase %in% subset(classification.result, relevant==1)$phrase)
  # if(nrow(none.found)>0){
  #   xlsx::write.xlsx(unique(none.found[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Unclassified phrases", row.names=F, append=T)
  # }
  # 
  # round.limit=subset(classification.result, phrase %in% subset(classification.result, exit.status==4)$phrase)
  # if(nrow(round.limit)>0){
  #   xlsx::write.xlsx(unique(round.limit[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Round limit reached", row.names=F, append=T)
  # }
  # 
  # not.product=subset(classification.result, phrase %in% subset(classification.result, exit.status==2)$phrase)
  # if(nrow(not.product)>0){
  #   xlsx::write.xlsx(unique(not.product[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Not a product", row.names=F, append=T)
  # }
  # 
  # 
  # ## emailing it out
  # recipient <- gta_sql_get_value(paste0("SELECT user_email from gta_user_log WHERE user_id ='",user$id,"';"))
  # 
  # 
  # if(is.na(recipient)){
  #   recipient="fritz.johannes@gmail.com"
  # }
  # 
  # sender = gta_pwd("mail")$mail  
  # sbjct=paste("[HS-app ticket #",j.id,"; ",job.log$job.name[job.log$job.id == j.id],"] Processing finished", sep="")
  # 
  # if(nrow(none.found)==0){
  #   review.results="Our colleagues found at least one HS code for all submitted phrases."
  # } else {
  #   review.results=paste("Our colleagues found at least one HS code for ",
  #                        length(unique(subset(classification.result, relevant==1)$phrase)),
  #                        " of the submitted phrases.\nHowever, they could not classify ",
  #                        length(unique(none.found$phrase)),
  #                        " phrases. You may want to re-submit the 'Unclassified' sheet in the attached XLSX for another check.",
  #                        sep="")
  # }
  # 
  # related.sa=job.log$related.state.act[job.log$job.id == j.id]
  # 
  # if(is.na(related.sa)){
  #   mention.sa=""
  # } else {
  #   mention.sa=paste("The related state act is: ",related.sa,".", sep="")
  # }
  # 
  # message=paste0("Hello \n\nThe job '",
  #                job.log$job.name[job.log$job.id == j.id],
  #                "' is now fully reviewed.",
  #                mention.sa,
  #                "\n\n",
  #                review.results,
  #                "\n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGlobal Trade Alert Data")
  # 
  # library(mailR)
  # send.mail(from = sender,
  #           to = recipient,
  #           subject=sbjct,
  #           body=message,
  #           html=F,
  #           attach.files = paste(result.path, c(".xlsx"),sep=""),
  #           smtp = list(host.name = gta_pwd("mail")$host ,
  #                       port=gta_pwd("mail")$port,
  #                       user.name=sender, 
  #                       passwd=gta_pwd("mail")$password,
  #                       tls=T),
  #           authenticate = T)
  # 
  # rm(recipient, message, sbjct, sender, classifier.variables,classification.result)
  # 
  # 
  
  if(open.pool){gta_sql_pool_close()}
}
