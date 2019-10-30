gta_hs_process_completed_job <- function(processed.job=NULL, path = NULL){
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  
  setwd("/home/rstudio/Dropbox/GTA cloud")
  
  gta_sql_pool_open(db.title="ricardomain",
                    db.host = gta_pwd("ricardomain")$host,
                    db.name = gta_pwd("ricardomain")$name,
                    db.user = gta_pwd("ricardomain")$user,
                    db.password = gta_pwd("ricardomain")$password,
                    table.prefix = "hs_")
  
  
  if(is.null(processed.job)){
    stop("gta_hs_process_completed_job: No ID for the processed job is specified.")
  }
  
  if(is.null(path)){
    stop("Please specify a path to the database.")
  }
  
  ## classifying results
  result.path=paste(path,"results/Job ",processed.job," - Classification result", sep="")
  
  # load_all(path)
  job.log <- change_encoding(gta_sql_load_table("job_log"))
  job.log <<- job.log
  users <- change_encoding(gta_sql_load_table("user_log",table.prefix = "gta_"))
  users <<- users
  
  for(j.id in processed.job){
    if(job.log$nr.of.checks[job.log$job.id==j.id]>1){
      
      
      classifier.variables=gta_hs_create_classifier_variables(job.ids=j.id)
      classifier.variables<<-classifier.variables
      classification.result=gta_hs_classify_results("classifier.variables")
      
      
      if(length(processed.job)==1){
        classification.result$job.id=j.id
        classification.result=unique(classification.result)
      }
      save(classification.result, file=paste(result.path, ".Rdata", sep=""))
      
      ## XLSX with one sheet for those phrase with zero results, and those with 1 or more results.
      xlsx::write.xlsx(subset(classification.result, relevant==1)[,c("job.id","phrase","hs.code.6")], file=paste(result.path, ".xlsx", sep=""), sheetName = "Classified phrases", row.names=F)
      
      none.found=subset(classification.result, ! phrase %in% subset(classification.result, relevant==1)$phrase)
      if(nrow(none.found)>0){
        xlsx::write.xlsx(unique(none.found[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Unclassified phrases", row.names=F, append=T)
      }
      
      round.limit=subset(classification.result, phrase %in% subset(classification.result, exit.status==4)$phrase)
      if(nrow(round.limit)>0){
        xlsx::write.xlsx(unique(round.limit[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Round limit reached", row.names=F, append=T)
      }
      
      not.product=subset(classification.result, phrase %in% subset(classification.result, exit.status==2)$phrase)
      if(nrow(not.product)>0){
        xlsx::write.xlsx(unique(not.product[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Not a product", row.names=F, append=T)
      }
      
      
      ## emailing it out
      recipient=users$email[users$user.id==job.log$user.id[job.log$job.id==j.id]]
      
      if(is.na(recipient)){
        recipient="fritz.johannes@gmail.com"
      }
      
      sender = gta_pwd("mail")$mail  
      sbjct=paste("[HS-app ticket #",j.id,"; ",job.log$job.name[job.log$job.id == j.id],"] Processing finished", sep="")
      
      if(nrow(none.found)==0){
        review.results="Our colleagues found at least one HS code for all submitted phrases."
      } else {
        review.results=paste("Our colleagues found at least one HS code for ",
                             length(unique(subset(classification.result, relevant==1)$phrase)),
                             " of the submitted phrases.\nHowever, they could not classify ",
                             length(unique(none.found$phrase)),
                             " phrases. You may want to re-submit the 'Unclassified' sheet in the attached XLSX for another check.",
                             sep="")
      }
      
      related.sa=job.log$related.state.act[job.log$job.id == j.id]
      
      if(is.na(related.sa)){
        mention.sa=""
      } else {
        mention.sa=paste("The related state act is: ",related.sa,".", sep="")
      }
      
      message=paste0("Hello \n\nThe job '",
                     job.log$job.name[job.log$job.id == j.id],
                     "' is now fully reviewed.",
                     mention.sa,
                     "\n\n",
                     review.results,
                     "\n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGlobal Trade Alert Data")
      
      library(mailR)
      send.mail(from = sender,
                to = recipient,
                subject=sbjct,
                body=message,
                html=F,
                attach.files = paste(result.path, c(".xlsx"),sep=""),
                smtp = list(host.name = gta_pwd("mail")$host ,
                            port=gta_pwd("mail")$port,
                            user.name=sender, 
                            passwd=gta_pwd("mail")$password,
                            tls=T),
                authenticate = T)
      
      rm(recipient, message, sbjct, sender, classifier.variables,classification.result)
      
    } else {
      
      ## what to do with single-check jobs?
      
      # load_all(path)
      # 
      # code.suggested=rbind(subset(code.suggested, (! suggestion.id %in% estimation.set$suggestion.id)),
      #                      unique(estimation.set[,c(names(code.suggested))]))
      # 
      # assign.global("code.suggested",code.suggested)
      # save_all(path)
      
    }
  }
  
  
}
