gta_hs_process_completed_job<- function(processed.job=NULL){
 
  library(gtalibrary)
  
  if(is.null(processed.job)){
    
    stop("gta_hs_process_completed_job: No ID for the processed job is specified.")
  }
  
  ## classifying results
  result.path=paste("17 Shiny/5 HS code finder/results/Job ",processed.job," - Classification result", sep="")
  
  load_all(path)
  
  if(job.log$nr.of.checks[job.log$job.id==processed.job]>1){
    
    
    classifier.variables=gta_hs_create_classifier_variables(job.ids=processed.job)
    classifier.variables<<-classifier.variables
    classification.result=gta_hs_classify_results("classifier.variables")
    save(classification.result, file=paste(result.path, ".Rdata", sep=""))
    
    ## XLSX with one sheet for those phrase with zero results, and those with 1 or more results.
    xlsx::write.xlsx(subset(classification.result, relevant==1)[,c("job.id","phrase","hs.code.6")], file=paste(result.path, ".xlsx", sep=""), sheetName = "Classified phrases", row.names=F)
    
    none.found=subset(classification.result, ! phrase %in% subset(classification.result, relevant==1)$phrase)
    if(nrow(none.found)>0){
      xlsx::write.xlsx(unique(none.found[,c("phrase")]), file=paste(result.path, ".xlsx", sep=""), sheetName = "Unclassified phrases", row.names=F, append=T)
    }
    
    
    ## emailing it out
    recipient=users$email[users$user.id==job.log$user.id[job.log$job.id==processed.job]]
    
    if(is.na(recipient)){
      recipient="fritz.johannes@gmail.com"
    }
    
    sender = "data@globaltradealert.org"  
    sbjct=paste("[HS-app ticket #",processed.job,"; ",job.log$job.name[job.log$job.id == processed.job],"] Processing finished", sep="")
    
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
    
    related.sa=job.log$related.state.act[job.log$job.id == processed.job]
    
    if(is.na(related.sa)){
      mention.sa=""
    } else {
      mention.sa=paste("The related state act is: ",related.sa,".", sep="")
    }
    
    message=paste0("Hello \n\nThe job '",
                   job.log$job.name[job.log$job.id == processed.job],
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
              smtp = list(host.name = "mail.infomaniak.com",
                          port=587,
                          user.name=sender, 
                          passwd="B0d@nstrasse",
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
