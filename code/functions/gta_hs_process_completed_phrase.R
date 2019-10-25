gta_hs_process_completed_phrase <- function(processed.phrase=NULL, path = NULL){
  
  
  library(gtalibrary)
  
  if(is.null(processed.phrase)){
    stop("gta_hs_process_completed_job: No ID for the processed phrase is specified.")
  }
  
  if(is.null(path)){
    stop("Please specify a path to the database.")
  }
  
  # load_all(path)
  job.log <- change_encoding(gta_sql_load_table("job_log"))
  job.log <<- job.log
  users <- change_encoding(gta_sql_load_table("user_log",table.prefix = "gta_"))
  users <<- users
  

  classifier.variables=gta_hs_create_classifier_variables_phrase(phrase.ids=processed.phrase)
  classifier.variables<<-classifier.variables
  classification.result=gta_hs_classify_results("classifier.variables")
  
  if(length(processed.phrase)==1){
    classification.result$job.id=j.id
    classification.result=unique(classification.result)
  } 
  
}
  
