gta_hs_check_job_completion <- function(processed.job=NULL){
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  
  setwd("/home/rstudio/Dropbox/GTA cloud")
  # gta_setwd()
  
  database = "ricardomain"
  gta_sql_pool_open(db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "hs_")
  
  
  if(is.null(processed.job)){
    stop("gta_hs_check_job_completion: No ID for the processed job is specified.")
  }
  
  
  # Check if job is fully processed 
  remaining.phrases=gta_sql_get_value(paste0("SELECT COUNT(DISTINCT phrase_id)
                                                       FROM hs_job_phrase
                                                       WHERE processed = FALSE 
                                                       AND job_id = ",processed.job,";"))
  
  if(remaining.phrases>0){
    
    sql <- "UPDATE hs_job_log SET phrases_remaining = ?left WHERE job_id = ?jobID;"
    querysql <- sqlInterpolate(pool, 
                               sql, 
                               left = remaining.phrases,
                               jobID = processed.job)
    
    gta_sql_update_table(querysql)
    
  } else {
    
    sql <- "UPDATE hs_job_log SET job_processed = true WHERE job_id = ?jobID;"
    querysql <- sqlInterpolate(pool, 
                               sql, 
                               jobID = processed.job)
    
    gta_sql_update_table(querysql)
    
    sql <- "UPDATE hs_job_log SET phrases_remaining = 0 WHERE job_id = ?jobID;"
    querysql <- sqlInterpolate(pool, 
                               sql, 
                               jobID = processed.job)
    
    gta_sql_update_table(querysql)
    
    job.id.future <- processed.job
    future({ gta_hs_process_completed_job(processed.job=job.id.future) }) %...>% {
      print("JOB PROCESSED")
    }
    
  }
}
