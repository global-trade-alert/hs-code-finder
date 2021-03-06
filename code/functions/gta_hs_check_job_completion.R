gta_hs_check_job_completion <- function(processed.job=NULL, 
                                        open.pool=F){
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  
  setwd("/home/rstudio/Dropbox/GTA cloud")
  # gta_setwd()
  
  if(open.pool){
    
    gta_sql_pool_open(db.title="ricardomain",
                  db.host = gta_pwd("ricardomain")[['host']],
                  db.name = gta_pwd("ricardomain")[['name']],
                  db.user = gta_pwd("ricardomain")[['user']],
                  db.password = gta_pwd("ricardomain")[['password']],
                  table.prefix = "hs_")
    
  }
  
  
  if(is.null(processed.job)){
    stop("gta_hs_check_job_completion: No ID for the processed job is specified.")
  }
  
  
  # Check if job is fully processed 
  remaining.phrases=gta_sql_get_value(paste0("SELECT COUNT(DISTINCT phrase_id)
                                                       FROM hs_job_phrase
                                                       WHERE processed = FALSE 
                                                       AND job_id = ",processed.job,";"))
  

  sql <- "UPDATE hs_job_log SET phrases_remaining = ?left WHERE job_id = ?jobID;"
  querysql <- sqlInterpolate(pool, 
                             sql, 
                             left = remaining.phrases,
                             jobID = processed.job)
  
  gta_sql_update_table(querysql)
  
 if(remaining.phrases==0){
    
    
    sql <- "UPDATE hs_job_log SET job_processed = true WHERE job_id = ?jobID;"
    querysql <- sqlInterpolate(pool, 
                               sql, 
                               jobID = processed.job)
    
    gta_sql_update_table(querysql)
    
    job.id.future <- processed.job
    future({ gta_hs_process_completed_job(processed.job=job.id.future,
                                          open.pool=T) }) %...>% {
    print("JOB PROCESSED")
    }
    
  }
  
  if(open.pool){
    gta_sql_pool_close()
    gta_sql_kill_connections()
    }
  
}
