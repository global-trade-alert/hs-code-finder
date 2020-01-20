gta_hs_check_job_results <- function(job.ids=NULL,
                                     prob.threshold = 0,
                                     prob.is.na = TRUE,
                                     prob.return = TRUE) {

  results <- list(all=data.frame())
  
  for (j.id in job.ids) {
  
    query <- paste0("SELECT jp.job_id, cs.hs_code_6, hd.hs_description_4, hd.hs_description_6, jp.phrase_id, pl.phrase",if(prob.return){", cs.probability"},", pl.exit_status
                    FROM hs_job_phrase jp 
                    JOIN hs_phrase_log pl
                    ON pl.phrase_id = jp.phrase_id
                    JOIN hs_code_suggested cs
                    ON cs.phrase_id = jp.phrase_id
                    LEFT JOIN hs_codes_app happ
                    ON cs.hs_code_6=happ.hs_code_6
                    LEFT JOIN hs_descriptions hd
                    ON happ.hs_id = hd.hs_id                    
                    WHERE (jp.job_id = ",j.id,")",if(prob.return){paste0(" AND (cs.probability >= ",prob.threshold, " ", if(prob.is.na){"OR cs.probability IS"}else{"AND cs.probability IS NOT"}," NULL)")},";")
    
    result = gta_sql_get_value(query)
    
    # Add to overall results set
    results$all <- rbind(results$all, result)
    
    # 1 (UNPROCESSED) NOT ENOUGH ENTRIES TO PROCESS PHRASE
    # 2 (PROCESSED) IF CODE SELECTED AND CODE SUGGESTED ARE AVAILABLE
    # 3 (NOT A PRODUCT) IF MAJORITY OF CHECKS LABEL AS "NOT A PRODUCT"
    # 4 (NO CODES) IF CHECKED ENOUGH TIMES BUT NO CODES HAVE BEEN SELECTED FOR THIS PHRASE
    # 5 (ROUND LIMIT) IF NROUND >=4
    
    # create exit status tables
    exit.status <- c("unfinished",
                     "processed",
                     "service",
                     "no.codes",
                     "round.limit")
    
    exit.results <- list()
    exit.results <- list("all" = result)

    for (es in 1:5) {
      eval(parse(text=paste0("exit.results <- c(exit.results, list(",paste0(exit.status[es]),"= subset(result, exit.status == es)[,c('job.id','hs.code.6','phrase.id','phrase'",if(prob.return){",'probability'"},")]))")))
    }
    
    eval(parse(text=paste0("results <- c(results, list('job.",j.id,"' = exit.results))")))
    
  }
  
  return(results)
}