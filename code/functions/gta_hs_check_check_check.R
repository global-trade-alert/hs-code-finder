gta_hs_check_check_check <- function(check.ids=NULL) {
  
  
  
  query=paste0("SELECT cp.check_id, cp.phrase_id, phrase, csug.hs_code_6, hs_description_4, hs_description_6
                FROM hs_check_phrases cp
                JOIN hs_phrase_log pl
                ON pl.phrase_id=cp.phrase_id
                JOIN hs_code_selected csel
                ON cp.check_id=csel.check_id
                JOIN hs_code_suggested csug
                ON csel.suggestion_id = csug.suggestion_id
                JOIN hs_codes_app codes
                ON codes.hs_code_6 = csug.hs_code_6
                JOIN hs_descriptions hd
                ON codes.hs_id = hd.hs_id
                WHERE cp.check_id IN (",paste(check.ids, collapse=","),");")
  
  chk.chk.chk=gta_sql_get_value(query)
  
  if(nrow(chk.chk.chk)==0){stop("Check ID not found.")}

  return(chk.chk.chk)
  
}
