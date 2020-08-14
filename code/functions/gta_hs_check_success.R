gta_hs_check_success <- function(check.id=NULL,
                                 checks= NULL,
                                 phrase.id=NULL,
                                 recipients=NULL) {
  
  
  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(data.table)

  setwd("/home/rstudio/Dropbox/GTA cloud")
  
  database = "ricardomain"
  gta_sql_pool_open(db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "hs_")
  
  # new code selected
  codes.selected <- gta_sql_get_value(paste0("SELECT * FROM hs_code_selected WHERE check_id = ",check.id,";"))
  selected.check <- nrow(codes.selected) == checks[['selected.new']]
  
  # new code suggested
  suggested.check <- c()
  for (p.id in phrase.id) {
    codes.suggested <- gta_sql_get_value(paste0("SELECT * FROM hs_code_suggested WHERE phrase_id = ",p.id,";"))
    suggested.check <- c(suggested.check, nrow(codes.suggested) == checks[['suggested.new']]+checks[['suggested.old']])
  }
  suggested.check <- all(suggested.check)
  
  # words removed
  words.removed <- gta_sql_get_value(paste0("SELECT * FROM hs_words_removed WHERE check_id = ",check.id,";"))
  words.removed.check <- nrow(words.removed) == checks[['words.removed']]
  
  # additional suggestions
  additional.suggestions <- gta_sql_get_value(paste0("SELECT * FROM hs_additional_suggestions WHERE check_id = ",check.id,";"))
  additional.suggestions.check <- nrow(additional.suggestions) == checks[['additional.suggestions']]
  
  # check certainty
  check.certainty <- gta_sql_get_value(paste0("SELECT * FROM hs_check_certainty WHERE check_id = ",check.id,";"))
  check.certainty.check <- nrow(check.certainty) == checks[['check.certainty']]
  
  
  if (any(c(selected.check, suggested.check, words.removed.check, additional.suggestions.check, check.certainty.check))) {
  
  # SEND EMAIL TO JF
  sender = gta_pwd("mail")$mail  
  recipients = recipients
  attachment=NULL
  
  sbjct=paste("[HS App] Check was not saved correctly", sep="")
  message=paste0("Hello \n\n The check ",check.id," was not saved correctly. \n\nRegards\nGTA data team\n\n\n\n",
                 "########\n\n",
                 "Selected: ",nrow(codes.selected),"/",checks[['selected.new']],"\n",
                 "Suggested: ",nrow(code.suggested),"/",checks[['suggested.new']]+checks[['suggested.old']],"\n",
                 "Words removed: ",nrow(words.removed),"/",checks[['words.removed']],"\n",
                 "Additional suggestions: ",nrow(additional.suggestions),"/",checks[['additional.suggestions']],"\n",
                 "Certainty: ",nrow(check.certainty),"/",checks[['check.certainty']],"\n",
                 "########",
                 sep="")
  
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
  
  rm(recipients, message, sbjct, sender)
    
  }
    
    gta_sql_pool_close()
  
  }