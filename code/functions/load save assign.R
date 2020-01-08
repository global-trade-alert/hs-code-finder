assign.global <- function (assignTo, toAssign) {
  assign(assignTo, toAssign, envir = .GlobalEnv)
}

# save_all <- function(path) {
#   print("SAVE_ALL()")
#   save(check.certainty,
#        check.log,
#        check.phrases,
#        code.selected,
#        code.source,
#        code.suggested,
#        hs.codes.app,
#        hs.descriptions,
#        job.log,
#        job.phrase,
#        levels.of.certainty,
#        phrase.table,
#        suggestion.sources,
#        users,
#        words.removed,
#        report.services,
#        additional.suggestions,
#        phrases.to.import,
#        file = path)
  
  # if(exists("check.phrases")){
  #   if(nrow(check.phrases)>0){
  #     cp.path=paste(gsub(".Rdata","",path), " - checkphrases ", gsub("\\D","", as.character(Sys.time())), ".Rdata", sep="")
  #     save(check.phrases, file=cp.path )
  #   }
  # }
  
# }
# 
# load_all <- function(path) {
#   print("LOAD_ALL()")
#   load(file=path)
#   
#   check.certainty <- check.certainty
#   check.certainty <<- check.certainty
#   assign.global("check.certainty", check.certainty)
#   
#   check.log <- check.log
#   check.log <<- check.log
#   assign.global("check.log", check.log)
#   
#   code.selected <- code.selected
#   code.selected <<- code.selected
#   assign.global("code.selected", code.selected)
#   
#   check.phrases <- check.phrases
#   check.phrases <<- check.phrases
#   assign.global("check.phrases", check.phrases)
#   
#   code.source <- code.source
#   code.source <<- code.source
#   assign.global("code.source", code.source)
#   
#   code.suggested <- code.suggested
#   code.suggested <<- code.suggested
#   assign.global("code.suggested", code.suggested)
#   
#   hs.codes.app <- hs.codes.app
#   hs.codes.app <<- hs.codes.app
#   assign.global("hs.codes.app", hs.codes.app)
#   
#   hs.descriptions <- hs.descriptions
#   hs.descriptions <<- hs.descriptions
#   assign.global("hs.descriptions", hs.descriptions)
#   
#   job.log <- job.log
#   job.log <<- job.log
#   assign.global("job.log", job.log)
#   
#   job.phrase <- job.phrase
#   job.phrase <<- job.phrase
#   assign.global("job.phrase", job.phrase)
#   
#   levels.of.certainty <- levels.of.certainty
#   levels.of.certainty <<- levels.of.certainty
#   assign.global("levels.of.certainty", levels.of.certainty)
#   
#   phrase.table <- phrase.table
#   phrase.table <<- phrase.table
#   assign.global("phrase.table", phrase.table)
#   
#   report.services <- report.services
#   report.services <<- report.services
#   assign.global("report.services", report.services)
#   
#   suggestion.sources <- suggestion.sources
#   suggestion.sources <<- suggestion.sources
#   assign.global("suggestion.sources", suggestion.sources)
#   
#   users <- users
#   users <<- users
#   assign.global("users", users)
#   
#   words.removed <- words.removed
#   words.removed <<- words.removed
#   assign.global("words.removed", words.removed)
#   
#   additional.suggestions <- additional.suggestions
#   additional.suggestions <<- additional.suggestions
#   assign.global("additional.suggestions", additional.suggestions)
#   
#   
#   phrases.to.import <- phrases.to.import
#   phrases.to.import <<- phrases.to.import
#   assign.global("phrases.to.import", phrases.to.import)
#   
# }

remove_all<- function(x){
  rm(check.certainty,
     check.log,
     check.phrases,
     code.selected,
     code.source,
     code.suggested,
     hs.codes.app,
     hs.descriptions,
     job.log,
     job.phrase,
     levels.of.certainty,
     phrase.table,
     suggestion.sources,
     users,
     words.removed,
     report.services,
     additional.suggestions)
}



update_logs <- function() {
  print("updating importer-log")
  # load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
  sql <- "UPDATE hs_importer_log SET time_finish = ?newvalue WHERE ticket_number = ?forwhom;"
  query <- sqlInterpolate(pool, 
                          sql, 
                          forwhom = log.row,
                          newvalue = as.character(Sys.time()))
  
  gta_sql_update_table(query)
  
  # importer.log$time.finish[log.row]=Sys.time()
  # class(importer.log$time.finish)=c('POSIXt', 'POSIXct')
  # importer.log$under.preparation[log.row]=0
  sql <- "UPDATE hs_importer_log SET time_finish = ?newvalue WHERE ticket_number = ?forwhom;"
  query <- sqlInterpolate(pool, 
                          sql, 
                          forwhom = log.row,
                          newvalue = as.character(Sys.time()))
  
  gta_sql_update_table(query)
  
  sql <- "UPDATE hs_importer_log SET under_preparation = false WHERE ticket_number = ?forwhom;"
  query <- sqlInterpolate(pool, 
                          sql, 
                          forwhom = log.row)
  
  gta_sql_update_table(query)
  # save(importer.log, file = "17 Shiny/5 HS code finder/log/importer-log.Rdata")
  # load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
  
  print("Closing sink")
  sink()
  sink(type="message")
  
  print(paste("Updating round count to",rnd+1))
  rnd=rnd+1 
}

# Change encoding of character columns in a dataframe to UTF 8
change_encoding <- function(df = NULL) {
  for (col in colnames(df)){
    if (is.character(df[,col])) {
      Encoding(df[[col]]) <- "UTF-8"} }
  return(df)
}

# FUNCTION NECESSARY FOR CREATING RED TO GREEN GRADIENT FOR PROBABILITY INDICATOR
redToGreen <- function(prob=0) {
  rgb = c()
  for (p in prob) {
    if (is.na(p)==F) {
      p <- as.numeric(p)
      
      if (p < 0.5) {
        red = 255
      } else {
        red = 255/0.5*(1-p)
      }
      
      if (p > 0.5) {
        green = 255
      } else {
        green = 255/0.5*(p)
      }
      
      blue = 0
      color = paste0("rgba(",red,",",green,",",blue,")")
      rm(blue, green, red)
      rgb = c(rgb,color)
    }
  }
  return(rgb)
}



