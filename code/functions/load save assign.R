assign.global <- function (assignTo, toAssign) {
  assign(assignTo, toAssign, envir = .GlobalEnv)
}

save_all <- function() {
  print("SAVE_ALL()")
  save(check.certainty,
       check.log,
       check.phrases,
       code.selected,
       code.source,
       code.suggested,
       hs.codes,
       hs.descriptions,
       job.log,
       job.phrase,
       levels.of.certainty,
       phrase.table,
       suggestion.sources,
       users,
       words.removed,
       report.services,
       additional.suggestions,
       file = path)
  
  # if(exists("check.phrases")){
  #   if(nrow(check.phrases)>0){
  #     cp.path=paste(gsub(".Rdata","",path), " - checkphrases ", gsub("\\D","", as.character(Sys.time())), ".Rdata", sep="")
  #     save(check.phrases, file=cp.path )
  #   }
  # }
  
}

load_all <- function() {
  print("LOAD_ALL()")
  load(file=path)
  
  check.certainty <- check.certainty
  check.certainty <<- check.certainty
  assign.global("check.certainty", check.certainty)
  
  check.log <- check.log
  check.log <<- check.log
  assign.global("check.log", check.log)
  
  code.selected <- code.selected
  code.selected <<- code.selected
  assign.global("code.selected", code.selected)
  
  check.phrases <- check.phrases
  check.phrases <<- check.phrases
  assign.global("check.phrases", check.phrases)
  
  code.source <- code.source
  code.source <<- code.source
  assign.global("code.source", code.source)
  
  code.suggested <- code.suggested
  code.suggested <<- code.suggested
  assign.global("code.suggested", code.suggested)
  
  hs.codes <- hs.codes
  hs.codes <<- hs.codes
  assign.global("hs.codes", hs.codes)
  
  hs.descriptions <- hs.descriptions
  hs.descriptions <<- hs.descriptions
  assign.global("hs.descriptions", hs.descriptions)
  
  job.log <- job.log
  job.log <<- job.log
  assign.global("job.log", job.log)
  
  job.phrase <- job.phrase
  job.phrase <<- job.phrase
  assign.global("job.phrase", job.phrase)
  
  levels.of.certainty <- levels.of.certainty
  levels.of.certainty <<- levels.of.certainty
  assign.global("levels.of.certainty", levels.of.certainty)
  
  phrase.table <- phrase.table
  phrase.table <<- phrase.table
  assign.global("phrase.table", phrase.table)
  
  report.services <- report.services
  report.services <<- report.services
  assign.global("report.services", report.services)
  
  suggestion.sources <- suggestion.sources
  suggestion.sources <<- suggestion.sources
  assign.global("suggestion.sources", suggestion.sources)
  
  users <- users
  users <<- users
  assign.global("users", users)
  
  words.removed <- words.removed
  words.removed <<- words.removed
  assign.global("words.removed", words.removed)
  
  additional.suggestions <- additional.suggestions
  additional.suggestions <<- additional.suggestions
  assign.global("additional.suggestions", additional.suggestions)
  
}

remove_all<- function(x){
  rm(check.certainty,
     check.log,
     check.phrases,
     code.selected,
     code.source,
     code.suggested,
     hs.codes,
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