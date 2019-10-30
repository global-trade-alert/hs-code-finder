gta_hs_create_classifier_variables_phrase<- function(phrase.ids=NULL,
                                                     job.checks=3,
                                                     agreeable.threshold=.2,
                                                     path.to.cloud=NULL){
  
  library(gtalibrary)
  
  if(is.null(path.to.cloud)==F){
    setwd(path.to.cloud)
  } else {
    if(grepl(" cloud", getwd())){
      gta_setwd()
    } else{
      stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
    }
  }
  
  
  job.phrase <- gta_sql_load_table("job_phrase")
  job.phrase <<- job.phrase
  job.log <- gta_sql_load_table("job_log")
  job.log <<- job.log
  code.suggested <- gta_sql_load_table("code_suggested")
  code.suggested <<- code.suggested
  check.phrases <- gta_sql_load_table("check_phrases")
  check.phrases <<- check.phrases
  check.certainty <- gta_sql_load_table("check_certainty")
  check.certainty <<- check.certainty
  levels.of.certainty <- gta_sql_load_table("levels_of_certainty")
  levels.of.certainty <<- levels.of.certainty
  check.log <- gta_sql_load_table("check_log")
  check.log <<- check.log
  code.selected <- gta_sql_load_table("code_selected")
  code.selected <<- code.selected
  code.source <- gta_sql_load_table("code.source")
  code.source <<- code.source
  
  
  
  # if(any(! phrase.ids %in% subset(job.phrase, processed==T)$phrase.id)){
  #   stop("Some of the phrases you inserted are not processed.")
  # }
  
  hs.candidates=as.data.frame(subset(code.suggested, phrase.id %in% phrase.ids & is.na(hs.code.6)==F))
  hs.candidates$probability=NULL
  
  chosen.suggestions=as.data.frame(table(subset(code.selected, check.id %in% subset(check.phrases, phrase.id %in% phrase.ids)$check.id)$suggestion.id))
  names(chosen.suggestions)=c("suggestion.id","nr.times.chosen")
  chosen.suggestions$suggestion.id=as.numeric(as.character(chosen.suggestions$suggestion.id))
  
  hs.candidates=merge(hs.candidates, chosen.suggestions, by="suggestion.id", all.x=T)
  hs.candidates[is.na(hs.candidates)]=0
  
  checks.per.phrase=as.data.frame(table(subset(check.phrases, phrase.id %in% phrase.ids)$phrase.id))
  names(checks.per.phrase)=c("phrase.id","nr.of.checks")
  checks.per.phrase$phrase.id=as.numeric(as.character(checks.per.phrase$phrase.id))
  
  hs.candidates=merge(hs.candidates, checks.per.phrase, by="phrase.id", all.x=T)
  hs.candidates$selection.share=hs.candidates$nr.times.chosen/hs.candidates$nr.of.checks
  hs.candidates$selection.share[hs.candidates$selection.share>=1]=1
  
  
  ### could do a classifier as follows
  ## those with full agreement, one way or the other are classified as in or out
  ## for those in between, I use a randomforest to see where they should go using the attributes (share of values occuring in chosen cases or as dummies)
  ## 1) source, 
  ## 2) certainty, 
  ## 3) user ID 
  ## The KNN would be trained on the clear cases, I guess. 
  ## Cannot include the "votes in favor" metric since it's a clear indicator for inclusion ...
  ## However, can apply different probability thresholds for inclusion depending on the "votes in favor" metric!
  
  
  ## creating the estimation variables
  ## sources
  hs.candidates$source.user=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==99)$suggestion.id)
  hs.candidates$source.guru=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==1)$suggestion.id)
  hs.candidates$source.eurostat=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==2)$suggestion.id)
  hs.candidates$source.google=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==3)$suggestion.id)
  hs.candidates$source.zauba=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==4)$suggestion.id)
  hs.candidates$source.ets=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==5)$suggestion.id)
  hs.candidates$source.bianma=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==6)$suggestion.id)
  hs.candidates$source.cybex=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==7)$suggestion.id)
  hs.candidates$source.descr=as.numeric(hs.candidates$suggestion.id %in% subset(code.source, source.id==8)$suggestion.id)
  hs.candidates$source.nr=rowSums(hs.candidates[,c(which(grepl("source\\.",names(hs.candidates))))])
  
  ## check certainty
  certainty.distribution=aggregate(check.id ~ phrase.id + certainty.level,
                                   merge(check.certainty, subset(check.phrases, phrase.id %in% phrase.ids), by="check.id"), 
                                   function(x) length(unique(x)))
  certainty.distribution=merge(certainty.distribution, unique(hs.candidates[,c("phrase.id","nr.of.checks")]), by="phrase.id")
  certainty.distribution$certain.share=certainty.distribution$check.id/certainty.distribution$nr.of.checks
  
  certainty.distribution$value=5
  certainty.distribution$value[certainty.distribution$certainty.level=="highly"]=4
  certainty.distribution$value[certainty.distribution$certainty.level=="fairly"]=3
  certainty.distribution$value[certainty.distribution$certainty.level=="somewhat"]=2
  certainty.distribution$value[certainty.distribution$certainty.level=="not"]=1
  
  certainty.stats=merge(data.frame(phrase.id=unique(certainty.distribution$phrase.id)),
                        aggregate(certain.share ~ phrase.id, subset(certainty.distribution, certainty.level %in% c("highly","exactly")), sum),
                        by="phrase.id", all.x=T)
  certainty.stats$certain.share[certainty.stats$certain.share>1]=1
  
  certainty.stats=merge(certainty.stats, 
                        aggregate(value ~phrase.id, certainty.distribution, mean), 
                        by="phrase.id", all.x=T)
  data.table::setnames(certainty.stats, "value","certainty.mean")
  
  certainty.stats=merge(certainty.stats, 
                        aggregate(value ~phrase.id, certainty.distribution, median), 
                        by="phrase.id", all.x=T)
  data.table::setnames(certainty.stats, "value","certainty.median")
  
  certainty.stats=merge(certainty.stats, 
                        aggregate(value ~phrase.id, certainty.distribution, sd), 
                        by="phrase.id", all.x=T)
  data.table::setnames(certainty.stats, "value","certainty.sd")
  
  certainty.stats[is.na(certainty.stats)]=0
  
  hs.candidates=merge(hs.candidates, certainty.stats, by="phrase.id", all.x=T)
  
  ## these are marked as a service
  odd.phrases=subset(hs.candidates, is.na(certain.share))
  hs.candidates=subset(hs.candidates, is.na(certain.share)==F)
  
  ### User stats
  
  app.users=aggregate(check.id ~ user.id, check.log, function(x) length(unique(x)))
  app.users=app.users$user.id[app.users$check.id>=50]
  
  choice.factors=c("Not checked","Discarded","Selected",
                   paste("Discarded",unique(levels.of.certainty$certainty.name), sep="-"),
                   paste("Selected",unique(levels.of.certainty$certainty.name), sep="-"))
  
  base.phrase=subset(code.suggested, phrase.id %in% phrase.ids)[,c("phrase.id","suggestion.id")]
  
  for(u.id in app.users){
    user.checks=unique(subset(check.log, user.id==u.id)$check.id)
    user.phrases=unique(subset(check.phrases, check.id %in% user.checks)$phrase.id)
    
    # only taking most recent in case of multiple checks per phrase
    user.checks=aggregate(check.id ~phrase.id, subset(check.phrases, check.id %in% user.checks), max)$check.id
    
    u.check=subset(check.phrases, check.id %in% user.checks)
    u.check=merge(u.check, code.suggested[,c("phrase.id","suggestion.id")], by="phrase.id", all.x=T)
    
    u.select=subset(code.selected, check.id %in% user.checks)
    u.select$user.choice="Selected"
    
    u.check=merge(u.check, u.select,by=c("check.id","suggestion.id"), all.x=T)
    rm(u.select)
    
    u.check$user.choice[is.na(u.check$user.choice)]="Discarded"
    
    for(level in unique(check.certainty$certainty.level)){
      level.checks=subset(check.certainty, certainty.level==level)$check.id
      u.check$user.choice[u.check$check.id %in% level.checks]=paste(u.check$user.choice[u.check$check.id %in% level.checks],level, sep="-")
      
    }
    u.check$check.id=NULL
    
    u.check=merge(base.phrase, u.check, by=c("phrase.id", "suggestion.id"), all.x=T)
    u.check$user.choice[is.na(u.check$user.choice)]="Not checked"
    u.check$user.choice=factor(u.check$user.choice, levels=choice.factors)
    
    
    setnames(u.check, "user.choice", paste0("user.",u.id))  
    
    hs.candidates=merge(hs.candidates, u.check, by=c("phrase.id","suggestion.id"), all.x=T)
    
    rm(u.check)
    print(u.id)
    
  }
  
  
  ## shared CPC code with common acceptance/refusal selections
  phrase.cpc=unique(hs.candidates[,c("phrase.id","hs.code.6","selection.share")])
  phrase.cpc$hs=as.numeric(as.character(phrase.cpc$hs.code.6))
  phrase.cpc=merge(phrase.cpc, cpc.to.hs, by="hs")
  
  agreed=unique(subset(phrase.cpc, selection.share>=(1-agreeable.threshold))[,c("phrase.id","cpc")])
  if(nrow(agreed)>0){
    agreed$cpc.chosen=T
    phrase.cpc=merge(phrase.cpc, agreed, by =c("phrase.id","cpc"), all.x=T)
    phrase.cpc$cpc.chosen[is.na(phrase.cpc$cpc.chosen)]=F
    
  } else {
    phrase.cpc$cpc.chosen=F
  }
  
  
  refused=unique(subset(phrase.cpc, selection.share<=(agreeable.threshold))[,c("phrase.id","cpc")])
  if(nrow(refused)>0){
    
    refused$cpc.refused=T
    phrase.cpc=merge(phrase.cpc, refused, by =c("phrase.id","cpc"), all.x=T)
    phrase.cpc$cpc.refused[is.na(phrase.cpc$cpc.refused)]=F
    
  } else {
    phrase.cpc$cpc.refused=F
  }
  

  
  
  agreement.levels=c("neither","both","chosen","refused")
  phrase.cpc$share.cpc="neither"
  phrase.cpc$share.cpc[phrase.cpc$cpc.chosen==T & phrase.cpc$cpc.refused==T]="both"
  phrase.cpc$share.cpc[phrase.cpc$cpc.chosen==T & phrase.cpc$cpc.refused==F]="chosen"
  phrase.cpc$share.cpc[phrase.cpc$cpc.chosen==F & phrase.cpc$cpc.refused==T]="refused"
  phrase.cpc$share.cpc=factor(phrase.cpc$share.cpc, levels = agreement.levels)
  
  hs.candidates=merge(hs.candidates, phrase.cpc[,c("phrase.id","hs.code.6","share.cpc")], by=c("phrase.id","hs.code.6"))
  
  return(hs.candidates)
}
