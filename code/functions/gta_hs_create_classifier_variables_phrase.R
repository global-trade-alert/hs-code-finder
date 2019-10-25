gta_hs_create_classifier_variables_phrase<- function(phrase.ids=NULL,
                                              job.checks=3,
                                              agreeable.threshold=2/3,
                                              disagreeable.threshold=1/3,
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
  
  
  
  if(any(! phrase.ids %in% subset(job.phrase, processed==T)$phrase.id)){
    stop("Some of the phrases you inserted are not processed.")
  }
  
  hs.candidates=as.data.frame(subset(code.suggested, phrase.id %in% phrase.ids & is.na(hs.code.6)==F))
  
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
  
  user.variables=data.frame()
  for(u.id in unique(check.log$user.id)){
    user.checks=unique(subset(check.log, user.id==u.id)$check.id)
    
    for(p.id in phrase.ids){
      
      u.check=subset(check.phrases, check.id %in% user.checks & phrase.id == p.id)
      
      u.hs=subset(code.suggested, phrase.id==p.id )$hs.code.6
      u.suggest=subset(code.suggested, phrase.id==p.id )$suggestion.id
      
      if(nrow(u.check)==0){
        
        u.check=data.frame(phrase.id=p.id,
                           hs.code.6=u.hs,
                           suggestion.id=u.suggest,
                           not.checked=1,
                           selected.exactly=0,
                           selected.highly=0,
                           selected.fairly=0,
                           selected.somewhat=0,
                           selected.not=0,
                           discarded.exactly=0,
                           discarded.highly=0,
                           discarded.fairly=0,
                           discarded.somewhat=0,
                           discarded.not=0
        )
        
      } else {
        
        # determine selection y/n
        u.selected=merge(subset(code.suggested, phrase.id==p.id),
                         subset(code.selected, check.id %in% user.checks),
                         by="suggestion.id")$suggestion.id
        
        
        
        # determine certainty
        u.certainty=unique(as.character(subset(check.certainty, check.id %in% subset(check.phrases, 
                                                                                     phrase.id %in% p.id &
                                                                                       check.id %in% user.checks)$check.id)$certainty.level))
        
        # store
        u.check=data.frame(phrase.id=p.id,
                           hs.code.6=u.hs,
                           suggestion.id=u.suggest,
                           not.checked=0,
                           selected.exactly=0,
                           selected.highly=0,
                           selected.fairly=0,
                           selected.somewhat=0,
                           selected.not=0,
                           discarded.exactly=0,
                           discarded.highly=0,
                           discarded.fairly=0,
                           discarded.somewhat=0,
                           discarded.not=0
                           )
        
        for(cert in u.certainty){
          eval(parse(text=paste0("u.check$selected.",cert,"[u.check$suggestion.id %in% u.selected] =1")))
        }
        
        
      }
      
      user.variables=rbind(user.variables, u.check)
      rm(u.check, u.hs, u.suggest)
      
    }
    
    names(user.variables)[4:14]=paste0("user.",u.id,".",names(user.variables)[4:14])
    user.variables$hs.code.6=NULL
    hs.candidates=merge(hs.candidates, user.variables, by=c("phrase.id","suggestion.id"), all.x=T)
    user.variables=data.frame()
    
  }
  
  
  ## shared CPC code with highly agreeable/disagreeabe selections
  phrase.cpc=unique(hs.candidates[,c("phrase.id","hs.code.6", "selection.share")])
  phrase.cpc$hs=as.numeric(as.character(phrase.cpc$hs.code.6))
  phrase.cpc=merge(phrase.cpc, cpc.to.hs, by="hs")
  cpc.per.phrase=aggregate(cpc ~phrase.id, phrase.cpc, function(x) length(unique(x)))
  
  phrase.id.vector=phrase.cpc$phrase.id
  cpc.vector=phrase.cpc$cpc
  selection.vector=phrase.cpc$selection.share
  
  agreeable.positions=which(selection.vector>=agreeable.threshold)
  disagreeable.positions=which(selection.vector<=disagreeable.threshold)
  
  agree.vector=character(nrow(phrase.cpc))
  disagree.vector=character(nrow(phrase.cpc))
  
  
  for(i in 1:nrow(phrase.cpc)){
    phrase.positions=which(phrase.id.vector==phrase.id.vector[i])
    agree.vector[i]=cpc.vector[i] %in% cpc.vector[setdiff(intersect(phrase.positions, agreeable.positions),i)]
    disagree.vector[i]=cpc.vector[i] %in% cpc.vector[setdiff(intersect(phrase.positions, disagreeable.positions),i)]
    
    print(i/nrow(phrase.cpc))
  }
  
  phrase.cpc$cpc.agree=agree.vector
  phrase.cpc$cpc.disagree=disagree.vector
  
  phrase.cpc$share.cpc="neither"
  phrase.cpc$share.cpc[phrase.cpc$cpc.agree==T & phrase.cpc$cpc.disagree==T]="both"
  phrase.cpc$share.cpc[phrase.cpc$cpc.agree==T & phrase.cpc$cpc.disagree==F]="agree"
  phrase.cpc$share.cpc[phrase.cpc$cpc.agree==F & phrase.cpc$cpc.disagree==T]="disagree"
  phrase.cpc$share.cpc=as.factor(phrase.cpc$share.cpc)
  
  hs.candidates=merge(hs.candidates, phrase.cpc[,c("phrase.id","hs.code.6","share.cpc")], by=c("phrase.id","hs.code.6"))
  
  return(hs.candidates)
}
