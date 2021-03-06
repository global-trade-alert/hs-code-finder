gta_hs_estimate_classifier<- function(agreed.refusal.threshold=.2,
                                      agreed.acceptance.threshold=.3,
                                      train.share=.8,
                                      relevance.threshold=.5,
                                      path.to.cloud=NULL){
  
  
  database <<- "ricardomain"
  
  gta_sql_pool_open(db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "hs_")
 
  if(is.null(path.to.cloud)==F){
    setwd(path.to.cloud)
  } else {
    if(grepl(" cloud", getwd())){
      gta_setwd()
    } else{
      stop("You are not in the GTA cloud folder, please specify 'path.to.cloud' or use setwd() yourself.")
    }
  }
  
 
  # job.phrase=gta_sql_load_table("job.phrase")
  code.selected=gta_sql_load_table("code.selected")
  check.phrases=gta_sql_load_table("check.phrases")
  
  
  processed.phrase=subset(aggregate(check.id ~ phrase.id,
                                    subset(check.phrases, check.id %in% code.selected$check.id),
                                    function(x) length(unique(x))), check.id>=3)$phrase.id
  
  estimation.set=gta_hs_create_classifier_variables_phrase(phrase.ids=processed.phrase)
  rm(job.phrase, processed.phrase)
  
  #### ESTIMATION
  ## splitting off the training set from the estimation set
  training.set=subset(estimation.set, (selection.share<=agreed.refusal.threshold)|(selection.share>=agreed.acceptance.threshold))
  
  training.set$evaluation=as.numeric(training.set$selection.share>=relevance.threshold)
  training.set$train.id=1:nrow(training.set)
  
  ## preparing the training set
  train.split=sample(unique(training.set$train.id), ceiling(nrow(training.set)*train.share))
  
  train=training.set[,setdiff(names(training.set), c("phrase.id","suggestion.id","hs.code.6","nr.times.chosen","nr.of.checks","selection.share"))]
  
  train.x = subset(train, train.id %in% train.split)
  test.x = subset(train, ! train.id %in% train.split)
  
  test.y=test.x$evaluation
  test.x$train.id=NULL
  test.x$evaluation=NULL
  
  train.y=train.x$evaluation
  train.x$train.id=NULL
  train.x$evaluation=NULL
  
  library(SuperLearner)
  hs.classifier = SuperLearner(train.y,train.x, family = binomial(),
                               SL.library = "SL.xgboost")
  
  pred.train=data.frame(obs=train.y, pred=predict(hs.classifier, train.x)$pred[,1])
  pred.test= data.frame(obs=test.y, pred=predict(hs.classifier, test.x)$pred[,1])
  
  
  pred_rocr = ROCR::prediction(pred.train$pred, pred.train$obs)
  train.auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
  train.auc
  
  pred_rocr = ROCR::prediction(pred.test$pred, pred.test$obs)
  test.auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
  test.auc
  
  
  classifier.variables=setdiff(names(training.set), c("phrase.id","suggestion.id","hs.code.6","nr.times.chosen","nr.of.checks","selection.share", "evaluation","train.id"))
  save(hs.classifier, classifier.variables, test.auc, train.auc, file="17 Shiny/5 HS code finder/database/HS classifier.Rdata")
  
  ## estimating the unsure cases
  # estimate=estimation.set[,setdiff(names(estimation.set), c("phrase.id","suggestion.id","hs.code.6","nr.times.chosen","nr.of.checks","selection.share"))]
  # 
  # 
  # 
  # estimate$train.id=NULL
  # estimate$evaluation=NULL
  # 
  # estimation.set$relevance= predict(hs.classifier, estimate)$pred[,1]
  # 
  # save(estimation.set, training.set, test.auc, train.auc, users, file="17 Shiny/5 HS code finder/database/HS classifier data.Rdata")
  # 
  
}
