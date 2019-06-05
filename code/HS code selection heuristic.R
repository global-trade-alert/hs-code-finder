rm(list = ls())

# setwd("GTA cloud")
setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")

load("17 Shiny/5 HS code finder/database/GTA HS code database.Rdata")

### Goal: Identifying what are the factors best explaining whether a chosen HS code is correct

### 1. I want to look at some descriptive stats to get a feeling: how many users and how many checks do they each process; how many results do each search engine spit out;...

### Checks per user
table(merge(check.log, users, by="user.id", all.x = T)$name)

### Suggestions per source
table(merge(code.source, suggestion.sources, by="source.id", all.x = T)$source.name)
paste("Out of a total suggestions:", length(unique(code.source$suggestion.id)))



###########################################################################
### 2. I construct a master dataframe with one row per suggestion
###########################################################################
## 2.1 Focus on a dataframe only with the following restrictions:
jobs.with.at.least.3.checks <- subset(job.log, nr.of.checks>=3)$job.id
users.with.at.least.10.checks <- subset(aggregate(check.id ~ user.id, check.log, function(x) length(unique(x))), check.id>=10)$user.id
phrases.with.at.least.3.checks <- subset(aggregate(check.id ~ phrase.id, check.phrases, function(x) length(unique(x))), check.id>=3)$phrase.id

phrases.with.more.than.3.checks <- subset(aggregate(check.id ~ phrase.id, check.phrases, function(x) length(unique(x))), check.id>3)$phrase.id

## 2.2 Merge dataframes together
master <- merge(subset(code.suggested, phrase.id %in% phrases.with.at.least.3.checks), phrase.table, all.x = T)

# Take out those phrases with no HS code suggested
master <- subset(master, is.na(hs.code.6)==F)

# Now add 3 check IDs
test <- aggregate(check.id ~ phrase.id, subset(check.phrases, phrase.id %in% phrases.with.at.least.3.checks), min)
setnames(test, "check.id", "check1")
test <- merge(test, aggregate(check.id ~ phrase.id, subset(check.phrases, phrase.id %in% phrases.with.at.least.3.checks), median))
setnames(test, "check.id", "check2")
test$check2 <- as.integer(test$check2)
test <- merge(test, aggregate(check.id ~ phrase.id, subset(check.phrases, phrase.id %in% phrases.with.at.least.3.checks), max))
setnames(test, "check.id", "check3")
master <- merge(master, test, all.x = T);rm(test)

# Add info on whether phrase was adjusted
master$adjusted.phrase <- as.numeric(master$check1 %in% words.removed$check.id | master$check2 %in% words.removed$check.id | master$check3 %in% words.removed$check.id)

# Add info on whether additional suggestion
master$additional.suggestion <- as.numeric(master$check1 %in% additional.suggestions$check.id)

# Add user info: 1 if selected, 0 if not, and NA if not checked
for(user in users.with.at.least.10.checks){
  eval(parse(text = paste0("master$user.", user, " <- NA ")))
  
  eval(parse(text = paste0("master$user.", user, "[master$check1 %in% subset(check.log, user.id==", user, ")$check.id] <- 0")))
  eval(parse(text = paste0("master$user.", user, "[master$check2 %in% subset(check.log, user.id==", user, ")$check.id] <- 0")))
  eval(parse(text = paste0("master$user.", user, "[master$check3 %in% subset(check.log, user.id==", user, ")$check.id] <- 0")))

  eval(parse(text = paste0("master$user.", user, "[master$check1 %in% subset(check.log, user.id==", user, ")$check.id & master$hs.code.6 %in% subset(code.suggested, suggestion.id %in% subset(code.selected, check.id %in% subset(check.log, user.id==", user, ")$check.id)$suggestion.id)$hs.code.6] <- 1")))
  eval(parse(text = paste0("master$user.", user, "[master$check2 %in% subset(check.log, user.id==", user, ")$check.id & master$hs.code.6 %in% subset(code.suggested, suggestion.id %in% subset(code.selected, check.id %in% subset(check.log, user.id==", user, ")$check.id)$suggestion.id)$hs.code.6] <- 1")))
  eval(parse(text = paste0("master$user.", user, "[master$check3 %in% subset(check.log, user.id==", user, ")$check.id & master$hs.code.6 %in% subset(code.suggested, suggestion.id %in% subset(code.selected, check.id %in% subset(check.log, user.id==", user, ")$check.id)$suggestion.id)$hs.code.6] <- 1")))
  # master$user.1[master$check3 %in% subset(check.log, user.id==1)$check.id & master$hs.code.6 %in% subset(code.suggested, suggestion.id %in% subset(code.selected, check.id %in% subset(check.log, user.id==1)$check.id)$suggestion.id)$hs.code.6] <- 1
  # subset(code.suggested, suggestion.id %in% subset(code.selected, check.id %in% subset(check.log, user.id==1)$check.id)$suggestion.id)$hs.code.6
}

test <- aggregate()

master$nr.user <- sum(master$user.1 + master$user.2 + master$user.8 + master$user.15)




##


###########################################################################
### 3. I identify what I consider to be a correct match.
###########################################################################
## 3.1 Approach: Where all three users agree

## 3.2 Approach: At least 2 users agree and have certainty.level equal to 1 or 2



