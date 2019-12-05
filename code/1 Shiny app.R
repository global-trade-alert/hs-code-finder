##############################
#                            #
#  HS CODE APP PRODUCTION    #
#                            #
##############################

require(shiny)
library(shinyjs)
library(openxlsx)
library(DT)
library(shinyWidgets)
library(gtalibrary)
library(data.table)
library(mailR)
library(tidyverse)
library(splitstackshape)
library(promises)
library(future)
library(clipr)
library(gtalibrary)
library(gtasql)
library(RMySQL)
library(pool)
library(keyring)
library(RMariaDB)
library(dplyr)


plan(multiprocess)

rm(list = ls())

gta_setwd()
# setwd("/home/rstudio/Dropbox/GTA cloud")
# wdpath="17 Shiny/5 HS code finder/"
wdpath="0 dev/hs-code-finder-jf/"

## helpful functions
## HS app functions
for(fct in list.files(paste0(wdpath,"code/functions"), pattern = ".R", full.names=T)){
  source(fct)
}

source(paste0(wdpath,"code/server.R"))
source(paste0(wdpath,"code/ui.R"))

gta_sql_pool_open(db.title="ricardomain",
                  db.host = gta_pwd("ricardomain")$host,
                  db.name = gta_pwd("ricardomain")$name,
                  db.user = gta_pwd("ricardomain")$user,
                  db.password = gta_pwd("ricardomain")$password,
                  table.prefix = "hs_")



data.base.0 = gta_sql_load_table("codes_app")
data.base.0$indicator = "<div class='indicator'></div>"
data.base.0 <- merge(data.base.0, gta_sql_load_table("descriptions"), by="hs.id", all.x=T)
data.base.0 <- data.base.0[,c("indicator","hs.code.6","hs.description.6","hs.description.4","hs.code.4","hs.code.2","hs.id")]
data.base.0 <- data.base.0

# Define random phrase id
job.id <- 0
phr.id <- 0
query <- "Select User"
data.subset.0 <- data.base.0[NULL,]
# data.subset.0 <- data.subset.0

# To keep track of changes in the apps state
data.ledger.0 <- data.base.0[,c("hs.id","hs.code.6","hs.code.4")]
data.ledger.0$user.generated <- 0
data.ledger.0$selected <- 0
data.ledger.0$search.generated <- 0
# data.ledger.0$selected[data.ledger.0$hs.code.6 %in% unique(data.subset.0$hs.code.6)] <- 1
data.ledger.0 <- data.ledger.0

# search.query <- query
chosen.user <- "Select"
userinput = F
nonefound.check = F
searchinput = F

# Load table users
users <- change_encoding(gta_sql_load_table("user_log", table.prefix = "gta_"))
users <<- users
levels.of.certainty <- change_encoding(gta_sql_load_table("levels_of_certainty"))
levels.of.certainty <<- levels.of.certainty



shinyApp(ui = ui,
         server = server,
         onStart = function() {
           gta_sql_pool_open(db.title="ricardomain",
                             db.host = gta_pwd("ricardomain")$host,
                             db.name = gta_pwd("ricardomain")$name,
                             db.user = gta_pwd("ricardomain")$user,
                             db.password = gta_pwd("ricardomain")$password,
                             table.prefix = "hs_")
           
           onStop(function() {
             gta_sql_pool_close()
           })
         },
         options = list(launch.browser=T)
)