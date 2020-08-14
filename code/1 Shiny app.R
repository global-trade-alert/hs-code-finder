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
path <<- "17 Shiny/5 HS code finder (DEV)/"
# path="0 dev/hs-code-finder-jf/"
# path="0 dev/hs-code-finder-pb/"

gta_sql_kill_connections()
gta_sql_pool_open(db.title="ricardomain",
                  db.host = gta_pwd("ricardomain")[['host']],
                  db.name = gta_pwd("ricardomain")[['name']],
                  db.user = gta_pwd("ricardomain")[['user']],
                  db.password = gta_pwd("ricardomain")[['password']],
                  table.prefix = "hs_")

## helpful functions
## HS app functions
for(fct in list.files(paste0(path,"code/functions"), pattern = ".R", full.names=T)){
  source(fct)
}

data.base.0 = gta_sql_load_table("codes_app")
data.base.0$indicator = "<div class='indicator'></div>"
data.base.0 <- merge(data.base.0, gta_sql_load_table("descriptions"), by="hs.id", all.x=T)
data.base.0 <- data.base.0[,c("indicator","hs.code.6","hs.description.6","hs.description.4","hs.code.4","hs.code.2","hs.id")]
data.base.0 <<- data.base.0

# Load table users
levels.of.certainty <- change_encoding(gta_sql_load_table("levels_of_certainty"))
levels.of.certainty <<- levels.of.certainty

source(paste0(path,"code/server.R"))
source(paste0(path,"code/ui.R"))

shinyApp(ui = ui,
         server = server,
         onStart = function() {
           gta_sql_pool_open(db.title="ricardomain",
                  db.host = gta_pwd("ricardomain")[['host']],
                  db.name = gta_pwd("ricardomain")[['name']],
                  db.user = gta_pwd("ricardomain")[['user']],
                  db.password = gta_pwd("ricardomain")[['password']],
                  table.prefix = "hs_")
           
           onStop(function() {
             gta_sql_pool_close()
             gta_sql_kill_connections()
           })
         },
         options = list(launch.browser=F)
)
