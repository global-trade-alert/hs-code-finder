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
# library(profvis)
plan(multiprocess)
# gta_update_library()

rm(list = ls())

# gta_setwd()
setwd("/home/rstudio/Dropbox/GTA cloud")
# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

# path="0 dev/hs-code-finder-pb/database/GTA HS code database.Rdata"
# wdpath="0 dev/hs-code-finder-pb/"
wdpath="17 Shiny/5 HS code finder/"

## helpful functions
## HS app functions
for(fct in list.files(paste0(wdpath,"/code/functions"), pattern = ".R", full.names=T)){
  source(fct)
}

gta_sql_pool_open(db.title="ricardomain",
                  db.host = gta_pwd("ricardomain")$host,
                  db.name = gta_pwd("ricardomain")$name,
                  db.user = gta_pwd("ricardomain")$user,
                  db.password = gta_pwd("ricardomain")$password,
                  table.prefix = "hs_")

# # CODE TO REMOVE PHRASES/JOBS/CHECKS MANUALLY
# load_all(path)
# phrase.to.remove <- c(1270)
# 
# phrase.log <- subset(phrase.log, ! phrase.id %in% phrase.to.remove)
# additional.suggestions <- subset(additional.suggestions, ! check.id %in% check.phrases$check.id[check.phrases$phrase.id %in% phrase.to.remove])
# check.certainty <- subset(check.certainty, ! check.id %in% check.phrases$check.id[check.phrases$phrase.id %in% phrase.to.remove])
# code.selected <- subset(code.selected, ! check.id %in% check.phrases$check.id[check.phrases$phrase.id %in% phrase.to.remove])
# check.log <- subset(check.log, ! check.id %in% check.phrases$check.id[check.phrases$phrase.id %in% phrase.to.remove])
# check.phrases <- subset(check.phrases, ! phrase.id %in% phrase.to.remove)
# code.suggested <- subset(code.suggested, ! phrase.id %in% phrase.to.remove)
# job.phrase <- subset(job.phrase, ! phrase.id %in% phrase.to.remove)
# 
# job.to.remove <- c(18,19)
# 
# job.log <- subset(job.log, ! job.id %in% job.to.remove)
# job.phrase <- subset(job.phrase, ! job.id %in% job.to.remove)
# check.log <- subset(check.log, ! job.id %in% job.to.remove)
# 
# checks.to.remove <- c(1506,1507,821,823,824,827,828)
# 
# additional.suggestions <- subset(additional.suggestions, ! check.id %in% checks.to.remove)
# words.removed <- subset(words.removed, ! check.id %in% checks.to.remove)
# code.selected <- subset(code.selected, ! check.id %in% checks.to.remove)
# check.log <- subset(check.log, ! check.id %in% checks.to.remove)
# check.phrases <- subset(check.phrases, ! check.id %in% checks.to.remove)
# check.certainty <- subset(check.certainty, ! check.id %in% checks.to.remove)
# 
# save_all(path)


# Build starting set
# load_all(path)

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

ui <- fluidPage(
  useShinyjs(),
  theme = "style.css",
  tags$head(tags$link(rel="stylesheet", type="text/css", href="tipped.css")),
  tags$head(tags$script(src="tipped.js")),
  
  ######## DATA COUNTS #########
  tags$div(id="loading",
           tags$div(class="loading-background"),
           tags$div(class="img-holder",
                    tags$img(src="loading.svg"))),
  tags$div(class="header",
           tags$div(class="username",
                    textInput("username",
                              label=NULL,
                              placeholder = "Create User"),
                    actionButton("create.user",
                                 "Create"),
                    selectInput("users",
                                label=NULL,
                                choices = c("Choose User"="Select",unique(users$user.login)),
                                multiple = F)),
           tags$div(class="logo",
                    tags$div(class="import-button",
                             actionButton("import.toggle.button",
                                          "Import Values")),
                    img(src="gta logo-white.svg"))),
  
  tags$div(class="overall-wrap",
           tags$div(class = "settings",
                    tags$div(class = "settings-inner",
                             tags$div(class = "refine-query", id="term",
                                      HTML("<p class='search-term'>Search term:</p>"),
                                      actionButton("call_names_refresh",
                                                   "Refresh"),
                                      checkboxGroupButtons("query.refine",
                                                           label=NULL,
                                                           choices = query,
                                                           selected = query),
                                      conditionalPanel(condition = "output.showFinderCheck",
                                                       tags$div(class="finder_check",
                                                                uiOutput("finder_check_text"),
                                                                uiOutput("finder_check_button"))),
                                      tags$div(class="not-a-product",
                                               # tags$p("This is not a good/product, but a service:"),
                                               actionButton("not.product",
                                                            "This is not a good/product"))),
                             tags$div(class = "refresh-button",
                                      actionButton("names.refresh",
                                                   "Next term")),
                             tags$div(class="settings-bottom",
                                      tags$div(class="scroll",
                                               tags$div(class="suggestions",
                                                        tags$span("Can't find anything in the list?"),
                                                        tags$p("Use these pre-defined searches to check more sources"),
                                                        uiOutput("search.engines"),
                                                        textAreaInput("suggestions.search.terms",
                                                                      label = "Please share any alternative search terms you used to find the codes.",
                                                                      placeholder = "Separate multiple terms using a semicolon e. g. Coke; Pepsi",
                                                                      width = '100%',
                                                                      rows = 4)))),
                             tags$div(class = "toggle-all-button",
                                      actionButton("toggle_all",
                                                   "Select All"),
                                      actionButton("untoggle_all",
                                                   "Deselect All")),
                             tags$div(class="continue-button",
                                      tags$div(class="toggle-continue",
                                               tags$p("Commit Selection"))),
                             tags$div(id="save-selection", class = "save-selection",
                                      tags$div(class="wrap-buttons",
                                               tags$div(class="close-button"),
                                               tags$div(class="selected-codes-output",
                                                        textAreaInput("selected_codes_output",
                                                                      label = "These are your selected values:",
                                                                      value= "")),
                                               tags$div(class="confidence",
                                                        fluidRow(
                                                          column(
                                                            width = 12,
                                                            prettyRadioButtons(inputId = "radio1",
                                                                               label = "How confident are you about the chosen/suggested codes?",
                                                                               choices = unique(levels.of.certainty$certainty.name),
                                                                               inline = T,
                                                                               selected = "Unchosen")))),
                                               actionButton("none_found",
                                                            "None found"),
                                               actionButton("save_selection",
                                                            "Save selection"),
                                               actionButton("save_selection_clipboard",
                                                            "Save selection & Copy CSV to clipboard"),
                                               tags$div(class="chrome_message",
                                                        tags$p("*Copying to clipboard works on Chrome only.")))),
                             tags$div(id="save-selection-unrelated", class = "save-selection-unrelated",
                                      tags$div(class="wrap-buttons",
                                               tags$div(class="close-button-unrelated"),
                                               tags$div(class="selected-codes-output",
                                                        textAreaInput("selected_codes_output_unrelated",
                                                                      label = "These are your selected values:",
                                                                      value= "")),
                                               tags$div(class="confidence",
                                                        fluidRow(
                                                          column(
                                                            width = 12,
                                                            prettyRadioButtons(inputId = "radio1_unrelated",
                                                                               label = "How confident are you about the chosen/suggested codes?",
                                                                               choices = unique(levels.of.certainty$certainty.name),
                                                                               inline = T,
                                                                               selected = "Unchosen")))),
                                               
                                               tags$div(class="chrome_message",
                                                        tags$p("*Copying to clipboard works on Chrome only.")))),
                             tags$div(class="import-wrap",
                                      tags$div(class="import-wrap-inner",
                                               tags$div(class="import-close-button"),
                                               tags$div(class="app-switcher",
                                                        tags$div(class="tab-nav-wrapper",  
                                                                 tags$ul(class="tab-list nav",
                                                                         tags$li(class="active import-excel toggle-importer", id="import-toggle-excel",
                                                                                 HTML("<a data-toggle='tab' href='#import'>Import Excel</a>")
                                                                         ),
                                                                         tags$li(class="search toggle-importer", id="import-toggle-manual",
                                                                                 HTML("<a data-toggle='tab' href='#search'>Add values</a>")
                                                                         )))),
                                               tags$div(class="import-wrap-settings",
                                                        textInput(inputId = "import.job.name",
                                                                  label="Specify a task name"),
                                                        checkboxInput(inputId = "prioritize",
                                                                      label="Prioritize this query"),
                                                        numericInput(inputId = "process.by.others",
                                                                     value = 3,
                                                                     label = "X users should check these products"),
                                                        textInput(inputId = "state.act.id",
                                                                  label = "State Act ID, if existing"),
                                                        textInput(inputId = "import.email.adress",
                                                                  label = "Notify me when finished importing",
                                                                  placeholder = "Email address"),
                                                        checkboxInput(inputId = "update.email",
                                                                      label = "Update user email address")),
                                               tags$div(class = "import-tab tab-pane fade active in", id="import",
                                                        fileInput("import.xlsx", NULL)),
                                               tags$div(class = "search-tab tab-pane fade", id="search",
                                                        textAreaInput("manual.import.values",
                                                                      "Input values manually:")),
                                               actionButton(inputId = "finish_import",
                                                            label = "Finish import"))),
                             tags$div(id="selected_codes_output_old",
                                      tags$div(class="old-codes-close-button"),
                                      textAreaInput("selected_codes_output_old_area",
                                                    label = "These are your selected values from the query before:",
                                                    value= ""))
                             
                    )),
           
           
           tags$div(class="content",
                    tags$div(class="content-inner",
                             tags$div(class="add-new",
                                      textInput("new_hs_code",
                                                label = NULL,
                                                placeholder = "Add new code"),
                                      actionButton("register_new_hs_code",
                                                   "Add new")),
                             tags$div(class="results",
                                      tags$style("table.dataTable tr.selected td, table.dataTable tr { background-color:#f9f9f9 !important; }
                                                 table.dataTable tr.selected td, table.dataTable tr.selected { background-color:#f3f3f3 !important; }
                                                 table.dataTable tr.selected td, table.dataTable tr.group { background-color:#e7e7e7 !important; cursor:pointer; } 
                                                 table.dataTable tr.selected td, table.dataTable td { background-color:transparent !important; }
                                                 "),
                                      dataTableOutput("hstable"),
                                      tags$div(class="table-info")))),
           
           
           # CLOSE SETTINGS
           
           tags$script(src="tooltips.js"),
           tags$script('$(document).keyup(function(event) {
                       if ($("#new_hs_code").is(":focus")) {
                       if (event.key == "Enter") {
                       $("#register_new_hs_code").click();
                       }}
                                                 });'),
           tags$script('$( document ).on("click", ".group-start", function(){
                       var hscode = $(this).find(".hs4code").html();
                       Shiny.setInputValue("checkGroupSelect", hscode, {priority: "event"});
                       console.log(hscode);
           });'),
           tags$script('$( document ).on("click", ".toggle-importer", function(){
                       var importType = jQuery(this).attr("id");
                       Shiny.setInputValue("checkImporterToggle", importType, {priority: "event"});
                       console.log(importType);
           });'),
           tags$script('$( document ).on("click", ".toggle-continue", function(){
                       Shiny.setInputValue("updateSelectedCodesOutput", "go", {priority: "event"});
                       console.log("SUCCESS");
           });'),
           tags$script(HTML("$('.toggle-continue').on('click', function () {
                            $('.save-selection').toggleClass('active')
                            });")),
           tags$script(HTML("$('.close-button').on('click', function () {
                            $('.save-selection').toggleClass('active')
           });")),
           tags$script(HTML("$('.import-close-button').on('click', function () {
                            $('.import-wrap').toggleClass('active')
           });")),
           tags$script(HTML("$('.old-codes-close-button').on('click', function () {
                            $('#selected_codes_output_old').toggleClass('active')
           });")),
           tags$script(HTML("$('#unrelated-search').on('click', function () {
                            console.log('SUCCESS');
                            $('.save-selection').addClass('unrelated-search')
           });")),
           tags$script(HTML("$('#related-search').on('click', function () {
                            $('.save-selection').removeClass('unrelated-search')
           });")),
           tags$script(HTML("$('.search').on('click', function () {
                            $('#unrelated-search').removeClass('active');
                            $('#related-search').addClass('active');
                            $('.save-selection').removeClass('unrelated-search');
           });")),
           tags$script(HTML("$('.import-search-terms').on('click', function () {
                            $('#unrelated-search').removeClass('active');
                            $('#related-search').addClass('active');
                            $('.save-selection').removeClass('unrelated-search')
           });")),
           tags$script(HTML("$('.check-suggestions').on('click', function () {
                            $('#unrelated-search').removeClass('active');
                            $('#related-search').addClass('active');
                            $('.save-selection').removeClass('unrelated-search')
           });"))
  )
  
)

######## SERVER #########
library(dplyr)

# Function enabling rbinding if there are different number of columns present
server <- function(input, output, session) {
  print("START APP")
  
  data.base <- data.base.0
  data.base.backup <<- data.base.0
  data.subset <- data.subset.0
  data.subset.backup <<- data.subset.0
  data.ledger <- data.ledger.0
  data.ledger.backup <<- data.ledger.0
  importToggle <- "import-toggle-excel"

  # FUNCTION TO GET LIST OF SELECTED ROWS
  initialSelectAll <- T
  selected.rows <- function() {
    print("SELECTED.ROWS()")
    isolate({
      if (initialSelectAll == T) {
        initialSelectAll <<- F
        if (nrow(data.subset) > 0) {
          return (c(seq(1,nrow(data.subset),1)))
        } else {
          return (c())
        }
      } else {
        if (sum(data.ledger$selected) > 0) {
          existing.rows <- data.subset
          selected.codes <- data.ledger$hs.code.6[data.ledger$selected == 1]
          existing.rows$selected <- 0
          existing.rows$selected[existing.rows$hs.code.6 %in% selected.codes] <- 1
          print(paste("Function selected rows 2: ", paste(data.ledger$hs.code.6[data.ledger$selected == 1], collapse = ", ")))
          return(c(which(existing.rows$selected == 1, arr.ind = T)))
        } else {
          return (c())
        }
      }
    })
  }
  
  
  # OUTPUT TABLE
  output$hstable <- DT::renderDataTable(DT::datatable(
    names(),
    rownames = FALSE,
    escape = FALSE,
    extensions = "RowGroup",
    
    selection = list(mode="multiple", 
                     selected =   selected.rows()),
    # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
    options = list(
      pageLength = 10000,
      columnDefs = list(list(visible = FALSE, targets = c(4,5,6,7)), list(sortable=FALSE, targets = c(0))),
      rowGroup = list(startRender= JS ("function ( rows, group ) {
                                       var name =  rows.data().pluck(4);
                                       return $('<tr/>')
                                       .append( '<td><div></div></td>' )
                                       .append( '<td class=\\'hs4code\\' colspan=\\'1\\'>'+group+'</td>' )
                                       .append( '<td class=\\'probability\\' colspan=\\'1\\'></td>' )
                                       .append( '<td colspan=\\'4\\'>'+name[0]+'</td>' );}"),
                      dataSrc=5),
      language = list(
        zeroRecords = "The HS code finder could not identify any HS codes for the term. Can you help?"),
      drawCallback=JS("function (){ Tipped.create('.create-tooltip');}")
    )),
    server = T)
  
  
  first.check <- T
  
  # TABLE OUTPUT FOR PREDEFINED LIST OF WORDS
  names <- eventReactive(input$names.refresh, {
    print("NAMES REFRESH")
    code.suggested = gta_sql_load_table("code_suggested")
    
    sql <- "SELECT exit_status FROM hs_phrase_log WHERE phrase_id = ?phraseID;"
    query <- sqlInterpolate(pool,
                            sql,
                            phraseID = phr.id)
    
    exit.status=gta_sql_get_value(query)
    
    data.subset <- data.subset[with(data.subset, order(c(hs.code.6))),]
    data.subset <<- data.subset
    
    data.output <- merge(data.subset, subset(code.suggested, phrase.id == phr.id)[,c("probability","hs.code.6")], by = "hs.code.6", all.x=T)
    
    if (exit.status == 2) {
      data.output$probability.html[is.na(data.output$probability)==F] <- paste0("<div class='create-tooltip help' title = '<span>Based on previous classifications, this HS code belongs to the current search term with a probability of ",round(data.output$probability[is.na(data.output$probability)==F]*100, digits = 0)," percent.</span>'><div class='probability-bg-wrap'><div class='probability-wrap'><div class='probability probability-available' style='width:",data.output$probability[is.na(data.output$probability)==F]*100,"%; background-color:",redToGreen(data.output$probability),";'></div></div></div></div>")
      data.output$probability.html[is.na(data.output$probability)] <- paste0("<div class='create-tooltip help' title = '<span>Based on previous classifications, this HS code belongs to the current search term with a probability of 0 percent.</span>'><div class='probability-bg-wrap'><div class='probability-wrap'><div class='probability probability-none'></div></div></div></div>")
    } else {
      data.output$probability.html[is.na(data.output$probability)] <- paste0("<div class='probability-hide'><div class='probability-wrap'><div class='probability probability-none'></div></div></div>")
    }
    data.output <- unique(data.output[,c("indicator","hs.code.6","probability.html","hs.description.6","hs.description.4","hs.code.4","hs.code.2","hs.id")])
    
    row.names(data.output) <- NULL
    data.output <<- data.output
    
    
  })
  
  i = F
  observeEvent(input$users, {
    if(input$users != "Select") {
      print(paste("INITIAL DATA SUBSET SET UP: "))
      refresh_names()
      i = T
    }
  })
  
  
  proxy <- dataTableProxy('hstable')
  
  # OBSERVE SELECT ALL BUTTON
  observeEvent(input$toggle_all, {
    print(paste("OBSERVE TOGGLE ALL BUTTON: "))
    DT::selectRows(proxy, input$hstable_rows_all)
  })
  
  # OBSERVE DESELECT ALL BUTTON
  observeEvent(input$untoggle_all, {
    print(paste("OBSERVE UNTOGGLE ALL BUTTON: "))
    DT::selectRows(proxy, NULL)
  })
  
  # OBSERVE GROUP SELECT
  observeEvent(input$checkGroupSelect, {
    print(input$checkGroupSelect)
    
    returned.code <- input$checkGroupSelect
    
    if (all(subset(data.ledger, hs.code.4 %in% returned.code)$selected != 1)) {
      data.ledger$selected[data.ledger$hs.code.4 %in% returned.code]  <- 1
    } else {
      data.ledger$selected[data.ledger$hs.code.4 %in% returned.code]  <- 0
    }
    
    data.ledger <<- data.ledger  
    rows <- selected.rows()
    DT::selectRows(proxy, rows)
  })
  
  # OBSERVE CHANGES IN REFINE.QUERY
  observeEvent(input$query.refine, ignoreNULL = F, {
    if (length(input$query.refine)==0) {
      showNotification("Please select at least one word of the original phrase", duration = 3)
      updateCheckboxGroupButtons(session, "query.refine", choices = query, selected = query[1])
    }
    print(paste(input$query.refine, collapse=", "))
  })
  
  
  observeEvent(input$query.refine, {
    search.query <<- paste(paste(input$query.refine, collapse=" "))
  })
  
  output$output.query <- renderPrint({
    print(search.query)  })
  
  output$search.engines <- renderUI({
    search.query <- paste(input$query.refine, collapse=" ")
    
    HTML(paste0("<div class='search-engines'>",
                "<div class='google'>
                <a href='https://google.com/search?q=",paste("HS+Code",gsub(" ","+", search.query), sep="+"),"' target='_blank'>
                <img src='google.png' />
                </a>
                </div>",
                "<div class='duckduckgo'>
                <a href='https://duckduckgo.com/?q=",paste("HS+Code",gsub(" ","+", search.query), sep="+"),"' target='_blank'>
                <img src='duckduckgo.png' />
                </a>
                </div>",
                "<div class='zauba'>
                <a href='https://www.zauba.com/USA-htscodes/",paste(gsub(" ","+", search.query), sep="-"),"' target='_blank'>
                <img src='zauba.png' />
                </a>
                </div>",
                "<div class='etcn'>
                <a href='http://hs.e-to-china.com/ks-",paste(gsub(" ","+", search.query), sep="+"),"-d_3-t_1.html' target='_blank'>
                <img src='etcn.png' />
                </a>
                </div>",
                "<div class='eurostat'>
                <a href='https://eurostat.prod.3ceonline.com/' target='_blank'>
                <img src='eurostat.png' />
                </a>
                </div>",
                "<div class='foreign_trade'>
                <a href='https://www.foreign-trade.com/reference/hscode.htm' target='_blank'>
                <img src='foreign_trade.png' />
                </a>
                </div>
                </div>"
    ))
  })
  
  
  # IMPORT XLSX FILE
  
  observeEvent(input$import.toggle.button, {
    shinyjs::addClass(selector = ".import-wrap", class = "active")
  })
  
  #CHECK IF MANUAL OR EXCEL IMPORT IS ACTIVE
  
  # OBSERVE GROUP SELECT
  observeEvent(input$checkImporterToggle, {
    print(input$checkImporterToggle)
    
    importToggle <- input$checkImporterToggle
    importToggle <<- importToggle
    
  })
  
  observeEvent(input$finish_import, {
    
    if(input$users=="Select"){
      showNotification("Please select or create a user before finishing your import",duration = 1000)
    } else {
      
      # LOAD LOG
      importer.log <- gta_sql_load_table("importer.log")
      importer.log <<- importer.log
      
      filename = paste0(Sys.Date()," - ",max(importer.log$ticket.number)+1," - ",chosen.user,".xlsx")
      
      # UPDATE EMAIL ADDRESS
      # UPDATE EMAIL ADDRESS #2 : in case we don't have one on file
      
      if(((input$update.email == T)|(is.na(users$user.email[users$user.login == input$users])))) {
        
        sql <- "UPDATE gta_user_log SET user_email = ?newvalue WHERE user_login = ?forwhom;"
        query <- sqlInterpolate(pool, 
                                sql, 
                                forwhom = input$users,
                                newvalue = input$import.email.adress)
        print(query)
        
        gta_sql_update_table(query)
        
      }
      
      users <- gta_sql_load_table("user.log", table.prefix = "gta_")
      users <<- users
      
      # if email-field empty: get user email
      if (input$import.email.adress == "" & (is.na(users$user.email[users$user.login == input$users]) | users$user.email[users$user.login == input$users] == "")) {
        showNotification("Please input a mail address", duration = 5)
      } else {
        
        if (input$import.email.adress == "") {
          email.address <- users$user.email[users$user.login == input$users]
        } else {
          email.address = input$import.email.adress
        }
        # FILL IMPORTER LOG
        importer.log.new <- data.frame(user.id = users$user.id[users$user.login == input$users],
                                       order.email = email.address,
                                       job.name = input$import.job.name,
                                       ticket.number = max(importer.log$ticket.number)+1,
                                       time.order = Sys.time(),
                                       under.preparation = 1,
                                       xlsx.file = filename,
                                       is.priority = input$prioritize,
                                       process.by.others = input$process.by.others,
                                       related.state.act = input$state.act.id)
        importer.log.new <<- importer.log.new
        
        # importer.log.new = data.frame(user.id = 23,
        #                               order.email = "mail@patrickbuess.ch",
        #                               job.name = "Test DB",
        #                               ticket.number = max(importer.log$ticket.number)+1,
        #                               time.order = Sys.time(),
        #                               under.preparation = 1,
        #                               xlsx.file = "importfile",
        #                               is.priorityf = TRUE,
        #                               process.by.others = 4,
        #                               related.state.act = "NULL")
        
        gta_sql_append_table(append.table = "importer.log",
                             append.by.df = "importer.log.new")
        
        rm(importer.log.new)
        
        # IF EXCEL IS CHOSEN
        if (importToggle == "import-toggle-excel") {
          file = input$import.xlsx
          importfile <- openxlsx::read.xlsx(xlsxFile = file$datapath, sheet = 1, colNames = F, rowNames = F)
          
          openxlsx::write.xlsx(importfile, file=paste0(wdpath,"/xlsx imports/",filename), sheetName = "sheet", append = F, rowNames = F, colNames = F)
        } else if (importToggle == "import-toggle-manual") {
          
          import.phrases <- as.data.frame(paste(unlist(strsplit(as.character(input$manual.import.values),";"))))
          import.phrases[,1] <- trimws(import.phrases[,1], which = "both")
          import.phrases <<- import.phrases
          openxlsx::write.xlsx(import.phrases, file=paste0(wdpath,"/xlsx imports/",filename), sheetName = "sheet", append = F, rowNames = F, colNames = F)
          print(import.phrases)
        }
        
        shinyjs::removeClass(selector = ".import-wrap", class = "active")
        showNotification("Thank you. You will be notified by email once the import is completed.", duration = 60)
      }
    }
  })
  
  # SELECT ROWS MECHANISM
  selectedRow <- observe(suspended=F, { input$hstable_rows_selected 
    isolate({
      # print(paste("#Selected 1: ", length(data.ledger$hs.code.6[data.ledger$selected ==1])))
      # print(paste("#input$hstable_rows_selected 1: ", paste(input$hstable_rows_selected, collapse = ", ")))
      selected <<- subset(data.ledger, selected == 1)$hs.code.6
      # print(selected)
      selected.user <<- subset(data.ledger, user.generated == 1)$hs.code.6
      
      data.ledger$selected[data.ledger$selected == 0 & data.ledger$hs.code.6 %in% data.subset$hs.code.6[c(input$hstable_rows_selected)]]  <- 1
      data.ledger$selected[data.ledger$selected == 1 & ! data.ledger$hs.code.6 %in% data.subset$hs.code.6[c(input$hstable_rows_selected)]]  <- 0
      
      # print(paste("#Selected 2: ", length(data.ledger$hs.code.6[data.ledger$selected ==1])))
      # print(paste("#input$hstable_rows_selected 2: ", paste(input$hstable_rows_selected, collapse = ", ")))
      data.ledger.subset <<- subset(data.ledger, hs.code.6 %in% unique(data.subset$hs.code.6))
      data.ledger <<- data.ledger
      assign.global("data.ledger", data.ledger)
      
      
    })
  })
  
  selected_codes_output <- observe(suspended=F, { input$hstable_rows_selected 
    codes <- paste(data.ledger$hs.code.6[data.ledger$selected == 1], collapse = ", ")
    updateTextAreaInput(session,
                        inputId = "selected_codes_output",
                        label= "These are your selected codes:",
                        value = codes,
                        placeholder = codes
    )
  })
  
  
  # REGISTER NEW CODE
  observeEvent(input$register_new_hs_code, {
    print(paste("REGISTER NEW CODE: "))
    code <- input$new_hs_code
    if (! code %in% c(data.base$hs.code.6, data.base$hs.code.4, data.base$hs.code.2)) {
      showNotification("Suggested code not found",duration = 5)
    } else {
      returned <- gta_hs_code_check(as.numeric(code))
      returned <- as.character(sprintf("%06s",returned) %>% gsub(pattern = " ", replacement = "0", x = .))
      data.returned <- subset(data.base, hs.code.6 %in% returned) 
      
      ledger.temp <- data.ledger
      ledger.temp$user.generated[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
      ledger.temp$user.generated[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$user.generated != 1] <- 0
      ledger.temp$selected[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1 
      ledger.temp$selected[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$selected != 1] <- 0
      data.ledger <<- ledger.temp
      
      data.subset <- unique(rbind(data.returned, data.subset))
      data.subset$indicator[data.subset$hs.code.6 %in% data.ledger$hs.code.6[data.ledger$user.generated == 1]] <- "<div class='indicator user'></div>"
      row.names(data.subset) <- NULL
      data.subset <<- data.subset
      userinput <<- T
      click("names.refresh")
      reset("new_hs_code")
      
    }
  })
  
  # Conditionally show finder check button and text
  output$showFinderCheck <- reactive({
    input$query.refine
    
    code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
    code.suggested <<- code.suggested
    
    phrase.log <- change_encoding(gta_sql_load_table("phrase_log"))
    phrase.log <<- phrase.log
    
    code.source <- change_encoding(gta_sql_load_table("code_source"))
    code.source <<- code.source
    
    suggestion.ids <- subset(code.suggested, phrase.id == phr.id)$suggestion.id
    all.sources <- subset(code.source, suggestion.id %in% suggestion.ids)$source.id
    
    if(! paste(input$query.refine, collapse=" ") %in% unique(phrase.log$phrase)) {
      return(TRUE)
    } else {
      if (length(all.sources) != 0){
        if(length(all.sources[! all.sources %in% c(seq(1,8,1))]) == 0) {
          return(TRUE)
        }
      }
    }
    rm(code.suggested)
    
  })
  outputOptions(output, "showFinderCheck", suspendWhenHidden = FALSE)
  
  # Conditionally show search term infos
  output$condSearchTerm <- reactive({
    if (input$users == "Select") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  outputOptions(output, "condSearchTerm", suspendWhenHidden = FALSE)
  
  
  # CHECK IF PHRASE WAS RUN THROUGH CODE FINDER ALREADY
  output$finder_check_text <- renderUI({
    
    phrase.log <- change_encoding(gta_sql_load_table("phrase_log"))
    phrase.log <<- phrase.log
    code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
    code.suggested <<- code.suggested
    code.source <- change_encoding(gta_sql_load_table("code_source"))
    code.source <<- code.source
    
    if(! paste(input$query.refine, collapse=" ") %in% unique(phrase.log$phrase)) {
      tags$div(class="text-box",
               tags$p("Search codes for adjusted query"))
      
    } else {
      suggestion.ids <- subset(code.suggested, phrase.id == phr.id)$suggestion.id
      
      all.sources <- subset(code.source, suggestion.id %in% suggestion.ids)$source.id
      if (length(all.sources) != 0){
        if(length(all.sources[! all.sources %in% c(seq(1,8,1))]) == 0) {
          tags$div(class="text-box",
                   
                   HTML("<p><strong>Note:</strong> This term has already run through the HS code search function.</p>"))
        }
      }
    }
    rm(phrase.log, code.suggested, code.source)
  })
  
  output$finder_check_button <- renderUI({
    phrase.log <- change_encoding(gta_sql_load_table("phrase_log"))
    phrase.log <<- phrase.log
    
    if(! paste(input$query.refine, collapse=" ") %in% unique(phrase.log$phrase)) {
      tags$div(class="button",
               actionButton("search_adjusted",
                            "Search"))
    }
    rm(phrase.log)
  })
  
  # Report terms which are not a product 
  observeEvent(input$not.product, {
    
    report.services.update <- data.frame(phrase.id = phr.id,
                                         user.id = users$user.id[users$user.login == input$users],
                                         stringsAsFactors = F)
    report.services.update <<- report.services.update
    
    gta_sql_append_table(append.table = "report.services",
                         append.by.df = "report.services.update")
    rm(report.services.update)
    
    
    check.log.update <- data.frame(user.id = users$user.id[users$user.login == input$users],
                                   time.stamp = Sys.time(),
                                   check.successful = TRUE,
                                   job.id = job.phrase$job.id[job.phrase$phrase.id == phr.id],
                                   stringsAsFactors = F)
    check.log.update <<- check.log.update
    
    gta_sql_append_table(append.table = "check.log",
                         append.by.df = "check.log.update")
    rm(check.log.update)
    
    
    
    # check.phrases <<- check.phrases
    sql <- "SELECT MAX(check_id) FROM hs_check_log WHERE user_id = ?fromwhom;" # the WHERE condition is a safeguard in case another user saves at the exact same time
    query <- sqlInterpolate(pool, 
                            sql, 
                            fromwhom = users$user.id[users$user.login == input$users])
    
    this.check.id=gta_sql_get_value(query)
    rm(query)
    
    
    check.phrases.update <- data.frame(check.id = this.check.id,
                                       phrase.id = phr.id,
                                       stringsAsFactors = F)
    check.phrases.update <<- check.phrases.update
    
    gta_sql_append_table(append.table = "check.phrases",
                         append.by.df = "check.phrases.update")
    
    rm(check.phrases.update, this.check.id)
    
    refresh_names()
  })
  
  observeEvent(input$call_names_refresh, {
    refresh_names()
  })
  
  # PRINT CURRENT WORD
  output$printcurrent <- renderUI({
    HTML(paste(currentWord()))
  })
  
  # OBSERVE SELECTED VALUES
  printSelected <- reactive({
    paste(data.ledger$hs.code.6[data.ledger$selected == 1], collapse = ", ")
  })
  
  # OBSERVE SAVE SELECTION BUTTON
  observeEvent(input$save_selection, {
    save_selection("standard")
  })
  
  # OBSERVE SAVE SELECTION CLIPBOARD BUTTON
  observeEvent(input$save_selection_clipboard, {
    save_selection("clipboard")
  })
  
  # OBSERVE SAVE SELECTION NONE_FOUND BUTTON
  observeEvent(input$none_found, {
    save_selection("none_found")
  })
  
  # OBSERVE SAVE SELECTION CLIPBOARD UNRELATED SEARCH BUTTON
  observeEvent(input$save_selection_clipboard_unrelated, {
    save_selection("unrelated_search")
  })
  
  # OBSERVE CHOSEN USER VALUE
  observeEvent(input$users, {
    chosen.user <<- input$users
    updateTextInput(session,
                    "import.email.adress",
                    value = users$email[users$user.login == input$users])
  })
  
  observeEvent(input$create.user, {
    name = input$username
    print(name)
    
    if (name %in% unique(users$user.login)) {
      showNotification("This name already exists",duration = 5)
    } else {
      
      this.user.id=gta_sql_get_value("SELECT MAX(user_id) FROM gta_user_log;")
      
      users.update <- data.frame(user.id = this.user.id+1,
                                 user.login = name,
                                 user.email = "name@mail.com",
                                 stringsAsFactors = F)
      users.update <<- users.update
      
      gta_sql_append_table(append.table="user_log",
                           append.by.df = "users.update",
                           table.prefix = "gta_")
      rm(users.update, this.user.id)
      
      users <- change_encoding(gta_sql_load_table("user_log", table.prefix = "gta_"))
      users <<- users 
      updateSelectInput(session, "users", choices = unique(users$user.login), selected = name)
      reset("username")
    }
    
  })
  
  
  # Function to refresh names in table
  
  refresh_names <- function(type="check.suggestion") {
    print("REFRESH_NAMES()")
    
    all.done = F
    
    job.log <- change_encoding(gta_sql_load_table("job_log"))
    job.log <<- job.log
    job.phrase <- change_encoding(gta_sql_load_table("job_phrase"))
    job.phrase <<- job.phrase
    phrase.log <- change_encoding(gta_sql_load_table("phrase_log"))
    phrase.log <<- phrase.log
    check.phrases <- change_encoding(gta_sql_load_table("check_phrases"))
    check.phrases <<- check.phrases
    check.log <- change_encoding(gta_sql_load_table("check_log"))
    check.log <<- check.log
    users <- change_encoding(gta_sql_load_table("user_log", table.prefix = "gta_"))
    users <<- users
    code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
    code.suggested <<- code.suggested
    
    if (type == "check.suggestion") {
      
      # COUNT REMAINING PHRASES FOR USER PER JOB
      should.do <- subset(job.log, job.processed == F)
      
      if(nrow(should.do)==0) {
        
        showNotification("No more phrases to process", duration = 5)
        
      } else {
        
        should.do$remaining <- 0
        
        for(j.id in unique(should.do$job.id)) {
          should.do$remaining[should.do$job.id == j.id] <- nrow(subset(job.phrase, job.id == j.id & processed == F & ! phrase.id %in% subset(check.phrases, check.id %in% subset(check.log, user.id == users$user.id[users$user.login == input$users])$check.id)$phrase.id))
        }
        should.do <- subset(should.do, remaining != 0)
        
        # ORDER JOBS BY PRIORITY AND REMAINING PHRASES, CHOOSE JOB ID FROM ROW 1
        if (nrow(should.do)>0) {
          should.do$is.priority <- ifelse(should.do$is.priority, 1, 0)
          should.do <- should.do[with(should.do, order(-is.priority, remaining)),]
          
          job.id <- should.do$job.id[1]
          job.id <<- job.id
          jobID.temp <- job.id
          jobID.temp <<- jobID.temp
          
          should.do <- job.phrase$phrase.id[job.phrase$job.id == jobID.temp & job.phrase$processed == FALSE & ! job.phrase$phrase.id %in% subset(check.phrases, check.id %in% subset(check.log, user.id == users$user.id[users$user.login == input$users] & job.id == jobID.temp)$check.id)$phrase.id]
          should.do <<- should.do
          
          rm(jobID.temp)
          
        } else {
          showNotification("You are all done, thank you!", duration = 1000)
          all.done = T
        }
        
        
        if (all.done == F) {
          
          if(length(should.do)>1) {
            phr.id <<- sample(should.do,1)
            # phr.id <<- 2326
          } else {
            phr.id <<- should.do
            # phr.id <<- 2326
          }
          
          query <<- paste(unlist(strsplit(as.character(phrase.log$phrase[phrase.log$phrase.id == phr.id])," ")))
          updateCheckboxGroupButtons(session, "query.refine", choices = query, selected = query)
          
          data.subset <- subset(data.base, hs.code.6 %in% subset(code.suggested, phrase.id == phr.id)$hs.code)
          row.names(data.subset) <- NULL
          data.subset <<- data.subset
          
        } else {
          
          phr.id <<- 0
          query <<- ""
          updateCheckboxGroupButtons(session, "query.refine", choices = query, selected = query)
          
          data.subset <- data.base[NULL,]
          row.names(data.subset) <- NULL
          data.subset <<- data.subset
        }
        
      }
    }
    
    print(paste0("JOB ID:", job.id))
    print(paste0("PHRASE ID:", phr.id))
    
    if (type=="empty") {
      
      phr.id <<- 0
      query <<- ""
      updateCheckboxGroupButtons(session, "query.refine", choices = query, selected = query)
      
      data.subset <- data.base[NULL,]
      row.names(data.subset) <- NULL
      data.subset <<- data.subset
      
    }
    
    # To keep track of changes in the apps state
    data.ledger$selected <- 0
    data.ledger$selected[data.ledger$hs.code.6 %in% unique(data.subset$hs.code.6)] <- 1
    data.ledger <<- data.ledger
    assign.global("data.ledger",data.ledger)
    assign.global("phr.id",phr.id)
    
    click("names.refresh")
    
    rm(job.phrase, check.phrases, check.log, users, code.suggested, job.log, phrase.log)
  }
  
  # Functions for HS Code finder App
  
  save_selection <- function(type) {
    print("SAVE_SELECTION()")
    # load_all(path) 
    toggleClass("loading","active")
    
    if(input$users=="Select"){
      showNotification("Please select or create a user before saving your selection",duration = 1000)
    } else if (is.null(input$radio1)==T) {
      showNotification("Please select a confidence level",duration = 5)
    } else {
      
      
      
      if (type %in% c("standard","clipboard")) {
        
        # Variables catching the number of rows to be inserted, to check later if insertion was successful
        checks <- list()
        
        # check if phrase has been adjusted
        # phrase.log
        phrase.log <- change_encoding(gta_sql_load_table("phrase_log"))
        phrase.log <<- phrase.log
        
        if (! tolower(paste(input$query.refine, collapse=" ")) %in% unique(tolower(phrase.log$phrase))) {
          
          phrase.log.update <- data.frame(phrase.id = 123456789,
                                          phrase = tolower(paste(input$query.refine, collapse = " ")),
                                          source = "adjusted",
                                          processing.round=1,
                                          stringsAsFactors = F)
          phrase.log.update <<- phrase.log.update
          
          gta_sql_append_table(append.table = "phrase.log",
                               append.by.df = "phrase.log.update")
          rm(phrase.log.update)
          checks <- c(checks, list("nrow.phraselog" = 1))
          
          new.phr.id=gta_sql_get_value(paste0("SELECT phrase_id 
                                          FROM hs_phrase_log
                                          WHERE phrase='",tolower(paste(input$query.refine, collapse = " ")),"'
                                          AND source='adjusted';"))
          
          if(is.na(new.phr.id)){
            stop("SAVE_SELECTION - new phrase: The phrase log update went wrong. I cannot find the phrase I just added.")
          }  else {
            
            new.phr.id <<- new.phr.id  
            
          }
          
          # update code.suggested to include the values of the original phrase ID for the new one
          new.code.suggested=gta_sql_get_value(paste0("SELECT *
                                                        FROM hs_code_suggested
                                                        WHERE phrase_id =",phr.id,";"))
          
          new.code.suggested$phrase.id=new.phr.id
          # new.code.suggested$suggestion.id <- seq(123456789, 123456789+(nrow(new.code.suggested))-1,1)
          # print("NEW CODE SUGGESTED")
          # print(new.code.suggested)
          
          gta_sql_append_table(append.table="code.suggested",
                               append.by.df = "new.code.suggested")
          checks <- c(checks, list("newphrase.suggested.new" = nrow(new.code.suggested)))
          rm(new.code.suggested)
          
          
          # update code.source to include the values of the original phrase ID for the new one
          old.code.suggested=gta_sql_get_value(paste0("SELECT *
                                                        FROM hs_code_suggested
                                                        WHERE phrase_id =",phr.id,";"))
          
          old.code.source=gta_sql_get_value(paste0("SELECT *
                                                        FROM hs_code_source
                                                        WHERE suggestion_id IN(",paste(old.code.suggested$suggestion.id, collapse=","),");"))
          
          new.code.suggested=gta_sql_get_value(paste0("SELECT *
                                                        FROM hs_code_suggested
                                                      WHERE phrase_id =",new.phr.id,";"))
          
          
          new.code.source=merge(old.code.suggested[,c("hs.code.6","suggestion.id")],
                                old.code.source, by="suggestion.id")
          new.code.source$suggestion.id=NULL
          
          new.code.source=merge(new.code.suggested[,c("hs.code.6","suggestion.id")],
                                new.code.source, by="hs.code.6")
          
          new.code.source<<-unique(new.code.source[,c("suggestion.id","source.id")])
          
          gta_sql_append_table(append.table="code.source",
                               append.by.df = "new.code.source")
          checks <- c(checks, list("newphrase.source.new" = nrow(new.code.source)))
          
          rm(new.code.source, old.code.source, new.code.suggested, old.code.suggested)
          
        } else {
          new.phr.id <<- numeric()
        }
        rm(phrase.log)
        
        
        
        # codes.suggested
        code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
        code.suggested <<- code.suggested
        
        suggested.new <- subset(data.ledger, (user.generated == 1 | search.generated == 1) & selected == 1)
        suggested.new <- subset(suggested.new, ! hs.code.6 %in% subset(code.suggested, phrase.id == phr.id)$hs.code.6)
        
        checks <- c(checks, list("suggested.old" = nrow(subset(code.suggested, phrase.id == phr.id))))
        checks <- c(checks, list("suggested.new" = nrow(suggested.new)))
        
        rm(code.suggested)
        
        # update code.suggested with user-provided information
        if (nrow(suggested.new) != 0) {
          
          # looping over phrase ids 
          for(p.id in c(phr.id, new.phr.id)){
            
            suggested.new$phrase.id = p.id
            suggested.new$probability=NA
            
            code.suggested.update <- suggested.new[,c("phrase.id","hs.code.6","probability")]
            code.suggested.update <<- code.suggested.update
            
            gta_sql_append_table(append.table = "code.suggested",
                                 append.by.df = "code.suggested.update")
            
            # Get newly added suggestion.ids, because of primary key, they can differ from the ones in this environment
            sql <- "SELECT * FROM hs_code_suggested WHERE phrase_id = ?phraseID;" 
            query <- sqlInterpolate(pool, 
                                    sql, 
                                    phraseID = p.id)
            
            code.suggested.new=gta_sql_get_value(query)
            code.suggested.new = subset(code.suggested.new, hs.code.6 %in% code.suggested.update$hs.code.6)
            
            
            if (length(suggested.new$hs.code.6[suggested.new$user.generated == 1]) > 0) {
              
              code.source.update <- data.frame(source.id=1,
                                               suggestion.id = subset(code.suggested.new, hs.code.6 %in% subset(suggested.new, user.generated == 1)$hs.code.6)$suggestion.id,
                                               stringsAsFactors = F)
              
              code.source.update <<- code.source.update
              
              gta_sql_append_table(append.table = "code.source",
                                   append.by.df = "code.source.update")
              
              rm(code.source.update)
              
            }
            
          }
          
        }
        
        
        # CREATE NEW CHECK and store its ID
        # Check.log
        c.time=Sys.time()
        check.log.update <- data.frame(check.id = 123456789,
                                       user.id = users$user.id[users$user.login == input$users],
                                       time.stamp = c.time,
                                       check.successful = TRUE,
                                       job.id = job.id)
        check.log.update <<- check.log.update
        
        gta_sql_append_table(append.table = "check.log",
                             append.by.df = "check.log.update")
        rm(check.log.update)
        
        c.log=gta_sql_get_value(paste0("SELECT *
                                       FROM hs_check_log
                                       WHERE check_id = (SELECT MAX(check_id)  
                                       FROM hs_check_log
                                       WHERE user_id =",users$user.id[users$user.login == input$users],");"))
        
        if(as.POSIXct(c.log$time.stamp)==as.character(c.time)){
          this.check.id<<-c.log$check.id
          rm(c.log, c.time)
          
        } else {
          stop("Check log update failed")
        }
        
        # Add code.selected
        code.suggested <- change_encoding(gta_sql_load_table("code_suggested"))
        code.suggested <<- code.suggested
        
        code.selected.new <- subset(code.suggested, hs.code.6 %in% subset(data.ledger, selected == 1)$hs.code.6 & phrase.id == phr.id)
        
        checks <- c(checks, list("selected.new" = nrow(code.selected.new)))
        
        if (nrow(code.selected.new) > 0) {
          
          code.selected.update <- data.frame(check.id = this.check.id,
                                             suggestion.id = code.selected.new$suggestion.id)
          code.selected.update <<- code.selected.update
          
          gta_sql_append_table(append.table = "code.selected",
                               append.by.df = "code.selected.update")
          
          print(code.selected.update)
          
          rm(code.selected.update, code.selected.new)
        }
        
        
        # Check.phrases
        checks <- c(checks, list("check.phrases.new" = 0))
        for(p.id in c(phr.id, new.phr.id)){
          
          checks[['check.phrases.new']] <- checks[['check.phrases.new']]+1
          
          check.phrases.update <- data.frame(check.id = this.check.id,
                                             phrase.id = p.id)
          check.phrases.update <<- check.phrases.update
          
          gta_sql_append_table(append.table = "check.phrases",
                               append.by.df = "check.phrases.update")
          
          rm(check.phrases.update)
          
        }
        
        
        # words.removed
        words.all <- paste(unlist(strsplit(as.character(tolower(phrase.log$phrase[phrase.log$phrase.id == phr.id]))," ")))
        removed <- words.all[! words.all %in% paste(unlist(strsplit(as.character(tolower(input$query.refine))," ")))]
        
        checks <- c(checks, list("words.removed" = length(removed)))
        
        if (length(removed) > 0) {
          
          words.removed.update <- data.frame(check.id = c(rep(this.check.id, length(removed))),
                                             words.removed = removed)
          words.removed.update <<- words.removed.update
          
          gta_sql_append_table(append.table = "words.removed",
                               append.by.df = "words.removed.update")
        }
        
        
        if (input$suggestions.search.terms != "") {
          
          additional.suggestions.phrases=as.data.frame(strsplit(input$suggestions.search.terms,split=';', fixed=TRUE))
          
          additional.suggestions.update <- data.frame(check.id = this.check.id,
                                                      term = additional.suggestions.phrases[,1],
                                                      user.id = users$user.id[users$user.login == input$users])
          additional.suggestions.update <<- additional.suggestions.update
          
          gta_sql_append_table(append.table = "additional.suggestions",
                               append.by.df = "additional.suggestions.update")
          
          checks <- c(checks, list("additional.suggestions" = nrow(additional.suggestions.update)))
          
          rm(additional.suggestions.phrases, additional.suggestions.update)
        }
        
        # check.certainty
        
        check.certainty.update <- data.frame(check.id = this.check.id,
                                             certainty.level = input$radio1)
        check.certainty.update <<- check.certainty.update
        
        gta_sql_append_table(append.table = "check.certainty",
                             append.by.df = "check.certainty.update")
        
        checks <- c(checks, list("check.certainty" = nrow(check.certainty.update)))
        
        # Updating job.phrase (only for original phrase.id, not new phrase id [if exists])
        successful.checks=gta_sql_get_value(paste0("SELECT COUNT(DISTINCT check_id)
                                            FROM hs_check_log
                                            WHERE check_successful=1
                                            AND check_id IN (
                                                   SELECT check_id
                                                   FROM hs_check_phrases
                                                   WHERE phrase_id =",phr.id,"
                                            );"))
        
        
        
        ## looping over all jobs that happen to include this phrase
        jobs.incl.phrase=gta_sql_get_value(paste0("SELECT job_id
                                                 FROM hs_job_phrase
                                                  WHERE phrase_id =",phr.id,";"))
        
        if(is.na(jobs.incl.phrase[1])==F){
          
          for(j.id in jobs.incl.phrase){
            required.checks=gta_sql_get_value(paste0("SELECT nr_of_checks
                                                     FROM hs_job_log
                                                     WHERE job_id =",j.id,";"))
            
            
            
            ## EXCLUDE JOBS THAT ARE ALREADY MARKED AS PROCESSED AS TO PREVENT THEM FROM ADDING +1 TO THE NR OF COMPLETED JOBS
            nround <- gta_sql_get_value(sqlInterpolate(pool, "SELECT processing_round FROM hs_phrase_log WHERE phrase_id = ?phraseID;", phraseID = phr.id))
            exit.status = 1
            
            if (nround >= 4) {
              exit.status <- 5
              
            } else {
              
              # CHECK IF PROBABILITIES SHOULD BE CALCULATED FOR THIS PHRASE
              # JF on 21 Nov 2019: I set this to be checked at every round now.
              calc.prob = successful.checks >= required.checks
              # if(nround==1){
              #   # CHECK HOW MANY TOTAL CHECKS HAVE BEEN DONE AND IF IT'S HIGHER THAN THE REQUIRED ONES
              #   calc.prob = successful.checks >= required.checks
              # } else {
              #   # Check how many times the phrase has been processed in this job already
              #   calc.prob = successful.checks %% (nround*required.checks) == 0
              # }
              
              print(paste0("JOB ID: ",j.id))
              print(paste0("PHRASE ID: ",phr.id))
              print(paste0("SUCCESSFUL CHECKS: ",successful.checks))
              print(paste0("REQUIRED CHECKS: ",required.checks))
              print(paste0("PROCESSING ROUND: ",nround))
              print(paste0("CALC PROB: ",calc.prob))
              
              if(calc.prob){
                
                # DECIDE EXIT STATUS
                # 2 (PROCESSED) IF CODE SELECTED AND CODE SUGGESTED ARE AVAILABLE
                # 3 (NOT A PRODUCT) IF MAJORITY OF CHECKS LABEL AS "NOT A PRODUCT"
                # 4 (NO CODES) IF CHECKED ENOUGH TIMES BUT NO CODES HAVE BEEN SELECTED FOR THIS PHRASE
                # 5 (ROUND LIMIT) IF NROUND >=4
                
                sql <- "SELECT * FROM hs_report_services WHERE phrase_id = ?phraseID;"
                query <- sqlInterpolate(pool,
                                        sql,
                                        phraseID = phr.id)
                services=gta_sql_get_value(query)
                
                sql <- "SELECT * FROM hs_check_phrases WHERE phrase_id = ?phraseID;"
                query <- sqlInterpolate(pool,
                                        sql,
                                        phraseID = phr.id)
                all.checks=gta_sql_get_value(query)
                
                # comment PB: 
                # Check.phrases should be reloaded from DB again, as there should be a new check in there now
                # Also this condition seems wrong: Reported services will be accounted for in the check.phrases table as well,
                # that means check.phrases will always have more instances of the phrase than report.services. It should look like this I believe:
                if (nrow(unique(services))>(nrow(unique(all.checks))/2)) {
                  exit.status <- 3
                  
                } else {
                  
                  nr.chosen.codes=gta_sql_get_value(paste0("SELECT COUNT(DISTINCT suggestion_id)
                                                           FROM hs_code_selected
                                                           WHERE check_id IN (
                                                           SELECT check_id 
                                                           FROM hs_check_phrases
                                                           WHERE phrase_id = ",phr.id,"
                                                           );"))
                  
                  
                  if(nr.chosen.codes==0) {
                    # no codes found
                    exit.status <- 4
                    
                  } else {
                    
                    # SAVE PROBABILITIES FOR THAT PHRASE
                    phr.id.probability.future <- phr.id
                    future({ gta_hs_classify_results(processed.phrase = phr.id,
                                                     job.id=j.id) }) %...>% {
                                                       print(paste0("Phrase ",phr.id.probability.future," processed"))
                                                     }
                    
                    
                  }
                }
              }
            }
            
            if (exit.status %in% c(3,4,5)) {
              sql <- "UPDATE hs_phrase_log SET exit_status = ?exitStatus WHERE phrase_id = ?phraseID;"
              query <- sqlInterpolate(pool,
                                      sql,
                                      exitStatus = exit.status,
                                      phraseID = phr.id)
              gta_sql_update_table(query)
              
              sql <- "UPDATE hs_job_phrase SET processed = 1 WHERE (phrase_id = ?phraseID AND job_id = ?jobID);"
              query <- sqlInterpolate(pool,
                                      sql,
                                      phraseID = phr.id,
                                      jobID = j.id)
              gta_sql_update_table(query)
            }
            
            rm(required.checks)
            
            
            
            # Check if job is fully processed 
            # (could be more than one as one phrase may be part of more than one job)
            
            
            remaining.phrases=gta_sql_get_value(paste0("SELECT COUNT(DISTINCT phrase_id)
                                                       FROM hs_job_phrase
                                                       WHERE processed = FALSE 
                                                       AND job_id = ",j.id,";"))
            
            if(remaining.phrases==0){        
              
              sql <- "UPDATE hs_job_log SET job_processed = true WHERE job_id = ?jobID;"
              query <- sqlInterpolate(pool, 
                                      sql, 
                                      jobID = j.id)
              
              gta_sql_update_table(query)
              
              sql <- "UPDATE hs_job_log SET phrases_remaining = 0 WHERE job_id = ?jobID;"
              query <- sqlInterpolate(pool, 
                                      sql, 
                                      jobID = j.id)
              
              gta_sql_update_table(query)
              
              job.id.future <- j.id
              future({ gta_hs_process_completed_job(processed.job=job.id.future, path = wdpath) }) %...>% {
                print("JOB PROCESSED")
              }
              
            } else {
              
              sql <- "UPDATE hs_job_log SET phrases_remaining = ?left WHERE job_id = ?jobID;"
              query <- sqlInterpolate(pool, 
                                      sql, 
                                      left = remaining.phrases,
                                      jobID = j.id)
              
              gta_sql_update_table(query)
            }
            
            
            
          }
          
        }
        
        # future({ gta_hs_check_success(check.id = this.check.id,
        #                               checks = checks,
        #                               phrase.id=c(phr.id, new.phr.id),
        #                               recipients=c("patrick.buess@student.unisg.ch")) }) %...>% {
        #                                 print("Check checked successfully")
        #                               }
        
        
        
      }
      
      if (type == "none_found") {
        # Check.log
        
        # CREATE NEW CHECK and store its ID
        # Check.log
        c.time=Sys.time()
        check.log.update <- data.frame(check.id = 123456789,
                                       user.id = users$user.id[users$user.login == input$users],
                                       time.stamp = c.time,
                                       check.successful = FALSE,
                                       job.id = job.id)
        check.log.update <<- check.log.update
        
        gta_sql_append_table(append.table = "check.log",
                             append.by.df = "check.log.update")
        
        rm(check.log.update)
        
        c.log=gta_sql_get_value(paste0("SELECT *
                                       FROM hs_check_log
                                       WHERE check_id = (SELECT MAX(check_id)  
                                       FROM hs_check_log
                                       WHERE user_id =",users$user.id[users$user.login == input$users],");"))
        
        if(as.POSIXct(c.log$time.stamp)==as.character(c.time)){
          this.check.id<<-c.log$check.id
          rm(c.log, c.time)
          
        } else {
          stop("Check log update failed")
        }
        
        
        # Check.phrases
        check.phrases <- change_encoding(gta_sql_load_table("check_phrases"))
        check.phrases <<- check.phrases
        
        check.phrases.update <- data.frame(check.id = this.check.id,
                                           phrase.id = phr.id)
        check.phrases.update <<- check.phrases.update
        
        gta_sql_append_table(append.table = "check.phrases",
                             append.by.df = "check.phrases.update")
        
        rm(check.phrases.update)
        
        # check.certainty
        
        check.certainty <- change_encoding(gta_sql_load_table("check_certainty"))
        check.certainty <<- check.certainty
        
        check.certainty.update <- data.frame(check.id = this.check.id,
                                             certainty.level = input$radio1)
        check.certainty.update <<- check.certainty.update
        
        gta_sql_append_table(append.table = "check.certainty",
                             append.by.df = "check.certainty.update")
        
        rm(check.certainty.update)
        
      }
      
      
      # START NEW ASYNC HS_CODE_FINDER SEARCH FOR THAT TERM
      phr.id.future <- phr.id
      query.refine.future <- input$query.refine
      # query.refine.future <<- "keyring"
      # phr.id.future <<- 2561
      
      future({ gta_hs_code_finder(products = tolower(paste(query.refine.future, collapse=" ")))}) %...>%  {
        found.temp <- .
        # found.temp <- gta_hs_code_finder(products = tolower(paste(query.refine.future, collapse=" ")))
        
        phrase.log.future <- gta_sql_load_table("phrase.log")
        code.suggested.future <- gta_sql_load_table("code.suggested")
        suggestion.sources.future <- gta_sql_load_table("suggestion.sources")
        
        codes <- code.suggested.future$hs.code.6[code.suggested.future$phrase.id == phr.id.future]
        new.codes <- subset(found.temp, ! hs.code %in% codes)
        
        if (nrow(new.codes)>0) {
          new.codes <- new.codes[,c("hs.code","source.names")]
          new.codes$phrase.id <- phr.id.future
          new.codes$suggestion.id <- seq(max(code.suggested.future$suggestion.id)+1,max(code.suggested.future$suggestion.id)+nrow(new.codes),1)
          names(new.codes) <- c("hs.code.6","source.names","phrase.id","suggestion.id")
          new.codes$probability=NA
          
          # code.suggested <- rbind(code.suggested, new.codes[,c("hs.code.6","phrase.id","suggestion.id", "probability")])
          # code.suggested <<- code.suggested
          
          code.suggested.update.future <- new.codes[,c("hs.code.6","phrase.id","suggestion.id", "probability")]
          code.suggested.update.future <<- code.suggested.update.future
          
          gta_sql_append_table(append.table = "code.suggested",
                               append.by.df = "code.suggested.update.future")
          
          # Get newly added suggestion.ids, because of primary key, they can differ from the ones in this environment
          sql <- "SELECT * FROM hs_code_suggested WHERE phrase_id = ?phraseID;" # the WHERE condition is a safeguard in case another user saves at the exact same time
          query <- sqlInterpolate(pool, 
                                  sql, 
                                  phraseID = phr.id.future)
          
          new.codes.current=gta_sql_get_value(query)
          new.codes.current = subset(new.codes.current, hs.code.6 %in% code.suggested.update.future$hs.code.6)
          
          new.codes <- merge(new.codes[,c("source.names","hs.code.6")], new.codes.current[,c("hs.code.6","suggestion.id")], by="hs.code.6")[,c("source.names","suggestion.id")]
          
          new.codes <- cSplit(new.codes, which(colnames(new.codes)=="source.names"), direction="long", sep=";")
          names(new.codes) <- c("source.name","suggestion.id")
          new.codes <- merge(new.codes, suggestion.sources.future, by="source.name", all.x=T)
          
          # code.source <- rbind(code.source, new.codes[,c("source.id","suggestion.id")])
          
          code.source.update.future <- new.codes[,c("source.id","suggestion.id")]
          code.source.update.future <<- code.source.update.future
          
          gta_sql_append_table(append.table = "code.source",
                               append.by.df = "code.source.update.future")
          
          rm(code.source.update.future, code.suggested.update, codes,found.temp, new.codes.current)
          
          print("Number of new codes found:")
          print(nrow(new.codes))
          print("ALL DONE ASYNC")
        }
        rm(new.codes)
      }
      
      
      if (type %in% c("clipboard","unrelated_search")) {
        runjs("var copyText = document.getElementById('selected_codes_output');
              copyText.select();
              document.execCommand('copy');
              //alert('Copied the text: ' + copyText.value);
              ")
        # clipr::write_clip(printSelected())
      }
      
      codes.old <- paste(data.ledger$hs.code.6[data.ledger$selected == 1], collapse = ", ")
      updateTextAreaInput(session,
                          inputId = "selected_codes_output_old_area",
                          label= "These are your selected codes from the query before:",
                          value = codes.old,
                          placeholder = codes.old
      )
      
      toggleClass("selected_codes_output_old","active")
      
      toggleClass("save-selection","active")
      toggleClass("loading","active")
      refresh_names()
    }
    
  }
  
}

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