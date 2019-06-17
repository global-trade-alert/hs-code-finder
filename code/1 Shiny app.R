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
# library(profvis)
plan(multiprocess)
# gta_update_library()

rm(list = ls())

setwd("/home/rstudio/Dropbox/GTA cloud")
# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

path="17 Shiny/5 HS code finder/database/GTA HS code database.Rdata"

## helpful functions
## HS app functions
for(fct in list.files("17 Shiny/5 HS code finder/code/functions", pattern = ".R", full.names=T)){
  source(fct)
}




# # CODE TO REMOVE PHRASES/JOBS/CHECKS MANUALLY
# load_all()
# phrase.to.remove <- c(1270)
# 
# phrase.table <- subset(phrase.table, ! phrase.id %in% phrase.to.remove)
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
# save_all()


# Build starting set
load_all()

data.base.0 = hs.codes
data.base.0$indicator = "<div class='indicator'></div>"
data.base.0 <- merge(data.base.0, hs.descriptions, by="hs.id", all.x=T)
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
                                choices = c("Choose User"="Select",unique(users$name)),
                                multiple = F)),
           tags$div(class="logo",
                    img(src="gta logo-white.svg"))),
  
  tags$div(class="overall-wrap",
           tags$div(class = "settings",
                    tags$div(class = "settings-inner",
                             tags$div(class="app-switcher",
                                      tags$div(class="tab-nav-wrapper",  
                                               tags$ul(class="tab-list nav",
                                                       tags$li(class="active check-suggestions",
                                                               HTML("<a data-toggle='tab' href='#term'>Check <br/>suggestions</a>")
                                                       ),
                                                       tags$li(class="search",
                                                               HTML("<a data-toggle='tab' href='#search'>Search <br/>codes</a>")
                                                       ),
                                                       tags$li(class="import-search-terms",
                                                               HTML("<a data-toggle='tab' href='#import'>Import <br/>search terms</a>")
                                                       )))),
                             conditionalPanel(condition = "output.condSearchTerm",
                                              tags$div(class = "search-field app-switcher-search",
                                                       
                                                       tags$div(class="search-tab tab-pane fade", id="search",
                                                                tags$div(class="tab-nav-wrapper",
                                                                         tags$ul(class="tab-list nav",
                                                                                 tags$li(id="related-search", class="active",
                                                                                         HTML("<a data-toggle='tab' href='#related'>Related to <br/>suggestion</a>")
                                                                                 ),
                                                                                 tags$li(id="unrelated-search", class="",
                                                                                         HTML("<a data-toggle='tab' href='#unrelated'>Own search</a>")))),
                                                                tags$div(class="related tab-pane fade active in", id="related",
                                                                         
                                                                         HTML("<p class='search-term'>Search term:</p>"),
                                                                         actionButton("call_hs_code_finder",
                                                                                      "Search"),
                                                                         # USE LATER WHEN SEARCH IS REQUIRED
                                                                         textInput(inputId = 'search.field',
                                                                                   label = NULL,
                                                                                   placeholder = "Search...")),
                                                                tags$div(class="unrelated tab-pane fade", id="unrelated",
                                                                         
                                                                         HTML("<p class='search-term'>Search term:</p>"),
                                                                         actionButton("call_hs_code_finder_unrelated",
                                                                                      "Search"),
                                                                         # USE LATER WHEN SEARCH IS REQUIRED
                                                                         textInput(inputId = 'search.field.unrelated',
                                                                                   label = NULL,
                                                                                   placeholder = "Search..."),
                                                                         tags$div(class="selection-unrelated-wrap",
                                                                                  # tags$p("These are your selected values:"),
                                                                                  textAreaInput("unrelated_selected_codes",
                                                                                                label=NULL,
                                                                                                placeholder="")
                                                                         ))),
                                                       
                                                       tags$div(class = "refine-query tab-pane active fade in", id="term",
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
                                                       tags$div(class = "import-tab tab-pane fade", id="import",
                                                                actionButton("import_uploaded_file",
                                                                             "Import"),
                                                                HTML("<p class='search-term'>Import Excel:</p>"),
                                                                fileInput("import.xlsx", NULL))
                                              ),
                                              tags$div(class = "refresh-button",
                                                       actionButton("names.refresh",
                                                                    "Next term"))),
                             # USE TO DISPLAY SELECTION
                             # tags$div(class = "choices",
                             #          htmlOutput('selected')),
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
                                               actionButton("save_selection_clipboard_unrelated",
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
                                               textInput(inputId = "import.job.name",
                                                         label="Name of Import"),
                                               checkboxInput(inputId = "prioritize",
                                                             label="Prioritize this query"),
                                               # checkboxInput(inputId = "process.by.me",
                                               #               label="Have this query processed by me"),
                                               numericInput(inputId = "process.by.others",
                                                            value = 0,
                                                            label = "Have this query processed by X others"),
                                               textInput(inputId = "state.act.id",
                                                         label = "State Act ID, if existing"),
                                               textInput(inputId = "import.email.adress",
                                                         label = "Notify me when finished importing",
                                                         placeholder = "Email address",
                                                         value = "your-email"),
                                               checkboxInput(inputId = "update.email",
                                                             label = "Update user email adress"),
                                               actionButton(inputId = "finish.import",
                                                            label = "Finish Import"))),
                             tags$div(id="selected_codes_output_old",
                                      tags$div(class="old-codes-close-button"),
                                      textAreaInput("selected_codes_output_old_area",
                                                    label = "These are your selected values from the query before:",
                                                    value= ""))
                             # SAVE TO CLIPBOARD FUNCTIONALITY
                             # tags$div(class="clipboard",
                             #          rclipboardSetup(),
                             #          uiOutput("clip")
                             
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
  data.subset <- data.subset.0
  data.ledger <- data.ledger.0
  
  # Add clipboard buttons
  # output$clip <- renderUI({
  # rclipButton("clipbtn", "Copy to Clipboard", printSelected(), icon("clipboard"))
  # })
  
  # Workaround for execution within RStudio
  # observeEvent(input$clipbtn, clipr::write_clip(printSelected()))
  
  # observeEvent(input$checkGroupSelect, {
  # showNotification(input$checkGroupSelect)
  # })
  
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
    # colnames = c("Code","","","Name","",""),
    escape = FALSE,
    extensions = "RowGroup",
    
    selection = list(mode="multiple", 
                     selected =   selected.rows()),
    # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
    # as.numeric(row.names(data.subset)[data.subset$hs.code.6 %in% data.base$hs.code.6[data.base$selected == 1]])
    options = list(
      pageLength = 10000,
      columnDefs = list(list(visible = FALSE, targets = c(3,4,5,6)), list(sortable=FALSE, targets = c(0))),
      rowGroup = list(startRender= JS ("function ( rows, group ) {
                                       var name =  rows.data().pluck(3);
                                       return $('<tr/>')
                                       .append( '<td><div></div></td>' )
                                       .append( '<td class=\\'hs4code\\' colspan=\\'1\\'>'+group+'</td>' )
                                       .append( '<td colspan=\\'4\\'>'+name[0]+'</td>' );}"),
                      dataSrc=4),
      language = list(
        zeroRecords = "The HS code finder could not identify any HS codes for the term. Can you help?")
      )),
    server = T)
  
  # CALL HS CODE FINDER
  observeEvent(input$call_hs_code_finder, {
    toggleClass("loading","active")
    
    phrase = tolower(input$search.field)
    
    if (phrase %in% unique(tolower(phrase.table$phrase))) {
      data.returned <- subset(data.base, hs.code.6 %in% subset(code.suggested, phrase.id == phrase.table$phrase.id[tolower(phrase.table$phrase)==phrase])$hs.code.6)
      toggleClass("loading","active")
      
      data.returned$indicator <- "<div class='indicator search'></div>"
      
      #Adjust ledger
      ledger.temp <- data.ledger
      ledger.temp$search.generated[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
      ledger.temp$search.generated[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$search.generated != 1] <- 0
      ledger.temp$selected[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1 
      ledger.temp$selected[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$selected != 1] <- 0
      print(paste("length of search generated: ",length(ledger.temp[ledger.temp$search.generated == 1, ]$hs.code.6)))
      data.ledger <<- ledger.temp
      assign.global("data.ledger",data.ledger)
      data.subset <- subset(data.subset, ! hs.code.6 %in% data.returned$hs.code.6)
      data.subset <- rbind(data.subset, data.returned)
      row.names(data.subset) <- NULL
      data.subset <<- data.subset
      
      searchinput <<- T
      click("names.refresh")
      
    } else{
      future({
        gta_hs_code_finder(products = phrase)
      }) %...>% {
        retrieved.data <- .
        toggleClass("loading","active")
        if (nrow(retrieved.data) == 0) {
          showNotification("No codes found for this search term",duration = 1000)
        } else {
          if (! exists("search.sources")) {
            search.sources <- data.frame(hs.code.6 = character(),
                                         source.id = numeric(),
                                         product.name = character())
          }
          search.sources.temp = retrieved.data[,c("hs.code","source.names","product.name")]
          search.sources.temp <- cSplit(search.sources.temp, which(colnames(search.sources.temp)=="source.names"), direction="long", sep=";")
          names(search.sources.temp) <- c("hs.code.6","source.name","product.name")
          search.sources.temp <- merge(search.sources.temp, suggestion.sources, by="source.name",all.x=T)
          search.sources <<- unique(rbind(search.sources, search.sources.temp[,c("hs.code.6","source.id","product.name")]))
          rm(search.sources.temp)
          
          returned <- retrieved.data$hs.code
          returned <- as.character(sprintf("%06s",returned) %>% gsub(pattern = " ", replacement = "0", x = .))
          data.returned <- subset(data.base, hs.code.6 %in% returned) 
          data.returned$indicator <- "<div class='indicator search'></div>"
          
          
          #Adjust ledger
          ledger.temp <- data.ledger
          ledger.temp$search.generated[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
          ledger.temp$search.generated[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$search.generated != 1] <- 0
          ledger.temp$selected[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1 
          ledger.temp$selected[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$selected != 1] <- 0
          data.ledger <<- ledger.temp
          assign.global("data.ledger",data.ledger)
          data.subset <- subset(data.subset, ! hs.code.6 %in% data.returned$hs.code.6)
          data.subset <- rbind(data.subset, data.returned)
          row.names(data.subset) <- NULL
          data.subset <<- data.subset
          
          searchinput <<- T
          click("names.refresh")
        }
      }
      
    }
  })
  
  unrelated.search <<-F
  # CALL HS CODE FINDER UNRELATED SEARCH
  observeEvent(input$call_hs_code_finder_unrelated, {
    toggleClass("loading","active")
    
    phrase = tolower(input$search.field.unrelated)
    
    if (phrase %in% unique(tolower(phrase.table$phrase))) {
      data.returned <- subset(data.base, hs.code.6 %in% subset(code.suggested, phrase.id == phrase.table$phrase.id[tolower(phrase.table$phrase)==phrase])$hs.code.6)
      toggleClass("loading","active") 
      
      data.subset <- subset(data.base, hs.code.6 %in% data.returned$hs.code.6)
      data.subset$indicator <- "<div class='indicator search'></div>"
      row.names(data.subset) <- NULL
      data.subset <<- data.subset
      
      # To keep track of changes in the apps state
      data.ledger$selected <- 0
      data.ledger$selected[data.ledger$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
      data.ledger$search.generated <- 0
      data.ledger$search.generated[data.ledger$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
      data.ledger <<- data.ledger
      assign.global("data.ledger",data.ledger)
      
      unrelated.search <<- T
      click("names.refresh")
      
    } else {
      
      future({
        gta_hs_code_finder(products = phrase)
      }) %...>% {
        retrieved.data <- .
        toggleClass("loading","active")
        if (nrow(retrieved.data) == 0) {
          showNotification("No codes found for this search term",duration = 1000)
        } else {
          
          search.sources <- data.frame(hs.code.6 = character(),
                                       source.id = numeric(),
                                       product.name = character())
          
          search.sources.temp = retrieved.data[,c("hs.code","source.names","product.name")]
          search.sources.temp <- cSplit(search.sources.temp, which(colnames(search.sources.temp)=="source.names"), direction="long", sep=";")
          names(search.sources.temp) <- c("hs.code.6","source.name","product.name")
          search.sources.temp <- merge(search.sources.temp, suggestion.sources, by="source.name",all.x=T)
          search.sources <<- search.sources.temp
          rm(search.sources.temp)
          
          returned <- retrieved.data$hs.code
          returned <- as.character(sprintf("%06s",returned) %>% gsub(pattern = " ", replacement = "0", x = .))
          data.returned <- subset(data.base, hs.code.6 %in% returned) 
          
          data.subset <- subset(data.base, hs.code.6 %in% data.returned$hs.code.6)
          data.subset$indicator <- "<div class='indicator search'></div>"
          row.names(data.subset) <- NULL
          data.subset <<- data.subset
          
          # To keep track of changes in the apps state
          data.ledger$selected <- 0
          data.ledger$selected[data.ledger$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
          data.ledger$search.generated <- 0
          data.ledger$search.generated[data.ledger$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
          data.ledger <<- data.ledger
          assign.global("data.ledger",data.ledger)
          
          unrelated.search <<- T
          click("names.refresh")
        }
      }
    }
    
  })
  
  # CALL HS CODE FINDER WITH ADJUSTED QUERY
  observeEvent(input$search_adjusted, {
    toggleClass("loading","active")
    phrase = tolower(paste(input$query.refine, collapse=" "))
    future({
      gta_hs_code_finder(products = phrase)
    }) %...>% {
      toggleClass("loading","active")
      retrieved.data <- .
      if (nrow(retrieved.data) == 0) {
        showNotification("No codes found for this search term",duration = 1000)
      } else {
        if (! exists("search.sources")) {
          search.sources <- data.frame(hs.code.6 = character(),
                                       source.id = numeric(),
                                       product.name = character())
        }
        search.sources.temp = retrieved.data[,c("hs.code","source.names","product.name")]
        search.sources.temp <- cSplit(search.sources.temp, which(colnames(search.sources.temp)=="source.names"), direction="long", sep=";")
        names(search.sources.temp) <- c("hs.code.6","source.name","product.name")
        search.sources.temp <- merge(search.sources.temp, suggestion.sources, by="source.name",all.x=T)
        search.sources <<- unique(rbind(search.sources, search.sources.temp[,c("hs.code.6","source.id","product.name")]))
        rm(search.sources.temp)
        
        returned <- retrieved.data$hs.code
        returned <- as.character(sprintf("%06s",returned) %>% gsub(pattern = " ", replacement = "0", x = .))
        data.returned <- subset(data.base, hs.code.6 %in% returned) 
        data.returned$indicator <- "<div class='indicator search'></div>"
        
        #Adjust ledger
        ledger.temp <- data.ledger
        ledger.temp$search.generated[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
        ledger.temp$search.generated[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$search.generated != 1] <- 0
        ledger.temp$selected[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1 
        ledger.temp$selected[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$selected != 1] <- 0
        data.ledger <<- ledger.temp
        assign.global("data.ledger",data.ledger)
        data.subset <- subset(data.subset, ! hs.code.6 %in% data.returned$hs.code.6)
        data.subset <- rbind(data.subset, data.returned)
        row.names(data.subset) <- NULL
        data.subset <<- data.subset
        
        searchinput <<- T
        click("names.refresh")
      }
    }
  })
  
  # THIS IS THE SEARCH FIELD, USE AT LATER DATE, MAYBE
  # search <- eventReactive(input$search.field, {
  #   
  #   search.pattern <- paste(unlist(strsplit(as.character(input$search.field)," ")))
  #   # search.pattern <- paste(unlist(strsplit(as.character(c("test animal airplane"))," ")))
  #   
  #   print(search.pattern)
  #   # print("search")
  #   if (length(search.pattern)==0){
  #     # data.subset <<- data.base
  #     data.subset <<- data.base
  #   } else {
  #     # data.temp <- rownames_to_column(df = data.base, 'rownames')
  #     data.temp <- data.base %>% filter(grepl(paste(search.pattern, collapse="|"), full.name, ignore.case = T) | 
  #                                         grepl(paste(search.pattern, collapse="|"), hs.code.6, ignore.case = T))
  #     # data.temp <- column_to_rownames(df = as.data.frame(data.temp), 'rownames')
  #     # data.subset <<- data.temp 
  #     data.subset <<- data.temp
  #   }
  #   
  # })
  # 
  
  first.check <- T
  
  # TABLE OUTPUT FOR PREDEFINED LIST OF WORDS
  names <- eventReactive(input$names.refresh, {
    # input$save_selection
    # input$names.refresh
    # print(names(data.subset))
    
    data.subset <- data.subset[with(data.subset, order(c(hs.code.6))),]
    row.names(data.subset) <- NULL
    data.subset <<- data.subset
    data.output <<- data.subset
    
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
  observeEvent(input$query.refine, {
    print(paste(input$query.refine, collapse=", "))
    # print(paste("Search.query",paste(input$query.refine, collapse=" ")))
    # search.query <<- paste(input$query.refine, collapse=" ")
    # output$query.refine.output <- renderText({ input$query.refine })
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
  
  observeEvent(input$import_uploaded_file, {
    file = input$import.xlsx
    importfile = read.xlsx(xlsxFile = file$datapath, sheet = 1, rowNames = F, colNames = F)
    shinyjs::addClass(selector = ".import-wrap", class = "active")
  })
  
  observeEvent(input$finish.import, {
    
    shinyjs::removeClass(selector = ".import-wrap", class = "active")
    showNotification("Thank you. You will be notified by email once the import is completed.", duration = 60)
    file = input$import.xlsx
    
    # LOAD LOG
    load("17 Shiny/5 HS code finder/log/importer-log.Rdata")
    filename = paste0(Sys.Date()," - ",max(importer.log$ticket.number)+1," - ",chosen.user,".xlsx")
    
    # UPDATE EMAIL ADRESS
    if(input$update.email == T) {
      load_all()
      users$email[users$name == input$users] <- input$import.email.adress
      users <<- users
      save_all()
    }
    
    # FILL IMPORTER LOG
    importer.log.new = data.frame(user.id = users$user.id[users$name == input$users],
                                  order.email = input$import.email.adress,
                                  job.name = input$import.job.name,
                                  ticket.number = max(importer.log$ticket.number)+1,
                                  time.order = Sys.time(),
                                  under.preparation = 1,
                                  xlsx.file = filename,
                                  is.priority = input$prioritize,
                                  process.by.others = input$process.by.others,
                                  related.state.act = input$state.act.id)
    
    importer.log <- gta_rbind(list=list(importer.log, importer.log.new))
    rm(importer.log.new)
    
    importfile <- read.xlsx(xlsxFile = file$datapath, sheet = 1, colNames = F, rowNames = F)
    
    write.xlsx(importfile, file=paste0("17 Shiny/5 HS code finder/xlsx imports/",filename), sheetName = "sheet", append = F, rowNames = F, colNames = F)
    save(importer.log, file="17 Shiny/5 HS code finder/log/importer-log.Rdata")
    
  })
  
  # SELECT ROWS MECHANISM
  selectedRow <- observe(suspended=F, { input$hstable_rows_selected 
    isolate({
      print(paste("#Selected 1: ", length(data.ledger$hs.code.6[data.ledger$selected ==1])))
      print(paste("#input$hstable_rows_selected 1: ", paste(input$hstable_rows_selected, collapse = ", ")))
      # selected <<- subset(data.base, (! hs.code.6 %in% data.subset$hs.code.6 & selected == 1) | (hs.code.6 %in% data.subset$hs.code.6 & hs.code.6 %in% data.subset$hs.code.6[c(input$hstable_rows_selected)]))$hs.code.6
      selected <<- subset(data.ledger, selected == 1)$hs.code.6
      
      selected.user <<- subset(data.ledger, user.generated == 1)$hs.code.6
      
      data.ledger$selected[data.ledger$selected == 0 & data.ledger$hs.code.6 %in% data.subset$hs.code.6[c(input$hstable_rows_selected)]]  <- 1
      data.ledger$selected[data.ledger$selected == 1 & ! data.ledger$hs.code.6 %in% data.subset$hs.code.6[c(input$hstable_rows_selected)]]  <- 0
      # data.ledger$selected <- 1
      
      print(paste("#Selected 2: ", length(data.ledger$hs.code.6[data.ledger$selected ==1])))
      print(paste("#input$hstable_rows_selected 2: ", paste(input$hstable_rows_selected, collapse = ", ")))
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
  
  
  # # UPDATE UNRELATED SEARCH BOX TEXT AREA OUTPUT
  # unrelated_selected_codes <- observe(suspended=F, { input$hstable_rows_selected 
  #   codes <- paste(data.ledger$hs.code.6[data.ledger$selected == 1], collapse = ", ")
  #   updateTextAreaInput(session,
  #                       inputId = "unrelated_selected_codes",
  #                       label= "These are your selected codes:",
  #                       value = codes,
  #                       placeholder = codes
  #   )
  # })
  
  # OBSERVE REFRESH BUTTON
  # currentWord <- eventReactive(input$names.refresh, {
  #   print(tuple.names$names[tuple.names$id == phr.id])
  # })
  
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
      data.returned$indicator <- "<div class='indicator user'></div>"
      
      ledger.temp <- data.ledger
      ledger.temp$user.generated[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1
      ledger.temp$user.generated[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$user.generated != 1] <- 0
      ledger.temp$selected[ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6)] <- 1 
      ledger.temp$selected[! ledger.temp$hs.code.6 %in% unique(data.returned$hs.code.6) & ledger.temp$selected != 1] <- 0
      data.ledger <<- ledger.temp
      
      data.subset <- rbind(data.returned, data.subset)
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
    suggestion.ids <- subset(code.suggested, phrase.id == phr.id)$suggestion.id
    all.sources <- subset(code.source, suggestion.id %in% suggestion.ids)$source.id
    
    if(! paste(input$query.refine, collapse=" ") %in% unique(phrase.table$phrase)) {
      return(TRUE)
    } else {
      if (length(all.sources) != 0){
        if(length(all.sources[! all.sources %in% c(seq(1,8,1))]) == 0) {
          return(TRUE)
        }
      }
    }
    
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
    if(! paste(input$query.refine, collapse=" ") %in% unique(phrase.table$phrase)) {
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
  })
  
  output$finder_check_button <- renderUI({
    if(! paste(input$query.refine, collapse=" ") %in% unique(phrase.table$phrase)) {
      tags$div(class="button",
               actionButton("search_adjusted",
                            "Search"))
    }
  })
  
  # Report terms which are not a product 
  observeEvent(input$not.product, {
    load_all()
    
    report.services <<- rbind(report.services, 
                              data.frame(phrase.id = phr.id,
                                         user.id = users$user.id[users$name == input$users]))
    
    job.phrase$processed[job.phrase$phrase.id == phr.id] <- TRUE
    job.phrase <<- job.phrase
    
    check.log <- rbind(check.log, 
                       data.frame(check.id = max(check.log$check.id)+1,
                                  user.id = users$user.id[users$name == input$users],
                                  time.stamp = Sys.time(),
                                  check.successful = TRUE,
                                  job.id = job.phrase$job.id[job.phrase$phrase.id == phr.id]))
    
    check.log <<- check.log
    
    check.phrases <- rbind(check.phrases,
                           data.frame(check.id = max(check.log$check.id),
                                      phrase.id = phr.id))
    check.phrases <<- check.phrases
    
    save_all()
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
                    value = users$email[users$name == input$users])
  })
  
  observeEvent(input$create.user, {
    name = input$username
    print(name)
    if (name %in% unique(users$name)) {
      showNotification("This name already exists",duration = 5)
    } else {
      load_all()
      users <<- rbind(users, data.frame(user.id = max(users$user.id)+1,
                                        name = name,
                                        gta.layer = "core",
                                        email = "name@mail.com"))
      save_all()
      load_all()
      updateSelectInput(session, "users", choices = unique(users$name),selected = name)
      reset("username")
    }
    
  })
  
  
  # Function to refresh names in table
  
  refresh_names <- function(type="check.suggestion") {
    print("REFRESH_NAMES()")
    load_all()
    
    all.done = F
    
    if (type == "check.suggestion") {
      
      # COUNT REMAINING PHRASES FOR USER PER JOB
      should.do <- subset(job.log, job.processed == F)
      should.do$remaining <- 0
      for(j.id in unique(should.do$job.id)) {
        should.do$remaining[should.do$job.id == j.id] <- nrow(subset(job.phrase, job.id == j.id & processed == F & ! phrase.id %in% subset(check.phrases, check.id %in% subset(check.log, user.id == users$user.id[users$name == input$users])$check.id)$phrase.id))
      }
      should.do <- subset(should.do, remaining != 0)
      
      # ORDER JOBS BY PRIORITY AND REMAINING PHRASES, CHOOSE JOB ID FROM ROW 1
      if (nrow(should.do)>0) {
        should.do$is.priority <- ifelse(should.do$is.priority, 1, 0)
        should.do <- should.do[with(should.do, order(-is.priority, remaining)),]
        
        job.id <- should.do$job.id[1]
        job.id <<- job.id
        
        should.do <- job.phrase$phrase.id[job.phrase$job.id == job.id & job.phrase$processed == FALSE & ! job.phrase$phrase.id %in% subset(check.phrases, check.id %in% subset(check.log, user.id == users$user.id[users$name == input$users])$check.id)$phrase.id]
        should.do <<- should.do
        
      } else {
        showNotification("You are all done, thank you!", duration = 1000)
        all.done = T
      }
      
      
      if (all.done == F) {
        
        if(length(should.do)>1) {
          phr.id <<- sample(should.do,1)
        } else {
          phr.id <<- should.do
        }
        
        query <<- paste(unlist(strsplit(as.character(phrase.table$phrase[phrase.table$phrase.id == phr.id])," ")))
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
  }
  
  # Functions for HS Code finder App
  
  save_selection <- function(type) {
    print("SAVE_SELECTION()")
    load_all() 
    if(input$users=="Select"){
      showNotification("Please select or create a user before saving your selection",duration = 1000)
    } else if (is.null(input$radio1)==T) {
      showNotification("Please select a confidence level",duration = 5)
    } else {
      
      if (type %in% c("standard","clipboard","unrelated_search")) {
        
        new.job.unrelated <- F
        
        if(type=="unrelated_search") {
          
          job.log <- rbind(job.log, 
                           data.frame(job.id = max(job.log$job.id)+1,
                                      job.type = "Own search",
                                      job.name = input$search.field.unrelated,
                                      user.id = users$user.id[users$name == input$users],
                                      nr.of.checks = 1,
                                      check.hierarchy = FALSE,
                                      is.priority = FALSE,
                                      self.check = FALSE,
                                      related.state.act = NA,
                                      job.processed = FALSE,
                                      submission.id = Sys.Date()))
          job.log <<- job.log
          job.id <- max(job.log$job.id)
          job.id <<- job.id
          
          new.job.unrelated <- T
          
        }
        
        # check if phrase has been adjusted
        # Phrase.table
        new.phrase <- F
        if (type %in% c("standard","clipboard")) {
          
          if (! tolower(paste(input$query.refine, collapse=" ")) %in% unique(tolower(phrase.table$phrase))) {
            old.id <- phr.id
            phrase.table <- rbind(phrase.table, 
                                  data.frame(phrase.id = max(phrase.table$phrase.id)+1,
                                             phrase = tolower(paste(input$query.refine, collapse = " ")),
                                             source = "adjusted"))
            phrase.table <<- phrase.table
            
            phr.id <- max(phrase.table$phrase.id)
            phr.id <<- phr.id
            
            new.phrase <- T
          }
        }
        
        if (type %in% c("unrelated_search")) {
          
          if (! tolower(input$search.field.unrelated) %in% unique(tolower(phrase.table$phrase))) {
            old.id <- phr.id
            phr.id <- max(phrase.table$phrase.id)+1
            phr.id <<- phr.id
            
            phrase.table <- rbind(phrase.table, 
                                  data.frame(phrase.id = phr.id,
                                             phrase = tolower(input$search.field.unrelated),
                                             source = "unrelated search"))
            phrase.table <<- phrase.table
            
            
            new.phrase <- T
            
          } else{
            old.id <- phr.id
            phr.id=min(phrase.table$phrase.id[tolower(phrase.table$phrase)==tolower(input$search.field.unrelated)], na.rm = T)
            phr.id <<- phr.id
          }
          
        }
        
        if (new.phrase == T | new.job.unrelated == T) {
          job.phrase <- rbind(job.phrase, 
                              data.frame(job.id = job.id,
                                         phrase.id = phr.id,
                                         processed = TRUE))
          job.phrase <<- job.phrase
          
        } else if (new.phrase == F) {
          
          ## checking whether this phrase has been checked the required number of times
          phrase.user.unique <- unique(merge(subset(check.log, 
                                                    check.successful==T), 
                                             check.phrases, by="check.id", all.x=T)[,c("user.id","phrase.id")])
          
          successful.checks <- nrow(unique(subset(phrase.user.unique, phrase.id == phr.id)))
          
          ## looping over all jobs that happen to include this phrase.
          for(j.id in unique(subset(job.phrase, phrase.id==phr.id)$job.id)){
            required.checks=job.log$nr.of.checks[job.log$job.id==j.id]
            
            ## adjust for the present check which is not in the check.log yet
            required.checks=required.checks-1 
            
            if(successful.checks>=required.checks){
              job.phrase$processed[job.phrase$phrase.id == phr.id & job.phrase$job.id==j.id] <- TRUE
              job.phrase <<- job.phrase
              
              
            }
            rm(required.checks)
            
          }
          rm(successful.checks)
          
          
        }
        
        # codes.suggested
        if(type %in% c("standard","clipboard")) {
          if (new.phrase == F) {
            suggested.new <- subset(data.ledger, (user.generated == 1 | search.generated == 1) & selected == 1)
            suggested.new <- subset(suggested.new, ! hs.code.6 %in% subset(code.suggested, phrase.id == phr.id)$hs.code.6)
            
          } else if (new.phrase == T) {
            ## isn't this removing suggestions that may happen to overlap between the old and the new phrase? 
            ## If so, is this on purpose?
            suggested.new <- subset(data.ledger, hs.code.6 %in% unique(code.suggested$hs.code.6[code.suggested$phrase.id == old.id]) | selected == 1)
          }
        }
        if (type=="unrelated_search") {
          if (new.phrase == F) {
            suggested.new <- subset(data.ledger, (user.generated == 1 | search.generated == 1) & selected == 1)
            suggested.new <- subset(suggested.new, ! hs.code.6 %in% subset(code.suggested, phrase.id == phr.id)$hs.code.6)
            
          } else if (new.phrase == T) {
            ## isn't this removing suggestions that may happen to overlap between the old and the new phrase? 
            ## If so, is this on purpose?
            suggested.new <- subset(data.ledger, hs.code.6 %in% unique(code.suggested$hs.code.6[code.suggested$phrase.id == old.id]) | selected == 1  | user.generated == 1 | search.generated == 1)
          }
        }
        
        
        
        
        if (nrow(suggested.new) != 0) {
          suggested.new$phrase.id = phr.id
          
          suggested.new$suggestion.id <- seq((max(code.suggested$suggestion.id)+1),(max(code.suggested$suggestion.id))+nrow(suggested.new),1)
          code.suggested <- rbind(code.suggested, 
                                  suggested.new[,c("suggestion.id","phrase.id","hs.code.6")])
          code.suggested <<- code.suggested
          
          if (exists("search.sources")) {
            search.sources <- subset(search.sources, hs.code.6 %in% suggested.new$hs.code.6[suggested.new$search.generated == 1])
            search.sources <- merge(search.sources, suggested.new[,c("hs.code.6","suggestion.id")], by="hs.code.6", all.x=T)
            code.source <- rbind(code.source, 
                                 search.sources[,c("source.id","suggestion.id")])
            code.source <<- code.source
            rm(search.sources)
            
          }
          if (length(suggested.new$hs.code.6[suggested.new$user.generated == 1]) > 0) {
            suggested.new <- subset(suggested.new, user.generated == 1)
            suggested.new$source.id <- 99
            code.source <- rbind(code.source, 
                                 suggested.new[,c("source.id","suggestion.id")])
            code.source <<- code.source
          }
        }
        
        
        # Check.log
        check.log <- rbind(check.log, 
                           data.frame(check.id = max(check.log$check.id)+1,
                                      user.id = users$user.id[users$name == input$users],
                                      time.stamp = Sys.time(),
                                      check.successful = TRUE,
                                      job.id = job.id))
        check.log <<- check.log
        
        # Add code.selected
        code.selected.new <- subset(code.suggested, hs.code.6 %in% subset(data.ledger, selected == 1)$hs.code.6 & phrase.id == phr.id)
        if (nrow(code.selected.new) > 0) {
          code.selected.new$check.id= max(check.log$check.id)
          code.selected <- rbind(code.selected, 
                                 code.selected.new[,c("check.id","suggestion.id")])
          code.selected <<- code.selected
          rm(code.selected.new)
        }
        
        
        # Check.phrases
        check.phrases <- rbind(check.phrases, 
                               data.frame(check.id = max(check.log$check.id),
                                          phrase.id = phr.id))
        check.phrases <<- check.phrases
        
        if (type %in% c("standard","clipboard")) {
          # words.removed
          words.all <- paste(unlist(strsplit(as.character(tolower(phrase.table$phrase[phrase.table$phrase.id == phr.id]))," ")))
          removed <- words.all[! words.all %in% paste(unlist(strsplit(as.character(tolower(input$query.refine))," ")))]
          
          if (length(removed) > 0) {
            words.removed <- rbind(words.removed, 
                                   data.frame(check.id = c(rep(max(check.log$check.id), length(removed))),
                                              words.removed = removed))
            words.removed <<- words.removed
          }
          
        }
        
        
        if (type %in% c("standard","clipboard")) {
          if (input$suggestions.search.terms != "") {
            
            additional.suggestions.phrases=as.data.frame(strsplit(input$suggestions.search.terms,split=';', fixed=TRUE))
            additional.suggestions <- rbind(additional.suggestions, 
                                            data.frame(check.id = max(check.log$check.id),
                                                       term = additional.suggestions.phrases[,1],
                                                       user.id = users$user.id[users$name == input$users]))
            additional.suggestions <<- additional.suggestions
            rm(additional.suggestions.phrases)
          }
        }
        
        # check.certainty
        check.certainty <- rbind(check.certainty, 
                                 data.frame(check.id = max(check.log$check.id),
                                            certainty.level = input$radio1))
        check.certainty <<- check.certainty
        
        
        # Check if job is fully processed 
        # (could be more than one as one phrase may be part of more than one job)
        
        for(j.id in unique(subset(job.phrase, phrase.id==phr.id)$job.id)){
          
          if(nrow(subset(job.phrase, job.id==j.id & processed==F))==0){
            job.log$job.processed[job.log$job.id==j.id]=T
            job.log <<- job.log
            save_all()
            
            gta_hs_process_completed_job(processed.job=j.id)
          }
          
        }
        
      }
      if (type == "none_found") {
        # Check.log
        check.log <- rbind(check.log, 
                           data.frame(check.id = max(check.log$check.id)+1,
                                      user.id = users$user.id[users$name == input$users],
                                      time.stamp = Sys.time(),
                                      check.successful = FALSE,
                                      job.id = job.id))
        check.log <<- check.log
        
        # Check.phrases
        check.phrases <- rbind(check.phrases, 
                               data.frame(check.id = max(check.log$check.id),
                                          phrase.id = phr.id))
        check.phrases <<- check.phrases
        
        # check.certainty
        check.certainty <- rbind(check.certainty, 
                                 data.frame(check.id = max(check.log$check.id),
                                            certainty.level = input$radio1))
        
        check.certainty <<- check.certainty
        
      }
      
      
      save_all()
      
      
      if (type != "unrelated_search") {
        
        phr.id.future <<- phr.id
        query.refine.future <<- input$query.refine
        # query.refine.future <<- "hot air balloon"
        
        future({ gta_hs_code_finder(products = tolower(paste(query.refine.future, collapse=" ")))}) %...>%  {
          found.temp <- .
          load_all()
          codes <- code.suggested$hs.code.6[code.suggested$phrase.id == phr.id.future]
          new.codes <- subset(found.temp, ! hs.code %in% codes)
          new.codes <- new.codes[,c("hs.code","source.names")]
          new.codes$phrase.id <- phr.id.future
          new.codes$suggestion.id <- seq(max(code.suggested$suggestion.id)+1,max(code.suggested$suggestion.id)+nrow(new.codes),1)
          names(new.codes) <- c("hs.code.6","source.names","phrase.id","suggestion.id")
          code.suggested <- rbind(code.suggested, new.codes[,c("hs.code.6","phrase.id","suggestion.id")])
          code.suggested <<- code.suggested
          
          new.codes <- new.codes[,c("source.names","suggestion.id")]
          new.codes <- cSplit(new.codes, which(colnames(new.codes)=="source.names"), direction="long", sep=";")
          names(new.codes) <- c("source.name","suggestion.id")
          new.codes <- merge(new.codes, suggestion.sources, by="source.name", all.x=T)
          code.source <- rbind(code.source, new.codes[,c("source.id","suggestion.id")])
          code.source <<- code.source
          save_all()
          print("ALL DONE ASYNC")
        }
        
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
      refresh_names()
    }
    
    }
  
  }


shinyApp(ui = ui, server = server)
