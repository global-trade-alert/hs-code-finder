

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
      
      # UPDATE EMAIL ADDRESS
      # UPDATE EMAIL ADDRESS #2 : in case we don't have one on file
      
      if(((input$update.email == T)|(is.na(gta_sql_get_value(paste0("SELECT user_email from gta_user_log WHERE user_login ='",input$users,"';")))))) {
        
        sql <- "UPDATE gta_user_log SET user_email = ?newvalue WHERE user_login = ?forwhom;"
        query <- sqlInterpolate(pool, 
                                sql, 
                                forwhom = input$users,
                                newvalue = input$import.email.adress)
        print(query)
        
        gta_sql_update_table(query)
        
      }
      
      
      
      
      # if email-field empty: get user email
      if (input$import.email.adress == "" & is.na(gta_sql_get_value(paste0("SELECT user_email from gta_user_log WHERE user_login ='",input$users,"';")))) {
        
        showNotification("Please input a mail address", duration = 5)
        
      } else {
        
        if (input$import.email.adress == "") {
          email.address <- gta_sql_get_value(paste0("SELECT user_email from gta_user_log WHERE user_login ='",input$users,"';"))
        } else {
          email.address = input$import.email.adress
        }
        
        
        # FILL IMPORTER LOG
        ticket.nr=gta_sql_multiple_queries(paste0("INSERT INTO hs_importer_log (user_id, order_email, job_name, time_order, under_preparation, is_priority, process_by_others, related_state_act) 
               VALUES (",
                      gta_sql_get_value(paste0("SELECT user_id from gta_user_log WHERE user_login ='",input$users,"';")),",'",
                      email.address,"','",
                      input$import.job.name,
                      "',CURRENT_TIMESTAMP,1,",
                      as.numeric(input$prioritize),",",
                      as.numeric(input$process.by.others),",",
                      input$state.act.id,");
               SELECT MAX(ticket_number) FROM ricardo.hs_importer_log;"),
                                           output.queries = 2)
        
        filename=paste0(Sys.Date()," - ", ticket.nr, " - ",input$user,".xlsx")
        
        gta_sql_get_value(paste0("UPDATE hs_importer_log SET xlsx_file = '",
                                 filename,
                                 "' WHERE ticket_number=",ticket.nr,";"))
        
        rm(ticket.nr)

        
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
    
    all.sources= gta_sql_get_value(paste0("SELECT DISTINCT(source_id) 
                                            FROM hs_code_source 
                                            WHERE suggestion_id IN (SELECT suggestion_id 
                                                                    FROM hs_code_suggested
                                                                    WHERE phrase_id IN (",paste(phr.id, collapse=","),"));"))
                      
    if(! paste(input$query.refine, collapse=" ") %in% gta_sql_get_value("SELECT DISTINCT(phrase) FROM hs_phrase_log;")) {
      
      return(TRUE)
      
    } else {
      
      if (length(all.sources) != 0){
        if(any(! all.sources %in% gta_sql_get_value("SELECT DISTINCT(source_id) FROM hs_suggestion_sources;"))) {
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
    
   
    if(! paste(input$query.refine, collapse=" ") %in% gta_sql_get_value("SELECT DISTINCT(phrase) FROM hs_phrase_log;")) {
      tags$div(class="text-box",
               tags$p("Search codes for adjusted query"))
      
    } else {
      
      
      all.sources= gta_sql_get_value(paste0("SELECT DISTINCT(source_id) 
                                            FROM hs_code_source 
                                            WHERE suggestion_id IN (SELECT suggestion_id 
                                                                    FROM hs_code_suggested
                                                                    WHERE phrase_id IN (",paste(phr.id, collapse=","),"));"))
      
      if (length(all.sources) != 0){
        if(any(! all.sources %in% gta_sql_get_value("SELECT DISTINCT(source_id) FROM hs_suggestion_sources;"))) {
          tags$div(class="text-box",
                   
                   HTML("<p><strong>Note:</strong> This term has already run through the HS code search function.</p>"))
        }
      }
    }
    
  })
  
  output$finder_check_button <- renderUI({

    if(! paste(input$query.refine, collapse=" ") %in% gta_sql_get_value("SELECT DISTINCT(phrase) FROM hs_phrase_log;")) {
      tags$div(class="button",
               actionButton("search_adjusted",
                            "Search"))
    }
    
  })
  
  # Report terms which are not a product 
  observeEvent(input$not.product, {
    
    current.user.id=gta_sql_get_value(paste0("SELECT user_id from gta_user_log WHERE user_login ='",input$users,"';"))
    current.job.id=gta_sql_get_value(paste0("SELECT job_id from hs_job_phrase WHERE phrase_id ='",phr.id,"';")) ### JF would prefer this to be set differently. We should know what job the user is working on at this point. The present query could result in more than 1 answer (if a phrase is associated to more than 1 job).
    
    new.check.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_check_log (user_id, time_stamp, check_successful, job_id)
                                                  VALUES (",current.user.id,",CURRENT_TIMESTAMP,1,",current.job.id,");
                                                  SELECT MAX(check_id) FROM hs_check_log;"),
                                          output.queries = 2)
    
    gta_sql_update_table(paste0("INSERT INTO hs_report_services (user_id,check_id,phrase_id)
                                 VALUES (",current.user.id,",",new.check.id,",",phr.id,");"))
    
    gta_sql_update_table(paste0("INSERT INTO hs_check_phrases (check_id, phrase_id)
                                 VALUES (",new.check.id,",",phr.id,");"))
    
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
                    value = gta_sql_get_value(paste0("SELECT user_email from gta_user_log WHERE user_login ='",input$users,"';")))
  })
  
  observeEvent(input$create.user, {
    
    if (input$username %in% gta_sql_get_value("SELECT user_login from gta_user_log;")) {
      
      showNotification("This name already exists",duration = 5)
      
    } else {
      
      
      gta_sql_update_table(paste0("INSERT INTO gta_user_log (user_login,gta_layer)
                                 VALUES (",input$username,",'core');"))
      
      
      ## those two lines should be removed in this update.
      users <- change_encoding(gta_sql_load_table("user_log", table.prefix = "gta_"))
      users <<- users 
      
      updateSelectInput(session, "users", choices = gta_sql_get_value("SELECT user_login from gta_user_log;"), selected = input$username)
      reset("username")
    }
    
  })
  
  
  # Function to refresh names in table
  
  refresh_names <- function(type="check.suggestion") {
    print("REFRESH_NAMES()")
    
    all.done = F

    if (type == "check.suggestion") {
      
      ## JF rewrite to:
      ## (1) Are there any open phrases to which the user can contribute?
      ## (2A) If so, get highest-priority phrase from highest-priority job.
      ## (2B) If not, say thank you and set app to zero.
      
      ## I do this all in one SQL query. First, to show off that I can, but more nobely because this avoid handling 'NA's along the way.
      ## Before reading the quers note that there are in general two branches starting with 'WHERE/AND phrase_id'. 
      ## The first branch retrieves all unprocessed phrases from the job with the highest priority that the user can still contribute to.
      ## The second branch retrieves all phrase id's which the user can process.
      ## The first branch is so long because one has prioritise over jobs that the user can still contribute to. 
      
      ## It feels like this query is longer than it needs to be but I can't put my finger on it. 
      
      next.phrase=gta_sql_get_value(paste0( "SELECT phrase_id 
                                             FROM (SELECT phrase_id, COUNT(check_id)
                                                   FROM hs_check_phrases AS tbl1
                                                   WHERE phrase_id IN (SELECT phrase_id 
                                                                       FROM hs_job_phrase
                                                                       WHERE job_id = (SELECT job_id 
                                                                                       FROM (SELECT jp.job_id, is_priority, COUNT(DISTINCT(jp.phrase_id))
                                                                                             FROM hs_job_phrase jp
                                                                                             JOIN hs_job_log jl
                                                                                             ON jp.job_id = jl.job_id 
                                                                                             WHERE jp.processed=0
                                                                                             AND jp.job_id IN (SELECT job_id
                                                                                                                FROM hs_job_phrase
                                                                                                                WHERE processed = 0
                                                                                                                AND phrase_id NOT IN (SELECT cp.phrase_id
                                                                                                                                      FROM hs_check_phrases cp
                                                                                                                                      JOIN hs_phrase_log pl
                                                                                                                                      ON cp.phrase_id = pl.phrase_id
                                                                                                                                      AND cp.processing_round = pl.processing_round
                                                                                                                                      AND cp.check_id IN (SELECT check_id
                                                                                                                                                         FROM hs_check_log
                                                                                                                                                         WHERE user_id = ",
                                                                                                                                                          gta_sql_get_value(paste0("SELECT user_id from gta_user_log WHERE user_login ='",input$users,"';")),
                                                                                                                                                          ")))
                                                                                             GROUP BY job_id
                                                                                             ORDER by is_priority DESC, COUNT(jp.phrase_id) ASC
                                                                                             LIMIT 1) 
                                                                                            AS tbl_jobs_priority))
                                                   AND phrase_id IN (SELECT phrase_id
                                                                      FROM hs_job_phrase
                                                                      WHERE processed = 0
                                                                      AND phrase_id NOT IN (SELECT cp.phrase_id
                                                                                            FROM hs_check_phrases cp
                                                                                            JOIN hs_phrase_log pl
                                                                                            ON cp.phrase_id = pl.phrase_id
                                                                                            AND cp.processing_round = pl.processing_round
                                                                                            AND cp.check_id IN (SELECT check_id
                                                                                                               FROM hs_check_log
                                                                                                               WHERE user_id = ",
                                                                                                               gta_sql_get_value(paste0("SELECT user_id from gta_user_log WHERE user_login ='",input$users,"';")),
                                                                    ")))
                                                   GROUP BY phrase_id
                                                   ORDER by COUNT(check_id) DESC
                                                   LIMIT 1) 
                                             AS tbl_phrase_priority"))
      

        if(is.na(next.phrase) == F) {
          
          phr.id<<-next.phrase
          query <<- paste(unlist(strsplit(as.character(gta_sql_get_value(paste0("SELECT phrase FROM hs_phrase_log WHERE phrase_id =",next.phrase)))," ")))
          updateCheckboxGroupButtons(session, "query.refine", choices = query, selected = query)
          
          data.subset <- subset(data.base, hs.code.6 %in% gta_sql_get_value(paste0("SELECT hs_code_6 FROM hs_code_suggested WHERE phrase_id =",next.phrase)))
          row.names(data.subset) <- NULL
          data.subset <<- data.subset
          
        } else {
          showNotification("You are all done, thank you!", duration = 1000)
          
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
        
        if (! tolower(paste(input$query.refine, collapse=" ")) %in% unique(tolower(gta_sql_get_value("SELECT phrase FROM hs_phrase_log")))) {
          
          
          new.phr.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_phrase_log (phrase, source, processing_round, exit_status)
                                                  VALUES ('",tolower(paste(input$query.refine, collapse = " ")),"','adjusted',1,1);
                                                  SELECT MAX(phrase_id) FROM hs_phrase_log;"),
                                                output.queries = 2)

          checks <- c(checks, list("nrow.phraselog" = 1))
        
          if(is.na(new.phr.id)){
            stop("SAVE_SELECTION - new phrase: The phrase log update went wrong. I cannot find the phrase I just added.")
          }  else {
            
            new.phr.id <<- new.phr.id  
            
          }
          
          # update code.suggested to include the values of the original phrase ID for the new one
          gta_sql_multiple_queries(paste0("DROP TABLE IF EXISTS hs_cs_temp;
                                      CREATE TABLE hs_cs_temp AS
                                      SELECT * 
                                      FROM hs_code_suggested
                                      WHERE phrase_id =",phr.id,";
                                      UPDATE hs_cs_temp
                                      SET phrase_id=",new.phr.id,";
                                      INSERT INTO hs_code_suggested (phrase_id, hs_code_6, probability)
                                      SELECT phrase_id, hs_code_6, probability
                                      FROM hs_cs_temp;
                                      DROP TABLE IF EXISTS hs_cs_temp;"),
                                   output.queries = 1)
        
          
          
          # update code.source to include the values of the original phrase ID for the new one
          
          gta_sql_update_table(paste0("INSERT INTO hs_code_source
                                       SELECT hs_new.suggestion_id, hs_src.source_id
                                       FROM hs_code_suggested hs_new
                                       JOIN hs_code_suggested hs_old
                                       ON hs_old.hs_code_6=hs_new.hs_code_6
                                       JOIN hs_code_source hs_src
                                       ON hs_old.suggestion_id=hs_src.suggestion_id
                                       WHERE hs_new.phrase_id=",new.phr.id,"
                                       AND hs_old.phrase_id=",phr.id,";"))
          
          checks <- c(checks, list("newphrase.source.new" = nrow(new.code.source)))
          
          } else {
          new.phr.id <<- numeric()
        }
        
        
        
        # codes.suggested
        suggested.new <- subset(data.ledger, (user.generated == 1 | search.generated == 1) & selected == 1)
        suggested.new <- subset(suggested.new, ! hs.code.6 %in% gta_sql_get_value(paste0("SELECT hs_code_6 
                                                                                          FROM hs_code_suggested
                                                                                          WHERE phrase_id=",phr.id,";")))
        
        checks <- c(checks, list("suggested.old" =  gta_sql_get_value(paste0("SELECT COUNT(*) 
                                                                              FROM hs_code_suggested
                                                                              WHERE phrase_id=",phr.id,";"))))
        checks <- c(checks, list("suggested.new" = nrow(suggested.new)))

        
        
        # update code.suggested with user-provided information
        ### I delete this block since I want to replace it with SQL but am not sure we need to.
        ### The problem addressed here is that user-added HS codes to the new.phr.id must be transferred to the original phrase.id. 
        ### How are those additions to new-phr.id stored? If they are stored as user suggestions to the new phrase, then I think the below should do the trick
        
        gta_sql_multiple_queries(paste0("DROP TABLE IF EXISTS hs_cs_temp;
                                      CREATE TABLE hs_cs_temp AS
                                      SELECT * 
                                      FROM hs_code_suggested
                                      WHERE phrase_id =",new.phr.id,"
                                      AND hs_code_6 NOT IN (SELECT hs_code_6
                                                            FROM hs_code_suggested
                                                            WHERE phrase_id =",phr.id,");
                                      UPDATE hs_cs_temp
                                      SET phrase_id=",phr.id,";
                                      INSERT INTO hs_code_suggested (phrase_id, hs_code_6, probability)
                                      SELECT phrase_id, hs_code_6, probability
                                      FROM hs_cs_temp;
                                      
                                      INSERT INTO hs_code_source
                                      SELECT hs_new.suggestion_id, hs_src.source_id
                                      FROM hs_cs_temp hs_new
                                      JOIN hs_code_suggested hs_old
                                      ON hs_old.hs_code_6=hs_new.hs_code_6
                                      JOIN hs_code_source hs_src
                                      ON hs_new.suggestion_id=hs_src.suggestion_id
                                      WHERE hs_new.phrase_id=",new.phr.id,"
                                      AND hs_old.phrase_id=",phr.id,";
                                      
                                      DROP TABLE IF EXISTS hs_cs_temp;
                                      "),
                                 output.queries = 1)
     
        
        
        # CREATE NEW CHECK and store its ID
        # Check.log
        
        current.user.id=gta_sql_get_value(paste0("SELECT user_id from gta_user_log WHERE user_login ='",input$users,"';"))
        current.job.id=gta_sql_get_value(paste0("SELECT job_id from hs_job_phrase WHERE phrase_id ='",phr.id,"';")) ### JF would prefer this to be set differently. We should know what job the user is working on at this point. The present query could result in more than 1 answer (if a phrase is associated to more than 1 job).
        
        this.check.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_check_log (user_id, time_stamp, check_successful, job_id)
                                                  VALUES (",current.user.id,",CURRENT_TIMESTAMP,1,",current.job.id,");
                                                  SELECT MAX(check_id) FROM hs_check_log;"),
                                              output.queries = 2)
        
        

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
                                             phrase.id = p.id,
                                             processing.round=p.round)
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
                                           phrase.id = phr.id,
                                           processing.round=p.round)
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