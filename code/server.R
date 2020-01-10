

server <- function(input, output, session) {
  print("START APP")
  cat("\n")
  
  data.base <- data.base.0
  data.subset <- data.base.0[NULL,]
  
  data.ledger <- reactiveValues(codes=data.base.0[NULL,c("hs.code.6")], selected=data.base.0[NULL,c("hs.code.6")])
  data.ledger.user <- reactiveValues(codes=data.base.0[NULL,c("hs.code.6")], selected=data.base.0[NULL,c("hs.code.6")])
  observe({
    print(paste0("TOTAL CODES: ", sum(c(length(data.ledger$codes),length(data.ledger.user$codes)))))
    print(paste0("DATA LEDGER SELECTED CHANGED: [SELECTED: ",length(data.ledger$selected),"] ", paste0(data.ledger$selected, collapse = ", ")))
    print(paste0("DATA LEDGER USER SELECTED CHANGED: [SELECTED: ",length(data.ledger.user$selected),"] ", paste0(data.ledger.user$selected, collapse = ", ")))
    print(paste0("NUMBER OF CODES LEDGER CODES: ", length(data.ledger$codes)))
    print(paste0("NUMBER OF CODES LEDGER USER CODES: ", length(data.ledger.user$codes)))
    cat("\n")
  })
  
  
  importToggle <- "import-toggle-excel"
  
  job.id <- reactiveValues(id=0)
  # job.id <<- job.id 
  phr.id <- reactiveValues(id=0)
  # phr.id <<- phr.id
  query <- reactiveValues(phrase="select user")
  # query <<- query
  user <- reactiveValues(id=0, name=NULL)
  
  observe({
    print(paste0("PHRASE ID CHANGED: ", phr.id$id))
    cat("\n")
  })
  
  observe({
    print(paste0("QUERY CHANGED: ", paste0(query$phrase, collapse = " ")))
    cat("\n")
  })
  
  observe({
    print(paste0("JOB ID CHANGED: ", job.id$id))
    cat("\n")
  })
  
  observe({
    print(paste0("USER ID CHANGED: ", user$id))
    print(paste0("USER NAME CHANGED: ", user$name))
    cat("\n")
  })
  
  userinput = F
  nonefound.check = F
  searchinput = F
  load.new.phrase = T
  
  # FUNCTION TO GET LIST OF SELECTED ROWS
  initialSelectAll <- T
  selected.rows <- function() {
    print("SELECTED.ROWS()")
    cat("\n")
    isolate({
      if (initialSelectAll == T) {
        initialSelectAll <<- F
        if (nrow(data.subset) > 0) {
          return (c(seq(1,nrow(data.subset),1)))
        } else {
          return (c())
        }
      } else {
        if (length(data.ledger$codes) > 0) {
          existing.rows <- data.subset
          selected.codes <- c(data.ledger$selected, data.ledger.user$selected)
          existing.rows$selected <- 0
          existing.rows$selected[existing.rows$hs.code.6 %in% selected.codes] <- 1
          return(c(which(existing.rows$selected == 1, arr.ind = T)))
        } else {
          return (c())
        }
      }
    })
  }
  
  # RenderUI output for query (necessary because reactVals variables cannot be accessed directly from ui function)
  output$query.refine <- renderUI({
    checkboxGroupButtons("query.refine",
                         label=NULL,
                         choices = query$phrase,
                         selected = query$phrase)
  })
  
  
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
    cat("\n")
    code.suggested = gta_sql_load_table("code_suggested")
    
    sql <- "SELECT exit_status FROM hs_phrase_log WHERE phrase_id = ?phraseID;"
    querysql <- sqlInterpolate(pool,
                               sql,
                               phraseID = phr.id$id)
    
    exit.status=gta_sql_get_value(querysql)
    
    data.subset <- data.subset[with(data.subset, order(c(hs.code.6))),]
    data.subset <<- data.subset
    data.output <- merge(data.subset, subset(code.suggested, phrase.id == phr.id$id)[,c("probability","hs.code.6")], by = "hs.code.6", all.x=T)
    
    data.ledger$codes <- data.subset$hs.code.6[! data.subset$hs.code.6 %in% data.ledger.user$codes]
    
    
    if (exit.status == 2) {
      data.output$probability.html[is.na(data.output$probability)==F] <- paste0("<div class='create-tooltip help' title = '<span>Based on previous classifications, this HS code belongs to the current search term with a probability of ",round(data.output$probability[is.na(data.output$probability)==F]*100, digits = 0)," percent.</span>'><div class='probability-bg-wrap'><div class='probability-wrap'><div class='probability probability-available' style='width:",data.output$probability[is.na(data.output$probability)==F]*100,"%; background-color:",redToGreen(data.output$probability),";'></div></div></div></div>")
      data.output$probability.html[is.na(data.output$probability)] <- paste0("<div class='create-tooltip help' title = '<span>Based on previous classifications, this HS code belongs to the current search term with a probability of 0 percent.</span>'><div class='probability-bg-wrap'><div class='probability-wrap'><div class='probability probability-none'></div></div></div></div>")
    } else {
      data.output$probability.html[is.na(data.output$probability)] <- paste0("<div class='probability-hide'><div class='probability-wrap'><div class='probability probability-none'></div></div></div>")
    }
    
    if (is.null(user$name)) {
      data.output <- data.output[NULL,]
      query$phrase <- ""
    } else {
      data.output <- unique(data.output[,c("indicator","hs.code.6","probability.html","hs.description.6","hs.description.4","hs.code.4","hs.code.2","hs.id")])
    }
    
    if (load.new.phrase) {
      data.ledger$selected <- data.ledger$codes
      load.new.phrase <<- F
    }
    
    row.names(data.output) <- NULL
    data.output <<- data.output
    
  })
  
  i = F
  observeEvent(input$users, {
    if(is.null(user$name)==F) {
      print(paste("INITIAL DATA SUBSET SET UP: "))
      cat("\n")
      refresh_names()
      i = T
    }
  })
  
  
  proxy <- dataTableProxy('hstable')
  
  # OBSERVE SELECT ALL BUTTON
  observeEvent(input$toggle_all, {
    print(paste("OBSERVE TOGGLE ALL BUTTON: "))
    cat("\n")
    
    DT::selectRows(proxy, input$hstable_rows_all)
  })
  
  # OBSERVE DESELECT ALL BUTTON
  observeEvent(input$untoggle_all, {
    print(paste("OBSERVE UNTOGGLE ALL BUTTON: "))
    cat("\n")
    
    DT::selectRows(proxy, NULL)
  })
  
  # OBSERVE GROUP SELECT
  observeEvent(input$checkGroupSelect, {
    print(paste0("GROUP CLICKED: ",input$checkGroupSelect))
    cat("\n")
    
    returned.code <- input$checkGroupSelect
    
    if (all(subset(data.subset, hs.code.4 %in% returned.code)$hs.code.6 %in% c(data.ledger$selected, data.ledger.user$selected))) {
      data.ledger$selected <- data.ledger$selected[! data.ledger$selected %in% subset(data.subset, hs.code.4 %in% returned.code)$hs.code.6]
      data.ledger.user$selected <- data.ledger.user$selected[! data.ledger.user$selected %in% subset(data.subset, hs.code.4 %in% returned.code)$hs.code.6]
    } else {
      data.ledger$selected <- unique(c(data.ledger$selected, subset(data.subset, hs.code.4 %in% returned.code & hs.code.6 %in% data.ledger$codes)$hs.code.6))
      data.ledger.user$selected <- unique(c(data.ledger.user$selected, subset(data.subset, hs.code.4 %in% returned.code & hs.code.6 %in% data.ledger.user$codes)$hs.code.6))
    }
    
    # data.ledger <<- data.ledger  
    rows <- selected.rows()
    DT::selectRows(proxy, rows)
  })
  
  # OBSERVE CHANGES IN REFINE.QUERY
  observeEvent(input$query.refine, ignoreNULL = F, {
    if (length(input$query.refine)==0 & is.null(user$name)==F) {
      showNotification("Please select at least one word of the original phrase", duration = 3)
      updateCheckboxGroupButtons(session, "query.refine", choices = query$phrase, selected = query$phrase[1])
    }
    print(paste0("CURRENT SELECTED QUERY: ", paste(input$query.refine, collapse=", ")))
    cat("\n")
    
  })
  
  
  # observeEvent(input$query.refine, {
  #   search.query <- paste(paste(input$query.refine, collapse=" "))
  # })
  
  # output$output.query <- renderPrint({
  #   print(search.query)  
  # })
  
  output$search.engines <- renderUI({
    search.query <- paste(input$query.refine, collapse=" ")
    
    HTML(paste0("<div class='search-engines'>",
                "<div class='google'>
                <a href='https://google.com/search?q=",paste("HS+Code",gsub(" ","+", search.query), sep="+"),"' target='_blank'>
                <img src='www/google.png' />
                </a>
                </div>",
                "<div class='duckduckgo'>
                <a href='https://duckduckgo.com/?q=",paste("HS+Code",gsub(" ","+", search.query), sep="+"),"' target='_blank'>
                <img src='www/duckduckgo.png' />
                </a>
                </div>",
                "<div class='zauba'>
                <a href='https://www.zauba.com/USA-htscodes/",paste(gsub(" ","+", search.query), sep="-"),"' target='_blank'>
                <img src='www/zauba.png' />
                </a>
                </div>",
                "<div class='etcn'>
                <a href='http://hs.e-to-china.com/ks-",paste(gsub(" ","+", search.query), sep="+"),"-d_3-t_1.html' target='_blank'>
                <img src='www/etcn.png' />
                </a>
                </div>",
                "<div class='eurostat'>
                <a href='https://eurostat.prod.3ceonline.com/' target='_blank'>
                <img src='www/eurostat.png' />
                </a>
                </div>",
                "<div class='foreign_trade'>
                <a href='https://www.foreign-trade.com/reference/hscode.htm' target='_blank'>
                <img src='www/foreign_trade.png' />
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
        querysql <- sqlInterpolate(pool, 
                                   sql, 
                                   forwhom = input$users,
                                   newvalue = input$import.email.adress)
        print(querysql)
        
        gta_sql_update_table(querysql)
        
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
        if (input$state.act.id != "") {
          state.act <- input$state.act.id
        } else {
          state.act <- "NULL"
        }
        ticket.nr=gta_sql_multiple_queries(paste0("INSERT INTO hs_importer_log (user_id, order_email, job_name, time_order, under_preparation, is_priority, process_by_others, related_state_act) 
               VALUES (",
                                                  user$id,",'",
                                                  email.address,"','",
                                                  input$import.job.name,
                                                  "',CURRENT_TIMESTAMP,1,",
                                                  as.numeric(input$prioritize),",",
                                                  as.numeric(input$process.by.others),",",
                                                  state.act,");
               SELECT MAX(ticket_number) FROM hs_importer_log;"),
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
          
          openxlsx::write.xlsx(importfile, file=paste0(path,"/xlsx imports/",filename), sheetName = "sheet", append = F, rowNames = F, colNames = F)
        } else if (importToggle == "import-toggle-manual") {
          
          import.phrases <- as.data.frame(paste(unlist(strsplit(as.character(input$manual.import.values),";"))))
          import.phrases[,1] <- trimws(import.phrases[,1], which = "both")
          import.phrases <<- import.phrases
          openxlsx::write.xlsx(import.phrases, file=paste0(path,"/xlsx imports/",filename), sheetName = "sheet", append = F, rowNames = F, colNames = F)
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
      selected <- data.subset$hs.code.6[c(input$hstable_rows_selected)]
      data.ledger$selected <- selected[selected %in% data.ledger$codes]
      data.ledger.user$selected <- selected[selected %in% data.ledger.user$codes]
    })
  })
  
  selected_codes_output <- observe(suspended=F, { input$hstable_rows_selected 
    codes <- paste(c(data.ledger$selected, data.ledger.user$selected), collapse = ", ")
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
    cat("\n")
    
    code <- input$new_hs_code
    if (! code %in% c(data.base$hs.code.6, data.base$hs.code.4, data.base$hs.code.2)) {
      showNotification("Suggested code not found",duration = 5)
    } else {
      returned <- gta_hs_code_check(as.numeric(code))
      returned <- as.character(sprintf("%06s",returned) %>% gsub(pattern = " ", replacement = "0", x = .))
      data.returned <- subset(data.base, hs.code.6 %in% returned) 
      
      data.ledger.user$codes <- unique(c(data.ledger.user$codes, data.returned$hs.code.6))
      data.ledger.user$selected <- unique(c(data.ledger.user$selected, data.returned$hs.code.6))
      data.ledger$codes <- data.ledger$codes[! data.ledger$codes %in% data.ledger.user$codes]
      data.ledger$selected <- data.ledger$selected[data.ledger$selected %in% data.ledger$codes]
      
      data.subset <- unique(rbind(data.returned, data.subset))
      data.subset$indicator[data.subset$hs.code.6 %in% data.ledger.user$codes] <- "<div class='indicator user'></div>"
      row.names(data.subset) <- NULL
      data.subset <<- data.subset
      userinput <<- T
      click("names.refresh")
      reset("new_hs_code")
      
    }
  })
  
  
  # Conditionally show search term infos
  output$condSearchTerm <- reactive({
    if (is.null(user$name)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  outputOptions(output, "condSearchTerm", suspendWhenHidden = FALSE)
  
  
  
  # Report terms which are not a product 
  observeEvent(input$not.product, {
    
    current.user.id=user$id
    current.job.id=job.id$id
    
    new.check.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_check_log (user_id, time_stamp, check_successful, job_id)
                                                  VALUES (",current.user.id,",CURRENT_TIMESTAMP,1,",current.job.id,");
                                                  SELECT MAX(check_id) FROM hs_check_log;"),
                                          output.queries = 2)
    
    gta_sql_update_table(paste0("INSERT INTO hs_report_services (user_id,check_id,phrase_id)
                                 VALUES (",current.user.id,",",new.check.id,",",phr.id$id,");"))
    
    p.round <- gta_sql_get_value(paste0("SELECT processing_round FROM hs_phrase_log WHERE phrase_id = ", phr.id$id))
    gta_sql_update_table(paste0("INSERT INTO hs_check_phrases (check_id, phrase_id, processing_round)
                                 VALUES (",new.check.id,",",phr.id$id,",",p.round,");"))
    
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
  
  # OBSERVE CHOSEN USER VALUE
  observeEvent(input$users, {
    
    if (input$users=="Select") {
      user$name <- NULL
      user$id <- 0
    } else {
      user$name <- input$users
      updateTextInput(session,
                      "import.email.adress",
                      value = gta_sql_get_value(paste0("SELECT user_email from gta_user_log WHERE user_login ='",user$name,"';")))
      user$id <- gta_sql_get_value(paste0("SELECT user_id from gta_user_log WHERE user_login ='",user$name,"';"))
      refresh_names()
    }
    
    
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
    cat("\n")
    
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
                                             FROM hs_job_phrase
                                             LEFT OUTER JOIN (SELECT phrase_id AS phrase_id2, COUNT(check_id) AS count_column
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
                                                                                                                                                         WHERE user_id = ",user$id,
                                            ")))
                                                                                             GROUP BY job_id
                                                                                             ORDER by is_priority DESC, COUNT(jp.phrase_id) ASC
                                                                                             LIMIT 1) 
                                                                                            AS tbl_jobs_priority))
                                                  
                                                   GROUP BY phrase_id) AS tbl_of_checks
                                          ON hs_job_phrase.phrase_id=tbl_of_checks.phrase_id2
                                          WHERE job_id=(SELECT job_id 
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
                                                                                                                                                         WHERE user_id = ",user$id,
                                            ")))
                                                                                             GROUP BY job_id
                                                                                             ORDER by is_priority DESC, COUNT(jp.phrase_id) ASC
                                                                                             LIMIT 1)
                                          AS tbl_jobs_priority2)
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
                                                                                                               WHERE user_id = ",user$id,
                                            ")))
                                          ORDER by tbl_of_checks.count_column DESC
                                          LIMIT 1"))
      
      # THIS IS A DUMMY QUERY THAT SHOWS THE ESSENCE OF THE UPDATE which accounts for COUNT()=NA      
      # query2=paste0("SELECT phrase_id
      #          FROM hs_job_phrase
      #          LEFT OUTER JOIN (SELECT phrase_id AS phrase_id2, COUNT(check_id) AS count_column
      #                FROM hs_check_phrases AS tbl1
      #                WHERE phrase_id IN (SELECT phrase_id 
      #                                    FROM hs_job_phrase
      #                                    WHERE job_id =62)
      #          GROUP BY phrase_id) AS tbl_of_checks
      #          ON hs_job_phrase.phrase_id=tbl_of_checks.phrase_id2
      #          WHERE job_id=62
      #          ORDER by tbl_of_checks.count_column DESC
      #          LIMIT 1")
      # 
      # gta_sql_get_value(query2)
      
      
      if(is.na(next.phrase) == F) {
      
        phr.id$id <- next.phrase
      
        job.id$id <- gta_sql_get_value(paste0("SELECT job_id FROM hs_job_phrase WHERE (phrase_id = ",phr.id$id, " AND processed = 0) LIMIT 1")) # CHECKLATER: How do we know which job is being processed?

        query.temp <- paste(unlist(strsplit(as.character(gta_sql_get_value(paste0("SELECT phrase FROM hs_phrase_log WHERE phrase_id =",phr.id$id)))," ")))
        query$phrase <<- query.temp
        updateCheckboxGroupButtons(session, "query.refine", choices = query.temp, selected = query.temp)
        
        data.subset <- subset(data.base, as.numeric(hs.code.6) %in% gta_sql_get_value(paste0("SELECT hs_code_6 FROM hs_code_suggested WHERE phrase_id =",phr.id$id)))
        row.names(data.subset) <- NULL
        data.subset <<- data.subset
        
      } else {
        showNotification("You are all done, thank you!", duration = 1000)
        
        phr.id$id <- 0
        query$phrase <- ""
        updateCheckboxGroupButtons(session, "query.refine", choices = "", selected = "")
        
        data.subset <- data.base[NULL,]
        row.names(data.subset) <- NULL
        data.subset <<- data.subset
      }
      
    }
    
    
    if (type=="empty") {
      
      phr.id$id <- 0
      query$phrase <- ""
      updateCheckboxGroupButtons(session, "query.refine", choices = query$phrase, selected = query$phrase)
      
      data.subset <- data.base[NULL,]
      row.names(data.subset) <- NULL
      data.subset <<- data.subset
      
    }
    
    # To keep track of changes in the apps state
    # data.ledger$selected <- 0
    # data.ledger$selected[data.ledger$hs.code.6 %in% unique(data.subset$hs.code.6)] <- 1
    # data.ledger <<- data.ledger
    load.new.phrase <<- T
    click("names.refresh")
  }
  
  # Functions for HS Code finder App
  save_selection <- function(type) {
    print("SAVE_SELECTION()")
    cat("\n")
    
    
    # load_all(path) 
    toggleClass("loading","active")
    
    if(is.null(user$name)){
      showNotification("Please select or create a user before saving your selection",duration = 1000)
      toggleClass("loading","active")
    } else if (is.null(input$radio1)==T) {
      showNotification("Please select a confidence level",duration = 5)
      toggleClass("loading","active")
    } else {
      
      
      
      if (type %in% c("standard","clipboard")) {
        
        # Variables catching the number of rows to be inserted, to check later if insertion was successful
        # checks <- list()
        
        # check if phrase has been adjusted
        # phrase.log
        
        # codes.suggested
        # get user added phrases, which are not yet stored in the code.suggested table
        suggested.new <- data.ledger.user$selected[! data.ledger.user$selected %in% gta_sql_get_value(paste0("SELECT hs_code_6 
                                                                                                              FROM hs_code_suggested
                                                                                                              WHERE phrase_id=",phr.id$id,";"))]
        
        
        # UDPATE CODE SUGGESTED WITH USER ADDED PHRASES
        if (length(suggested.new)>0) {
          gta_sql_multiple_queries(paste0("INSERT INTO hs_code_suggested (phrase_id, hs_code_6, probability) 
                                          VALUES ",paste0("(",phr.id$id,",",suggested.new,", NULL)",collapse=", ")), output.queries = 1)
        }
        
        
        # UPDATE CODE SOURCE WITH NEW CODE SUGGESTED ROWS
        gta_sql_multiple_queries(paste0("DROP TABLE IF EXISTS hs_cs_temp;
        
                                        CREATE TABLE hs_cs_temp AS
                                        SELECT * 
                                        FROM hs_code_suggested
                                        WHERE phrase_id =",phr.id$id,"
                                        AND suggestion_id NOT IN (SELECT suggestion_id
                                                              FROM hs_code_source);
                                        
                                        ALTER TABLE hs_cs_temp ADD source_id int DEFAULT 1;
                                        
                                        INSERT INTO hs_code_source
                                        SELECT hs_new.suggestion_id, hs_new.source_id
                                        FROM hs_cs_temp hs_new
                                        WHERE phrase_id = ",phr.id$id,";
                                        
                                        DROP TABLE IF EXISTS hs_cs_temp;"),
                                 output.queries = 1)
        
        
        # IF NEW PHRASE, ADD OLD PHRASE SUGGESTIONS TO NEW PHRASE
        if (! tolower(paste(input$query.refine, collapse=" ")) %in% unique(tolower(gta_sql_get_value("SELECT phrase FROM hs_phrase_log")))) {
          
          
          new.phr.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_phrase_log (phrase, source, processing_round, exit_status)
                                                  VALUES ('",tolower(paste(input$query.refine, collapse = " ")),"','adjusted',1,1);
                                                  SELECT MAX(phrase_id) FROM hs_phrase_log;"),
                                              output.queries = 2)
          
          # checks <- c(checks, list("nrow.phraselog" = 1))
          
          if(is.na(new.phr.id)){
            stop("SAVE_SELECTION - new phrase: The phrase log update went wrong. I cannot find the phrase I just added.")
          }  else {
            
            new.phr.id <- new.phr.id  
            
          }
          
          # update code.suggested to include the values of the original phrase ID for the new one
          gta_sql_multiple_queries(paste0("DROP TABLE IF EXISTS hs_cs_temp;
                                      CREATE TABLE hs_cs_temp AS
                                      SELECT * 
                                      FROM hs_code_suggested
                                      WHERE phrase_id =",phr.id$id,";
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
                                       AND hs_old.phrase_id=",phr.id$id,";"))
          
          # checks <- c(checks, list("newphrase.source.new" = nrow(new.code.source)))
          
        } else {
          new.phr.id <- phr.id$id
        } 
        
        
        print(paste0("PHR ID: ",phr.id$id))
        print(paste0("NEW PRHASE ID: ",new.phr.id))
        
        # CREATE NEW CHECK and store its ID
        # Check.log
        
        current.user.id=user$id
        current.job.id=job.id$id
        
        this.check.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_check_log (user_id, time_stamp, check_successful, job_id)
                                                  VALUES (",current.user.id,",CURRENT_TIMESTAMP,1,",current.job.id,");
                                                  SELECT MAX(check_id) FROM hs_check_log;"),
                                               output.queries = 2)
        rm(current.user.id, current.job.id)
        
        
        
        # Add code.selected
      
        gta_sql_multiple_queries(paste0("DROP TABLE IF EXISTS hs_cs_temp;
        
                                      CREATE TABLE hs_cs_temp AS
                                      SELECT * 
                                      FROM hs_code_suggested 
                                      WHERE phrase_id = ",phr.id$id,"
                                      AND hs_code_6 IN(",paste(paste0("'",as.numeric(c(data.ledger$selected, data.ledger.user$selected)),"'"), collapse=","),");
                                      
                                      ALTER TABLE hs_cs_temp
                                      ADD check_id INT NULL;
                                      
                                      UPDATE hs_cs_temp
                                      SET check_id = ",this.check.id,";
                                      
                                      INSERT INTO hs_code_selected
                                      SELECT check_id, suggestion_id
                                      FROM hs_cs_temp;
                                      
                                      DROP TABLE IF EXISTS hs_cs_temp;
                                      "),
                                 output.queries = 1)
        
        # Add code selected suggestion ids for new.phr.id
        if(new.phr.id != phr.id$id) {
          gta_sql_multiple_queries(paste0("DROP TABLE IF EXISTS hs_cs_temp;
        
                                      CREATE TABLE hs_cs_temp AS
                                      SELECT * 
                                      FROM hs_code_suggested 
                                      WHERE phrase_id = ",new.phr.id,"
                                      AND hs_code_6 IN(",paste(paste0("'",as.numeric(c(data.ledger$selected, data.ledger.user$selected)),"'"), collapse=","),");
                                      
                                      ALTER TABLE hs_cs_temp
                                      ADD check_id INT NULL;
                                      
                                      UPDATE hs_cs_temp
                                      SET check_id = ",this.check.id,";
                                      
                                      INSERT INTO hs_code_selected
                                      SELECT check_id, suggestion_id
                                      FROM hs_cs_temp;
                                      
                                      DROP TABLE IF EXISTS hs_cs_temp;
                                      "),
                                   output.queries = 1)
        }
        
        # Check.phrases
        # checks <- c(checks, list("check.phrases.new" = 0))
        toCheck <- c(phr.id$id)
        if(phr.id$id != new.phr.id) { toCheck = c(toCheck, new.phr.id) }
        
        for(p.id in toCheck){
          
          # checks[['check.phrases.new']] <- checks[['check.phrases.new']]+1
          p.round <- gta_sql_get_value(paste0("SELECT processing_round FROM hs_phrase_log WHERE phrase_id = ", p.id))
          # WHERE SHOULD p.round be defined, check.phrases will be updated with a new row for each phrase, how would processing round increase here?
          # CHECKLATER: replaced p.round with 1, as not sure if processing round really needed here
          gta_sql_update_table(paste0("INSERT INTO hs_check_phrases (check_id, phrase_id, processing_round)
                                       VALUES (",this.check.id,",",p.id,",",p.round,");"))
          
        }
        
        
        # words.removed
        words.all <- paste(unlist(strsplit(as.character(tolower(gta_sql_get_value(paste0("SELECT phrase 
                                                                                         FROM hs_phrase_log 
                                                                                         WHERE phrase_id =",phr.id$id,";"))))," ")))
        removed <- words.all[! words.all %in% paste(unlist(strsplit(as.character(tolower(input$query.refine))," ")))]
        
        # checks <- c(checks, list("words.removed" = length(removed)))
        
        if (length(removed) > 0) {
          
          for(rm.wd in unique(removed)){
            
            gta_sql_update_table(paste0("INSERT INTO hs_words_removed (check_id, words_removed)
                                         VALUES (",this.check.id,",'",rm.wd,"');"))
            
          }
        }
        
        
        if (input$suggestions.search.terms != "") {
          
          for(add.wd in unique(strsplit(input$suggestions.search.terms,split=';', fixed=TRUE))[[1]]) {
            add.wd <- trimws(add.wd, which="both")
            gta_sql_update_table(paste0("INSERT INTO hs_additional_suggestions (check_id, user_id, term)
                                         VALUES (",this.check.id,",",user$id,",'",add.wd,"');"))
            
          }
          
          # checks <- c(checks, list("additional.suggestions" = length(strsplit(input$suggestions.search.terms,split=';', fixed=TRUE))))
          
        }
      
          # check.certainty
        gta_sql_update_table(paste0("INSERT INTO hs_check_certainty (check_id, certainty_level)
                                    VALUES (",this.check.id,",'",input$radio1,"');"))
        
        
        # Updating job.phrase (only for original phrase.id, not new phrase id [if exists])
        successful.checks=gta_sql_get_value(paste0("SELECT COUNT(DISTINCT check_id)
                                            FROM hs_check_log
                                            WHERE check_successful=1
                                            AND check_id IN (
                                                   SELECT check_id
                                                   FROM hs_check_phrases
                                                   WHERE phrase_id =",phr.id$id,"
                                            );"))
        
        
        
        ## looping over all jobs that happen to include this phrase
        jobs.incl.phrase=gta_sql_get_value(paste0("SELECT job_id
                                                 FROM hs_job_phrase
                                                  WHERE phrase_id =",phr.id$id,";"))
        if(is.na(jobs.incl.phrase[1])==F){
          
          for(j.id in jobs.incl.phrase){
            required.checks=gta_sql_get_value(paste0("SELECT nr_of_checks
                                                     FROM hs_job_log
                                                     WHERE job_id =",j.id,";"))
            
            
            
            ## EXCLUDE JOBS THAT ARE ALREADY MARKED AS PROCESSED AS TO PREVENT THEM FROM ADDING +1 TO THE NR OF COMPLETED JOBS
            nround <- gta_sql_get_value(sqlInterpolate(pool, "SELECT processing_round FROM hs_phrase_log WHERE phrase_id = ?phraseID;", phraseID = phr.id$id))
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
              print(paste0("PHRASE ID: ",phr.id$id))
              print(paste0("SUCCESSFUL CHECKS: ",successful.checks))
              print(paste0("REQUIRED CHECKS: ",required.checks))
              print(paste0("PROCESSING ROUND: ",nround))
              print(paste0("CALC PROB: ",calc.prob))
              cat("\n")
              
              if(calc.prob){
                
                # DECIDE EXIT STATUS
                # 2 (PROCESSED) IF CODE SELECTED AND CODE SUGGESTED ARE AVAILABLE
                # 3 (NOT A PRODUCT) IF MAJORITY OF CHECKS LABEL AS "NOT A PRODUCT"
                # 4 (NO CODES) IF CHECKED ENOUGH TIMES BUT NO CODES HAVE BEEN SELECTED FOR THIS PHRASE
                # 5 (ROUND LIMIT) IF NROUND >=4
                
                sql <- "SELECT * FROM hs_report_services WHERE phrase_id = ?phraseID;"
                querysql <- sqlInterpolate(pool,
                                           sql,
                                           phraseID = phr.id$id)
                services=gta_sql_get_value(querysql)
                
                sql <- "SELECT * FROM hs_check_phrases WHERE phrase_id = ?phraseID;"
                querysql <- sqlInterpolate(pool,
                                           sql,
                                           phraseID = phr.id$id)
                all.checks=gta_sql_get_value(querysql)
                
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
                                                           WHERE phrase_id = ",phr.id$id,"
                                                           );"))
                  
                  
                  if(nr.chosen.codes==0) {
                    # no codes found
                    exit.status <- 4
                    
                  } else {
                    
                    # SAVE PROBABILITIES FOR THAT PHRASE
                    phr.id.probability.future <- phr.id$id
                    # gta_hs_classify_results(processed.phrase = phr.id$id,
                                            # job.id=j.id) 
                    future({ gta_hs_classify_results(processed.phrase = phr.id.probability.future,
                                                     job.id=j.id) }) %...>% {
                                                     print(paste0("Phrase ",phr.id.probability.future," processed"))
                                                     }
                    
                    
                  }
                }
              }
            }
            
            if (exit.status %in% c(3,4,5)) {
              sql <- "UPDATE hs_phrase_log SET exit_status = ?exitStatus WHERE phrase_id = ?phraseID;"
              querysql <- sqlInterpolate(pool,
                                         sql,
                                         exitStatus = exit.status,
                                         phraseID = phr.id$id)
              gta_sql_update_table(querysql)

              sql <- "UPDATE hs_job_phrase SET processed = 1 WHERE (phrase_id = ?phraseID AND job_id = ?jobID);"
              querysql <- sqlInterpolate(pool,
                                         sql,
                                         phraseID = phr.id$id,
                                         jobID = j.id)
              gta_sql_update_table(querysql)
              
              gta_hs_check_job_completion(j.id)
            }
            
            rm(required.checks)
            
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
        
        current.user.id=user$id
        current.job.id=job.id$id
        
        this.check.id=gta_sql_multiple_queries(paste0("INSERT INTO hs_check_log (user_id, time_stamp, check_successful, job_id)
                                                  VALUES (",current.user.id,",CURRENT_TIMESTAMP,0,",current.job.id,");
                                                  SELECT MAX(check_id) FROM hs_check_log;"),
                                               output.queries = 2)
        rm(current.user.id,current.job.id)
        
        
        
        # Check.phrases
        p.round <- gta_sql_get_value(paste0("SELECT processing_round FROM hs_phrase_log WHERE phrase_id = ", phr.id$id))
        print(paste0("THIS IS THE P ROUND NONE FOUND: ",p.round))
        gta_sql_update_table(paste0("INSERT INTO hs_check_phrases (check_id, phrase_id, processing_round)
                                     VALUES (",this.check.id,",",phr.id$id,",",p.round,");"))
        
        
        # check.certainty
        gta_sql_update_table(paste0("INSERT INTO hs_check_certainty (check_id, certainty_level)
                                    VALUES (",this.check.id,",'",input$radio1,"');"))
      }
      
      
      # START NEW ASYNC HS_CODE_FINDER SEARCH FOR THAT TERM
      phr.id.future <- phr.id$id
      query.refine.future <- input$query.refine
      # query.refine.future <<- "keyring"
      # phr.id.future <<- 2561

      future({ gta_hs_code_finder(products = tolower(paste(query.refine.future, collapse=" ")))}) %...>%  {
        found.temp <- .
        # found.temp <- gta_hs_code_finder(products = tolower(paste(query.refine.future, collapse=" ")))
        
        gta_sql_update_table("DROP TABLE IF EXISTS hs_found_temp;")
        
        found.temp=splitstackshape::cSplit(found.temp, which(names(found.temp)=="source.names"), sep="; ", direction="long")
        gta_sql_create_table(write.df = "found.temp")
        rm(found.temp)
        
        before <- nrow(gta_sql_get_value(paste0("SELECT * FROM hs_code_suggested WHERE phrase_id = ",phr.id.future)))
        
        gta_sql_multiple_queries(paste0("DELETE FROM hs_found_temp
                                        WHERE hs_code IN (SELECT hs_code_6
                                                          FROM hs_code_suggested
                                                          WHERE phrase_id =",phr.id.future,");
                                                          
                                        ALTER TABLE hs_found_temp
                                        ADD phrase_id INT NULL;
                                        
                                        UPDATE hs_found_temp
                                        SET phrase_id = ",phr.id.future,";
                                        
                                        ALTER TABLE hs_found_temp
                                        ADD probability DOUBLE NULL;
                                      
                                        INSERT INTO hs_code_suggested (phrase_id, hs_code_6, probability)
                                        SELECT phrase_id, hs_code, probability
                                        FROM hs_found_temp;

                                        INSERT INTO hs_code_source
                                        SELECT hs_sug.suggestion_id, hs_src.source_id
                                        FROM hs_found_temp hs_found
                                        JOIN hs_code_suggested hs_sug
                                        ON hs_found.hs_code=hs_sug.hs_code_6
                                        AND hs_found.phrase_id=hs_sug.phrase_id
                                        JOIN hs_suggestion_sources hs_src
                                        ON hs_found.source_names=hs_src.source_name;
                                        
                                        DROP TABLE IF EXISTS hs_found_temp;
                                        "),
                                 output.queries = 1)
        
        after <- nrow(gta_sql_get_value(paste0("SELECT * FROM hs_code_suggested WHERE phrase_id = ",phr.id.future)))
        print(paste0(after-before, " NEW SUGGESTIONS FOR PHRASE ", phr.id.future))
                       
        
      }
      
      
      if (type %in% c("clipboard","unrelated_search")) {
        runjs("var copyText = document.getElementById('selected_codes_output');
              copyText.select();
              document.execCommand('copy');
              //alert('Copied the text: ' + copyText.value);
              ")
        # clipr::write_clip(printSelected())
      }
      
      codes.old <- paste(c(data.ledger$selected, data.ledger.user$selected))
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
