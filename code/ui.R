


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
                                choices = c("Choose User"="Select",gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`")),
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