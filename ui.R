library(shiny)
library(shinythemes)
library(shinyjs)

ui <- navbarPage(
  title = "catIRTtools", theme = shinytheme("flatly"),
  ## CAT Simulations
  tabPanel(
    "CAT Simulation",
    
    tags$head(tags$style(HTML("
                  .selectize-input, .selectize-dropdown, .control-label,  .form-control, label {
                            font-size: 100%;
                  }
                  .shiny-file-input-progress {
                            display: none
                  }
                  "))),
    shinyjs::useShinyjs(),
    p(strong("Generate or Upload Item Parameters / Responses & Run CAT Simulations")),
    ### Sidebar
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, selectInput("irt_model", "IRT model", c("1PL", "2PL", "3PL", "4PL", "GRM", "MGRM", "PCM", "GPCM", "RSM", "NRM"), "GRM")),
          column(6, selectInput("item_sel_method", "Item Selection", c("MFI", "MEI"), "MFI"))
        ),
        fluidRow(
          column(6, selectInput("est_method", "Theta Estimation", c("ML", "WL", "EAP", "ROB"), "EAP")),
          column(6, selectInput("const_d", "Scaling (D)", c("1.702", "1.000"), "1.702"))
        ),
        fluidRow(
          column(7, HTML('Do not terminate the test before applying')),
          column(3, numericInput("ter_min_item", NULL, 3, min = 1)),
          column(2, HTML('items'))
        ),
        p(HTML('<b>Termination Criteria</b>')),
        fluidRow(
          column(6, checkboxInput("cb_ter_fixed", "Fixed", FALSE)),
          column(6, checkboxInput("cb_ter_var", "Variable", FALSE))
        ),
        div(
          id = "ter_fixed",
            numericInput("ter_length", "Test Length", 10, width = "50%")
          ),
        div(
          id = "ter_var",
          numericInput("ter_se", "SE", 0.40, width = "50%")
        ),
        fluidRow(
          column(6, numericInput("seed", "Seed", 26)),
          column(6, numericInput("replicate", "Replication(s)", 1))
          ),
        p(HTML('<b>Generate</b>')),
        fluidRow(
          column(4, checkboxInput("generate_item", "Item parameters", FALSE)),
          column(4, checkboxInput("generate_theta", "Theta values", FALSE)),
          column(4, checkboxInput("generate_response", "Responses", FALSE))
        ),
        div(
          id = "gen_item",
          fluidRow(
            column(6, numericInput("item_no", "How many items to generate?", 10, min = 1)),
            column(6, numericInput("item_cat", "Categories", 5, min = 2))
          )
        ),
        div(
          id = "gen_res",
          fluidRow(
            column(4, numericInput("min_theta", "Min Theta", -3.0)),
            column(4, numericInput("max_theta", "Max Theta", 3.0)),
            column(4, numericInput("increment", "Theta Increment", 0.10))
          )),
        div(
          id = "gen_theta",
          fluidRow(
            column(4, numericInput("theta_gen_mean", "Mean", 0)),
            column(4, numericInput("theta_gen_sd", "SD", 1)),
            column(4, numericInput("theta_gen_n", "Sample Size", 100))
          )),
        br(),
        p(HTML('<b>Upload</b>')),
        fluidRow(
          column(4, checkboxInput("upload_item", "Item parameters", FALSE)),
          column(4, checkboxInput("upload_theta", "Theta values", FALSE)),
          column(4, checkboxInput("upload_response", "Responses", FALSE))
        ),
        div(
          id = "up_item",
          fileInput("file_par", "File for Item Parameters",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          fluidRow(
            column(6, radioButtons("sep_par", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header_par", "First row as header", TRUE))
          )
        ),
        div(
          id = "up_res",
          fileInput("file_res", "File for Responses",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          fluidRow(
            column(6, radioButtons("sep_res", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header_res", "First row as header", TRUE))
          )
        ),
        div(
          id = "up_theta",
          fileInput("file_theta", "File for Theta",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          fluidRow(
            column(6, radioButtons("sep_res", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header_res", "First row as header", TRUE))
          )
        ),
        p(strong("Options for Output Save")),
        fluidRow(
          column(6, selectInput("out_sep", "Seperator for CSV", c(";", ",", "|"), ";")),
          column(6, selectInput("out_dec_sep", "Decimal Mark for CSV", c(".", ","), ".")),
        ),
        actionButton("submit", "Submit")
      ),
      ### Results
      mainPanel(
        div(id = "results", print("Your results will be here after you submit your request")),
        dataTableOutput("results_cat"),
        dataTableOutput("results_rep"),
        shinyjs::hidden(div(
          id = "results_all",
          print(p(strong("YOU CAN DOWNLOAD THE DATA & THE RESULTS BELOW."))),
          fluidRow(
            column(4, downloadButton("download_items", "Item Parameters")),
            column(4, downloadButton("download_res", "Responses")),
            column(4, downloadButton("download_sim", "Simulation Results"))
          ),
        ))
      )
    )
  ),
  
  ## IRT Calibrations
  tabPanel(
    "IRT Calibration",
    p(strong("IRT Calibration")),
    sidebarLayout(
      ### Sidebar
      sidebarPanel(
        fileInput("mirt_res", "File for responses",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput("ispoly", "Polytomous Items?", FALSE),
        selectInput("mirt_poly", "IRT Model", c("GRM" = "graded", "GPCM" = "gpcm"), "GRM"),
        shinyjs::hidden(div(
          id = "mirt_dicho",
          selectInput("mirt_dicho", "IRT Model", c("Rasch", "2PL", "3PL", "4PL"),"3PL"))),
        selectInput("mirt_est", "Theta Estimation Method", c("EAP", "MAP"), "EAP"),
        selectInput("mirt_D", "D", c("1.702", "1.000"), "1.702"),
        actionButton("mirt_calibrate", "Calibration & Estimation")
      ),
      
      ### Results
      mainPanel(
        shinyjs::hidden(div(
          id = "download_mirt_res",
          print(p(strong("Yen's Q3 Statistics for Local Independency"))),
          dataTableOutput("results_mirt"),
          print(p(strong("You can download results below"))),
          print(p(strong("Download Options"))),
          fluidRow(
            column(4, textInput("out_sep", "Seperator", ";")),
            column(4, textInput("out_dec_sep", "Decimal Seperator", "."))
          ),
          fluidRow(
            column(4, downloadButton("download_mirt_par", "Download Item Parameters")),
            column(4, downloadButton("download_mirt_theta", "Download Theta Estimates"))
          ),
          br(),
          fluidRow(
            column(4, downloadButton("download_mirt_par_comma", "Download Item Parameters (with Comma)")),
            column(4, downloadButton("download_mirt_theta_comma", "Download Theta Estimates (with Comma)"))
          ),
          br(),
          fluidRow(column(4, downloadButton("download_mirt_plots", "Download Plots")))
        ))
      )
    )
  ),
  
  ## Help
  tabPanel(
    "Sample Files",
    p(strong("You can find item parameter and response sample files below.")),
    div(p("This is a demo app. If you have any question please contact: author {at} author.com")),
    div(a(href = "https://eptlab.com/itempar.csv", "Item parameter sample file")),
    div(a(href = "https://eptlab.com/responses.csv", "Response pattern sample file"))
  ),
  
  ## Privacy
  tabPanel(
    "Privacy",
    p(strong("We do not keep any of the files and results on our servers."))
  )
)