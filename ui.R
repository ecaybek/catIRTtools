library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)

ui <- navbarPage(
  title = "catIRTtools", theme = shinytheme("spacelab"),
  ## CAT Simulations
  tabPanel("CAT Simulation",
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
        bsTooltip(id = "exp_ctrl", title = "You'll get disconnect if you have small item pool size.", 
                  placement = "left", trigger = "hover"),
        bsTooltip(id = "seed", title = "Seed is not used when the replication > 1.", 
                  placement = "bottom", trigger = "hover"),
        
        fluidRow(
          column(4, selectInput("irt_model", "IRT model", c("1PL", "2PL", "3PL", "4PL", "GRM", "MGRM", "PCM", "GPCM", "RSM", "NRM"), "GRM")),
          column(4, selectInput("item_sel_method", "Item Selection", c("MFI", "MEI", "MLWI", "MPWI", "MEPV", "Random" = "random"), "MFI")),
          column(4, selectInput("est_method", "Theta Estimation", c("ML", "MAP (BM)" = "BM", "EAP", "WL", "ROB"), "BM"))
        ),
        fluidRow(
          column(4, selectInput("const_d", "Scaling (D)", c("1.702", "1.000"), "1.702")),
          column(4, selectInput("first_item", "First Item", c("Theta = 0" = "0", "Random Theta" = "random"))),
          column(4, numericInput("exp_ctrl", "Max Item Exposure Rate", 1, min = 0, max = 1, step = 0.1))
        ),
        fluidRow(
          column(4, selectInput("cb_ter", "Termination Criteria", c("Variable", "Fixed"), "Variable")),
          column(4, shinyjs::hidden(numericInput("ter_se", "SE", 0.40, min = 0, max = 1, step = 0.05))),
          column(4, numericInput("ter_length", "Test Length", 10, min = 1, step = 1))
        ),
        fluidRow(
          column(4, numericInput("seed", "Seed", 26, min = 1, step = 1)),
          column(4, numericInput("rep", "Replication(s)", 1, min = 1, max = 500, step = 1)),
          column(4)
          ),
        p(HTML('<b>Generate</b>')),
        fluidRow(
          column(4, checkboxInput("generate_item", "Item parameters", FALSE)),
          column(8, selectInput("generate_theta", "Theta with", c("Normal Distribution", "Increment"), "Normal Distribution"))
        ),
        
        div(
          id = "gen_res",
          fluidRow(
            column(4, numericInput("min_theta", "Min Theta", -3.0)),
            column(4, numericInput("max_theta", "Max Theta", 3.0)),
            column(4, numericInput("increment", "Theta Increment", 0.10, min = 0.001, max = 1))
          )),
        shinyjs::hidden(div(
          id = "gen_theta",
          fluidRow(
            column(4, numericInput("theta_gen_mean", "Mean", 0)),
            column(4, numericInput("theta_gen_sd", "SD", 1)),
            column(4, numericInput("theta_gen_n", "Sample Size", 100, min = 5))
          ))),
        div(
          id = "gen_item",
          fluidRow(
            column(6, numericInput("item_no", "How many items to generate?", 10, min = 1, step = 1)),
            column(6, numericInput("item_cat", "Categories", 5, min = 2))
          )
        ),
        br(),
        p(HTML('<b>Upload</b>')),
        fluidRow(
          column(4, checkboxInput("upload_item", "Item parameters", FALSE)),
          column(4, checkboxInput("upload_response", "Responses", FALSE)),
          column(4, checkboxInput("upload_theta", "Theta values", FALSE))
        ),
        div(
          id = "up_item",
          fileInput("file_par", "File for Item Parameters",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          fluidRow(
            column(6, radioButtons("sep_par", "Seperator for File", c("Semicolon" = ";", "Comma" = ","))),
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
            column(6, radioButtons("sep_theta", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header_theta", "First row as header", TRUE))
          )
        ),
        p(strong("Options for Output Save")),
        fluidRow(
          column(6, selectInput("out_sep", "Seperator for CSV", c(";", ",", "|"), ";")),
          column(6, selectInput("out_dec_sep", "Decimal Mark for CSV", c(".", ","), "."))
        ),
        actionButton("submit", "Submit")
      ),
      ### Results
      mainPanel(
        div(id = "results", 
            print("Your results will be here after you submit your request."),
            br(),
            print("Simulations may take time according to your dataset.")),
        verbatimTextOutput("results_cat"),
        shinyjs::hidden(div(
          id = "results_all",
          print(p(strong("YOU CAN DOWNLOAD THE DATA & THE RESULTS BELOW."))),
          fluidRow(
            column(3, downloadButton("download_items", "Item Parameters")),
            column(3, downloadButton("download_res", "Full Responses")),
            column(3, downloadButton("download_sim", "Simulation Results")),
            column(3, downloadButton("download_cond", "Conditional Results"))
          )
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
        selectInput("mirt_poly", "IRT Model", c("GRM" = "graded", "GPCM" = "gpcm", "RSM" = "rsm", "Nominal" = "nominal"), "GRM"),
        shinyjs::hidden(div(
          id = "mirt_dicho",
          selectInput("mirt_dicho", "IRT Model", c("Rasch", "2PL", "3PL", "4PL"),"3PL"))),
        fluidRow(
          column(6, selectInput("mirt_est", "Theta Estimation Method", c("EAP", "MAP", "ML", "WLE"), "MAP")),
          column(6, selectInput("mirt_D", "D", c("1.702", "1.000"), "1.702"))),
        numericInput("yensur", "Hide Yen's Q3 statistics below:", 0.37),
        p(strong("Options for Output Save")),
        fluidRow(
          column(6, selectInput("mirt_out_sep", "Seperator for CSV", c(";", ",", "|"), ";")),
          column(6, selectInput("mirt_out_dec_sep", "Decimal Mark for CSV", c(".", ","), "."))
        ),
        actionButton("mirt_calibrate", "Calibration & Estimation")
      ),
      
      ### Results
      mainPanel(
        shinyjs::hidden(div(
          id = "download_mirt_res",
          print(p(strong("Yen's Q3 Statistics for Local Independency"))),
          DT::dataTableOutput("results_mirt", width = "80%", height = "auto"),
          print(p(strong("You can download results below"))),
          fluidRow(
            column(4, downloadButton("download_mirt_par", "Download Item Parameters")),
            column(4, downloadButton("download_mirt_theta", "Download Theta Estimates")),
            column(4, downloadButton("download_mirt_plots", "Download Plots"))
          )
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


