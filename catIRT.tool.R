library(shiny)
library(shinythemes)
library(catR)
library(plyr)
library(shinyjs)
library(mirt)

# APP UI
ui <- navbarPage(
  title = "catIRT Tools", theme = shinytheme("flatly"),
  ## CAT Simulations
  tabPanel(
    "CAT Simulation",

    tags$head(tags$style(HTML("
                  .selectize-input, .selectize-dropdown, .control-label, .form-control, label {
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
        numericInput("ter_min_item", "Minimum item to apply", 3),
        numericInput("ter_se", "Maximum standard error to stop the test", 0.40),
        numericInput("seed", "Please enter seed in order to reproduce your results", 26),
        fluidRow(
          column(6, checkboxInput("generate_item", "Generate item parameters?", FALSE)),
          column(6, checkboxInput("generate_response", "Generate responses?", FALSE))
        ),
        div(
          id = "div_itemno",
          numericInput("item_no", "How many items to generate?", 10),
          numericInput("item_cat", "Categories", 5)
        ),
        shinyjs::hidden(div(
          id = "upload_item",
          fileInput("file_par", "File for Item Parameters"),
          fluidRow(
            column(6, radioButtons("sep_par", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header_par", "First row as header", TRUE))
          )
        )),
        br(),
        div(id = "theta_inc", selectInput("increment", "Please select the theta increment", c(0.5, 0.1, 0.05, 0.01), 0.1)),
        shinyjs::hidden(div(
          id = "upload_res",
          fileInput("file_res", "File for Responses"),
          fluidRow(
            column(6, radioButtons("sep_res", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header_res", "First row as header", TRUE))
          )
        )),
        actionButton("submit", "Submit")
      ),
      ### Results
      mainPanel(
        div(id = "results", print("Your results will be here after you submit your request")),
        dataTableOutput("results_cat"),
        shinyjs::hidden(div(
          id = "results_all",
          print(p(strong("YOU CAN DOWNLOAD THE DATA & THE RESULTS BELOW."))),
          fluidRow(
            column(4, downloadButton("download_items", "Item Parameters")),
            column(4, downloadButton("download_res", "Responses")),
            column(4, downloadButton("download_sim", "Simulation Results"))
          ),
          br(),
          fluidRow(
            column(4, downloadButton("download_items_comma", "Item Parameters (with Comma)")),
            column(4, downloadButton("download_res_comma", "Responses (with Comma)")),
            column(4, downloadButton("download_sim_comma", "Simulation Results (with Comma)"))
          )
        ))
      )
    )
  ),

  ## IRT Calibrations
  tabPanel(
    "IRT Analysis",
    p(strong("IRT Analysis")),
    sidebarLayout(
      ### Sidebar
      sidebarPanel(
        fileInput("mirt_res", "File for responses"),
        selectInput("mirt_model", "IRT Model", c("Rasch", "2PL", "3PL", "4PL", "GRM" = "graded", "GPCM" = "gpcm"), "GRM"),
        selectInput("mirt_est", "Theta Estimation Method", c("EAP", "MAP"), "EAP"),
        selectInput("mirt_D", "D", c("1.702", "1.000"), "1.702"),
        actionButton("mirt_calibrate", "Calibration & Estimation")
      ),

      ### Results
      mainPanel(
        shinyjs::hidden(div(
          id = "download_mirt_res",
          print(p(strong("You can download results below"))),
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
    "Help",
    p(strong("Help"))
  ),

  ## Privacy
  tabPanel(
    "Privacy",
    p(strong("We do not keep any of the files and results on our servers."))
  )
)

# APP SERVER
server <- function(input, output, session) {
  observeEvent(input$generate_item, {
    shinyjs::toggle(id = "div_itemno", animType = "fade")
    shinyjs::toggle(id = "upload_item", animType = "fade")
  })

  observeEvent(input$generate_response, {
    shinyjs::toggle(id = "theta_inc", animType = "fade")
    shinyjs::toggle(id = "upload_res", animType = "fade")
  })

  observeEvent(input$irt_model, {
    if (input$irt_model == "1PL" |
      input$irt_model == "2PL" |
      input$irt_model == "3PL" |
      input$irt_model == "4PL") {
      updateNumericInput(session, inputId = "category", value = 2, min = 2, max = 2)
    }
  })

  observeEvent(input$submit, {
    ItemGenModel <- input$irt_model
    if (input$irt_model == "GRM" |
      input$irt_model == "GPCM" |
      input$irt_model == "MGRM" |
      input$irt_model == "PCM" |
      input$irt_model == "RSM" |
      input$irt_model == "NRM") {
      SimModel <- input$irt_model
      ncat <- input$item_cat
    } else {
      SimModel <- NULL
      ncat <- 2
    }

    EstMethod <- input$est_method
    constD <- as.numeric(input$const_d)
    Seed <- input$seed
    ItemSelMethod <- as.character(input$item_sel_method)
    PoolSize <- input$item_no

    if (is.null(input$file_par)) {
      if (input$irt_model == "GRM" |
        input$irt_model == "GPCM" |
        input$irt_model == "MGRM" |
        input$irt_model == "PCM" |
        input$irt_model == "RSM" |
        input$irt_model == "NRM") {
        ItemPool <- as.matrix(genPolyMatrix(items = PoolSize, nrCat = ncat, model = ItemGenModel, seed = Seed, same.nrCat = TRUE))
      } else {
        ItemPool <- as.matrix(genDichoMatrix(items = PoolSize, model = ItemGenModel, seed = Seed))
      }
    }
    else {
      ItemPool <- read.csv2(input$file_par$datapath,
        header = input$header_par,
        sep = input$sep_par
      )
      ItemPool <- as.matrix(ItemPool)
    }

    if (is.null(input$file_res)) {
      Responses <- NULL
      for (i in seq(-3, 3, by = as.numeric(input$increment))) {
        Responses <- rbind(Responses, data.frame(t(c(i, genPattern(i, ItemPool, model = SimModel, seed = Seed)))))
      }
    } else {
      Responses <- read.csv2(input$file_res$datapath,
        header = input$header_res,
        sep = input$sep_res
      )
      Responses <- as.matrix(Responses)
      ThetaList <- NULL
      withProgress(message = "Estimating Theta", detail = "for case 0", value = 0, {
        for (j in 1:nrow(Responses)) {
          ThetaEst <- thetaEst(ItemPool, Responses[j, ], model = SimModel, D = constD, method = EstMethod)
          ThetaList <- rbind(ThetaList, ThetaEst)
          increment <- (1 / nrow(Responses))
          incProgress(increment, detail = paste("case", j))
          Sys.sleep(0.1)
        }
      })
      rownames(ThetaList) <- NULL
      rownames(Responses) <- NULL
      Responses <- cbind(data.frame(ThetaList), data.frame(Responses))
    }
    i <- NULL

    # Adjusting the variable names
    colnames(Responses) <- c("theta", paste("i", c(1:nrow(ItemPool)), sep = "_"))

    # Creating variables
    AdministeredItems <- data.frame()
    UsedItem <- NULL
    ThetaHat <- 0
    ThetaPast <- NULL
    ThetaAll <- NULL
    SE <- 1
    SEPast <- NULL
    SEPerson <- NULL
    PastItemPars <- NULL
    PastResponses <- NULL

    withProgress(message = "Simulating", detail = "for case 0", value = 0, {
      # CAT LOOP
      for (i in 1:nrow(Responses)) { # Run the loop for everyone
        while (SE > input$ter_se | length(UsedItem) < input$ter_min_item) { # Termination Rule
          FirstItem <- nextItem(ItemPool, model = SimModel, theta = ThetaHat, out = UsedItem, criterion = ItemSelMethod, D = constD)
          ItemNumber <- as.numeric(FirstItem$item)
          VarItem <- paste("i", ItemNumber, sep = "_")
          PastItemPars <- rbind(PastItemPars, FirstItem$par)
          PastResponses <- c(PastResponses, Responses[i, VarItem])
          ThetaHat <- thetaEst(PastItemPars, PastResponses, model = SimModel, method = EstMethod, D = constD)
          SE <- semTheta(ThetaHat, PastItemPars, PastResponses, model = SimModel, method = EstMethod, D = constD)
          SEPast <- rbind(SEPast, SE)
          UsedItem <- c(UsedItem, FirstItem$item)
          UsedItem <- as.numeric(UsedItem)
          ThetaPast <- cbind(ThetaPast, ThetaHat)
          if (length(UsedItem) == nrow(ItemPool)) {
            break
          }
        }

        UsedItemDF <- as.data.frame(t(UsedItem))
        ThetaAll <- rbind(ThetaAll, ThetaHat)
        AdministeredItems <- rbind.fill(AdministeredItems, UsedItemDF)
        SEPerson <- rbind(SEPerson, SE)
        UsedItem <- NULL
        ThetaHat <- 0
        SE <- 1
        SEPast <- NULL
        ThetaPast <- NULL
        PastResponses <- NULL
        PastItemPars <- NULL
        Increment <- (1 / nrow(Responses))
        incProgress(Increment, detail = paste("case", i))
        Sys.sleep(0.1)
      }
    })

    shinyjs::toggle(id = "results", animType = "fade")

    # Deleting row names to prevent "duplicated row names" error message
    rownames(ThetaAll) <- NULL
    rownames(AdministeredItems) <- NULL
    rownames(SEPerson) <- NULL
    kItem <- rowSums(!is.na(AdministeredItems))
    # Merging theta estimations and admnistered items
    ThetaNItems <- cbind(Responses[, 1], ThetaAll, SEPerson, kItem, AdministeredItems)
    # Let's name the columns
    colnames(ThetaNItems) <- c("full_theta", "est_theta", "se", "k_item", paste("used_i", 1:(ncol(ThetaNItems) - 4), sep = ""))
    # Correlation between true and estimated theta values
    Correlation <- cor(ThetaNItems[, 1], ThetaNItems[, 2])
    MeanSE <- apply(ThetaNItems["se"], 2, mean)
    MeanK <- mean(rowSums(!is.na(AdministeredItems)))

    output$results_cat <- renderDataTable({
      data.frame(r = Correlation, MeanSE = MeanSE, MeanK = MeanK)
    })

    shinyjs::show(id = "results_all", animType = "fade")

    observeEvent(input$csvdecimal, {
      if (input$csvdecimal == TRUE) {
        ThetaNItems <- format(ThetaNItems, decimal.mark = ",")
      }
    })
    output$download_items <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(ItemPool, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_items_comma <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(format(ItemPool, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_res <- downloadHandler(
      filename = function() {
        "responses.csv"
      },
      content = function(file) {
        write.table(Responses, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_res_comma <- downloadHandler(
      filename = function() {
        "responses.csv"
      },
      content = function(file) {
        write.table(format(Responses, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_sim <- downloadHandler(
      filename = function() {
        "simresults.csv"
      },
      content = function(file) {
        write.table(ThetaNItems, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_sim_comma <- downloadHandler(
      filename = function() {
        "simresults.csv"
      },
      content = function(file) {
        write.table(format(ThetaNItems, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
  })

  observeEvent(input$mirt_calibrate, {
    mirtData <- read.csv2(input$mirt.responses$datapath,
      header = TRUE,
      sep = ";"
    )
    mirtModel <- input$mirt_model
    mirtEst <- input$mirt_est
    mirtD <- input$mirt_D
    mirtCalib <- mirt(mirtData, itemtype = mirtModel, 1, method = mirtEst, D = mirtD)
    mirtItemPar <- coef(mirtCalib, IRTpars = T, simplify = T)
    mirtItemFit <- itemfit(mirtCalib)
    mirtTheta <- fscores(mirtCalib)

    # mirt.q3 <- residuals(mirt.calib, df.p = T, type="Q3", Theta=mirt.theta, suppress = .37)

    # mirt.plot.iteminfotrace <- itemplot(mirt.calib, 1, type="infotrace")
    # mirt.plot.infotrace <- plot(mirt.calib, type="infotrace")
    # mirt.plot.infoSE <- plot(mirt.calib, type = "infoSE")
    # mirt.plot.rxx <- plot(mirt.calib, type="rxx")
    # shinyjs::show(id="res.mirt.item.par", animType = "fade")
    # shinyjs::show(id="res.mirt.item.fit", animType = "fade")
    # output$results.mirt.item.par <- renderDataTable({
    #  mirt.item.par <- mirt.item.par.f$items
    # })
    output$results.mirt.item.infotrace <- renderPlot({
      itemplot(mirtCalib, 1, type = "infotrace")
    })
    output$results.mirt.infotrace <- renderPlot({
      plot(mirtCalib, type = "infotrace")
    })
    output$results.mirt.infoSE <- renderPlot({
      plot(mirtCalib, type = "infoSE")
    })
    output$results.mirt.rxx <- renderPlot({
      plot(mirtCalib, type = "rxx")
    })
    output$results.mirt.itemfit <- renderDataTable({
      mirtItemFit
    })

    shinyjs::show(id = "download_mirt_res", animType = "fade")
    output$download_mirt.par <- downloadHandler(
      filename = function() {
        "mirt.itempar.csv"
      },
      content = function(file) {
        write.table(mirtItemPar$items, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_mirt_par_comma <- downloadHandler(
      filename = function() {
        "mirt.itempar.comma.csv"
      },
      content = function(file) {
        write.table(format(mirtItemPar$items, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_mirt_theta <- downloadHandler(
      filename = function() {
        "mirt.theta.csv"
      },
      content = function(file) {
        write.table(mirtTheta, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_mirt_theta_comma <- downloadHandler(
      filename = function() {
        "mirt.theta.comma.csv"
      },
      content = function(file) {
        write.table(format(mirtTheta, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download_mirt_plots <- downloadHandler(
      filename = function() {
        "IRTplots.pdf"
      },
      content = function(file) {
        pdf(file)
        for (k in 1:nrow(mirtItemPar$items)) {
          print(itemplot(mirtCalib, k, type = "infotrace"))
        }
        print(plot(mirtCalib, type = "infotrace"))
        print(plot(mirtCalib, type = "infoSE"))
        print(plot(mirtCalib, type = "rxx"))
        dev.off()
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
