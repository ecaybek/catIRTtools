library(shiny)
library(shinythemes)
library(catR)
library(plyr)
library(shinyjs)
library(mirt)
# library(DT)

# Define UI for application
ui <- navbarPage(
  title = "catIRT Tools", theme = shinytheme("cerulean"),
  tabPanel(
    "CAT Simulation",

    tags$head(tags$style(HTML("
                  .selectize-input, .selectize-dropdown, .control-label, .form-control, label {
                            font-size: 100%;
                  }
                  "))),
    tags$style(".shiny-file-input-progress {display: none}"),
    # Application title
    shinyjs::useShinyjs(),
    p(strong("Generate or Upload Item Parameters / Responses & Run CAT Simulations")),

    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, selectInput("irtmodel", "IRT model", c("1PL", "2PL", "3PL", "4PL", "GRM", "MGRM", "PCM", "GPCM", "RSM", "NRM"), "GRM")),
          column(6, selectInput("itemselmethod", "Item Selection", c("MFI", "MLWI", "MPWI", "MEI", "MEPV", "KL", "KLP")))
        ),

        numericInput("minitem", "Minimum item to apply", 3),
        numericInput("stderr", "Maximum standard error to stop the test", 0.40),
        numericInput("seed", "Please enter seed in order to reproduce your results", 26),
        fluidRow(
          column(6, checkboxInput("generateitem", "Generate item parameters?", FALSE)),
          column(6, checkboxInput("generateresponse", "Generate responses?", FALSE))
        ),

        div(
          id = "itemnodiv",
          numericInput("itemno", "How many items to generate?", 10),
          numericInput("category", "Categories", 5)
        ),
        shinyjs::hidden(div(
          id = "itemupload",
          fileInput("parameter.file", "File for Item Parameters"),
          fluidRow(
            column(6, radioButtons("seperator.par", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header.par", "First row as header", TRUE))
          )
        )),
        br(),
        div(id = "responseinc", selectInput("increment", "Please select the theta increment", c(1, 0.5, 0.1, 0.05, 0.01), 0.1)),
        shinyjs::hidden(div(
          id = "responseupload",
          fileInput("response.file", "File for Responses"),
          fluidRow(
            column(6, radioButtons("seperator.res", "Seperator for File", c(Semicolon = ";", Comma = ","))),
            column(6, checkboxInput("header.res", "First row as header", TRUE))
          )
        )),
        actionButton("submit", "Submit")
      ),
      # Show results
      mainPanel(
        div(id = "results", print("Your results will be here after you submit your request")),
        dataTableOutput("results.cat"),
        shinyjs::hidden(div(
          id = "results.allm",
          print(p(strong("You can download data & results below"))),
          fluidRow(
            column(4, downloadButton("download.items", "Download Item Parameters")),
            column(4, downloadButton("download.responses", "Download Responses")),
            column(4, downloadButton("download.results", "Download Simulation Results"))
          ),
          br(),
          fluidRow(
            column(4, downloadButton("download.items.comma", "Download Item Parameters (with Comma)")),
            column(4, downloadButton("download.responses.comma", "Download Responses (with Comma)")),
            column(4, downloadButton("download.results.comma", "Download Simulation Results (with Comma)"))
          )
        ))
      )
    )
  ),

  tabPanel(
    "IRT Analysis",
    p(strong("IRT Analysis")),
    sidebarLayout(
      sidebarPanel(
        fileInput("mirt.responses", "File for responses"),
        selectInput("mirt.model", "IRT Model", c("Rasch", "2PL", "3PL", "4PL", "GRM" = "graded", "GPCM" = "gpcm"), "GRM"),
        selectInput("mirt.est", "Theta Estimation Method", c("EAP", "MAP"), "EAP"),
        selectInput("mirt.d", "D", c("1.702", "1.000"), "1.702"),
        # checkboxGroupInput("mirt.plots","Output Options",c("Item Fit Statistics", "Item Characteristic Curves","Test Information Function","Test SE","Test Reliability")),
        actionButton("mirt.calibrate", "Calibration & Estimation")
      ),

      mainPanel(
        shinyjs::hidden(div(
          id = "results.mirt.download",
          print(p(strong("You can download results below"))),
          fluidRow(
            column(4, downloadButton("download.mirt.itempar", "Download Item Parameters")),
            column(4, downloadButton("download.mirt.theta", "Download Theta Estimates"))
          ),
          # column(4, downloadButton("download.results", "Download Simulation Results"))),
          br(),
          fluidRow(
            column(4, downloadButton("download.mirt.itempar.comma", "Download Item Parameters (with Comma)")),
            column(4, downloadButton("download.mirt.theta.comma", "Download Theta Estimates (with Comma)"))
          ),
          br(),
          fluidRow(column(4, downloadButton("download.mirt.plots", "Download Plots")))
        ))
      )
    )
  ),
  tabPanel(
    "Help",
    p(strong("Help"))
  ),
  tabPanel(
    "Privacy",
    p(strong("We do not keep any of the files and results on our servers."))
  )
)

server <- function(input, output, session) {
  observeEvent(input$generateitem, {
    shinyjs::toggle(id = "itemnodiv", animType = "fade")
    shinyjs::toggle(id = "itemupload", animType = "fade")
  })

  observeEvent(input$generateresponse, {
    shinyjs::toggle(id = "responseinc", animType = "fade")
    shinyjs::toggle(id = "responseupload", animType = "fade")
  })


  observeEvent(input$irtmodel, {
    if (input$irtmodel != "GRM" & input$irtmodel != "GPCM") {
      updateNumericInput(session, inputId = "category", value = 2, min = 2, max = 2)
    }
  })

  observeEvent(input$submit, {
    itemgenmodel <- input$irtmodel
    if (input$irtmodel == "GRM" | input$irtmodel == "GPCM") {
      simodel <- input$irtmodel
      categ <- input$category
    } else {
      simodel <- NULL
      categ <- 2
    }

    if (is.null(input$parameter.file)) {
      if (input$irtmodel == "GRM" | input$irtmodel == "GPCM") {
        items.GRM <- as.matrix(genPolyMatrix(items = input$itemno, nrCat = categ, model = itemgenmodel, seed = input$seed, same.nrCat = TRUE))
      } else {
        items.GRM <- as.matrix(genDichoMatrix(items = input$itemno, model = itemgenmodel, seed = input$seed))
      }
    }
    else {
      items.GRM <- read.csv2(input$parameter.file$datapath,
        header = input$header.par,
        sep = input$seperator.par
      )
      items.GRM <- as.matrix(items.GRM)
    }

    if (is.null(input$response.file)) {
      responses.GRM <- NULL
      for (i in seq(-3, 3, by = as.numeric(input$increment))) {
        responses.GRM <- rbind(responses.GRM, data.frame(t(c(i, genPattern(i, items.GRM, model = simodel, seed = input$seed)))))
      }
    } else {
      responses.GRM <- read.csv2(input$response.file$datapath,
        header = input$header.res,
        sep = input$seperator.res
      )
      responses.GRM <- as.matrix(responses.GRM)
      theta.list <- NULL
      withProgress(message = "Estimating Theta", detail = "for case 0", value = 0, {
        for (j in 1:nrow(responses.GRM)) {
          theta.est <- thetaEst(items.GRM, responses.GRM[j, ], model = simodel, D = 1.702, method = "EAP")
          theta.list <- rbind(theta.list, theta.est)
          increment <- (1 / nrow(responses.GRM))
          incProgress(increment, detail = paste("case", j))
          Sys.sleep(0.1)
        }
      })
      rownames(theta.list) <- NULL
      rownames(responses.GRM) <- NULL
      responses.GRM <- cbind(data.frame(theta.list), data.frame(responses.GRM))
    }
    i <- NULL
    colnames(responses.GRM) <- c("theta", paste("i", c(1:nrow(items.GRM)), sep = "_")) # Adjusting the variable names

    # Creating variables

    items.administered <- data.frame()
    aditem <- NULL
    temp.theta <- 0
    past.thetas <- NULL
    all.thetas <- NULL
    serror <- 1
    serror.past <- NULL
    serror.person <- NULL
    past.item.pars <- NULL
    past.responses <- NULL


    withProgress(message = "Simulating", detail = "for case 0", value = 0, {
      # CAT Loop
      for (i in 1:nrow(responses.GRM)) { # Run the loop for everyone
        while (serror > input$stderr | length(aditem) < input$minitem) { # Stopping rule
          firstitem <- nextItem(items.GRM, model = simodel, theta = temp.theta, out = aditem, criterion = input$itemselmethod, D = 1.702)
          itemnumber <- as.numeric(firstitem$item)
          varitem <- paste("i", itemnumber, sep = "_")
          past.item.pars <- rbind(past.item.pars, firstitem$par)
          past.responses <- c(past.responses, responses.GRM[i, varitem])
          temp.theta <- thetaEst(past.item.pars, past.responses, model = simodel, method = "EAP", D = 1.702)
          serror <- semTheta(temp.theta, past.item.pars, past.responses, model = simodel, method = "EAP", D = 1.702)
          serror.past <- rbind(serror.past, serror)
          aditem <- c(aditem, firstitem$item)
          aditem <- as.numeric(aditem)
          past.thetas <- cbind(past.thetas, temp.theta)
          if (length(aditem) == nrow(items.GRM)) {
            break
          }
        }

        admitems.df <- as.data.frame(t(aditem))
        all.thetas <- rbind(all.thetas, temp.theta)
        items.administered <- rbind.fill(items.administered, admitems.df)
        serror.person <- rbind(serror.person, serror)
        aditem <- NULL
        temp.theta <- 0
        serror <- 1
        serror.past <- NULL
        past.thetas <- NULL
        past.responses <- NULL
        past.item.pars <- NULL
        increment <- (1 / nrow(responses.GRM))
        incProgress(increment, detail = paste("case", i))
        Sys.sleep(0.1)
      }
    })

    shinyjs::toggle(id = "results", animType = "fade")


    rownames(all.thetas) <- NULL # Deleting row names to prevent "duplicated row names" error message
    rownames(items.administered) <- NULL # Deleting row names to prevent "duplicated row names" error message
    rownames(serror.person) <- NULL # Deleting row names to prevent "duplicated row names" error message
    item.k <- rowSums(!is.na(items.administered))
    theta.n.items <- cbind(responses.GRM[, 1], all.thetas, serror.person, item.k, items.administered) # Merging theta estimations and admnistered items
    colnames(theta.n.items) <- c("full_theta", "est_theta", "se", "totalitems", paste("used_i", 1:(ncol(theta.n.items) - 4), sep = "")) # Let's name the columns
    correlation <- cor(theta.n.items[, 1], theta.n.items[, 2]) # Correlation between true and estimated theta values
    mean.se <- apply(theta.n.items["se"], 2, mean)
    mean.k <- mean(rowSums(!is.na(items.administered)))

    output$results.cat <- renderDataTable({
      results.frame <- data.frame(cor = correlation, mean_se = mean.se, mean_k = mean.k)
    })

    shinyjs::show(id = "results.allm", animType = "fade")

    # output$results.all <- renderDataTable(datatable(theta.n.items,
    #                      extensions = list("Buttons" = NULL,
    #                                       "FixedColumns" = NULL),
    #                      options = list(dom = 'Bt',
    #                                    scrollX = TRUE,
    #                                    fixedColumns = list(leftColumns = 0),
    #                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    #                      formatRound(c(1:3), 2))

    observeEvent(input$csvdecimal, {
      if (input$csvdecimal == TRUE) {
        theta.n.items <- format(theta.n.items, decimal.mark = ",")
      }
    })

    output$download.items <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(items.GRM, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.items.comma <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(format(items.GRM, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.responses <- downloadHandler(
      filename = function() {
        "responses.csv"
      },
      content = function(file) {
        write.table(responses.GRM, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.responses.comma <- downloadHandler(
      filename = function() {
        "responses.csv"
      },
      content = function(file) {
        write.table(format(responses.GRM, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.results <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(theta.n.items, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.results.comma <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(format(theta.n.items, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
  })

  observeEvent(input$mirt.calibrate, {
    mirt.data <- read.csv2(input$mirt.responses$datapath,
      header = TRUE,
      sep = ";"
    )
    mirt.model <- input$mirt.model
    mirt.est <- input$mirt.est
    mirt.d <- input$mirt.d

    mirt.calib <- mirt(mirt.data, itemtype = mirt.model, 1, D = mirt.d)
    mirt.item.par.f <- coef(mirt.calib, IRTpars = T, simplify = T)
    mirt.itemfit <- itemfit(mirt.calib)

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
      itemplot(mirt.calib, 1, type = "infotrace")
    })
    output$results.mirt.infotrace <- renderPlot({
      plot(mirt.calib, type = "infotrace")
    })
    output$results.mirt.infoSE <- renderPlot({
      plot(mirt.calib, type = "infoSE")
    })
    output$results.mirt.rxx <- renderPlot({
      plot(mirt.calib, type = "rxx")
    })
    output$results.mirt.itemfit <- renderDataTable({
      mirt.itemfit
    })
    # output$results.mirt.theta <- renderDataTable({
    #  mirt.theta <- fscores(mirt.calib)
    # })
    shinyjs::show(id = "results.mirt.download", animType = "fade")
    output$download.mirt.itempar <- downloadHandler(
      filename = function() {
        "mirt.itempar.csv"
      },
      content = function(file) {
        write.table(mirt.item.par.f$items, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.mirt.itempar.comma <- downloadHandler(
      filename = function() {
        "mirt.itempar.comma.csv"
      },
      content = function(file) {
        write.table(format(mirt.item.par.f$items, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.mirt.theta <- downloadHandler(
      filename = function() {
        "mirt.theta.csv"
      },
      content = function(file) {
        write.table(mirt.theta, file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.mirt.theta.comma <- downloadHandler(
      filename = function() {
        "mirt.theta.comma.csv"
      },
      content = function(file) {
        write.table(format(mirt.theta, decimal.mark = ","), file,
          sep = ";",
          row.names = FALSE
        )
      }
    )
    output$download.mirt.plots <- downloadHandler(
      filename = function() {
        "IRTplots.pdf"
      },
      content = function(file) {
        pdf(file)
        for (k in 1:nrow(mirt.item.par.f$items)) {
          print(itemplot(mirt.calib, k, type = "infotrace"))
        }
        print(plot(mirt.calib, type = "infotrace"))
        print(plot(mirt.calib, type = "infoSE"))
        print(plot(mirt.calib, type = "rxx"))
        dev.off()
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
