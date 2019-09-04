library(shiny)
library(shinythemes)
library(shinyjs)
library(catR)
library(plyr)
library(mirt)

server <- function(input, output, session) {
  # CAT Simulations

  observeEvent(input$ter_type, {
    shinyjs::toggle(id = "ter_fixed", animType = "fade")
    shinyjs::toggle(id = "ter_var", animType = "fade")
  })

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
    if (input$ter_type == TRUE) {
      TerType <- 1
    } else {
      TerType <- 2
    }


    withProgress(message = "Simulating", detail = "for case 0", value = 0, {
      # CAT LOOP
      for (i in 1:nrow(Responses)) { # Run the loop for everyone
        while (
          if (TerType == 1) {
            length(UsedItem) < input$ter_fixed_item
          } else {
            SE > input$ter_se | length(UsedItem) < input$ter_min_item
          }) { # Termination Rule
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

  # IRT Calibrations
  observeEvent(input$ispoly, {
    shinyjs::toggle(id = "mirt_dicho", animType = "fade")
    shinyjs::toggle(id = "mirt_poly", animType = "fade")
  })

  observeEvent(input$mirt_calibrate, {
    mirtData <- read.csv2(input$mirt_res$datapath,
      header = TRUE,
      sep = ";"
    )
    mirtModel <- input$mirt_model
    mirtEst <- input$mirt_est
    mirtD <- input$mirt_D
    mirtCalib <- mirt(mirtData, itemtype = mirtModel, 1, D = mirtD)
    mirtItemPar <- coef(mirtCalib, IRTpars = T, simplify = T)
    mirtItemFit <- itemfit(mirtCalib)
    mirtTheta <- fscores(mirtCalib, method = mirtEst, D = mirtD)
    mirtQ3 <- residuals(mirtCalib, df.p = T, type = "Q3", Theta = mirtTheta, suppress = .37)

    output$results_mirt <- renderDataTable(
      round(mirtQ3, 2),
      options = list(
        scrollX = "600px",
        scrollY = "400px"
      )
    )
    output$results_mirt_item_infotrace <- renderPlot({
      itemplot(mirtCalib, 1, type = "infotrace")
    })
    output$results_mirt_infotrace <- renderPlot({
      plot(mirtCalib, type = "infotrace")
    })
    output$results_mirt_infoSE <- renderPlot({
      plot(mirtCalib, type = "infoSE")
    })
    output$results_mirt_rxx <- renderPlot({
      plot(mirtCalib, type = "rxx")
    })
    output$results_mirt_itemfit <- renderDataTable({
      mirtItemFit
    })

    shinyjs::show(id = "download_mirt_res", animType = "fade")
    output$download_mirt_par <- downloadHandler(
      filename = function() {
        "mirtItemPar.csv"
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
        "mirtItemParComma.csv"
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
        "mirtTheta.csv"
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
        "mirtThetaComma.csv"
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
