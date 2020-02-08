library(catR)
library(plyr)
library(mirt)

server <- function(input, output, session) {
# CAT SIMULATIONS
  ## Termination Rule Menu Show/Hide   
  observeEvent(input$cb_ter_fixed, {
    shinyjs::toggle(id = "ter_fixed", animType = "fade")
  })
  observeEvent(input$cb_ter_var, {
    shinyjs::toggle(id = "ter_var", animType = "fade")
  })
  ## Item Generation Menu Show/Hide   
  observeEvent(input$generate_item, {
    shinyjs::toggle(id = "gen_item", animType = "fade")
  })
  ## Response Generation Menu Show/Hide  
  observeEvent(input$generate_response, {
    shinyjs::toggle(id = "gen_res", animType = "fade")
  })
  ## Theta Generation Menu Show/Hide  
  observeEvent(input$generate_theta, {
    shinyjs::toggle(id = "gen_theta", animType = "fade")
  })
  ## Response Upload Menu Show/Hide  
  observeEvent(input$upload_response, {
    shinyjs::toggle(id = "up_res", animType = "fade")
  })
  ## Item Parameters Upload Menu Show/Hide  
  observeEvent(input$upload_item, {
    shinyjs::toggle(id = "up_item", animType = "fade")
  })
  ## Theta Upload Menu Show/Hide  
  observeEvent(input$upload_theta, {
    shinyjs::toggle(id = "up_theta", animType = "fade")
  })
  ## Fixing Category to 2 for Dichotomous IRT Models
  observeEvent(input$irt_model, {
    if (input$irt_model == "1PL" |
        input$irt_model == "2PL" |
        input$irt_model == "3PL" |
        input$irt_model == "4PL") {
      updateNumericInput(session, 
                         inputId = "category", 
                         value = 2, 
                         min = 2, 
                         max = 2)
    }
  })
  ## Reading Category Value for Polytomous IRT Models
  ## Setting IRT Model for Simulation
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
    ## Theta Estimation Method
    EstMethod <- input$est_method
    ## Scaling Factor (D)
    constD <- as.numeric(input$const_d)
    ## Replication Number
    Replica <- input$replicate
    ## Setting Seed Value
    ## Fixed for 1 Replication; Random for +1 Replications
    if(!is.na(Replica > 1)){
      Seed <- NULL
    } else {
      Seed <- input$seed
    }
    ## Item Selection Method
    ItemSelMethod <- as.character(input$item_sel_method)
    ## Item Pool Size
    PoolSize <- input$item_no
    ## Creating / Reading the Item Pool
    ItemPool <- NULL
    if (is.null(input$file_par)) {
      if (input$irt_model == "GRM" |
          input$irt_model == "GPCM" |
          input$irt_model == "MGRM" |
          input$irt_model == "PCM" |
          input$irt_model == "RSM" |
          input$irt_model == "NRM") {
        for(rep in 1:Replica){
          ItemPool[[rep]] <- as.matrix(
            genPolyMatrix(
              items = PoolSize, 
              nrCat = ncat, 
              model = ItemGenModel, 
              seed = Seed, 
              same.nrCat = TRUE))
        }
      } else {
        for(rep in 1:Replica){
          ItemPool[[rep]] <- as.matrix(
            genDichoMatrix(
              items = PoolSize, 
              model = ItemGenModel, 
              seed = Seed))
        }
      }
    }
    else {
      ItemPool[[1]] <- read.csv(input$file_par$datapath,
                            header = input$header_par,
                            sep = input$sep_par
      )
      ItemPool[[1]] <- reactive(as.matrix(ItemPool))
    }
    ## Setting Theta Values to Use Response Pattern Generation
    minTheta <- input$min_theta
    maxTheta <- input$max_theta
    incTheta <- input$increment
    ## Creating / Reading Response Pattern
    if (is.null(input$file_res)) {
      Response <- NULL
      Responses <- NULL
      ThetaMean <- input$theta_gen_mean
      ThetaSD <- input$theta_gen_sd
      ThetaN <- input$theta_gen_n
      ThetaRange <- seq(minTheta, maxTheta, by = as.numeric(incTheta))
      ThetaDist <- sort(rnorm(ThetaN, mean = ThetaMean, sd = ThetaSD))
      if(input$generate_response == TRUE){
        if(is.null(input$file_theta)){
          Theta4Pattern <- ThetaRange
        } else {
          Theta4Pattern <- read.csv(input$file_theta$datapath,
                                    header = input$header_res,
                                    sep = input$sep_res)
        }
      } else {
        Theta4Pattern <- ThetaDist
      }
      for(rep in 1:Replica){
        for (i in Theta4Pattern) {
          Response <- rbind(Response, 
                            data.frame(t(c(i, 
                                           genPattern(i, 
                                                      ItemPool[[rep]], 
                                                      model = SimModel, 
                                                      seed = Seed)))))
        }
        Responses[[rep]] <- Response
        Response <- NULL
      }
    } else {
      Responses[[1]] <- read.csv(input$file_res$datapath,
                             header = input$header_res,
                             sep = input$sep_res
      )
        Responses[[1]] <- as.matrix(Responses[[1]])
      ## Creating Necessary Variable
      ThetaList <- NULL
      ## Theta Estimation for Uploaded Response Pattern
      withProgress(message = "Estimating Theta", 
                   detail = "for case 0", 
                   value = 0, {
          for (j in 1:nrow(Responses[[1]])) {
            ThetaEst[[1]] <- thetaEst(ItemPool[[1]], 
                                        Responses[[1]][j, ], 
                                        model = SimModel, 
                                        D = constD, 
                                        method = EstMethod)
            ThetaList[[1]] <- rbind(ThetaList[[1]], 
                                      ThetaEst[[1]])
            increment <- (1 / nrow(Responses[[1]]))
            incProgress(increment, 
                        detail = paste("case", j))
            Sys.sleep(0.1)
          }
        rownames(ThetaList[[1]]) <- NULL
        rownames(Responses[[1]]) <- NULL
        Responses[[1]] <- cbind(data.frame(ThetaList[[1]]), 
                                  data.frame(Responses[[1]]))
      })

    }
    ## Adjusting the Variable Names for Response Pattern
    for (rep in 1:Replica){
      colnames(Responses[[rep]]) <- c("theta", 
                                      paste("i", 
                                            c(1:nrow(ItemPool[[rep]])), 
                                            sep = "_"))
    }
    ## Creating Necessary Variables
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
    RepResults <- NULL
    RepCMM <- NULL
    ThetaNItems <- NULL
    Correlation <- data.frame(r = NA)
    MeanSE <- data.frame(MeanSE = NA)
    MeanK <- data.frame(MeanK = NA)
    
    withProgress(message = "Replication", 
                 detail = "no: 1", 
                 value = 0, {
    ## CAT LOOP
    
      for (rep in 1:Replica){
        withProgress(message = "Simulating", 
                     detail = "case 1", 
                     value = 0, {
          for (i in 1:nrow(Responses[[rep]])) { # Run the loop for everyone
            while (### Termination Criteria
              ### while(if()) seems silly, but it works.
              if(input$cb_ter_fixed == TRUE){
                length(UsedItem) < input$ter_length | 
                length(UsedItem) < input$ter_min_item
              } else {
                SE > input$ter_se | 
                length(UsedItem) < input$ter_min_item
              }
              ) {
              FirstItem <- nextItem(ItemPool[[rep]], 
                                    model = SimModel, 
                                    theta = ThetaHat, 
                                    out = UsedItem, 
                                    criterion = ItemSelMethod, 
                                    D = constD)
              ItemNumber <- as.numeric(FirstItem$item)
              VarItem <- paste("i", 
                               ItemNumber, 
                               sep = "_")
              PastItemPars <- rbind(PastItemPars, 
                                    FirstItem$par)
              PastResponses <- c(PastResponses, 
                                 Responses[[rep]][i, VarItem])
              ThetaHat <- thetaEst(PastItemPars, 
                                   PastResponses,
                                   model = SimModel, 
                                   method = EstMethod, 
                                   D = constD)
              SE <- semTheta(ThetaHat, 
                             PastItemPars, 
                             PastResponses, 
                             model = SimModel, 
                             method = EstMethod, 
                             D = constD)
              SEPast <- rbind(SEPast, SE)
              UsedItem <- c(UsedItem, FirstItem$item)
              UsedItem <- as.numeric(UsedItem)
              ThetaPast <- cbind(ThetaPast, ThetaHat)
              if (length(UsedItem) == nrow(ItemPool[[rep]])) {
                break
              }
            }
          UsedItemDF <- as.data.frame(t(UsedItem))
          ThetaAll <- rbind(ThetaAll, 
                            ThetaHat)
          AdministeredItems <- rbind.fill(AdministeredItems, 
                                          UsedItemDF)
          SEPerson <- rbind(SEPerson, 
                            SE)
          UsedItem <- NULL
          ThetaHat <- 0
          SE <- 1
          SEPast <- NULL
          ThetaPast <- NULL
          PastResponses <- NULL
          PastItemPars <- NULL
          IncrementBar<- (1 / nrow(Responses[[rep]]))
          incProgress(IncrementBar, 
                      detail = paste("case", i+1))
          Sys.sleep(0.1)
          }})
      # Deleting row names to prevent "duplicated row names" error message
      rownames(ThetaAll) <- NULL
      rownames(AdministeredItems) <- NULL
      rownames(SEPerson) <- NULL
      kItem <- rowSums(!is.na(AdministeredItems))
      # Merging theta estimations and admnistered items
      ThetaNItems[[rep]] <- cbind(Responses[[rep]][, 1], 
                                  ThetaAll, 
                                  SEPerson, 
                                  kItem, 
                                  AdministeredItems)
      # Let's name the columns
      colnames(ThetaNItems[[rep]]) <- c("full_theta", 
                                        "est_theta", 
                                        "se", 
                                        "k_item", 
                                        paste("used_i", 
                                              1:(ncol(ThetaNItems[[rep]]) - 4), 
                                              sep = ""))
      # Put that on a list
      RepResults[[rep]] <- ThetaNItems[[rep]]
      # Correlation between true and estimated theta values
      Correlation[rep,1] <- round(cor(ThetaNItems[[rep]][, 1], 
                                      ThetaNItems[[rep]][, 2]), 
                                  2)
      MeanSE[rep,1] <- round(apply(ThetaNItems[[rep]]["se"], 2, mean), 2)
      MeanK[rep,1] <- round(mean(rowSums(!is.na(AdministeredItems))), 2)
      RepCMM <- c(mean(Correlation$r), mean(MeanSE$MeanSE), mean(MeanK$MeanK))
      IncrementRepBar<- (1 / Replica)
      incProgress(IncrementRepBar, 
                  detail = paste("no: ", rep+1))
      Sys.sleep(0.1)
    }
    })
    shinyjs::toggle(id = "results", animType = "fade")
    output$results_cat <- renderDataTable({
      data.frame(Replication = Replica, 
                 r = RepCMM[1], 
                 MeanSE = RepCMM[2], 
                 MeanK = RepCMM[3])
    })
    shinyjs::show(id = "results_all", animType = "fade")
    OutSep <- input$out_sep
    OutDecSep <- input$out_dec_sep
    output$download_items <- downloadHandler(
      filename = function(){
        "itempar.csv"
        },
      content = function(file){
        write.table(ItemPool, file, dec = OutDecSep, sep = OutSep, row.names = FALSE)
        }
      )
    output$download_res <- downloadHandler(
      filename = function() {
        "responses.csv"
      },
      content = function(file){
        write.table(Responses[[1]], file, dec = OutDecSep, sep = OutSep, row.names = FALSE)
      }
    )
    output$download_sim <- downloadHandler(
      filename = function() {
        "simresults.csv"
      },
      content = function(file){
        write.table(ThetaNItems[[1]], file, dec = OutDecSep, sep = OutSep, row.names = FALSE)
      }
    )
  })
    
  # IRT CALIBRATIONS

  observeEvent(input$ispoly, {
    shinyjs::toggle(id = "mirt_dicho", animType = "fade")
    shinyjs::toggle(id = "mirt_poly", animType = "fade")
  })
  
  observeEvent(input$mirt_calibrate, {
    mirtData <- read.csv(input$mirt_res$datapath,
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
      options = list(scrollX = "600px",
                     scrollY = "400px")
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