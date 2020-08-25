library(catR)
library(plyr)
library(mirt)
library(DT)

server <- function(input, output, session) {
  
  addTooltip(session, id = "exp_ctrl", title = "Don't set this value low for small item pool sizes.",
             placement = "left", trigger = "hover")
  
  #### CAT SIMULATIONS ########################################################
  #############################################################################
  
  ## Termination Rule Menu Show/Hide
  observeEvent(input$cb_ter, {
    shinyjs::toggle(id = "ter_length", animType = "fade")
    shinyjs::toggle(id = "ter_se", animType = "fade")
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
    shinyjs::toggle(id = "gen_res", animType = "fade")
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
      updateNumericInput(
        session,
        inputId = "item_cat",
        value = 2,
        min = 2,
        max = 2
      )
    }
  })
  
  ## Fixing Repication to 1 for User Uploaded Response Files
  observeEvent(input$file_res, {
    if (!is.null(input$file_res)) {
      updateNumericInput(
        session,
        inputId = "rep",
        value = 1,
        min = 1,
        max = 1
      )
    } else {
      
    }
  })
  
  ## Disabling Seed if the Replication > 1
  observeEvent(input$rep, {
    if (input$rep > 1) {
      updateNumericInput(
        session,
        inputId = "seed",
        value = NA,
        min = 0,
        max = 0
      )
    } else if (input$rep == 1){
      updateNumericInput(
        session,
        inputId = "seed",
        value = 26,
        min = 1,
        step = 1
      )
    }
  })
  
  ## Reading Category Value for Polytomous IRT Models
  ## Setting IRT Model for Simulation
  observeEvent(input$submit, {
    if (input$irt_model == "GRM" |
        input$irt_model == "GPCM" |
        input$irt_model == "MGRM" |
        input$irt_model == "PCM" |
        input$irt_model == "RSM" |
        input$irt_model == "NRM") {
      IRTModel <- input$irt_model
      ItemCat <- input$item_cat
    } else {
      IRTModel <- NULL
      ItemCat <- 2
    }
    
    # Required Varibles for Simulation
    ItemGenModel <- input$irt_model
    ThetaEstMethod <- input$est_method
    PoolSize <- input$item_no
    Replica <- input$rep
    ThetaN <- input$theta_gen_n
    ThetaM <- input$theta_gen_mean
    ThetaSd <- input$theta_gen_sd
    ConstD <- as.numeric(input$const_d)
    ExpCtrl <- input$exp_ctrl
    if(Replica == 1){
      Seed <- input$seed
    }
    else if(Replica > 1){
      Seed <- NULL
    }
    ItemPool <- NULL
    Responses <- NULL
    Theta <- NULL
    CAT <- NULL
    oCAT <- NULL

    ## Generating / Reading Item Pool
    if (is.null(input$file_par)) {
      if (!is.null(IRTModel)) {
        # Polytomous Item Pool
        ItemPool <- genPolyMatrix(
          items = PoolSize,
          nrCat = ItemCat,
          model = ItemGenModel,
          seed = Seed,
          same.nrCat = TRUE
        )
      } else {
        # Dichotomous Item Pool
        ItemPool <- genDichoMatrix(
          items = PoolSize,
          model = ItemGenModel,
          seed = Seed
          )
      }
    } else {
      ItemPool <- read.csv(
        input$file_par$datapath,
        header = input$header_par,
        sep = input$sep_par)
    }
    
    # Generating / Reading Theta
    if (is.null(input$file_theta)) {
        if (input$generate_theta == "Increment") {
          Theta <- seq(
            input$min_theta,
            input$max_theta,
            input$increment
            )
        } else {
          Theta <- rnorm(
            n = ThetaN,
            mean = ThetaM,
            sd = ThetaSd
            )
        }
    } else {
      Theta <- read.csv(
        input$file_theta$datapath,
        header = input$header_theta,
        sep = input$sep_theta
      )
      Theta <- as.matrix(Theta)
    }
    
    ## Reading Response Pattern
    if (is.null(input$file_res)) {
      for(rep in 1:Replica){
        Responses[[rep]] <- genPattern(
          Theta,
          ItemPool,
          model = IRTModel,
          D = ConstD,
          seed = Seed)
      }
      } else {
        Responses[[1]] <- read.csv(
          input$file_res$datapath,
          header = input$header_res,
          sep = input$sep_res
          )
        Responses[[1]] <- as.matrix(Responses[[1]])

        ### Theta Estimation for Uploaded Response Pattern
        if(is.null(input$file_theta)){
          Theta <- NULL
          for (j in 1:nrow(Responses[[1]])) {
            ThetaTemp <- thetaEst(
              ItemPool,
              Responses[[1]][j,],
              model = IRTModel,
              D = ConstD,
              method = ThetaEstMethod
            )
            Theta <- rbind(Theta, ThetaTemp)
          }
          rownames(Theta) <- NULL
        } else {
          
        }
        }
   
    ## Simulation
    if(input$first_item == "0"){
      StartList <- list(theta = 0, seed = Seed)
    } else if(input$first_item == "random"){
      StartList <- list(theta = -1:1, randomesque = 5, seed = Seed)
    }
    TestList <- list(D = ConstD,
                     itemSelect = input$item_sel_method)
    if(input$cb_ter == "Fixed"){
      StopList <- list(rule = "length",
                       thr = as.numeric(input$ter_length))
    } else {
      StopList <- list(rule = "precision",
                       thr = as.numeric(input$ter_se))
    }
    
    withProgress(message = 
                   paste0("CATs are working for your simulation",
                          emo::ji("smirk_cat")),
                 detail = "This may take time according to your dataset.",
                 style = "notification",
                 value = 0.5,
                 {
                   Sys.sleep(0.25)
                   for(rep in 1:Replica){
                     CAT[[rep]] <- simulateRespondents(
                       Theta,
                       ItemPool,
                       Responses[[rep]],
                       model = IRTModel,
                       rmax = ExpCtrl,
                       start = StartList,
                       test = TestList,
                       stop = StopList,
                       final = list(D = ConstD)
                       )
                     }
                 })

    ## Gathering the Results
    for (rep in 1:Replica){
      oCAT[["responsesMatrix"]] <- rbind.fill(
        oCAT[["responsesMatrix"]],
        data.frame(cbind(Replication = rep,
                         RespondentID = CAT[[rep]][["responses.df"]][, 1],
                         CAT[[rep]][["responsesMatrix"]]
                         )
                   )
        )
    }
    
    oSummary <- NULL
    oConditional <- NULL
    for (rep in 1:Replica) {
      oCAT[["fullResults"]] <- rbind.fill(
        oCAT[["fullResults"]],
        data.frame(
          cbind(
            Replication = rep,
            RespondentID = CAT[[rep]][["responses.df"]][, 1],
            CAT[[rep]][["final.values.df"]],
            CAT[[rep]][["responses.df"]]
            )
          )
        )
      
      Summary <- data.frame(
        Replication = rep,
        Corr = round(CAT[[rep]][["correlation"]], 4),
        Bias = round(CAT[[rep]][["bias"]], 4),
        RMSE = round(CAT[[rep]][["RMSE"]], 4),
        TestLength = round(CAT[[rep]][["testLength"]], 4),
        MeanSE = round(mean(CAT[[rep]][["final.values.df"]][["final.SE"]]), 4)
        )
     
      FirstCol <- c("Mean Theta",
                    "RMSE",
                    "Mean bias",
                    "Mean test length",
                    "Mean standard error",
                    "Proportion stop rule satisfied",
                    "Number of simulees")
      
      Conditional <- cbind(FirstCol, 
                           ReplicationID = rep(rep, 7),
                           rbind(
                           t(CAT[[rep]][["condTheta"]]),
                           t(CAT[[rep]][["condRMSE"]]),
                           t(CAT[[rep]][["condBias"]]),
                           t(CAT[[rep]][["condnItems"]]),
                           t(CAT[[rep]][["condSE"]]),
                           t(CAT[[rep]][["condthrOK"]]),
                           t(CAT[[rep]][["ndecile"]])
                           ))
      colnames(Conditional) <- c("Statistics", "ReplicationID", "D1", "D2", "D3", "D4", "D5",
                                 "D6", "D7", "D8", "D9", "D10")
      
      oConditional <- rbind(oConditional, Conditional)
      oSummary <- rbind(oSummary, Summary)
      }
    
    mSummary <- c("Mean",
                  round(mean(oSummary[,2]), 4),
                  round(mean(oSummary[,3]), 4),             
                  round(mean(oSummary[,4]), 4),
                  round(mean(oSummary[,5]), 4),
                  round(mean(oSummary[,6]), 4))
    
    oSummary <- rbind(oSummary,
                      mSummary)
    
    
    ## Results
    shinyjs::toggle(id = "results", animType = "fade")
    output$results_cat <- renderPrint({
      if(Replica > 1){
        oSummary
      } else {
        print(CAT[[1]])
      }
    })
    shinyjs::show(id = "results_all", animType = "fade")
    
    ## File Downloaders
    OutSep <- input$out_sep
    OutDecSep <- input$out_dec_sep
    
    output$download_summary <- downloadHandler(
      filename = function() {
        "CATSummary.txt"
      },
      content = function(file) {
        if(Replica > 1){
          sink(file, type = "output")
          for(i in 1:Replica){
            writeLines(paste0("\n ####### REPLICA NO: ", i, " #######"))
            print(CAT[[i]])
          }
          sink()
        } else {
          sink(file, type = "output")
          print(CAT[[1]])
          sink()
        }

      }
    )
  
    output$download_items <- downloadHandler(
      filename = function() {
        "itempar.csv"
      },
      content = function(file) {
        write.table(
          CAT[[1]][["itemBank"]],
          file,
          dec = OutDecSep,
          sep = OutSep,
          row.names = FALSE,
          quote = FALSE)
      }
    )
    
    output$download_res <- downloadHandler(
      filename = function() {
        "responses.csv"
      },
      content = function(file) {
        write.table(
          as.data.frame(
            oCAT[["responsesMatrix"]]),
          file,
          dec = OutDecSep,
          sep = OutSep,
          row.names = FALSE
        )
      }
    )
    output$download_sim <- downloadHandler(
      filename = function() {
        "simresults.csv"
      },
      content = function(file) {
        write.table(
          oCAT[["fullResults"]],
          file,
          dec = OutDecSep,
          sep = OutSep,
          row.names = FALSE
        )
      }
    )
    output$download_cond <- downloadHandler(
      filename = function() {
        "conditional.csv"
      },
      content = function(file) {
        write.table(
          oConditional,
          file,
          dec = OutDecSep,
          sep = OutSep,
          row.names = FALSE,
          quote = FALSE
        )
      }
    )
  })
  
  #### IRT CALIBRATIONS #######################################################
  #############################################################################
  
  observeEvent(input$ispoly, {
    shinyjs::toggle(id = "mirt_dicho", animType = "fade")
    shinyjs::toggle(id = "mirt_poly", animType = "fade")
  })
  
  observeEvent(input$mirt_calibrate, {
    mirtData <- read.csv(input$mirt_res$datapath,
                         header = TRUE,
                         sep = ";",
                         na.strings = "NA"
    )
    if(isTRUE(input$ispoly)){
      mirtModel <- input$mirt_poly
    } else {
      mirtModel <- input$mirt_dicho
    }
    
    mirtEst <- input$mirt_est
    mirtD <- input$mirt_D
    mirtCalib <- mirt(mirtData, 1, itemtype = mirtModel, D = mirtD)
    mirtItemPar <- coef(mirtCalib, IRTpars = T, simplify = T)
    mirtItemFit <- itemfit(mirtCalib, na.rm = TRUE)
    mirtTheta <- fscores(mirtCalib, method = mirtEst, na.rm = TRUE)
    mirtQ3 <- data.frame(residuals(mirtCalib, df.p = T, type = "Q3", Theta = mirtTheta, suppress = input$yensur))
    rownames(mirtQ3) <- colnames(mirtData)
    mirtThetaP <- data.frame(RespondentID = c(1:nrow(mirtTheta)), Theta = mirtTheta)
    
    output$results_mirt <- DT::renderDataTable(
      round(mirtQ3, 2)
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
    MirtOutSep <- input$mirt_out_sep
    MirtOutDecSep <- input$mirt_out_dec_sep
    output$download_mirt_par <- downloadHandler(
      filename = function() {
        "mirtItemPar.csv"
      },
      content = function(file) {
        write.table(mirtItemPar$items, 
                    file,
                    dec = MirtOutDecSep,
                    sep = MirtOutSep,
                    row.names = FALSE
        )
      }
    )
    output$download_mirt_theta <- downloadHandler(
      filename = function() {
        "mirtTheta.csv"
      },
      content = function(file) {
        write.table(mirtThetaP, 
                    file,
                    dec = MirtOutDecSep,
                    sep = MirtOutSep,
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