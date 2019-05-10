# Shiny Caret is an interactive interface for using various machine
# learning methods from the caret package
# (c) Nick Ward (University of Canterbury) 2018

#shiny
library(shiny)
library(shinyjs)
library(shinyalert)
library(DT)
#caret
library(caret)
library(recipes)
library(caretEnsemble) # look into this
#    library(embed) # look into this
#Data manipulation
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(summarytools)
library(ROSE)
library(doParallel)
#Graphics
library(naniar)
library(vcd)
library(ellipse)
library(corrplot)
library(lattice)
library(ggrepel)
library(RANN) # check need
library(PerformanceAnalytics)
library(ggplot2)
library(alluvial)
library(RColorBrewer)
library(dimRed)
library(rpart.plot)

options(shiny.maxRequestSize=50*1024^2)

###########################################################################
### Server logic
shinyServer(function(input, output, session) {

  react <- reactiveValues(
    ModelSet = list(),
    Recipe = NULL,
    Log = c(),
    partitionMessages = c(),
    AllowEnsemble = FALSE
  )

  ###########################################################################
  onSessionEnded( function() {
    #   stopApp()
  }, session = session)

  ###########################################################################
  observeEvent(input$DataSource, {
    shinyjs::toggle(id = "Header", condition = input$DataSource == "CSV file")
    shinyjs::toggle(id = "DateFormat", condition = input$DataSource == "CSV file")
    shinyjs::toggle(id = "Sep", condition = input$DataSource == "CSV file")
    shinyjs::toggle(id = "Quote", condition = input$DataSource == "CSV file")
    shinyjs::toggle(id = "CSVFile", condition = input$DataSource == "CSV file")
    shinyjs::toggle(id = "Package", condition = input$DataSource == "R dataset")
    shinyjs::toggle(id = "DataSet", condition = input$DataSource == "R dataset")
  })

  ###########################################################################
  observeEvent(input$Package, {
    results <- data(package = input$Package)$results
    if (nrow(results) == 0 || ncol(results) != 4) {
      updateSelectInput(session = session, inputId = "DataSet", choices = "", selected = "")
    } else {
      choices <- try({
        results <- data.frame(results, stringsAsFactors = FALSE)
        results$key <- paste0(results$Item," - ", results$Title)
        results <- results[ !duplicated(results$key), ]
        choices <- results$Item
        names(choices) <- results$key
        choices
      }, silent = TRUE)
      if (length(choices) == 0) {
        updateSelectInput(session = session, inputId = "DataSet", choices = "", selected = "")
      } else {
        updateSelectInput(session = session, inputId = "DataSet", choices = choices, selected = choices[1])
      }
    }
  })

  ###########################################################################
  getRawData <- reactive({
    req(input$DataSource)
    input$CSVFile
    input$DataSet
    isolate({
      if (input$DataSource == "CSV file") {
        req(input$CSVFile, input$CSVFile != "")
      } else if(input$DataSource == "R dataset") {
        req(input$Package, input$DataSet, input$DataSet != "")
      }
      d <- tryCatch({
        if (input$DataSource == "CSV file") {
          # when reading semicolon separated files,
          # having a comma separator causes `read.csv` to error
          d <- read.csv(input$CSVFile$datapath,
                        header = input$Header,
                        sep = input$Sep,
                        quote = input$Quote)
          req(d)

          # converts whole-number columns to "integer"
          numCols <- colnames(d)[unlist(lapply(d, is.numeric))]
          for (col in numCols) {
            if (all(is.wholenumber(d[,col]))) {
              #print(paste("Changing numeric column", colnames(d)[col], "to integer"))
              d[,col] <- as.integer(d[,col])
            }
          }

          # converts >90% unique factors back to character or date variables
          factCols <- which(allClass(d) == "factor")
          max <- nrow(d)
          for (col in factCols) {
            if (nlevels(d[,col])/max > 0.9) {
              #print(paste("Changing factor column", colnames(d)[col], "to character"))
              d[,col] <- as.character(d[,col])
            }
          }

          # converts 100% compatible character columns back to date variables
          charCols <- which(allClass(d) == "character")
          for (col in charCols) {
            dates <- as.Date(d[,col], input$DateFormat)
            if (all(is.na(dates) == is.na(d[,col]))) {
              #print(paste("Changing character column", colnames(d)[col], "to date"))
              d[,col] <- dates
            }
          }
        } else if(input$DataSource == "R dataset") {
          if (input$Package == "All") {
            data(list = input$DataSet)
            d <- get(input$DataSet)
          } else {
            data(list = input$DataSet, package = input$Package)
            d <- get(input$DataSet, asNamespace(input$Package))
          }
        }
        as.data.frame(d)
      },
      warn = function(e) {
        shinyalert(title = paste0("Dataset \"",input$DataSet,"\" did not load"), text = e$message, type = "warning")
        NULL},
      error = function(e) {
        shinyalert(title = paste0("Dataset \"",input$DataSet,"\" did not load"), text = e$message, type = "error")
        NULL
      })
      req(d, nrow(d) > 0, ncol(d) > 0)
      updateSelectInput(session=session, inputId = "Class", selected = "")
      updateSelectInput(session=session, inputId = "Weights", selected = "")
      updateSelectInput(session=session, inputId = "PreSplit", selected = "")
      updateSelectInput(session=session, inputId = "ID", selected = NULL)
      updateSelectInput(session=session, inputId = "HideCol", selected = NULL)
      d
    })
  })

  ###########################################################################
  output$RawData <- DT::renderDataTable({
    d <- getRawData()
    dt <- DT::datatable(data = d, rownames = TRUE, selection = "none")
    numericCols <- colnames(d)[unlist(lapply(d, is.numeric))]
    integerCols <- colnames(d)[unlist(lapply(d, is.wholenumber))]
    numericCols <- setdiff(numericCols, integerCols)
    if (length(numericCols) > 0) {
      dt <- formatRound(table = dt, columns = numericCols, digits = 2)
    }
    if (length(integerCols) > 0) {
      dt <- formatRound(table = dt, columns = integerCols, digits = 0)
    }
    dt
  }, server = TRUE)

  ###########################################################################
  # Removes any hidden columns from the Raw Data
  getData <- reactive({
    d <- getRawData()
    req(d)
    req(nrow(d) > 0, ncol(d)>0, length(colnames(d))> 0)

    #remove ID columns (after setting the row-names)
    if(!is.null(input$ID) & length(input$ID) > 0) {
      ids <- which(colnames(d) %in% input$ID)
      rn <- do.call(paste, c(d[ids], sep=":"))
      if (anyDuplicated(rn)) {
        rn <- paste(sep = "-", 1:length(rn),rn) #make unique
      }
      rownames(d) <- rn # assign the id column info to the row names
      #print(paste("dropping", colnames(d)[ids]))
      d <- d[,-ids]
    }

    # remove hidden
    if(!is.null(input$HideCol) && length(input$HideCol) > 0 && all(input$HideCol %in% colnames(d))) {
      d <- d[,-which(colnames(d) %in% input$HideCol)]
    }
    # remove PreSplit
    if(!is.null(input$PreSplit) & input$PreSplit != "" & length(input$PreSplit) > 0) {
      d <- d[, colnames(d) != input$PreSplit]
    }

    # remove obs where target is missing
    if(input$Class %in% colnames(d)) {
      d <- d[!is.na(d[,input$Class]),]
    }

    # ensure target is factor for Classification
    if (input$ProbType == "Classification" && input$Class %in% colnames(d) && is.numeric(d[,input$Class])) {
      d[, input$Class] <- as.factor(d[, input$Class])
    }

    #clean up invalid factor level names
    for (col in 1:ncol(d)) {
      var <- d[,col, drop = TRUE]
      if (is(var, "factor")) {
        newNames <- make.names(levels(var))
        if (any(newNames!=levels(var))) {
          levels(d[,col]) <- newNames
          #print(levels(d[,col]))
        }
      }
    }
    d
  })

  ###########################################################################
  getSomeRawData <- reactive({
    # grab a representative subsample of the data
    d <- getRawData()
    req(is(d,"data.frame"))
    if(nrow(d) <= maxRows) {
      return(d)
    } else {
      rows <- sample(nrow(d), maxRows)
      #preserve order
      rows <- sort(rows, decreasing = FALSE)
      return(d[rows,])
    }
  })

  ###########################################################################
  getSomeData <- reactive({
    # grab a representative subsample of the data
    d <- getData()
    req(is(d,"data.frame"))
    if(nrow(d) <= maxRows) {
      return(d)
    } else {
      rows <- sample(nrow(d), maxRows)
      #preserve order
      rows <- sort(rows, decreasing = FALSE)
      return(d[rows,])
    }
  })

  ###########################################################################
  getRawDataSummary <- reactive({
    d <- getSomeRawData()
    DataSummary(d)
  })

  ###########################################################################
  getDataSummary <- reactive({
    d <- getSomeData()
    DataSummary(d)
  })

  ###########################################################################
  observe({
    d <- getRawData()
    req(d)
    req(ncol(d) > 0)
    ds <- getRawDataSummary()
    choices <- colnames(d)
    names(choices) <- paste0(choices, " [", ds$type, "]")

    #set a default value for input$HideCol and appropriate choices
    use <- union(isolate(input$HideCol), colnames(d)[ds$constant])
    updateSelectInput(session = session, inputId = "HideCol", choices = choices, selected = sort(use))
  })

  ###########################################################################
  observe({
    ds <- getDataSummary()
    choices <- rownames(ds)
    names(choices) <- paste0(choices, " [",ds$type,"]")

    #set a default value for input$Class and appropriate choices
    defaultClass <- isolate(input$Class)
    if(!(defaultClass %in% choices)) {
      best <- c( choices[which(toupper(choices)=="TARGET")],
                 choices[which(toupper(choices)=="Y")],
                 choices[which(toupper(choices)=="CLASS")],
                 choices[which(toupper(choices)=="LABEL")],
                 choices[which(toupper(choices)=="CHURN")],
                 choices[which(ds$numeric)],
                 choices[which(ds$factor)],
                 choices )
      defaultClass <- best[1]
    }
    updateSelectInput(session = session, inputId = "Class", choices = choices, selected = defaultClass)
  })

  ###########################################################################
  observe({
    ds <- getRawDataSummary()
    choices <- rownames(ds)
    names(choices) <- paste0(choices, " [",ds$type,"]")

    #set a default value for input$ID and appropriate choices
    defaultID <- isolate(input$ID)
    defaultID <- defaultID[defaultID %in% choices & defaultID != ""]
    if(length(defaultID) == 0) {
      # choose character or whole_number columns that have 100% uniqueness
      defaultID <- rownames(ds)[ds$uniqueRatio==1.0 & (ds$wholeNumb | ds$text)]
      if (length(defaultID) > 1) {
        defaultID <- defaultID[1]
      }
    }
    if(length(defaultID) == 0) {
      best <- unique(c( choices[toupper(choices)=="ID"], choices[ds$text] ))
      defaultID <- best
    }
    if(length(defaultID) == 0) {
      defaultID <- NULL
    }
    updateSelectInput(session = session, inputId = "ID", choices = choices, selected = defaultID)
  })

  ###########################################################################
  observe({
    ds <- getDataSummary()
    choices <- rownames(ds)
    names(choices) <- paste0(choices, " [",ds$type,"]")

    updateSelectInput(session = session, inputId = "Weights", choices = choices[ds$numeric], selected = isolate(input$Weights))
    updateSelectInput(session = session, inputId = "PreSplit", choices = choices[ds$binary], selected = isolate(input$PreSplit))
  })

  ###########################################################################
  observeEvent(input$Class, {
    ds <- getDataSummary()
    ds <- ds[rownames(ds) == input$Class, ]

    if (nrow(ds) == 0) {
      ptype <- NA
    } else if (ds$binary | ds$factor | (ds$wholeNumb & ds$uniqueness < 11) ) { #arbitrary 11 used here
      ptype <- "Classification"
    } else if (ds$numeric & ds$uniqueRatio > 0.5) { #arbitrary 50% unique
      ptype <- "Regression"
    } else {
      ptype <- NA
    }
    if (is.na(ptype)) {
      updateRadioButtons(session = session, inputId = "ProbType", selected = "Any")
    } else {
      updateRadioButtons(session = session, inputId = "ProbType", selected = ptype)
    }
  })

  ###########################################################################
  observeEvent(input$ProbType, {
    if (input$ProbType == "Classification") {
      d <- getData()
      ds <- getDataSummary()
      col <- d[,which(colnames(d) == input$Class)]
      uniqCnt <- length(unique(na.omit(col)))
      updateRadioButtons(session = session, inputId = "BiClass", selected = ifelse(uniqCnt == 2, "Only", "Avoid"))
      allPredFactors <- all(ds[rownames(ds) != input$Class, "factor"], drop = TRUE)
      updateRadioButtons(session = session, inputId = "CatPredictors", selected = ifelse(allPredFactors, "Only", "Avoid"))
      updateSelectInput(session = session, inputId = "HypMetric", choices = c("Accuracy","Kappa"), selected = "Accuracy")
    } else if (input$ProbType == "Regression") {
      updateRadioButtons(session = session, inputId = "BiClass", selected = "Neutral")
      updateRadioButtons(session = session, inputId = "CatPredictors", selected = "Neutral")
      updateSelectInput(session = session, inputId = "HypMetric", choices = c("RMSE","Rsquared"), selected = "RMSE")
    }
  })

  ###########################################################################
  output$YSummary <- renderText({
    req(input$Class)
    d <- getSomeData()
    col <- d[,which(colnames(d) == input$Class)]
    type <- class(col)
    uniqCnt <- length(unique(na.omit(col)))

    if ("numeric" %in% type) {
      isInt <- all(unlist(lapply(col, FUN = is.wholenumber)))
      if (isInt & uniqCnt == 2 & nrow(d) > uniqCnt) {
        text <- paste("Since there are only", uniqCnt, "unique integer Y values, this is a binary classification problem we are looking at.")
      } else if (isInt & uniqCnt < 11 & nrow(d) > uniqCnt) { #arbitrary 11 used here
        text <- paste("Warning: Since there are", uniqCnt, "unique integer Y values, this is a multinomial classification problem we are looking at.\nTo make this explicit you may choose to make this column a factor")
      } else if (uniqCnt == 1) {
        text <- paste("Error: This column has constant data and should be removed. It is not a legitimate target variable.")
      } else if (!isInt & uniqCnt/nrow(d) < 0.5) { #arbitrary 0.5 used here
        text <- paste("Warning: A column with only", paste0(round(uniqCnt/nrow(d)*100),"%"), "unique numeric values is a suspicious dependent variable for regression")
      } else {
        text <- paste("This is a regression problem.")
      }
    } else if ("character" %in% type) {
      if (uniqCnt == nrow(d)) {
        text <- paste("Warning: This column has text data and should be removed or used as a row identifier")
      } else {
        text <- paste("Warning: This column has text data and should be removed or used as a partial row identifier")
      }
    } else if ("factor" %in% type) {
      if (uniqCnt == 2 & nrow(d) > uniqCnt) {
        text <- paste("This is a binary classification problem.")
      } else if (uniqCnt == nrow(d) ) {
        text <- paste("Error: This column effectively has unique text in each row and should be removed or used as a row identifier. It is not a legitimate target variable.")
      } else if (uniqCnt/nrow(d) > 0.3) { #arbitrary 0.3 used here
        text <- paste("Warning: There are", uniqCnt, "unique Y levels, that is", paste0(round(uniqCnt/nrow(d)*100),"%"),"of the rows. This is a suspicious dependent variable for classification")
      } else if (uniqCnt == 1) {
        text <- paste("Error: This column has constant data and should be removed. It is not a legitimate target variable.")
      } else {
        text <- paste("This is a multinomial (",uniqCnt," levels) classification problem.")
      }
    } else if ("Date" %in% type) {
      text <- paste("Warning: This column has date/time data and should be removed or used as a row identifier. It is not a legitimate target variable.")
    } else {
      text <- paste("Error: The type is", type,". It is not a legitimate target variable.")
    }
    text
  })

  ###########################################################################
  output$DataIssues <- renderText({
    d <- getRawData()
    ds <- getRawDataSummary()

    dn <- d[,ds$numeric]
    NZV <- caret::nearZeroVar(dn, names = TRUE)
    text <- c()
    if(length(NZV) > 0) {
      text <- c(text, paste("The following numeric variables suffer from Near Zero Variance:",paste0(NZV, collapse=", ")))
    }

    NZVf <- c()
    df <- d[,ds$factor]
    for(col in colnames(df)) {
      if (nzvf(df[,col, drop=TRUE])) {
        NZVf <- c(NZVf, col)
      }
    }
    if(length(NZVf) > 0) {
      text <- c(text, paste("The following factor variables suffer from Near Zero Variance:",paste0(NZVf, collapse=", ")))
    }

    text <- c()
    if(length(NZV) > 0) {
      text <- c(text, paste("The following variables suffer from Near Zero Variance:",paste0(NZV, collapse=", ")))
    }


    combos <- caret::findLinearCombos(na.omit(dn))
    if (length(combos) > 0 && length(combos$remove) > 0) {
      text <- c(text, paste("The following variables are possibly redundant due to correlation:", paste0(colnames(dn)[combos$remove], collapse=", ")))
    }

    NAM <- colnames(d)[ds["missingRate"] > 0.5]
    if (length(NAM) > 0) {
      text <- c(text, paste("The following variables suffer from excessive missing values:", paste0(NAM, collapse=", ")))
    }

    NotContinuous <- colnames(d)[ds["uniqueRatio"] < 0.3 & ds["wholeNumb"]]
    if (length(NotContinuous) > 0) {
      text <- c(text, paste("The following variables suffer from being non-continuous integer variables:", paste0(NotContinuous, collapse=", ")))
    }

    Continuous <- colnames(d)[ds["uniqueRatio"] > 0.9 & ds["factor"]]
    if (length(Continuous) > 0) {
      text <- c(text, paste("The following variables suffer from being near-continuous factor variables:", paste0(Continuous, collapse=", ")))
    }

    NZVF <- colnames(d)[ds["uniqueRatio"] > 0.9 & ds["factor"]]
    if (length(Continuous) > 0) {
      text <- c(text, paste("The following variables suffer from being near-continuous factor variables:", paste0(Continuous, collapse=", ")))
    }


    paste(text, collapse = "\n\n")
  })

  ###########################################################################
  output$RawDataSummary <- renderUI({
    print(summarytools::dfSummary(getSomeRawData()),
          method = 'render',
          omit.headings = TRUE,
          bootstrap.css = FALSE)
  })

  ###########################################################################
  output$MissingChart <- renderPlot({
    naniar::vis_miss( getSomeData() ) +
#      coord_flip() +
      ggtitle("Missing Value Distribution") +
      theme(plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))
  })

  ###########################################################################
  observe({
    if (ncol(getRawData()) > 15) {
      updateCheckboxInput(session = session, inputId = "ShowLegend", value = FALSE)
    }
  })

  ###########################################################################
  output$RisingChart <- renderPlot({
    d <- getSomeData()
    ds <- getDataSummary()
    d <- d[, ds$uniqueRatio > input$Continuous & ds$numeric, drop = FALSE]  # filter out non-continuous columns
    req(ncol(d) > 0)  # exit if no numeric columns
    # the for-loop below can be rewritten as an Xapply() type of operation (feel free to do this if it bothers you)
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col]
    }
    if (input$ScaleChart) {
      d <- scale(x = d, center = TRUE, scale = TRUE)
    }
    mypalette <- rainbow(ncol(d))
    p <- matplot(y = d, type = "l", xlab = "observations", ylab="Values", lty = 1, lwd=1, col = mypalette, main="Rising Order chart of continuous variables")
    if (input$ShowLegend) {
      legend(legend = colnames(d), x = "top", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    }
    p
  })

  ###########################################################################
  output$NaturalOrderChart <- renderPlot({
    d <- getContData()
    req(ncol(d) > 0)  # exit if no numeric columns
    if (input$ScaleChart) {
      d <- scale(x = d, center = TRUE, scale = TRUE)
    }
    mypalette <- rainbow(ncol(d))
    p <- matplot(y = d, type = "l", xlab = "observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Natural Order chart of numerical variables")
    if (input$ShowLegend) {
      legend(legend = colnames(d), x = "top", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    }
    p
  })

  ###########################################################################
  getNumData <- reactive({
    d <- getData()
    types <- allClass(d) %in% c("numeric","integer")
    if (any(types)) {
      d[,which(types), drop = FALSE]
    } else {
      data.frame()
    }
  })

  ###########################################################################
  getContData <- reactive({
    d <- getData()
    ds <- getDataSummary()
    d[, ds$uniqueRatio > input$Continuous & ds$numeric, drop = FALSE]
  })

  ###########################################################################
  getFactData <- reactive({
    d <- getData()
    ds <- getDataSummary()
    noncontNames <- colnames(d)[ds$uniqueRatio < input$Continuous & ds$numeric]
    for(col in noncontNames) {
      d[,col] <- as.factor(d[,col])
    }
    d[, factorNames(d), drop = FALSE]
  })

  ###########################################################################
  output$NumericNovelties <- renderPlot({
    #Boxplots (and outliers) are only meaningful for continuous data
    d <- getContData()
    req(ncol(d) > 0)
    xlab <- "Variable value"
    if (input$ScaleChart) {
      d <- scale(d, center = TRUE, scale = TRUE)
      xlab <- "Standardised variable value"
    }

    prob <- c()
    for (col in 1:ncol(d)) {
      stats <- boxplot.stats(x = d[,col], coef = input$Multiplier)
      if (!input$BoxPlotNovelties || length(stats$out) > 0) {
        prob <- c(prob, col)
      }
    }
    d <- d[, prob, drop = FALSE]
    req(ncol(d) > 0)
    if (ncol(d) == 1) {
      ylab <- colnames(d)
    } else {
      ylab <- NULL
    }

    par(mar = c(1,15,1,1))
    boxplot(d, range = input$Multiplier, main = paste("Continuous Univariate Novelties at IQR multiplier of", input$Multiplier),
            outpch = 21, outcol = "pink", outbg = "red", las = 1, horizontal = TRUE, ylab = ylab, boxwex = 0.5)
    if (ncol(d) == 1){
      axis(1, labels=colnames(d), at=1, las=1)
    }
  })

  ###########################################################################
  output$NumNoveltyTable <- renderDataTable({
    d <- getContData()
    req(ncol(d) > 0)
    if (input$ScaleChart) {
      d <- scale(d, center = TRUE, scale = TRUE)
    }
    out <- vector(mode = "logical", length = nrow(d))
    for (col in 1:ncol(d)) {
      q <- quantile(d[,col], c(1,3)/4, na.rm = TRUE)
      iqr <- q[2]-q[1]
      lbound <- q[1] - input$Multiplier * iqr
      ubound <- q[2] + input$Multiplier * iqr
      out <- out | d[,col] < lbound | d[,col] > ubound
    }
    novelties <- getData()[out,]
    DT::datatable(data = novelties, caption = paste("Continuous Univariate Novelty Observations at IQR multiplier of", input$Multiplier))
  })

  ###########################################################################
  observe({
    d <- getContData()
    vars <- colnames(d)
    num <- ncol(d)
    if (input$Class %in% vars) {
      num <- num-1
    }
    if (num < 2) {
      shinyjs::hideElement(id="DimReductType")
    } else {
      shinyjs::showElement(id="DimReductType")
      if (input$Class %in% vars) {
        updateSelectInput(session = session, inputId = "DimReductType", choices = c("PCA","PLS","MDS"), selected = "PLS")
      } else {
        updateSelectInput(session = session, inputId = "DimReductType", choices = c("PCA","MDS"), selected = "PCA")
      }
    }
  })

  ###########################################################################
  output$MultiDimNovelties <- renderPlot({
    d <- getContData()
    req(ncol(d) > 0)
    vars <- colnames(d)
    num <- length(vars)
    if (input$Class %in% vars) {
      num <- num-1
    }
    req(num >= 2)
    if (input$Class %in% colnames(d)) {
      if (input$DimReductType == "PLS") {
        form <- as.formula(paste0(input$Class, " ~ ."))
      } else {
        form <- as.formula(paste0("~. -", input$Class))
      }
    } else {
      form <- as.formula(paste0("~."))
    }
    par(pty="s")
    switch(
      input$DimReductType,
      "PCA" = biplot(prcomp(formula = form, data = d, rank. = 2, center = TRUE, scale. = input$ScaleChart, na.action = na.omit), main = "PCA - Biplot of first two components", cex = 0.6, var.axes = FALSE, col = c("brown","palegreen4")),
      "PLS" = biplot(plsr(formula = form, ncomp = 2, data = d, center = TRUE, scale = input$ScaleChart, na.action = na.omit), main = "PLS - Biplot of first two components", cex = 0.6, var.axes = FALSE, col = c("brown","palegreen4")),
      "MDS" = {
        cmd <- cmdscale(dist(scale(d, center = TRUE, scale = input$ScaleChart), method = "manhattan"), k = 2)
        plot(cmd, xlab = "PC1", ylab = "PC2", asp = 1, type = "n", main = "MDS - Biplot of first two components")
        text(cmd, labels = rownames(d), cex = 0.6, col = "brown")
      }
    )
    if (input$DimReductType %in% c("PCA", "PLS","MDS") && input$ShowLegend) {
      legend("topleft", c("Observations", "Variables"), fill = c("brown","palegreen4"))
    }
  })

  ###########################################################################
  observe({
    d <- getFactData()
    if (ncol(d) == 0) {
      choices <- c()
    } else {
      choices <- colnames(d)
      levs <- allLevels(d)
      choices <- choices[order(levs, decreasing = FALSE)]
      names(choices) <- paste0(choices, " [", sort(levs, decreasing = FALSE), "]")
    }
    if (length(choices) == 0) {
      selected <- NULL
      shinyjs::hideElement(id="FactorNovelties")
      shinyjs::hideElement(id="Factors")
    } else if (length(choices) > 2) {
      selected <- choices[1:2]
      shinyjs::showElement(id="FactorNovelties")
      shinyjs::showElement(id="Factors")
    } else {
      selected <- choices[1:length(choices)]
      shinyjs::showElement(id="FactorNovelties")
      shinyjs::showElement(id="Factors")
    }
    updateSelectizeInput(session = session, inputId = "Factors", choices = choices, selected = selected)
  }, priority = 10)

  ###########################################################################
  output$FactorNovelties <- renderPlot({
    #For factor data - show mosaic plot
    d <- getFactData()
    req(input$Factors)
    req(all(input$Factors %in% colnames(d)))
    formula = as.formula(paste0("~", paste0(input$Factors, collapse = "+")))
    mosaic(formula = formula, data = d, main = "Factor novelties", sub = "Novelty shows in red",
           legend = TRUE, shade = TRUE)
  })

  ###########################################################################
  output$YBalance <- renderPlot({
    req(input$Class)
    d <- getData()
    column <- d[,input$Class, drop = TRUE]
    ggplot(data = d, aes(x = column)) +
      geom_histogram(fill = "blue", alpha = 0.7, stat = ifelse(is(column, "numeric"), "bin", "count")) +
      geom_bar(fill = "blue", alpha = 0.7) +
      labs(title = "Histogram of dependent variable") +
      labs(x = input$Class, y = "Count") +
      theme(plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))
  })

  ###########################################################################
  output$FeaturePlot <- renderPlot({
    chart.Correlation(getNumData())
  })

  ###########################################################################
  output$MissCorr <- renderPlot({
    req(input$Class)
    d <- getSomeData()
    m <- ifelse(is.na(d[,colnames(d) != input$Class]), 1, 0)
    cm <- colMeans(m)
    cnts <- colSums(m) / nrow(m)
    m <- m[, cm > 0 & cm < 1 & cnts > input$ThresholdMissing]
    req(ncol(m) > 0)
    corrgram::corrgram(cor(m), order = "OLO", abs = TRUE)
    title(main="Predictor missing value correlation")
  })

  ###########################################################################
  output$MissingnessInformation <- renderPlot({
    req(input$Class)
    d <- getSomeData()
    m <- ifelse(is.na(d[,colnames(d) != input$Class]), 1, 0)
    cm <- colMeans(m)
    cnts <- colSums(m) / nrow(m)
    m <- m[, cm > 0 & cm < 1 & cnts > input$ThresholdMissing]
    req(ncol(m) > 0)
    m <- cbind(d[, colnames(d) == input$Class], m)
    colnames(m)[1] <- input$Class
    form <- as.formula(paste(input$Class, "~ ."))
    model <- pls::mvr(formula = form, data = as.data.frame(m), center = TRUE, scale = input$ScaleChart)
    data <- data.frame(d[,input$Class], scores(model)[,1])
    colnames(data) <- c(input$Class, "Comp1")
    if(input$ProbType == "Regression") {
      ggplot2::ggplot(data, mapping = aes(x=data[,2], y=data[,1])) +
        ggtitle(label = "Relationship between target and missingness first-component") +
        labs(x = "Component 1", y = input$Class) +
        geom_point() +
        theme(plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))
    } else {
      ggplot2::ggplot(data, mapping = aes(x=data[,1], y=data[,2])) +
        ggtitle(label = paste("Relationship between", input$Class, "and missingness first-component")) +
        labs(y = "Component 1", x = input$Class) +
        geom_boxplot(notch = FALSE, coef = 1.5, fill = "orange") +
        coord_flip() +
        theme(plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))
    }
  })

  ###########################################################################
  getPartition <- reactive({
    d <- getRawData()
    if(!is.null(input$PreSplit) && length(input$PreSplit) > 0 && input$PreSplit != "") {
      splitter <- d[ ,input$PreSplit, drop = TRUE]
      lvs <- unique(splitter)
      req(length(lvs) == 2)
      shinyjs::hideElement(id="Ratio")
      train <- 2
      if (toupper(as.character(lvs[1])) == "TRAIN")  {
        train <- 1
      } else if (sum(splitter==lvs[1]) > nrow(d)) {
        train <- 1
      }
      split <- splitter == lvs[train]
      split <- (1:nrow(d))[split] # want this as row numbers
    } else {
      req(input$Class)
      req(any(colnames(d) == input$Class))
      shinyjs::showElement(id="Ratio")
      y <- d[, which(colnames(d) == input$Class), drop = TRUE]
      req(length(y) > 1)
      if (class(y) == "character") {
        split <- sample(x = 1:nrow(d), size = floor(input$Ratio * nrow(d)))
      } else {
        results <- tryCatch({
          unlist(createDataPartition(y = y, p = input$Ratio))
        },
        error = function(e) {
          return(e)
        })
        if (is(results, "error")) {
          react$partitionMessages <- c(results$messages, warnings())
          split <- NULL
        } else {
          react$partitionMessages <- warnings()
          split <- results
        }
      }
    }
    split
  })

  ###########################################################################
  output$SplitWarnings <- renderText({
    req(react$partitionMessages)
    paste(collapse="\n", react$partitionMessages)
  })

  ###########################################################################
  output$SplitSummary <- renderText({
    train <- length(getPartition())
    test <- nrow(getData()) - train
    paste("Before preprocessing there are", train, "train cases and", test, "test cases")
  })

  ###########################################################################
  getTrainData <- reactive({
    getData()[getPartition(), ]
  })

  ###########################################################################
  getTestData <- reactive({
    getData()[-getPartition(), ]
  })

  ###########################################################################
  observe({
    input$Restart
    req(input$Class)
    react$Recipe <- recipes::recipe(formula = as.formula(paste(input$Class, "~ .")), data = getTrainData())
    if (input$Weights != "") {
      update_role(react$Recipe, matches(input$Weights), new_role = "case_weight")
    }
    updateCheckboxInput(session = session, inputId = "Missing", value = FALSE)
    updateSelectInput(session = session, inputId = "Impute", selected = "none")
    updateSelectInput(session = session, inputId = "Balance", selected = "none")
    updateCheckboxInput(session = session, inputId = "NZV", value = FALSE)
    updateCheckboxInput(session = session, inputId = "LinComb", value = FALSE)
    updateCheckboxInput(session = session, inputId = "YJ", value = FALSE)
    updateCheckboxInput(session = session, inputId = "Other", value = FALSE)
    updateSelectInput(session = session, inputId = "Convert", selected = "")
    updateSelectInput(session = session, inputId = "DateFeatures", selected = "decimal")
    updateCheckboxInput(session = session, inputId = "Center", value = FALSE)
    updateCheckboxInput(session = session, inputId = "Scale", value = FALSE)
    updateSelectInput(session = session, inputId = "DimReduce",selected = "none")

    isolate({
      # numPresent <- any(react$Recipe$var_info$type == "numeric")
      # nomPresent <- any(react$Recipe$var_info$type == "nominal")
      dtePresent <- any(react$Recipe$var_info$type == "date")
      # othPresent <- !all(react$Recipe$var_info$type %in% c("numeric","date","nominal"))
    })
    shinyjs::toggleElement(id = "DateFeatures", condition = dtePresent)
    shinyjs::toggleElement(id = "Balance", condition = input$ProbType == "Classification")
  })

  ###########################################################################
  observeEvent(
    input$Impute,
    ##### Imputation #####
    {
      react$Recipe <- remove_step(react$Recipe, c("step_knnimpute", "step_bagimpute", "step_medianmpute", "step_modeimpute", "step_rollimpute", "step_meanimpute"))
      if (input$Impute != "none") {
        react$Recipe <- switch(
          input$Impute,
          "omit"         = step_naomit(react$Recipe, all_predictors()),
          "knnImpute"    = step_knnimpute(react$Recipe, all_predictors(), neighbors = 5),
          "bagImpute"    = step_bagimpute(react$Recipe, all_predictors(), impute_with = imp_vars(all_predictors()), trees = 25) ,
          "medianImpute" = step_medianimpute(react$Recipe, all_numeric(), -all_outcomes()) %>% step_modeimpute(all_nominal(), -all_outcomes()),
          "rollImpute"   = step_rollimpute(react$Recipe, all_numeric(), -all_outcomes(), window = 5, statistic = median) %>% step_modeimpute(all_nominal(), -all_outcomes()),
          "mean"         = step_meanimpute(react$Recipe, all_numeric(), -all_outcomes()) %>% step_modeimpute(all_nominal(), -all_outcomes())
        )
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$Balance,
    ##### Balance Y column #####
    {
      react$Recipe <- remove_step(react$Recipe, c("step_upsample", "step_downsample"))
      if (input$ProbType == "Classification" && input$Balance != "none") {
        react$Recipe <- switch(
          input$Balance,
          "up"      = step_upsample(react$Recipe, all_outcomes(), ratio = 1.0, skip = TRUE),
          "down"    = step_downsample(react$Recipe, all_outcomes(), ratio = 1.0, skip = TRUE),
          "up-down" = step_downsample(react$Recipe, all_outcomes(), ratio = 0.5, skip = TRUE) %>% step_upsample(all_outcomes(), ratio = 1.0, skip = TRUE)
        )
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$LinComb,
    ##### Remove Linear Combinations (one of) #####
    {
      if(input$LinComb) {
        react$Recipe <- step_lincomb(react$Recipe, all_numeric(), -all_outcomes())
      } else {
        react$Recipe <- remove_step(react$Recipe, c("step_lincomb"))
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$YJ,
    ##### Yeo-Johnson reshaping #####
    {
      if(input$YJ) {
        react$Recipe <- step_YeoJohnson(react$Recipe, all_numeric(), -all_outcomes())
      } else {
        react$Recipe <- remove_step(react$Recipe, c("step_YeoJohnson"))
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$Missing,
    ##### Drop near-all-missing columns #####
    {
      if (input$Missing) {
        react$Recipe <- step_missing(react$Recipe, all_predictors(), ratio = 0.5)
      } else {
        react$Recipe <- remove_step(react$Recipe, "step_missing")
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$NZV,
    ##### Drop Near-Zero Variance columns #####
    {
      if (input$NZV) {
        react$Recipe <- step_nzv(react$Recipe, all_predictors(), options = list(freq_cut = 95/5, unique_cut = 10))
      } else {
        react$Recipe <- remove_step(react$Recipe, "step_nzv")
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$Other,
    ##### Accumulate Other level #####
    {
      if (input$Other) {
        react$Recipe <- step_other(react$Recipe, all_nominal(), -all_outcomes(), threshold = 0.05, other = "other")  ##arbitrary value 0.05
      } else {
        react$Recipe <- remove_step(react$Recipe, "step_other")
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$Convert,
    ##### Create new columns through conversion #####
    {
      shinyjs::toggleElement(id = "OneHot", condition = input$Convert)
      if (input$Convert) {
        react$Recipe <- step_dummy(react$Recipe, all_nominal(), -all_outcomes(),  one_hot = input$OneHot)
      } else {
        react$Recipe <- remove_step(react$Recipe, c("step_dummy"))
      }
    }
  )



  ###########################################################################
  observeEvent(
    input$Center,
    ##### Center numeric columns #####
    {
      if (input$Center) {
        react$Recipe <- step_center(react$Recipe, all_numeric(), -all_outcomes())
      } else {
        react$Recipe <- remove_step(react$Recipe, "step_center")
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$Scale,
    ##### Scale numeric columns #####
    {
      if (input$Scale) {
        react$Recipe <- step_scale(react$Recipe, all_numeric(), -all_outcomes())
      } else {
        react$Recipe <- remove_step(react$Recipe, "step_scale")
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$DimReduce,
    ##### Dimensional Reduction #####
    {
      shinyjs::toggleElement(id = "VarThresh", condition = input$DimReduce %in% c("corr","pca"))
      react$Recipe <- remove_step(react$Recipe, c("step_corr", "step_ica", "step_pls", "step_kpls", "step_isomap", "step_pca"))
      if(input$DimReduce != "none") {
        react$Recipe <- switch(
          input$DimReduce,
          "corr"   = step_corr(react$Recipe, all_numeric(), threshold = input$VarThresh),
          "ica"    = step_ica(react$Recipe, all_numeric(), -all_outcomes(), num_comp = 99),
          "pls"    = step_pls(react$Recipe, all_numeric(), -all_outcomes(), num_comp = 99) ,
          "kpls"   = step_kpca(react$Recipe, all_numeric(), -all_outcomes(), num_comp = 99, options = list(kernel = "rbfdot", kpar = list(sigma = 0.2))),
          "isomap" = step_isomap(react$Recipe, all_numeric(), -all_outcomes(), num_comp = 99),
          "pca"    = step_pca(react$Recipe, all_numeric(), -all_outcomes(), threshold = input$VarThresh, options = list(retx = FALSE, center = TRUE, scale. = input$Scale, tol = NULL))
        )
      }
    }
  )


  ###########################################################################
  observeEvent(
    input$DateFeatures,
    ##### Create new columns from Date columns #####
    {
      react$Recipe <- remove_step(react$Recipe, c("step_date"))
      if (length(input$DateFeatures) > 0) {
        react$Recipe <- step_date(react$Recipe, all_predictors(), -all_nominal(), -all_numeric(), features = input$DateFeatures)
      }
    }
  )

  ###########################################################################
  getPrepRecipe <- reactive({
    req(is(react$Recipe, "recipe"))
    tryCatch({
      recipes::prep(react$Recipe, verbose = Verbose, retain = TRUE)
    }, error = function(e) {
      e$message
    })
  })

  ###########################################################################
  getPreProcTrain <- reactive({
    prepRecipe <- getPrepRecipe()
    req(class(prepRecipe) == "recipe")
    recipes::juice(prepRecipe, everything(), composition = "data.frame")
  })

  # ###########################################################################
  # getPreProcTest <- reactive({
  #   prepRecipe <- getPrepRecipe()
  #   req(class(prepRecipe) == "recipe")
  #   recipes::bake(prepRecipe, everything(), new_data = getTestData(), composition = "data.frame")
  # })

  ###########################################################################
  output$PreProcSummary <- renderPrint({
    getPrepRecipe()
  })

  ###########################################################################
  output$ProcessedDataSummary <- renderUI({
    print(summarytools::dfSummary(getPreProcTrain()),
          method = 'render',
          omit.headings = TRUE,
          bootstrap.css = FALSE)
  })

  ###########################################################################
  output$PreProcTable <- DT::renderDataTable({
    d <- getPreProcTrain()
    dt <- DT::datatable(data = d, rownames = TRUE, selection = "none")
    numeric <- which(allClass(d) == "numeric")
    if (length(numeric) > 0) {
      dt <- formatRound(table = dt, columns = numeric, digits = 2)
    }
    dt
  }, server = TRUE)

  ###########################################################################
  getModels <- reactive({
    input$RefreshModels
    mi <- getModelInfo()

    # correction to model info
    mi[["chaid"]]$tags <- c(mi[["chaid"]]$tags, "Categorical Predictors Only")

    Label <- vector(mode="character", length = length(mi))
    Library <- vector(mode="character", length = length(mi))
    Hyperparams <- vector(mode="character", length = length(mi))
    Regression <- vector(mode="logical", length = length(mi))
    Classification <- vector(mode="logical", length = length(mi))
    Tags <- vector(mode="character", length = length(mi))
    ClassProbs <- vector(mode="character", length = length(mi))
    for (row in 1:length(mi)){
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode="logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Library[row] <- paste(collapse="<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse="<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse="<br/>", mi[[row]]$tags)
      ClassProbs <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    m <- data.frame(Model = names(mi), Label, Library, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
    m[sample(nrow(m)),] # randomise the rows so the sames models are not always at the top
  })

  ###########################################################################
  observe({
    if (input$ProbType == "Classification") {
      showElement(id = "BiClass")
      showElement(id = "Probs")
      showElement(id = "CatPredictors")
      hideElement(id = "DimReduction")
    } else {
      hideElement(id = "BiClass")
      hideElement(id = "Probs")
      hideElement(id = "CatPredictors")
      showElement(id = "DimReduction")
    }
  })

  ###########################################################################
  observe({
    d <- getData()
    req(d, input$Class, input$Class %in% colnames(d))
    lvls <- length(unique(d[, input$Class]))
    if (input$ProbType == "Classification" && lvls == 2) {
      updateSelectInput(session = session, inputId = "HypMetric", choices = c(clasChoices, biClasChoices), selected = clasChoices[1])
    } else if (input$ProbType == "Classification" && lvls != 2) {
      updateSelectInput(session = session, inputId = "HypMetric", choices = clasChoices, selected = clasChoices[1])
    } else {
      updateSelectInput(session = session, inputId = "HypMetric", choices = regChoices, selected = regChoices[1])
    }
  })

  ###########################################################################
  getFiltModels <- reactive({
    d <- getModels()
    d <- switch(input$ProbType,
                "Classification" = d[ d$Classification != "", ],
                "Regression" = d[ d$Regression != "", ],
                "Any" = d
    )

    #     what about "Binary Predictors Only"? TODO
    d <- switch(input$BiClass,
                "Only" = d[ grepl(pattern = "Two Class Only", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Two Class Only", d$Tags), ],
                "Neutral" = d
    )

    d <- switch(input$Probs,
                "Only" = d[ d$ClassProb != "", ],
                "Avoid" = d[ d$ClassProb == "", ],
                "Neutral" = d
    )

    d <- switch(input$CatPredictors,
                "Only" = d[ grepl(pattern = "Categorical Predictors Only", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Categorical Predictors Only", d$Tags), ],
                "Neutral" = d
    )

    d <- switch(input$Linearity,
                "Only" = d[ grepl(pattern = "Linear", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Linear", d$Tags), ],
                "Neutral" = d
    )

    d <- switch(input$Ensembled,
                "Only" = d[ grepl(pattern = "Ensemble", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Ensemble", d$Tags), ],
                "Neutral" = d
    )

    d <- switch(input$Regularisation,
                "Only" = d[ grepl(pattern = "Regularization", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Regularization", d$Tags), ],
                "Neutral" = d
    )

    d <- switch(input$Robustness,
                "Only" = d[ grepl(pattern = "Robust Methods", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Robust Methods", d$Tags), ],
                "Neutral" = d
    )

    d <- switch(input$Transparency,
                "Only" = d[ grepl(pattern = "Rule-Based Model|Feature Extraction|Model Tree", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Rule-Based Model|Feature Extraction|Model Tree", d$Tags), ],
                "Neutral" = d
    )
    d <- switch(input$Complexity,
                "Only" = d[ grepl(pattern = "Kernel Method", d$Tags), ],
                "Avoid" = d[ !grepl(pattern = "Kernel Method", d$Tags), ],
                "Neutral" = d
    )

    if (input$AvailPackages) {
      d <- d[ !grepl(pattern="<i class", d$Library), ]
    }
    d
  })

  ###########################################################################
  observeEvent(input$ChooseAll, {
    tableProxy <- dataTableProxy(outputId = "ModelTable", session = session)
    selectRows(tableProxy, input$ModelTable_rows_all)
  })

  ###########################################################################
  output$ModelTable <- DT::renderDataTable({
      d <- getFiltModels()
      DT::datatable(data = d, class='display', rownames = FALSE, selection = "multiple", escape = FALSE, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 5
      ))
    })


  ###########################################################################
  getSelectedModels <- reactive({
    getFiltModels()[input$ModelTable_rows_selected, ]
  })


  ###########################################################################
  output$SelectedModelsTable <- renderTable({
    getSelectedModels()[, c("Model", "Label")]
  })

  ###########################################################################
  output$SelWarn <- renderText({
    if (nrow(getSelectedModels()) == 0) {
      "Select one or more models in the previous panel"
    }
  })

  ###########################################################################
  getModelsDummyVars <- reactive({
    data <- getFiltModels()
    req(nrow(data) > 1)
    mat1 <- matrix(0, nrow = nrow(data), ncol = 2)
    colnames(mat1) <- c("Regression", "Classifiaction")
    mat1[,1] <- ifelse(data$Regression != "", 1, 0)
    mat1[,2] <- ifelse(data$Classification != "", 1, 0)

    tags <- unique(unlist(strsplit(x = data$Tags, split = "<br/>")))
    mat2 <- matrix(0, nrow = nrow(data), ncol = length(tags))
    req(length(tags) >= 2)  # for 2D pc1 x pc2 chart
    colnames(mat2) <- tags
    for (t in 1:length(tags)) {
      mat2[,t] <-ifelse ( grepl(pattern = tags[t], x = data$Tags, fixed = TRUE), 1, 0 )
    }
    cbind(mat1,mat2)
  })

  ###########################################################################
  observe({
    d <- getSelectedModels()
    if (nrow(d) < 3) {
      shinyjs::hideElement(id = "Suggest")
    } else {
      shinyjs::showElement(id = "Suggest")
    }
    if (nrow(d) == 0) {
      shinyjs::hideElement(id = "Install")
    } else if (nrow(d[grepl(pattern="<i class", d$Library),]) > 0) {
      shinyjs::showElement(id = "Install")
    } else {
      shinyjs::hideElement(id = "Install")
    }
  })

  ###########################################################################
  observeEvent(input$Install, {
      d <- getSelectedModels()
      req(nrow(d) > 0)
      shinyjs::hideElement(id = "Install")
      librs <- unlist(strsplit(d$Library, split="<br/>"))
      librs <- librs[grepl(pattern = "<i.*>", x = librs)]
      librs <- gsub(pattern = " +<.*>", replacement = "", x = librs)
      for (lib in unique(librs)) {
        if (!require(package = lib, character.only = TRUE)) {
          try(silent = TRUE, expr = {
            showNotification(paste("Installing package", lib), duration = NULL, id = lib)
            install.packages(pkgs = lib, quiet = TRUE, warn.conflicts = TRUE, character.only = TRUE)
            library(package = lib, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
            removeNotification(id = lib)
          })
        }
      }
  }, ignoreInit = TRUE)

  ###########################################################################
  observeEvent(input$Suggest, {
    req(length(input$ModelTable_rows_selected) > 2)
    b <- as.data.frame(getModelsDummyVars())
    picked <- b[input$ModelTable_rows_selected, ]
    suggested <- caret::maxDissim(a = picked, b = b, method = input$DistMetric, n = 1)
    tableProxy <- dataTableProxy(outputId = "ModelTable", session = session)
    selected <- c(input$ModelTable_rows_selected, suggested)
    selectRows(proxy = tableProxy, selected = selected)
  })

  ###########################################################################
  get2Ddata <- reactive({
    # reduce to 2 PC and plot in xy
    d <- dist(getModelsDummyVars(), method = input$DistMetric)
    set.seed(input$ModelTable_rows_selected)
    d2d <- cmdscale(d, k = 2)
    req(ncol(d2d) == 2) # can be less than 2 so check is needed
    colnames(d2d) <- c("PC1", "PC2")
    rownames(d2d) <- getFiltModels()$Model
    d2d <- as.data.frame(d2d)
    d2d$Selected <- (1:nrow(d2d) %in% input$ModelTable_rows_selected)
    d2d
  })

  ###########################################################################
  output$D2D <- renderPlot({
    d2d <- get2Ddata()
    ggplot(data = d2d, mapping = aes(x = PC1, y = PC2, color = Selected)) +
      ggtitle(label = "2D Representation of Model Types") +
      geom_point() +
      geom_text_repel(label = rownames(d2d)) +
      theme(plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))
  })

  ###########################################################################
  output$D2DTable <- renderTable({
    req(input$D2D_brush)
    names <- rownames(brushedPoints(df = get2Ddata(), brush = input$D2D_brush))
    fm <- getFiltModels()
    fm[fm$Model %in% names, c("Model","Label", "Tags", "Hyperparams")]
  }, sanitize.text.function = function(x) x)

  ###########################################################################
  observe({
    shinyjs::toggle(id="Number", condition = input$Method != "none")
    shinyjs::toggle(id="Repeats", condition = grepl("[d_]cv$", input$Method))
    shinyjs::toggle(id="TuneLength", condition = input$Method != "none")
    shinyjs::toggle(id="Balance", condition = input$ProbType == "Classification")
  })

  ###########################################################################
  observe({
    if (grepl("cv", input$Method)) {
      updateNumericInput(session = session, inputId = "Number", label = "Number of CV folds", value = 10)
    } else {
      updateNumericInput(session = session, inputId = "Number", label = "Number of iterations", value = 25)
    }
  })

  ###########################################################################
  getTrainControl <- reactive({
    req(input$Method)
    req(input$ProbType)
    req(input$Class)
    y <- getTrainData()[,input$Class]
    req(length(y) > 0)

    #Prepare the resampling index (considering the effect on ensembling)
    if(any(input$Method %in% c("boot","adaptive_boot"))) {
      index <- caret::createResample(y, times = input$Number, list = TRUE)
      react$AllowEnsemble <- TRUE
    } else if(any(input$Method %in% c("cv","adaptive_cv"))) {
      index  <- caret::createFolds(y, k = input$Number, list = TRUE, returnTrain = TRUE)
      react$AllowEnsemble <- TRUE
    } else if(input$Method == "repeatedcv") {
      index <- caret::createMultiFolds(y, k = input$Number, times = input$Repeats)
      react$AllowEnsemble <- TRUE
    } else if(any(input$Method %in% c("LGOCV","adaptive_LGOCV"))) {
      index <- caret::createDataPartition(y, times = input$Number, p = 0.5, list = TRUE, groups = min(5, length(y)))
      react$AllowEnsemble <- TRUE
    } else {  #c("boot632", "optimism_boot", "boot_all", "LOOCV")
      react$AllowEnsemble <- FALSE
      index <- NULL
      #stop(paste0("caretList does not currently know how to handle cross-validation method='", x$Method, "'. Please specify trControl$index manually"))
    }

    trainControl(
      method = input$Method,
      number = input$Number,
      repeats = ifelse(grepl("[d_]cv$", input$Method), input$Repeats, NA),
      verboseIter = Verbose,
      index = index,
#      sampling = ?????
      preProcOptions = NULL, # Not allowed with Recipes
#      timingSamps = 1000,  # this causes crashes relating to unknown variable names being supplied to predict()
      search = input$Search,
      trim = TRUE,
      allowParallel = TRUE,
      classProbs = input$ProbType == "Classification",
      savePredictions = "final",
      selectionFunction = input$SelectionFunc,
      summaryFunction = ifelse(input$HypMetric %in% c("ROC","Sens","Spec"), twoClassSummary, defaultSummary)
    )
  })

  ###########################################################################
  getSelectedModelTypes <- reactive({
    ml <- c()
    if (is.null(input$ModelTable_rows_selected)) {
      if (input$NullModel) {
        ml <- c("null") # Add the null model
      }
    } else {
      ml <- getFiltModels()[input$ModelTable_rows_selected, "Model"]
      if (input$NullModel & length(ml) > 0) {
        ml <- c("null", ml) # Add the null model
      }
    }
    ml
  })

  ###########################################################################
  observe({
    shinyjs::toggle(id = "ResultsFor", condition = input$ResultsFor != "")
  })

  ###########################################################################
  # Reset any trained models if the train-data, recipe and trControl specifications change
  # This must not trigger due to changes in react$ModelSet
  observeEvent(
    {
      getTrainData()
      getTrainControl()
      input$Reset
      #react$Recipe()  might be better tp allow the preprocessing to vary - to see what works for certain methods
    },
    {
      req(length(react$ModelSet) > 0)
      d <- getTrainData()
      req(nrow(d) > 0)
      showNotification("Resetting trained models", duration = 10, session = session)
      updateRadioButtons(session = session, inputId = "ResultsFor", choices = c(""), selected = "")
      modelSet <- react$ModelSet
      for (m in 1:length(modelSet)) {
         modelSet[[m]] <- NULL
      }
      react$ModelSet <- modelSet

      # if(length(input$ModelTable_rows_selected) > 0) {
      #   proxy <- dataTableProxy(outputId = "ModelTable", session = session)
      #   selectRows(proxy, selected = NULL)
      # }
    })

  ###########################################################################
  observe({
    if(length(getSelectedModelTypes()) == 0) {
      shinyjs::disable(id = "Train")
    } else {
      shinyjs::enable(id = "Train")
    }
  })

  ###########################################################################
  observe({
    input$Train
    req(isolate(input$Class))

    isolate({
      mTypes <- getSelectedModelTypes()
    })

    if (input$Weights != "") {
      recipe <- react$Recipe %>%
        update_role(input$Weights, new_role="case weight")
    } else {
      recipe <- react$Recipe 
    }

    req(length(mTypes) > 0)
    count <- 0
    done <- c()
    assign("last.warning", NULL, envir = baseenv())
    for (method in mTypes) {
      if (!is.null(isolate(react$ModelSet[[method]]))) {  # note the dependency upon reactive react$ModelSet
        count <- count + 1
        done <- c(done, method)
        next
      }
      if (isolate(input$Parallel)) {
        startParallel()
      }
      shinyjs::disable(id = "Train")
      prog <- (count+0.2) / length(mTypes)
      withProgress({
        isolate({
          assign("last.warning", NULL, envir = baseenv())
          setProgress( value = prog, message = paste("Training", method) )
          warns <- utils::capture.output( {
            output <- utils::capture.output( {
              result <- tryCatch(
                 caret::train(
                    x = recipe,
                  data = getTrainData(),
                  method = method,
                  metric = input$HypMetric,
                  trControl = getTrainControl(),
                  tuneLength = ifelse(input$Method == "none", 1, input$TuneLength),
                  maximize = !(input$HypMetric %in% minimiseMetric)
                ), error = function(e) {return(e)})
            }, type = "output")
          }, type = "message")
        })
      })
      if(is(result, "error")) {
        react$ModelSet[[method]] <- "Model failed to train"
        react$Log[[method]] <- c(output, unique(warns), result$message) #, rlang::last_error())
      } else {
        react$ModelSet[[method]] <- result
        react$Log[[method]] <- c(output, unique(warns))
      }

      if (isolate(input$Parallel)) {
        stopParallel()
      }
      done <- c(done, method)
      shinyjs::showElement(id = "ResultsFor")
      updateRadioButtons(session = session, inputId = "ResultsFor", choices = done, selected = method)
      invalidateLater(millis = 2000, session = session)
      return()  # early exit if something was trained (sucessfully or otherwise)
    } #end of for loop

    updateRadioButtons(session = session, inputId = "ResultsFor", choices = done, selected = done[length(done)])
    # to get here there must be no models to train in this pass
    shinyjs::enable(id = "Train")
    showNotification("Finished training", duration = 10, session = session)
  }, priority = -10)

  ###########################################################################
  output$TrainLog <- renderText({
    req(input$ResultsFor, react$Log)
    mess <- react$Log[[input$ResultsFor]]
    req(mess)
    paste(collapse = "\n", mess)
  })

  ###########################################################################
  output$TrainModelRawPlot <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, is(mod, "train"))
    req(mod$method != "null")
    plot(mod$finalModel)
  })

  ###########################################################################
  output$VarImp <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    req(mod$method != "null")

    trellis.par.set(caretTheme())
    # Not every model can produce a Variable-Importance plot so handle silently
    try({
      plot(varImp(mod), main=paste(sep = " - ", "Variable Importance", input$ResultsFor))
    }, silent = TRUE)
  })

  ###########################################################################
  output$Optimize <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    trellis.par.set(caretTheme())
    try({
      plot(mod, main=paste(sep = " - ", "Hyper-parameter optimisation", input$ResultsFor))
    }, silent = TRUE)
  })

  ###########################################################################
  output$Hyperparams <- renderTable({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(is(mod, "train"))
    mod$bestTune
  })
  ###########################################################################
  output$ModelCoef <- renderPrint({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    beta <- try({
       coef(mod$finalModel)
    }, silent = TRUE)
    req(class(beta) == "numeric")
    print(beta)
  })

  ###########################################################################
  output$ModelSummary <- renderPrint({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    print(summary(mod$finalModel))
  })

  ###########################################################################
  output$Predictors <- renderPrint({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    pred <- predictors(mod$finalModel)
    req(pred)
    print(pred)
  })

  ###########################################################################
  output$ModelSummary <- renderPrint({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    print(summary(mod$finalModel))
  })

  ###########################################################################
  getConfusionMatrix <- reactive({
    req(input$ProbType == "Classification", input$Class, input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, "character"))
    caret::confusionMatrix(mod)
  })

  ###########################################################################
  output$TrainedModelPlot <- renderPlot({
    req(input$Class, input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(!inherits(mod, "character"))
    if (input$ProbType == "Regression") {
      train <- getTrainData()
      isY <- colnames(train) == input$Class
      predictions <- predict(mod, newdata = train[, !isY])
      d <- data.frame(predictions, train[,isY])
      colnames(d) <- c("pred", "obs")
      par(pty = "s")
      range <- range(c(d$obs, d$pred), na.rm = TRUE)
      plot(d, xlim = range, ylim = range,  main = "Predicted versus Observed for training data")
      abline(a = 0, b = 1, col = "blue", lty = 2)
    } else {
      cm <- getConfusionMatrix()$table
      cmdf <- melt(data = cm)
      colnames(cmdf) <- c("Prediction", "Observation", "value")
      cmdf$value <- cmdf$value / sum(cmdf$value)
      alluvial::alluvial(
        cmdf[,1:2],
        freq = cmdf$value,
        col = ifelse(cmdf[, 1] == cmdf[, 2], "green", "red"),
        alpha = 0.5,
        hide  = cmdf$value == 0
      )
      mtext("Resampled confusion matrix", 3, line=3, font=2)
    }
  })

  ###########################################################################
  output$TrainPlot <- renderPlot({
    req(input$Class, input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(!inherits(mod, "character"))
    model <- mod$finalModel
    if(any(class(model) %in% c("nullModel","fbrs","gausspr"))) {
      NULL
    } else if(any(class(model) %in% c("rpart"))) {
        rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE)
    } else if(any(class(model) %in% c("lm","glm","enet")) & input$ProbType == "Regression") {
      par(mfrow = c(2, 2),
          oma = c(0, 0, 2, 0))
      plot(model)
    } else if(any(class(model) %in% c("glm") & input$ProbType == "Classification")) {
      NULL
    } else if(any(class(model) %in% c("blassoAveraged", "RRF", "regsubsets", "gam"))) {
      plot(model)
    } else {  # give it a try
      plot(model, main=class(model))
    }
})

  ###########################################################################
  output$FinalModelCM <- renderPrint({
    print(getConfusionMatrix())
  })

  ###########################################################################
  getTrainedModels <- reactive({
    req(length(react$ModelSet) > 1)
    bad <- unlist(lapply(react$ModelSet, FUN = inherits, what = c("character")))
    req(sum(!bad) > 1)
    models <- react$ModelSet[!bad]
    for(m in 1:length(models)) {
      if (is(models[[m]], "caretStack")) {
        models[[m]] <- models[[m]]$ens_model
      }
    }
    models
  })

  ###########################################################################
  getTimings <- reactive({
    d <- getTrainData()
    thousand <- sample(nrow(d), size = 1000, replace = TRUE)
    d <- d[thousand, !colnames(d) == input$Class]
    timing <- c()
    for (mod in getTrainedModels()) {
      t <- system.time(
        predict(mod,  newdata = d, type = "raw")
      )
      timing <- c(timing, t[1])
    }
    timing
  })

  ###########################################################################
  getResampledModels <- reactive({
    res <- try({
      resamples(getTrainedModels())
    }, silent = FALSE)
    req(is(res, "resamples"))

    #Add timings to the metrics
    timings <- getTimings()
    df <- res$values
    origNames <- colnames(df)
    res$metrics[length(res$metrics)+1] <- "logTiming"
    for (m in 1:length(res$models)) {
       timing <- rep(log10(timings[m]), times = nrow(df))
       df <- cbind(df, timing)
    }
    colnames(df) <- c(origNames, paste(sep="~", res$models, "logTiming"))
    res$values <- df
    res
  })

  ###########################################################################
  observe({
    mods <- getResampledModels()
    shinyjs::toggle(id = "NullNormalise", condition = length(mods$models) > 1 & input$NullModel)
    shinyjs::toggle(id = "SelectedModelCM", condition = input$ProbType == "Classification")
    shinyjs::toggle(id = "FinalModelCM", condition = input$ProbType == "Classification")
    shinyjs::toggle(id = "ExcludeNull", condition = input$NullModel)
  })

  ###########################################################################
  output$Selection <- renderPlot({
    mods <- getResampledModels()
    if(!input$NullModel & "null" %in% mods$models) {
      mods$models[mods$models == "null"] <- NULL
      nullCols <- grepl(pattern = "^null~", x = colnames(mods$values))
      mods$values[, nullCols] <- NULL
    }
    req(length(mods$models) > 0)

    #scale metrics using null model
    if(input$NullNormalise & "null" %in% mods$models) {
      actualNames <- colnames(mods$values)

      # Normalise the various hyper-metrics
      for (metric in c(regChoices, clasChoices, biClasChoices)) {
        col <- paste(sep="~", "null", metric)
        if (col %in% actualNames) {
          nullMetric <- mean(mods$values[, col], na.rm = TRUE)
          if(!is.na(nullMetric) & nullMetric != 0) {
            for (model in mods$models) {
              mcol <- paste(sep="~", model, metric)
              if(mcol %in% actualNames) {
                mods$values[, mcol] <- mods$values[, mcol] / nullMetric
              }
            }
          }
        }
      }

      # Normalise the prediction timings per 1000
      nullMetric <- mean(mods$values[, "null~logTiming"], na.rm = TRUE)
      for (model in mods$models) {
         mcol <- paste(sep="~", model, "logTiming")
         if(mcol %in% actualNames) {
            mods$values[, mcol] <- mods$values[, mcol] - nullMetric
         }
      }
    }

    # plot metrics
    trellis.par.set(caretTheme())
    bwplot(mods)
  })

  ###########################################################################
  # Maybe I should use the residuals in the resampled objects
  output$ResidualCorr <- renderPlot({
    rModels <- getResampledModels()
    mods <- c(rModels$models)  #make a copy

    # drop the null model as it is not a true model for correlation purposes
    if (input$ExcludeNull) {
      mods <- mods[-which(mods == "null")]
    }

    #remove any emsembles (i.e. no Recipe classes)
    recips <- vector(mode = "logical", length = length(mods))
    for (i in 1:length(mods)) {
      recips[i] <- is(react$ModelSet[[mods[i]]], "train.recipe")
    }
    mods <- mods[recips]

    req(length(mods) > 1)

    modelSet <- react$ModelSet[mods]
    d <- getTrainData()
    y <- d[,input$Class]
    r <- matrix(NA, nrow = nrow(d), ncol = length(mods))
    yhat <- as.data.frame(predict(modelSet, newdata = d))
    if (any(class(y) == "numeric")) {
      for (m in 1:ncol(yhat)) {
        r[,m] <- y - yhat[,m]
      }
    } else {
      for (m in 1:ncol(yhat)) {
        r[,m] <- ifelse(y == yhat[,m], 0, 1)
      }
    }
    colnames(r) <- mods
    corrplot::corrplot(corr = cor(r), method = "circle", type = "upper", title = "Residual Correlation", diag = TRUE, mar = c(10,4.5,4.5,3) )
  })

  ###########################################################################
  output$Cluster <- renderPlot({
    models <- getResampledModels()
    # hide null model from dendrogram
    if (FALSE) {
      models$models <- models$models[models$models != "null"]
      cols <- grepl(pattern = "^null~", x = colnames(models$values))
      models$values <- models$values[, !cols]
    }
    req(length(models$models) > 2)
    plot(caret::cluster(models), sub= "")
  })

  ###########################################################################
  observe({
    req(input$HypMetric)
    if (length(react$ModelSet) == 0) {
      shinyjs::hideElement(id = "SelectedModel")
    }
    req(length(react$ModelSet) > 0)
    bad <- sapply(react$ModelSet, is, "character")
    models <- react$ModelSet[!bad]
    mNames <- names(models)
    mNames <- mNames[mNames != "null"]
    req(length(mNames) > 0)
    cols <- paste(sep = "~", mNames, input$HypMetric)
    models <- getResampledModels()
    mNames <- try({
      means <- apply(models$values[, cols, drop = FALSE], MARGIN = 2, FUN = mean, na.rm = TRUE)
      mNames[order(means, decreasing = !(input$HypMetric %in% minimiseMetric))]
    }, silent = TRUE)
    updateRadioButtons(session = session, inputId = "SelectedModel", choices = mNames, selected = mNames[[1]])
    shinyjs::toggle(id = "SelectedModel", condition = mNames[[1]] != "")
  })

  ###########################################################################
  isValid2 <- function(x) {
    !is.null(x) && is(x, "train") && is.function(x$modelInfo$prob)
  }

  ###########################################################################
  observe({
    if (length(react$ModelSet) > 0) {
      ok <- sapply(react$ModelSet, isValid2)
      mNames <- names(react$ModelSet)[ok] # Should not be able to select models that have no class probs
      mNames <- mNames[mNames != "null"]
      shinyjs::toggleElement(id = "Ensemble", condition = length(mNames) > 1 & react$AllowEnsemble)
      updateSelectizeInput(session = session, inputId = "Ensemble", choices = mNames, selected = "")
    } else {
      shinyjs::hideElement(id = "Ensemble")
      updateSelectizeInput(session = session, inputId = "Ensemble", choices = c(""), selected = "")
    }
  })

  ###########################################################################
  observe({
    shinyjs::toggleElement(id = "AddModel", condition = length(input$Ensemble) > 1 & react$AllowEnsemble)
  })

  ###########################################################################
  observeEvent(input$AddModel, {
    req(length(input$Ensemble) > 1)
    shinyjs::disable(id = "Train")
    prog <- (length(react$ModelSet)+0.2) / (length(react$ModelSet)+1)
    method <- paste(collapse = "+", input$Ensemble)
    ensembModels <- react$ModelSet[input$Ensemble]
    class(ensembModels) <- "caretList" #Note we are faking this to be a caretList class

    withProgress({
      assign("last.warning", NULL, envir = baseenv())
      setProgress( value = prog, message = paste("Training", method) )
      warns <- utils::capture.output( {
        output <- utils::capture.output( {
          result <- tryCatch({
            trControl = getTrainControl()
            trControl$index <- NULL
            caretStack(all.models = ensembModels, method = "glm", trControl = trControl, metric = input$HypMetric, tuneLength = input$TuneLength)
          }, error = function(e) {return(e)})
        }, type = "output")
      }, type = "message")
    })
    if(is(result, "error")) {
      react$ModelSet[[method]] <- "Model failed to train"
      react$Log[[method]] <- c(output, unique(warns), result$message) #, rlang::last_error())
    } else {
      react$ModelSet[[method]] <- result
      react$Log[[method]] <- c(output, unique(warns))
    }
    updateRadioButtons(session = session, inputId = "ResultsFor", choices = names(react$ModelSet), selected = method)
    updateNavbarPage(session = session, inputId = "Navbar", selected = "Training")
    shinyjs::enable(id = "Train")
  })

  ###########################################################################
  observeEvent(input$Next, {
    if (input$Navbar == "Data") {
      updateNavbarPage(session = session, inputId = "Navbar", selected = "Split")
    } else if (input$Navbar == "Split") {
      updateNavbarPage(session = session, inputId = "Navbar", selected = "Preproc")
    } else if (input$Navbar == "Preproc") {
     updateNavbarPage(session = session, inputId = "Navbar", selected = "Model Types")
    } else if (input$Navbar == "Model Types") {
      updateNavbarPage(session = session, inputId = "Navbar", selected = "Training")
    } else if (input$Navbar == "Training") {
      updateNavbarPage(session = session, inputId = "Navbar", selected = "Selection")
    } else if (input$Navbar == "Selection") {
      updateNavbarPage(session = session, inputId = "Navbar", selected = "Performance")
    }
  })

  ###########################################################################
  observe({
    if (input$Navbar == "Performance") {
      shinyjs::hideElement(id = "Next")
    } else {
      shinyjs::showElement(id = "Next")
    }
  })

  ###########################################################################
  getTestConfusionMatrix <- reactive({
    req(input$ProbType == "Classification")
    req(input$Class, input$SelectedModel)
    mod <- react$ModelSet[[input$SelectedModel]]
    req(!inherits(mod, "character"))
    data <- getTestData()
    isY <- colnames(data) == input$Class
    caret::confusionMatrix(
      data = predict(mod, newdata = data[, !isY]),
      reference = data[,isY],
      dnn = c("Prediction", "Observed"),
      mode = "everything"
    )
  })

  ###########################################################################
  output$SelectedModelPlot <- renderPlot({
    req(input$SelectedModel)
    mod <- react$ModelSet[[input$SelectedModel]]
    req(!inherits(mod, "character"))
    if (input$ProbType == "Classification") {
      cm <- getTestConfusionMatrix()$table
      # alternative to below: mosaicplot(cm)
      cmdf <- melt(data = cm)
      colnames(cmdf) <- c("Prediction", "Observation", "value")
      cmdf$value <- cmdf$value / sum(cmdf$value)
      alluvial::alluvial(
        cmdf[,1:2],
        freq = cmdf$value,
        col = ifelse(cmdf[, 1] == cmdf[, 2], "green", "red"),
        alpha = 0.5,
        hide  = cmdf$value == 0
      )
      mtext("Resampled confusion matrix", 3, line=3, font=2)
    } else if (input$ProbType == "Regression") {
      req(input$ProbType == "Regression")
      test <- getTestData()
      isY <- colnames(test) == input$Class
      predictions <- predict(mod, newdata = test[, !isY])
      d <- data.frame(predictions, test[, isY])
      colnames(d) <- c("pred", "obs")
      par(pty = "s")
      range <- range(c(d$obs, d$pred), na.rm = TRUE)
      plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
      abline(a = 0, b = 1, col = "blue", lty = 2)
    }
  })

  ###########################################################################
  output$Performance <- renderPrint({
    req(input$SelectedModel)
    mod <- react$ModelSet[[input$SelectedModel]]
    req(!inherits(mod, "character"))

    test <- getTestData()
    isY <- colnames(test) == input$Class
    predictions <- predict(mod, newdata = test[, !isY])
    d <- data.frame(test[, isY], predictions)
    colnames(d) <- c("obs", "pred")

    if (input$ProbType == "Classification") {
      N <- sum(!is.na(d$pred))
      n <- sum(is.na(d$pred))
      results <- c("Num predicted" = N, "Num pred Failed" = n)
      print(results, digits = 1)
    } else {
      ds <- defaultSummary(data = d)
      RMSE <- ds[["RMSE"]]
      R2 <- ds[["Rsquared"]]
      MAE <- ds[["MAE"]]
      N <- sum(!is.na(d$pred))
      n <- sum(is.na(d$pred))
      upperlim <- ds[["RMSE"]] * sqrt(N / qchisq(0.05,N))
      nMod <- react$ModelSet[["null"]]
      if (!is.null(nMod) && !inherits(nMod, "character")) {
        predictions <- predict(nMod, newdata = test[, !isY])
        d <- data.frame(test[, isY], predictions)
        colnames(d) <- c("obs", "pred")
        ns <- defaultSummary(data = d, lev = lev)
        RelRMSE <- ds[["RMSE"]] / ns[["RMSE"]]
        RelMAE <- ds[["MAE"]] / ns[["MAE"]]
        results <- c("RMSE" = RMSE, "Rsquared" = R2, "Mean Absol Err" = MAE, "95% conf. RMSE below" = upperlim)
        print(results, digits = 6)
        results <- c("RMSE %" = RelRMSE*100, "MAE %" = RelMAE*100)
        print(results, digits = 3)
      } else {
        results <- c("RMSE" = RMSE, "Rsquared" = R2, "Mean Absol Err" = MAE, "95% confident RMSE below" = upperlim)
        print(results, digits = 6)
      }
      results <- c("Num predicted" = N, "Num pred Failed" = n)
      print(results, digits = 1)
    }
  })

  ###########################################################################
  output$SelectedModelSummary <- renderPrint({
    mod <- react$ModelSet[[input$SelectedModel]]
    req(mod)
    req(!inherits(mod, "character"))
    try({
      print(mod$finalModel)
    }, silent = TRUE)
  })


})
