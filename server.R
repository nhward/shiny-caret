# Shiny Caret is an interactive interface for using various machine learning methods from the caret package
# (c) Nick Ward (University of Canterbury) 2018-2020 ----

# Server Code ------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # input ServerFile ------------------------------------------------------
  shinyFiles::shinyFileChoose(input, 'ServerFile', session = session, roots = roots, defaultRoot = "wd")
  
  # reactive  values ------------------------------------------------------
  react <- reactiveValues(
    ModelSet = list(),
    Recipe = NULL,
    RecipeChanged = 0,
    Log = c(),
    Warn = c(),
    AllowEnsemble = FALSE,
    Prob2Class = FALSE,
    MethodSet = c(),
    Help = TRUE,
    HighCardinality = c(),
    ROC = NULL,
    File = NULL
  )

  # on session ended ------------------------------------------------------
  onSessionEnded(fun = function() {
    print("Session ended")
    # rm(list = ls(all.names = TRUE))
    # gc()
  })

  # reactive function getProjects ----------------------------------------------
  getProjects <- reactive({
    files <- file.info(list.files(PROJECT_FOLDER, full.names = TRUE), extra_cols = FALSE)
    key <- !files$isdir & str_ends(rownames(files), ".RDA")
    proj <- rownames(files)[key]
    proj <- sub(pattern = paste0("^", PROJECT_FOLDER, .Platform$file.sep), replacement = "", x = proj)
    sub(pattern = "\\.RDA$", replacement = "", x = proj)
  })
  
  
  # observe event Save ------------------------------------------------------
  observeEvent(input$Save, {
    req(input$Project != "")
    print("Saving state")
    if (!dir.exists(PROJECT_FOLDER)) {
      dir.create(PROJECT_FOLDER)
    }
    FileName <- paste0(PROJECT_FOLDER, .Platform$file.sep, input$Project, ".RDA")
    INPUT <- shiny::reactiveValuesToList(input)
    REACT <- shiny::reactiveValuesToList(react)
    save(list = c("INPUT", "REACT"), file = FileName)
  })
  
  # Reactive Poll loadProject ----------------------------------------------------
  loadProject <- reactive({
      input$Save
      f <- paste0(PROJECT_FOLDER, .Platform$file.sep, isolate(input$Project), ".RDA")
      cat("Project file", f, "read\n")
      load(f, envir = .GlobalEnv)
    }
  )

  # observe event LoadProject ------------------------------------------------------ 
  observe({
    input$LoadProjectReq
    req(isolate(input$Project != ""))
    shinyjs::disable(id = "LoadProjectReq")
    loadProject()
    changed <- 0
    for (id in names(INPUT)) {
      VAR <- isolate(input[[id]])
      if (!equals(VAR, INPUT[[id]])) {
        # Avoid processing buttons of all kinds (Action & bsButton)
        if (id %in% c("help", "Save", "Next", "LoadProjectReq", "Evaluate", "Restart", 
                      "RefreshModels", "Suggest", "ChooseAll", "Install", "Reset", "Train", 
                      "Pause", "UndoModel", "AddModel", "GradTrain")) {
          
          # Avoid processing file inputs of any kind 
        } else if (id %in% c("LocalFile", "ServerFile")) {
          # do nothing
          # Avoid processing the non-tab menus
        } else if (id %in% c("Submenu", "Submenu_state", "sidebarItemExpanded")) {
          # Special processing for Plotly events
        } else if (grepl(pattern = "^plotly_.*", x = id)) {
          # do nothing
          # Special processing for DT::Table events
        } else if (grepl(pattern = ".*_(rows_|cells_|search|row_last_).*", x = id)) {
          table <- gsub(pattern = "_(rows_|cells_|search).*", replacement = "", x = id)
          if (stringr::str_ends(string = id, pattern = "_rows_selected")) {
            DTproxy <- DT::dataTableProxy(table)
            print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
            DT::selectRows(proxy = DTproxy, selected = INPUT[[id]], ignore.selectable = TRUE)
            #changed <- changed + 1  # As this does not reflect in Table_rows_selected it must be trusted to have occurred
          } else if (stringr::str_ends(string = id, pattern = "_search")) {
            DTproxy <- DT::dataTableProxy(table)
            print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
            DT::updateSearch(proxy = DTproxy, keywords = list(global = INPUT[[id]], columns = NULL))
            #changed <- changed + 1  # As this does not reflect in Table_search it must be trusted to have occurred
          }
        } else if (is.numeric(VAR) | is.numeric(INPUT[[id]])) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateSliderInput(session = session, inputId = id, value = INPUT[[id]])
          changed <- changed + 1
        } else if (is.logical(VAR) | is.logical(INPUT[[id]])) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateCheckboxInput(session = session, inputId = id, value = INPUT[[id]])
          changed <- changed + 1
        } else if (id %in% c("Sep", "Quote", "Decimal", "ProbType", "PredictorsTag", "ResultsFor", "SelectedModel")) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateRadioButtons(session = session, inputId = id, selected = INPUT[[id]])
          changed <- changed + 1
        } else if (id %in% c("URL", "DateFormat")) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateTextInput(session = session, inputId = id, value = INPUT[[id]])
          changed <- changed + 1
        } else if (id %in% c("DataSource", "Navbar", "TrainTab")) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateTabItems(session = session, inputId = id, selected = INPUT[[id]])
          changed <- changed + 1
        } else if (id %in% c()) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateTabItems(session = session, inputId = id, selected = INPUT[[id]])
          changed <- changed + 1
        } else if (is.character(VAR) | is.character(INPUT[[id]])) {
          print(paste("Updating", id, "from", VAR, "to", INPUT[[id]]))
          updateSelectizeInput(session = session, inputId = id, selected = INPUT[[id]])  #Assume no selectInput()
          changed <- changed + 1
        }
      }
    }
    
    for (id in names(REACT)) {
      VAR <- isolate(react[[id]])
      if (!equals(VAR, REACT[[id]])) {
        print(paste0("Updating react$", id))
        react[[id]] <- REACT[[id]]
      }
    }
    
    # ResultsFor update is masked behind a button
    if (length(react$ModelSet) > 0) {
      updateRadioButtons(session = session, inputId = "ResultsFor", choices = names(react$ModelSet), selected = INPUT$ResultsFor)
    }
    
    if (changed > 0) {
      print(paste("Changed =", changed))
      invalidateLater(millis = 2000, session = session)
    } else {
      showNotification(ui = "Project-load completed", duration = 5, type = "message")
      shinyjs::enable(id = "LoadProjectReq")
    }
  }, priority = -10)
  
  # observe event Package ------------------------------------------------------
  observeEvent(input$Package, {
    results <- data(package = input$Package)$results
    if (nrow(results) == 0 || ncol(results) != 4) {
      updateSelectizeInput(session = session, inputId = "DataSet", choices = "", selected = "")
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
        updateSelectizeInput(session = session, inputId = "DataSet", choices = "")
      } else {
        updateSelectizeInput(session = session, inputId = "DataSet", choices = as.list(choices))
      }
    }
  })

  # reactive function LoadFileType ------------------------------------------------------
  LoadFileType <- reactive({
    d <- tryCatch({
      # when reading semicolon separated files, having a comma separator causes `read.csv` to error - hence the trycatch
      if (grepl("\\.csv$", react$File, ignore.case = TRUE)) {
        dd <- read.csv(react$File, header = input$Header, sep = input$Sep, quote = input$Quote, na.strings = input$MissingStrings, 
                       dec = input$Decimal, check.names = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
        dd
      } else if (grepl("\\.xls.{0,1}$", react$File, ignore.case = TRUE)) {
        dd <- as.data.frame(readxl::read_excel(react$File, sheet = 1, na = input$MissingStrings, col_names = input$Header))
        dd
      } else if (grepl("\\.rdata$", react$File, ignore.case = TRUE)) {
          # when reading semicolon separated files, having a comma separator causes `read.csv` to error - hence the trycatch
          loaded <- new.env()
          what <- load(react$File, envir = loaded)
          req(len(what) > 0)
          if (length(what) > 1) {
            #create alert to resolve VarName 
            choices <- what[base::apply(X = what, FUN = is.data.frame)]
            showElement(id = "VarName")
            #   updateSelectizeInput(session = session, inputId = "VarName", choices = choices)
            get(input$VarName, envir = loaded)
          } else {
            hideElement(id = "VarName")
            get(what, envir = loaded)
          }
      }
      
    },
    warn = function(e) {
      shinyalert(title = paste0("File \"",react$File,"\" did not load"), text = e$message, type = "warning")
      NULL},
    error = function(e) {
      shinyalert(title = paste0("File \"",react$File,"\" did not load"), text = e$message, type = "error")
      NULL
    })
    names(d) <- make.names(names(d),unique = TRUE)
    d
  })

  # reactive function loadR ------------------------------------------------------
  loadR <- reactive({
    d <- tryCatch({
      if (input$Package == "All") {
        data(list = input$DataSet)
        get(input$DataSet)
      } else {
        data(list = input$DataSet, package = input$Package)
        get(input$DataSet, asNamespace(input$Package))
      }
    },
    warn = function(e) {
      shinyalert(title = paste0("Dataset \"",input$DataSet,"\" did not load"), text = e$message, type = "warning")
      NULL},
    error = function(e) {
      shinyalert(title = paste0("Dataset \"",input$DataSet,"\" did not load"), text = e$message, type = "error")
      NULL
    })
  })
  
  # reactive Continuous ####------------------------------------------------------------
  Continuous <- reactive({
    input$Continuous
  })
  getContinuousDebounced <- debounce(Continuous, millis = 1000)
  
  observeEvent(input$ServerFile, {
    file <- parseFilePaths(roots, input$ServerFile)[1,"datapath", drop = TRUE]
    req(file)
    updateTextInput(session = session, inputId = "ServerFileName", value = as.character(file))
  })
  
  observeEvent(input$LocalFile, {
    file <- input$LocalFile[1,"datapath", drop = TRUE]
    req(file)
    updateTextInput(session = session, inputId = "LocalFileName", value = as.character(file))
  })
  
  observe({
    req(input$Project == "")
    updateSelectizeInput(session = session, inputId = "Project", choices = getProjects(), selected = input$Project)
  })
  
  # reactive function getRawData ------------------------------------------------------
  getRawData <- reactive({
    req(input$DataSource)
    if (input$DataSource == "Server Data File") {
      req(input$ServerFileName != "")
      react$File <- input$ServerFileName
      d <- LoadFileType()
      name <- gsub(pattern = ".*(/|\\\\)", replacement = "", x = input$ServerFileName)
      updateSelectizeInput(session = session, inputId = "Project", choices = c(name, isolate(getProjects())), selected = name)

    } else if (input$DataSource == "Local Data File") {
      req(input$LocalFileName != "")
      react$File <- input$LocalFileName
      d <- LoadFileType()
      name <- gsub(pattern = ".*(/|\\\\)", replacement = "", x = input$LocalFileName)
      updateSelectizeInput(session = session, inputId = "Project", choices = c(name, isolate(getProjects())), selected = name)

    } else if (input$DataSource == "Remote Data Resource") {
      req(input$URL)
      URL <- input$URL
      react$File <- URL
      updateSelectizeInput(session = session, inputId = "Project", choices = c(URL, isolate(getProjects())), selected = URL)
      d <- LoadFileType()
      
    } else if (input$DataSource == "Package Dataset") {
      req(input$Package != "", input$DataSet != "")
      name <- paste(sep = "::", input$Package, input$DataSet)
      updateSelectizeInput(session = session, inputId = "Project", choices = c(name, isolate(getProjects())), selected = name)
      d <- loadR()
    }
    req(d, nrow(d) > 0, ncol(d) > 0)
    
    showNotification(id = "checking", ui = "Checking column types", duration = NULL)
    # converts whole-number columns to "integer"
    numCols <- which(allClass(d) == "numeric")
    for (col in numCols) {
      if (all(is.wholenumber(d[,col]))) {
        showNotification(ui = paste("Changing numeric column", colnames(d)[col], "to integer"), duration = 3)
        d[,col] <- as.integer(d[,col])
      }
    }
    
    # converts 100% compatible character columns back to date variables
    charCols <- which(allClass(d) %in% c("character","factor"))
    for (col in charCols) {
      dates <- as.Date(d[,col], format = input$DateFormat)
      if (all(is.na(dates) == is.na(d[,col]))) {
        showNotification(ui = paste("Changing character column", colnames(d)[col], "to date"), duration = 3)
        d[,col] <- dates
      }
    }
    
    # converts <= 15 unique levels to factor
    charCols <- which(allClass(d) == "character")
    max <- nrow(d)
    for (col in charCols) {
      if (length(unique(d[,col])) <= getContinuousDebounced()[1]) {
        showNotification(ui = paste("Changing text column", colnames(d)[col], "to factor"), duration = 3)
        d[,col] <- as.factor(d[,col])
      }
    }
    
    updateTextInput(session = session, inputId = "Project", value = input$CSVFile$name)
    d
    removeNotification(id = "checking")
    
    choices <- formattedColNames(d)
    CHOICES <- toupper(choices)
    
   #set a default value for input$Target and appropriate choices
    best <- c( choices[CHOICES == "TARGET"],
               choices[CHOICES == "Y"],
               choices[CHOICES == "CLASS"],
               choices[CHOICES == "LABEL"],
               choices[CHOICES == "CHURN"],
               rev(choices) 
    )
    guess <- best[1]
    updateSelectizeInput(session = session, inputId = "Target", choices = as.list(choices), selected = as.list(guess))
    updateSelectizeInput(session = session, inputId = "Weights", selected = "")
    updateSelectizeInput(session = session, inputId = "PreSplit", selected = "")
    updateSelectizeInput(session = session, inputId = "ID", selected = list())
    updateSelectizeInput(session = session, inputId = "HideCol", selected = list())
    updateSelectizeInput(session = session, inputId = "Group", selected = NULL)
    updateCheckboxInput(session = session, inputId = "AddWeights", value = FALSE)
    updateSelectizeInput(session = session, inputId = "BalanceFactors", selected = NULL)
    d
  })
  
  # render ObservationCount ------------------------------------------------------
  output$ObservationCount <- renderInfoBox({
    d <- getRawData()
    infoBox(title = "Observations", value = nrow(d), icon = icon("align-justify"), color = DATAColour, fill = TRUE )
  })
  
  # render VariableCount ------------------------------------------------------
  output$VariableCount <- renderInfoBox({
    d <- getRawData()
    infoBox(title = "Variables", value = ncol(d), icon = icon("columns"), color = DATAColour, fill = TRUE)
  })
  
  # reactive GetData ------------------------------------------------------
  getData <- reactive({
    d <- getRawData()
    req(d)
    req(nrow(d) > 0, ncol(d) > 0, length(colnames(d)) > 0)

    # ensure target is factor for Classification
    if (isRoleValid(input$Target, d) && is.binary(d[, input$Target, drop = TRUE])) {
      tar <- d[, input$Target]
      d[, input$Target] <- as.factor(tar)
      showNotification(ui = "Converting binary outcome variable to nominal", type = "warning")
    }
    
    #clean up invalid factor level names
    for (col in 1:ncol(d)) {
      var <- d[,col, drop = TRUE]
      if (is(var, "factor")) {
        newNames <- make.names(levels(var))
        if (any(newNames != levels(var))) {
          levels(d[,col]) <- newNames
          #print(levels(d[,col]))
        }
      }
    }
    
    if (input$AddWeights && length(input$BalanceFactors) > 0) {
      d <- observationWeights(factors = input$BalanceFactors, data = d)
    }

    if (input$AddIds) {
      if (!any(is.na(as.numeric(rownames(d))))) { 
        d$RowName <- as.numeric(rownames(d))
      } else {
        d$RowName <- rownames(d)
      }
    }
    d
  })

  # render Data ------------------------------------------------------
  output$Data <- DT::renderDataTable({
    d <- getData()
    dt <- DT::datatable(data = d, rownames = TRUE, selection = "none",
                        extensions = c('Scroller','FixedHeader'),
                        options = list(
                          scrollX = TRUE,
                          deferRender = TRUE,
                          scrollY = 540,
                          scroller = TRUE
                        ))
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
  })


  # reactive function getSomeRawData ------------------------------------------------------
  getSomeRawData <- reactive({
    # grab a representative subsample of the data
    d <- getRawData()
    req(is(d,"data.frame"))
    mrow <- getMaxRowsDebounced()
    if (nrow(d) <= mrow) {
      return(d)
    } else {
      rows <- sample(nrow(d), mrow)
      #preserve order
      rows <- sort(rows, decreasing = FALSE)
      return(d[rows,])
    }
  })

  
  # reactive getMaxRows  ------------------------------------------------------
  MaxRows <- reactive({
     input$MaxRows  
  })
  getMaxRowsDebounced <- debounce(MaxRows, millis = 1000)
  
  # reactive getSomeData ------------------------------------------------------
  getSomeData <- reactive({
    # grab a representative subsample of the data
    d <- getData()
    req(is(d,"data.frame"))
    if (input$ID != "" & input$ID %in% colnames(d)) {
      rownames(d) <- d[, input$ID]
    }
    mrow <- getMaxRowsDebounced()
    if (nrow(d) <= mrow) {
      return(d)
    } else {
      rows <- sample(nrow(d), mrow)
      #preserve order
      rows <- sort(rows, decreasing = FALSE)
      return(d[rows,])
    }
  })
  
  # reactive getSomePredictorData ------------------------------------------------------
  # predictors plus outcome - actually
  getSomePredictorData <- reactive({
    d <- getSomeData()
    leaveOut <- getNonPredictors()
    d[, !colnames(d) %in% leaveOut]
  })

  # reactive getPredictorData ------------------------------------------------------
  # predictors plus outcome - actually
  getPredictorData <- reactive({
    d <- getData()
    leaveOut <- getNonPredictors()
    d[, !colnames(d) %in% leaveOut]
  })

  # reactive getRawDataSummary ------------------------------------------------------
  getRawDataSummary <- reactive({
    d <- getSomeRawData()
    DataSummary(d)
  })

  Multiplier <- reactive({
    input$Multiplier
  })
  getMultiplier <- debounce(Multiplier, millis = 1000)
  
  # reactive getDataSummary ------------------------------------------------------
  getDataSummary <- reactive({
    d <- getSomeData()
    DataSummary(d, getMultiplier(), input$UseYJ)
  })

  # observe ------------------------------------------------------
  observe({
    d <- getRawData()
    req(d)
    req(ncol(d) > 0)
    ds <- getRawDataSummary()
    choices <- colnames(d)
    names(choices) <- paste0(choices, " [", ds$type, "]")
    #set appropriate choices
    updateSelectizeInput(session = session, inputId = "HideCol", choices = as.list(choices))
  })

  # reactive getDataSummary ------------------------------------------------------
  getIDChoices <- reactive({
    ds <- getDataSummary()
    choicesID <- rownames(ds)
    names(choicesID) <- paste0(choicesID, " [",ds$type,"]")
    index <- ds$uniqueRatio == 1 & (ds$wholeNumb | ds$text | ds$date)
    if (any(index)) {
      choicesID <- choicesID[index]
    } else {
      choicesID <- c()
    }
    as.list(choicesID)
  })

  # observe event AddIds ------------------------------------------------------
  # observeEvent(
  #   input$AddIds,
  #   {
  #     #set a default value for input$ID and appropriate choices
  #     if (input$AddIds) {
  #       updateSelectizeInput(session = session, inputId = "ID", choices = c("RowName"), selected = "RowName")
  #       shinyjs::disable(id = "ID")
  #     } else {
  #       existID <- input$ID
  #       choicesID <- getIDChoices()
  #       if (!all(existID %in% choicesID)) {
  #         existID <- NULL
  #       }
  #       updateSelectizeInput(session = session, inputId = "ID", choices = choicesID, selected = existID)
  #       shinyjs::enable(id = "ID")
  #     }
  #   }
  # )

  # observe ------------------------------------------------------
  observe({
      ds <- getDataSummary()
      req(ds)
      choices <- as.list(rownames(ds))
      names(choices) <- paste0(choices, " [",ds$type,"]")
      imbalanced <- choices[ds$imbalanceRatio > 1.1]
      imbalanced[is.na(imbalanced)] <- FALSE
      if (input$AddWeights && length(input$BalanceFactors) > 0) {
        updateSelectizeInput(session = session, inputId = "Weights", choices = list("Weighting"), selected = "Weighting")
        shinyjs::disable(id = "Weights")
      } else {
        wChoices <- choices[ds$numeric & !ds$binary]
        wChoices[["none"]] <- ""
        updateSelectizeInput(session = session, inputId = "Weights", choices = wChoices, selected = isolate(input$Weights))
        shinyjs::enable(id = "Weights")
      }
      if (input$PreSplit != "") {
        updateSelectizeInput(session = session, inputId = "Groups", choices = list("none" = ""), selected =  NULL)
        shinyjs::disable(id = "Groups")
      } else {
        gChoices <- choices[(ds$factor | ds$wholeNum) & ds$uniqueness > 5]
        gChoices[["none"]] <- ""
        updateSelectizeInput(session = session, inputId = "Groups", choices = gChoices, selected = isolate(input$Groups))
        shinyjs::enable(id = "Groups")
      }
      lowCard <- setdiff(choices[ds$uniqueness <= input$Continuous[1] & ds$uniqueness > 1], c(input$PreSplit, input$Groups))
      updateSelectizeInput(session = session, inputId = "BalanceFactors", choices = lowCard, selected = isolate(input$BalanceFactors))
      shinyjs::toggle(id = "BalanceFactors", condition = input$AddWeights)

      pChoices <- choices[ds$binary | (ds$factor & ds$uniqueness == 2)]
      pChoices[["none"]] <- ""
      updateSelectizeInput(session = session, inputId = "PreSplit", choices = pChoices, selected = isolate(input$PreSplit))
      ids <- getIDChoices()
      if (length(ids) == 0) {
        # updateCheckboxInput(session = session, inputId = "AddIds", value = TRUE)  #triggers before length(ids) has settled down
      } else if (length(ids) == 1) {
        updateSelectizeInput(session = session, inputId = "ID", choices = ids, selected = ids[1])
      } else {
        updateSelectizeInput(session = session, inputId = "ID", choices = ids, selected = isolate(input$ID), )
      }
  })

  # observe event Target ------------------------------------------------------
  observeEvent(input$Target, {
    ds <- getDataSummary()
    ds <- ds[rownames(ds) == input$Target, ]

    if (nrow(ds) == 0) {
      ptype <- NA
    } else if (ds$binary | ds$factor | (ds$wholeNumb & ds$uniqueness < getContinuousDebounced()[1]) ) {
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

  # render plot ClassPieChart ------------------------------------------------------
  output$ClassPieChart <- renderPlot({
    d <- getSomeData()
    req(isRoleValid(input$Target, d))
    target <- d[,input$Target, drop = TRUE]
    req(is.factor(target) || is.binary(target))
    
    if(isRoleValid(input$Weights, d)) {
      df <- data.frame(var = target, wt = d[,input$Weights])
      t <- df %>% dplyr::count(var, wt = wt)
    } else {
      df <- data.frame(var = target)
      t <- df %>% dplyr::count(var)
    }
    colnames(t)[1] <- "Level"
    ggplot(data = t, mapping = aes(x = 0, y = n, fill = Level)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = NULL, x = NULL, y = NULL) +
      theme(axis.text.x = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks.length = unit(0, "mm"),
            plot.margin = unit(c(0,0,0,0), "mm"),
            plot.background = element_blank(),
            legend.spacing = unit(c(0,0,0,0), "mm"),
            axis.title = element_blank()
      )
  }, bg = "transparent")

  # render plot GroupPieChart ------------------------------------------------------
  output$GroupPieChart <- renderPlot({
    req(input$Groups != "")
    d <- getSomeData()
    column <- d[,input$Groups, drop = TRUE]

    Level <- as.factor(column)
    t <- table(Level, useNA = "no")
    df <- as.data.frame(t)
    ggplot(data = df, mapping = aes(x = 0, y = Freq, fill = Level)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = NULL, x = NULL, y = NULL) +
      theme(axis.text.x = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks.length = unit(0, "mm"),
            plot.margin = unit(c(0,0,0,0), "mm"),
            plot.background = element_blank(),
            legend.spacing = unit(c(0,0,0,0), "mm"),
            axis.title = element_blank()
      )
  }, bg = "transparent")

  # render plot SplitPieChart ------------------------------------------------------
  output$SplitPieChart <- renderPlot({
    d <- getSomeData()
    req(isRoleValid(input$PreSplit, d))
    df <- data.frame(var = d[,input$PreSplit])
    t <- df %>% dplyr::count(var)
    colnames(t)[1] <- "Level"
    ggplot(data = t, mapping = aes(x = 0, y = n, fill = Level)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = NULL, x = NULL, y = NULL) +
      theme(axis.text.x = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks.length = unit(0, "mm"),
            plot.margin = unit(c(0,0,0,0), "mm"),
            plot.background = element_blank(),
            legend.spacing = unit(c(0,0,0,0), "mm"),
            axis.title = element_blank()
      )
  }, bg = "transparent")

  # reactive getNonPredictors ------------------------------------------------------
  getNonPredictors <- reactive({
    ignore <- c(input$ID, input$Weights, input$PreSplit, input$HideCol, input$Groups)
    ignore[ignore != ""]
  })

  # observe ------------------------------------------------------
  observe({
    req(input$Navbar == "DataColumns")
    d <- getSomeData()
    req(d)
    req(isRoleValid(input$Target, d))
    col <- d[,input$Target, drop = TRUE]
    type <- class(col)
    if (isRoleValid(input$Weights, d)) {
      Xweighted <- "weighted "
    } else {
      Xweighted <- ""
    }
    uniqCnt <- length(unique(na.omit(col)))
    if (uniqCnt == 1) {
       text <- paste("Error: The outcome has constant data.\nIt is not a legitimate outcome variable.")
    } else if ("Date" %in% type) {
      text <- paste("Warning: This outcome is a date/time variablea.\nIt is not a legitimate outcome variable.")
    } else if (is.numeric(col)) {
      isInt <- is.wholenumber(col)
      if (isInt & uniqCnt < input$Continuous[1]) {
        text <- paste("Warning: There are", uniqCnt, "unique integer outcome values.\nTo make a multi-class classification problem you should explicitly change the outcome to a factor")
      } else if (!isInt & uniqCnt < input$Continuous[2]) {
        text <- paste("Warning: The outcome has only", uniqCnt, "unique numeric values.\nIt is a dubious outcome variable for regression")
      } else {
        text <- paste0("This is a ", Xweighted ," regression problem.")
      }
    } else if ("character" %in% type) {
        text <- paste("Warning: The outcome is textual data and is not a legitimate outcome variable")
    } else if ("factor" %in% type) {
      if (uniqCnt == 2 && nrow(d) > 2) {
        text <- paste0("This is a ", Xweighted ,"binary classification problem.")
      } else if (uniqCnt > input$Continuous[2]) {
        text <- paste("Error: The outcome has", uniqCnt, "factor levels.\nIt is not a legitimate outcome variable for classification.")
      } else if (uniqCnt > input$Continuous[1]) {
        text <- paste("Warning: The outcome has", uniqCnt, "factor levels.\nIt is a dubious outcome variable for classification.")
      } else {
        text <- paste0("This is a ", Xweighted ,"multinomial classification problem with ",uniqCnt," levels.")
      }
    } else {
      text <- paste0("Error: The outcome type is '", type,"'.\nIt is not a legitimate outcome variable.")
    }
    targetMessage <- text
    output$TargetCheck <- renderInfoBox({
      infoBox(title = "Outcome", value = targetMessage, icon = icon("bullseye"), color = DATAColour, fill = TRUE)
    })

    predCount <- length(setdiff(colnames(d), c(input$Target, getNonPredictors())))
    output$PredictorCount <- renderInfoBox({
      infoBox(title = "Predictors", value = predCount, icon = icon("columns"), color = DATAColour, fill = TRUE)
    })
    ds <- getDataSummary()
    ds <- ds[!rownames(ds) %in% getNonPredictors(),]
    cont <- getContinuousDebounced()
    possCard <- sum(ds$uniqueness > cont[1] & ds$uniqueness <= cont[2] & (ds$text | ds$factor))
    highCard <- sum(ds$uniqueness > cont[2] & (ds$text | ds$factor))
    output$StringCount <- renderInfoBox({
      infoBox(title = "High Cardinality", value = paste0("Certain ", highCard, ",  Possible ", possCard), icon = icon("layer-group"), color = DATAColour, fill = TRUE)
    })

    countnzv <- sum(ds$nzv & !rownames(ds) %in% getNonPredictors())
    output$NZVCount <- renderInfoBox({
      infoBox(title = "Near-Zero Var.", value = countnzv, icon = icon("adjust"), color = DATAColour, fill = TRUE)
    })

    ds <- ds[(ds$numeric | ds$date),]
    possCont <- sum(ds$uniqueness > cont[1] & ds$uniqueness <= cont[2])
    certainCont <- sum(ds$uniqueness > cont[2])
    output$ContCount <- renderInfoBox({
      infoBox(title = "Continuous", value = paste0("Certain ", certainCont, ",  Possible ", possCont), icon = icon("sort-amount-down"), color = DATAColour, fill = TRUE)
    })
  })

  ###########################################################################
  output$TargetCheck <- renderInfoBox({
    d <- getSomeData()
    req(isRoleValid(input$Target, d))

    col <- d[,input$Target, drop = TRUE]
    type <- class(col)
    uniqCnt <- length(unique(na.omit(col)))

    if (is.numeric(col)) {
      isInt <- is.wholenumber(col)
      if (isInt & uniqCnt == 2 & nrow(d) > uniqCnt) {
        text <- paste("Since there are only", uniqCnt, "unique integer Y values, this is a binary classification problem.")
      } else if (isInt & uniqCnt < 11 & nrow(d) > uniqCnt) { #arbitrary 11 used here
        text <- paste("Warning: Since there are", uniqCnt, "unique integer Y values, this might be a multi-class classification problem.\nTo make this explicit you may choose to make this column a factor")
      } else if (uniqCnt == 1) {
        text <- paste("Error: This column has constant data and should be removed. It is not a legitimate target variable.")
      } else if (!isInt & uniqCnt/nrow(d) < 0.5) { #arbitrary 0.5 used here
        text <- paste("Warning: A column with only", paste0(ceiling(uniqCnt/nrow(d)*100),"%"), "unique numeric values is a suspicious outcome variable for regression")
      } else {
        text <- paste("This is a regression problem.")
      }
    } else if ("character" %in% type) {
      if (uniqCnt == nrow(d)) {
        text <- paste("Warning: This column effectively has text data and should be encoded or used as a row identifier")
      } else {
        text <- paste("Warning: This column has text data and should be removed or used as a partial row identifier")
      }
    } else if ("factor" %in% type) {
      if (uniqCnt == 2 & nrow(d) > uniqCnt) {
        text <- paste("This is a binary classification problem.")
      } else if (uniqCnt == nrow(d) ) {
        text <- paste("Error: This column effectively has unique text in each row and should be removed or used as a row identifier. \nIt is not a legitimate target variable.")
      } else if (uniqCnt/nrow(d) > 0.3) { #arbitrary 0.3 used here
        text <- paste("Warning: There are", uniqCnt, "unique Y levels, that is", paste0(ceiling(uniqCnt/nrow(d)*100),"%"),"of the rows. \nThis is a suspicious outcome variable for classification")
      } else if (uniqCnt == 1) {
        text <- paste("Error: This column has constant data and should be removed. It is not a legitimate target variable.")
      } else {
        text <- paste0("This is a multinomial (",uniqCnt," levels) classification problem.")
      }
    } else if ("Date" %in% type) {
      text <- paste("Warning: This column has date/time data and should be removed or used as a row identifier. \nIt is not a legitimate outcome variable.")
    } else {
      text <- paste0("Error: The type is '", type,"'. It is not a legitimate outcome variable.")
    }
    infoBox(title = "Outcome", value = text, icon = icon("bullseye"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$RatioObs <- renderInfoBox({
    d <- getSomePredictorData()
    count <- length(colnames(d)) - ifelse(isRoleValid(input$Target, d), 1, 0)
    rows <- nrow(getData())
    text <- paste0("Observations ", rows,", Predictors ",count, ", Ratio of Obs/Pred ", round(rows/count,1))
    infoBox(title = "Ratio", value = text, icon = icon("balance-scale-right"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$StringCount <- renderInfoBox({
    ds <- getDataSummary()
    ds <- ds[!rownames(ds) %in% getNonPredictors(),]
    cont <- getContinuousDebounced()
    poss <- sum(ds$uniqueness > cont[1] & ds$uniqueness <= cont[2] & (ds$text | ds$factor))
    high <- sum(ds$uniqueness > cont[2] & (ds$text | ds$factor))
    infoBox(title = "High Cardinality", value = paste0("Certain ", high, ",  Possible ", poss), icon = icon("layer-group"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$NZVCount <- renderInfoBox({
    ds <- getDataSummary()
    count <- sum(ds$nzv & !rownames(ds) %in% getNonPredictors())
    infoBox(title = "Near-Zero Var.", value = count, icon = icon("adjust"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$ContCount <- renderInfoBox({
    ds <- getDataSummary()
    ds <- ds[!rownames(ds) %in% getNonPredictors() & (ds$numeric | ds$date),]
    cont <- getContinuousDebounced()
    poss <- sum(ds$uniqueness > cont[1] & ds$uniqueness <= cont[2])
    certain <- sum(ds$uniqueness > cont[2])
    infoBox(title = "Continuous", value = paste0("Certain ", certain, ",  Possible ", poss), icon = icon("sort-amount-down"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$MissObs <- renderInfoBox({
    d <- getSomePredictorData() # avoid the hidden vars
    req(d)
    m <- sum(base::apply(X = d, MARGIN = 1, FUN = function(x) any(is.na(x))))
    some <- round(m / dim(d)[1] * 100, 1)
    rows <- base::apply(X = d, MARGIN = 1, FUN = function(x) sum(is.na(x))) / ncol(d)
    cnt <- sum(rows > input$MissObsThreshold / 100)
    heavy <- round(cnt / nrow(d) * 100, 1)
    infoBox(title = "Missing Observations", value = paste0("Heavy ", heavy, "%,  Some ", some, "%"), icon = icon("align-justify"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$MissVar <- renderInfoBox({
    d <- getSomePredictorData()
    req(d)
    some <- sum(base::apply(X = d, MARGIN = 2, FUN = function(x) any(is.na(x))))
    ds <- getDataSummary()
    cols <- ds$missingRate > input$MissVarThreshold / 100
    heavy <- sum(cols)
    infoBox(title = "Missing Predictor Variables", value = paste0("Heavy ", heavy, ",  Some ", some), icon = icon("columns"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$MissTarget <- renderInfoBox({
    d <- getSomeData()
    req(d)
    req(isRoleValid(input$Target, d))
    some <- round(sum(is.na(d[,input$Target])) / nrow(d) * 100, 1)
    infoBox(title = "Missing Outcome Variable", value = paste0(some,"%"), icon = icon("exclamation-triangle"), color = DATAColour, fill = TRUE)
  })

  ###########################################################################
  output$DataSummary <- renderUI({
    Data <- getSomeData()
    summary <- summarytools::dfSummary(Data, graph.magnif = 1.5)
    summarytools::view(summary, 
                       method = 'render',
                       report.title = NA,
                       headings = FALSE,
                       bootstrap.css = FALSE,
                       footnote = NA,
                       max.tbl.height = 600,
                       collapse = TRUE,
                       silent = TRUE
    )
  })

  ###########################################################################
  output$MissingChart1 <- renderPlotly({
    data <- getSomeData()
    req(ncol(data) > 0)
    sort = input$SortOrder
    if (length(sort) > 0 && all(sort %in% colnames(data))) {
      data <- data[order(data[,sort]), ]
      ylab = paste("Observations in", paste0(sort, collapse = ","), "order")
    } else {
      ylab = "Observations in natural order"
    }
    if (input$HideIrrelevant) {
      colCounts <- base::colSums(is.na(data))
      if (any(colCounts > 0)) {
        data <- data[, colCounts > 0, drop = FALSE]
      }
    }
    plot <- visdat::vis_miss(data, cluster = FALSE, sort_miss = FALSE, show_perc = TRUE) +
      labs(x = "Variables", title = "Missing values throughout the data", y = ylab) +
      coord_flip()
    plotly(plot, tooltip = c("variable", "value"))
  })
  
  ###########################################################################
  output$MissingChart2 <- renderPlot({
    d <- getSomeData()
    req(ncol(d) > 0)
    missingCounts(d)
  }, bg = "transparent")

  ###########################################################################
  output$MissObservations <- renderText({
    d <- getData()
    # remove unwanted columns
    m <- sum(base::apply(X = d, MARGIN = 1, FUN = function(x) any(is.na(x))))
    paste0(m, " (", round(m / dim(d)[1] * 100), "%)")
  })

  ###########################################################################
  output$MissVariables <- renderText({
    d <- getData()
    # remove unwanted columns
    m <- sum(base::apply(X = d, MARGIN = 2, FUN = function(x) any(is.na(x))))
    paste0(m, " (", round(m / dim(d)[2] * 100), "%)")
  })

  
  ###########################################################################
  output$MissValues <- renderText({
    d <- getData()
    # remove unwanted columns
    m <- sum(is.na(d))
    paste0(m, " (", round(m / dim(d)[1] / dim(d)[2] * 100), "%)")
  })

  # reactive function getMissObsRatios ####
  getMissObsRatios <- reactive({
    d <- getData()
    base::apply(X = d, MARGIN = 1, FUN = function(x) sum(is.na(x))) / ncol(d)
  })
  
  ###########################################################################
  output$HeavyMissObs <- renderText({
    cnt <- sum(getMissObsRatios() > input$MissObsThreshold / 100)
    prop <- cnt / nrow(getData()) * 100
    paste0(cnt, " (", round(prop,1), "%)")
  })

  ###########################################################################
  output$HeavyMissVar <- renderText({
    d <- getData()
    ds <- getDataSummary()
    cols <- ds$missingRate > input$MissVarThreshold / 100
    cnt <- sum(cols)
    prop <- cnt / ncol(d) * 100
    paste0(cnt, " (",round(prop,1), "%)")
  })

  ###########################################################################
  getTrainedModels <- reactive({
    req(length(react$ModelSet) >= 1)
    bad <- unlist(lapply(react$ModelSet, FUN = inherits, what = c("character")))
    req(sum(!bad) > 0)
    react$ModelSet[!bad]
  })
  
  ###########################################################################
  getTimings <- reactive({
    d <- getTrainData()
    thousand <- sample(nrow(d), size = 1000, replace = TRUE)
    d <- d[thousand, ]
    timing <- c()
    for (mod in getTrainedModels()) {
      t <- system.time({
        try({
          predict(mod, newdata = d)
        }, silent = TRUE)
      })
      timing <- c(timing, t[1])
    }
    timing
  })
  
  ###########################################################################
  getResampledModels <- reactive({
    mods <- getTrainedModels()
    req(mods)
    for (i in seq(mods)) {
      if (is(mods[[i]], "caretStack")) {
        mods[[i]] <- mods[[i]]$ens_model
      }
    }
    res <- try({
      resamples(mods)
    }, silent = FALSE)
    req(is(res, "resamples"))

    if (!input$HideTimings) {
      #Add timings to the metrics
      timings <- getTimings()
      df <- res$values
      origNames <- colnames(df)
      res$metrics[length(res$metrics) + 1] <- "logTiming"
      for (m in 1:length(res$models)) {
        timing <- rep(log10(timings[m]), times = nrow(df))
        df <- cbind(df, timing)
      }
      colnames(df) <- c(origNames, paste(sep = "~", res$models, "logTiming"))
      res$values <- df
    }
    res
  })

  ###########################################################################
  observe({
    if (ncol(getRawData()) > 15) {
      updateCheckboxInput(session = session, inputId = "ShowLegend", value = FALSE)
    }
  })

  ###########################################################################
  getSomeDefinitelyContinuousData <- reactive({
    d <- getSomeData()
    ds <- getDataSummary()
    # need to remove special roles and restrict to numeric
    d <- d[, ds$uniqueness >= (getContinuousDebounced()[2]) & ds$numeric, drop = FALSE]
    d[, setdiff(colnames(d), getNonPredictors()), drop = FALSE]
  })

  ###########################################################################
  getSomePossiblyContinuousData <- reactive({
    d <- getSomeData()
    ds <- getDataSummary()
    # need to remove special roles and restrict to numeric
    d <- d[, ds$uniqueness >= (getContinuousDebounced()[1]) & ds$numeric, drop = FALSE]
    d[, setdiff(colnames(d), getNonPredictors()), drop = FALSE]
  })

  ###########################################################################
  getSomeDefinitelyDiscontData <- reactive({
    d <- getSomeData()
    ds <- getDataSummary()
    # need to remove special roles and restrict to numeric
    d <- d[, ds$uniqueness <= (getContinuousDebounced()[1]), drop = FALSE]
    d[, setdiff(colnames(d), getNonPredictors()), drop = FALSE]
  })

  ###########################################################################
  getSomePossiblyDiscontData <- reactive({
    d <- getSomeData()
    ds <- getDataSummary()
    # need to remove special roles
    d <- d[, ds$uniqueness <= (getContinuousDebounced()[2]), drop = FALSE]
    d[, setdiff(colnames(d), getNonPredictors()), drop = FALSE]
  })

  ###########################################################################
  output$RisingChart <- renderPlotly({
    d <- getSomePossiblyContinuousData()
    req(ncol(d) > 0)  # exit if no numeric columns
    ds <- getDataSummary()
    # the for-loop below can be rewritten as an Xapply() type of operation (feel free to do this if it bothers you)
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col]
    }
    if (input$ScaleChart) {
      d <- scale(x = d, center = TRUE, scale = TRUE)
    }
    d <- as.data.frame(d)
    d$DummyID <- 1:nrow(d)

    melted <- reshape2::melt(d, id = "DummyID")
    p <- ggplot(melted, aes_string(x = "DummyID", y = "value", colour = "variable")) +
      geom_line() +
      labs(title = "Rising Value Chart - Looking for discontinuities", x = "Observations in ascending value order",
           subtitle = paste("Numeric variables with number of unique values above", getContinuousDebounced()[1]))
    plotly(p, tooltip = c("variable", "value"))
  })

  ###########################################################################
  observe({
    ds <- getDataSummary()
    req(ncol(ds) > 0)
    choices <- rownames(ds)
    names(choices) <- paste0(choices, " [",ds$type,"]")
    choices <- choices[ds$date | ds$text]
    if (length(input$ID) > 0) {
      choices <- union(input$ID, choices)
    }
    updateSelectizeInput(session = session, inputId = "SortOrder", choices = as.list(choices), selected = input$ID)
  })

  # reactive Cols #### ------------------------------------------------------
  Cols <- reactive({
    input$Cols
  })
  getColsDebounced <- debounce(Cols, millis = 1000)
  
  ###########################################################################
  getSequenceList <- reactive({
    d <- getSomeData()
    if (is.null(input$SortOrder) | input$SortOrder == "") {
      showNotification(ui = "No sort order defined. Ensure an ID column is available", type = "warning")
      req(FALSE)
    }
    req(all(input$SortOrder %in% colnames(d)))
    #work-around for bug in tabplot which prohibits date columns as sortCol parameters
    for (col in input$SortOrder) {
      if (is(d[, col, drop = TRUE], "Date")) {
        d[,col] <- as.character(d[, col, drop = TRUE])
      }
    }
    tabplot::tableplot(d, sortCol = input$SortOrder, decreasing = FALSE, nCols = getColsDebounced(), plot = FALSE)
  })

  ###########################################################################
  output$SequenceChart1 <- renderPlot({
    plist <- getSequenceList()
    if (is(plist,"list")) {
      i <- 1
      req(length(plist) >= i)
      plot(plist[[i]])
    } else {
      plot(plist)
    }
  }, bg = "transparent")

  ###########################################################################
  output$SequenceChart2 <- renderPlot({
    plist <- getSequenceList()
    req(is(plist,"list"))
    i <- 2
    req(length(plist) >= i)
    plot(plist[[i]])
  }, bg = "transparent")

  ###########################################################################
  output$SequenceChart3 <- renderPlot({
    plist <- getSequenceList()
    req(is(plist,"list"))
    i <- 3
    req(length(plist) >= i)
    plot(plist[[i]])
  }, bg = "transparent")

  ###########################################################################
  output$SequenceChart4 <- renderPlot({
    plist <- getSequenceList()
    req(is(plist,"list"))
    i <- 4
    req(length(plist) >= i)
    plot(plist[[i]])
  })

  ###########################################################################
  output$SequenceChart5 <- renderPlot({
    plist <- getSequenceList()
    req(is(plist,"list"))
    i <- 5
    req(length(plist) >= i)
    plot(plist[[i]])
  }, bg = "transparent")

  ###########################################################################
  getVarImp <- reactive({
    sd <- getSomeData()
    req(isRoleValid(input$Target, sd))
    if (isRoleValid(input$Weights, sd)) {
      w <- sd[,input$Weights]
    } else {
      w <- NULL
    }
    ds <- getDataSummary()
    predictors <- colnames(sd)[!colnames(sd) %in% getNonPredictors() & !ds$text]
    d <- sd[, predictors]
    showNotification(id = "varimp", ui = "Calculating variable importance using Random Forest", duration = NULL)
    form <- paste0(input$Target, "~.")
    parallelMode <- isolate(input$Parallel)
    obj <- NA
    if (parallelMode) {
      obj <- startParallel("varimp")
    }
    rfx <- try({
      trc <- getTrainControl()
      trc$index <- NULL
      trc$method <- "cv"  # for performance use oob which is specific to rf
      trc$number = 10
      caret::train(form = as.formula(form), data = d, method = "rf", trControl = trc, weights = w, na.action = na.exclude)
    }, silent = TRUE)

    if (parallelMode) {
      response <- stopParallel(obj)
    }
    removeNotification(id = "varimp")
    if (is(rfx, "try-error")) {
      response <- rfx[[1]]
    }
    if (is(rfx, "try-error") | is.null(rfx)) {
      shinyalert::shinyalert(title = "Variable Importance", text = response)
      return(NULL)
    }
    vi <- varImp(rfx)$importance
    vi2 <- vi[, ncol(vi), drop = TRUE]  # use the last column if there is more than 1  - review this
    names(vi2) <- rownames(vi)
    sort(vi2, decreasing = TRUE)
  })

  ###########################################################################
  getVarUniOutliers <- reactive({
    d <- getSomePredictorData()
    nd <- ncol(d)
    outliers <-  rep(0, times = nd)

    rec <- recipes::recipe(~., data = d)
    if (input$UseYJ) {
      rec <- recipes::step_YeoJohnson(rec, all_numeric(), num_unique = 10)
    }
    if (input$ScaleChart) {
      rec <- recipes::step_center(rec, all_numeric()) %>%
        recipes::step_scale(all_numeric())
    }
    rec <- recipes::prep(rec, data = d, retain = TRUE)
    d <- recipes::juice(rec, composition = "data.frame")

    for (col in 1:nd) {
      column <- na.omit(d[, col, drop = TRUE])
      uniqueness <- length(unique(column))
      if (is(column, "numeric") & uniqueness >= getContinuousDebounced()[2]) {  # use definitely continuous
        stat <- boxplot.stats(x = column, coef = input$Multiplier, do.out = TRUE)
        outliers[col] <- length(stat$out)
      }
    }
    names(outliers) <- colnames(d)
    outliers
  })

  ###########################################################################
  output$NoveltyColumns <- renderPlotly({
    ds <- getDataSummary()
    leaveOut <- getNonPredictors()
    ds <- ds[!rownames(ds) %in% leaveOut,]
    cont <- getContinuousDebounced()
    contLev <- ifelse(ds$numeric, 0.4 * (pmax((cont[2] - ds$uniqueness), 0)), NA)
    factLev <- ifelse(ds$factor, 0.4 * pmax(ds$uniqueness - cont[1], 0), NA)
    NearZeroVar <- ifelse(ds$nzv, 0.2 * ds$notMissing, ifelse(ds$constant, 0.2 * ds$notMissing, NA))
    df1 <- data.frame(ContLevels  = input$ConsiderCont * contLev,
                      NominalLev  = input$ConsiderNom  * factLev,
                      Missingness = input$ObserveMiss  * ds$missing,
                      NearZeroVar = input$ConsiderNZV  * NearZeroVar,
                      Outliers    = input$ConsiderOut  * getVarUniOutliers(),
                      Variable    = rownames(ds),
                      row.names   = rownames(ds))
    vars <- c("ContLevels", "NominalLev", "Missingness", "NearZeroVar", "Outliers")
    df1 <- df1[,c("Variable", vars)]
    if (input$MergeVarImp && input$Target != "") {
      vi = getVarImp()
      if (!is.null(vi)) {
        vidf <- data.frame(Importance = vi, Variable = names(vi))
        df1 <- join(df1, vidf, by = "Variable")
        df1[,vars] <- df1[,vars] * df1$Importance
        df1$Importance <- NULL
      }
    }
    df <- tidyr::gather(data = df1, key = "Type", value = "Novelty", -Variable, na.rm = TRUE)
    df$Variable <- reorder(df$Variable, -df$Novelty, FUN = sum)
    p <- ggplot(data = df, aes(x = Variable, y = Novelty, fill = Type)) +
      geom_bar(position = "stack", stat = "Identity") +
      labs(title = "Novelty of variables", x = "Variable", y = "Novelty") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
    plotly(p)
  })

  ###########################################################################
  getObsUniOutliers <- reactive({
    data <- getSomeDefinitelyContinuousData()
    if (input$UseYJ) {
      mod <- caret::preProcess(data, method = c("YeoJohnson"))
      data <- predict(mod, data)
    }
    counts <- rep(0, nrow(data))
    for (col in 1:ncol(data)) {
      out <- IqrOutliers(x = data[,col], coef = getMultiplier())
      counts[out] <- counts[out] + 1
    }
    counts / ncol(data) * 10
  })

  ###########################################################################
  getMahalanobis2 <- reactive({
    data <- getSomePossiblyContinuousData()
    steps <- c("zv", "medianImpute")
    if (input$UseYJ) {
      steps <- c(steps, "YeoJohnson")
    }

    mod <- caret::preProcess(x = data, method = steps)
    data <- predict(mod, data)

    # if (input$UseDummy) {
    #   mod <- caret::dummyVars(formula = ~ ., data = data, fullRank = TRUE)
    #   data <- predict(mod, data)
    # } else {
    #   data <- data[, numericNames(data)]
    # }

    #since the steps below involve matrix inversion we must have full rank
    badCols  <- caret::findLinearCombos(x = data)$remove
    if (length(badCols) > 0) {
      data <- data[, -badCols]
    }

    rn <- rownames(data)
    nvar <- ncol(data)
    if (nvar == 0) {
      showNotification(ui = "There are no continuous numeric variables", type = "warning")
    }
    req(nvar > 0)
    if (!is.matrix(data)) {
      data <- as.matrix(data)
    }
    Sx <- cov(data, use = "pairwise")
    diag(Sx) <- diag(Sx) - 1e-10 * max(Sx) # add a ridge for computational stability
    MD2 <- stats::mahalanobis(x = data, center = colMeans(data, na.rm = TRUE), cov = Sx)
    names(MD2) <- rn
    list(MD2 = MD2, nvar = nvar)
  })


  ###########################################################################
  getLOF <- reactive({
    Rlof::lof(data = getSomePossiblyContinuousData(), method = input$DistanceMethod, k = input$Klof)
  })

  ###########################################################################
  output$NoveltyObservations <- renderPlotly({
    data <- getSomePredictorData()
    missing <- apply(data, MARGIN = 1, FUN = function(x) { sum(is.na(x)) })
    md2 <- getMahalanobis2()
    df1 <- data.frame(`Missingness` = input$ObserveMiss * 2 * missing / ncol(data),
                      `Multivar.Outlier` = input$ObserveMvOut * md2$MD2/qchisq(p = 0.95, df = md2$nvar),
                      `Univar.Outlier` = input$Observe1DOut * getObsUniOutliers(),
                      `Local.Outlier` = 1 * getLOF(),
                      `Observation` = rownames(data),
                      row.names = rownames(data))
    vars <- c("Missingness", "Multivar.Outlier", "Univar.Outlier","Local.Outlier")
    if (input$MergeWeighting && input$Weights %in% colnames(data)) {
      df1[, vars] <- df1[,vars] * data[, input$Weights]
    }
    novsum <- base::apply(X = df1[, vars], MARGIN = 1, FUN = sum)
    df1 <- df1[ order(novsum, decreasing = TRUE)[1:min(c(100, nrow(df1)))], ]
    d <- tidyr::gather(data = df1, key = "Type", value = "Novelty", -Observation, na.rm = TRUE)
    d$Observation <- reorder(d$Observation, -d$Novelty, FUN = sum)
    p <- ggplot(data = d, aes(x = Observation, y = Novelty, fill = Type)) +
      scale_x_discrete() +
      geom_bar(position = "stack", stat = "Identity") +
      labs(title = "Novelty of observations", x = "Observation", y = "Novelty") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
    plotly(p)
  })

  ###########################################################################
  output$OutlierPlots <- renderPlotly({
    req(input$ProbType != "Any")
    sd <- getSomeData()
    if (!isRoleValid(input$SortOrder, sd)) {
      order <- 1:nrow(sd)
      xLabel <- paste("Observations in natural order")
    } else {
      order <- sd[,input$SortOrder, drop = TRUE]
      xLabel <- paste("Observations in", input$SortOrder, "order")
    }
    mahal <- getMahalanobis2()
    data <- data.frame(md2 = mahal$MD2, order)
    if (input$ProbType == "Classification" && nlevels(sd[,input$Target]) > 1) {
      data$class <- sd[,input$Target]
    }
    if (input$ID != "") {
      data$ID <- sd[,input$ID, drop = TRUE]
    }
    data <- data[order(order), ]
    data$sequence <- 1:nrow(data)
    thresh <- qchisq(p = 0.999, df = mahal$nvar)
    plot1 <- ggplot(data = data, mapping = aes(y = md2, x = sequence)) +
      scale_y_continuous(limits = c(0, max(data$md2)*1.1)) +
      labs(y = "Mahalanobis distance squared", x = xLabel) +
      geom_abline(slope = 0, intercept = thresh, color = "red") 
    
    if (input$ProbType == "Classification" && nlevels(sd[,input$Target]) > 1) {
      plot1 <- plot1 +
        geom_point(mapping = aes(color = class)) +
        theme(legend.position = "left")
    } else {
      plot1 <- plot1 +
        geom_point() +
        theme(legend.position = "none")
    }

    dof <- mahal$nvar
    data <- data.frame(md2 = mahal$MD2)
    if (input$ID != "") {
      data$ID <- sd[ ,input$ID, drop = TRUE]
    }
    plot2 <- if (input$ProbType == "Classification" && nlevels(sd[,input$Target]) > 1) {
      data$class <- sd[,input$Target]
      data <- data[order(data$md2), , drop = FALSE]
      ggplot(data = data, mapping = aes(sample = md2, color = class)) +
        geom_qq(distribution = stats::qchisq, dparams = dof) +
        geom_qq_line(distribution = stats::qchisq, dparams = dof) +
        scale_y_continuous(limits = c(0, max(data$md2)*1.1)) +
        scale_x_continuous(limits = c(0, max(data$md2)*1.1)) +
        labs(x = "Quantiles of Chi squared", y = "Mahalanobis distance squared", title = "Outlier plot")
    } else {
      data <- data[order(data$md2), , drop = FALSE]
      ggplot(data = data, mapping = aes(sample = md2)) +
        geom_qq(distribution = stats::qchisq, dparams = dof) +
        geom_qq_line(distribution = stats::qchisq, dparams = dof) +
        scale_y_continuous(limits = c(0, max(data$md2)*1.1)) +
        scale_x_continuous(limits = c(0, max(data$md2)*1.1)) +
        labs(x = "Quantiles of Chi squared", y = "Mahalanobis distance squared", title = "Mahanobis Distance squared Outliers and Chi squared Quantile plot")
    }
    
    sideBySide(plots = list(plot1, plot2), tooltips = c("class", "md2", "sequence"), widths = c(9,3))
  })
  
  
  ###########################################################################
  output$RoleTypeChart <- plotly::renderPlotly({
    ds <- getDataSummary()
    Role <- vector(mode = "character", length  = nrow(ds))
    Role[rownames(ds) == input$Target] <- "Outcome"
    Role[rownames(ds) == input$ID] <- "ID"
    Role[rownames(ds) == input$PreSplit] <- "Partition"
    Role[rownames(ds) == input$Weights] <- "Weights"
    Role[rownames(ds) %in% input$Groups] <- "Groups"
    Role[rownames(ds) %in% input$HideCol] <- "Hidden"
    Role[Role == ""] <- "Predictor"
    ds$role <- Role
    ds$cardinality <- ds$uniqueness
    ds$name <- rownames(ds)
    plot <- ggplot(ds, aes(x = name, y = role, fill = type, label = cardinality)) +
      geom_raster() +
      labs(x = "Variables", y = "Roles", title = "Variable names, roles, types and cardinality") +
      theme( axis.text.x = element_text(angle = -45, hjust = 1))
    plotly(plot, tooltip = c("role", "type", "cardinality", "name"))
  })

  ###########################################################################
  getNumericPredictors <- reactive({
    d <- getPredictorData()
    types <- allClass(d) %in% c("numeric","integer")
    if (any(types)) {
      d <- d[,which(types), drop = FALSE]
    } else {
      d <- data.frame()
    }
    d
  })

  ###########################################################################
  getKnnDist <- reactive({
    d <- getNumericPredictors()
    d <- scale(d, scale = input$ScaleChart)
    if (isRoleValid(input$Target, d)) {
       d <- d[, colnames(d) != input$Target]
    }
    minPts <- ncol(d) + 1
    d <- na.omit(d)
    obj <- dbscan::kNN(as.matrix(d), k = minPts, approx = TRUE)
    minEps <- min(obj$dist[,minPts]) 
    maxEps <- max(obj$dist[,minPts])
    isolate({
      if (input$Eps <= minEps || input$Eps >= maxEps) {
        eps <- maxEps/3
      } else {
        eps <- input$Eps 
      }
    })
    updateSliderInput(session = session, inputId = "Eps", min = minEps, max = maxEps, value = eps)
    obj
  })
  
  ###########################################################################
  output$ClusterPlot <- renderPlot({
    d <- getNumericPredictors()
    d <- scale(d, scale = input$ScaleChart)
    if (isRoleValid(input$Target, d)) {
      d <- d[, colnames(d) != input$Target, drop = FALSE]
    }
    d <- as.matrix(na.omit(d))
    minPts <- ncol(d) + 1
    clust <- dbscan::hdbscan( x = d, minPts = minPts, gen_simplified_tree = FALSE, gen_hdbscan_tree = TRUE)
    output$ClusterCount <- renderValueBox({
      valueBox(subtitle = "Number of clusters", value = length(unique(clust$cluster)) - 1, color = EDAColour)
    })
    output$Outliers <- renderValueBox({
      valueBox(subtitle = "Number of outliers", value = sum(clust$cluster == 0), color = EDAColour)
    })
    output$ClusterPrint <- renderPrint({
      print(clust)
    })
    plot(clust)   #, main = "Hierarchical Density-based clustering (HDBSCAN)")
    title(main = "Hierarchical density based spatial clustering (numeric variables only)")
    abline(h = input$Eps, lty = 2)
  }, bg = "transparent")
  
  ###########################################################################
  output$KnnDistPlot <- renderPlot({
    obj <- getKnnDist()
    dbscan::kNNdistplot(obj)
    abline(h = input$Eps, lty = 2)
  }, bg = "transparent")
  
  # ###########################################################################
  # getNumData <- reactive({
  #   d <- getData()
  #   types <- allClass(d) %in% c("numeric","integer")
  #   if (any(types)) {
  #     d <- d[,which(types), drop = FALSE]
  #   } else {
  #     d <- data.frame()
  #   }
  #   # limit to first 20 columns for speed <<<<<<<<<<TODO make input$?
  #   if (ncol(d) > 20) {
  #     d <- d[,1:20]
  #   }
  #   d
  # })

  ###########################################################################
  output$NumericNovelties <- renderPlot({
    #Boxplots with coef are incompatible with plotly
    #Boxplots (and outliers) are only meaningful for continuous data
    d <- getSomeDefinitelyContinuousData()
    nvar <- ncol(d)
    if (nvar == 0) {
      showNotification(ui = "There are no continuous numeric variables", type = "warning")
    }
    req(nvar > 0)
    rec <- recipes::recipe(~., data = d)

    if (input$UseYJ) {
      rec <- recipes::step_YeoJohnson(rec, all_numeric(), num_unique = 10)
    }
    if (input$ScaleChart) {
      rec <- recipes::step_center(rec, all_numeric()) %>%
        recipes::step_scale(all_numeric())
      xlab <- "Standardised variable value"
    } else {
      xlab <- "Variable value"
    }
    rec <- recipes::prep(rec, data = d, retain = TRUE)
    d <- recipes::juice(rec, composition = "data.frame")

    outlier <- matrix(data = FALSE, nrow = nrow(d), ncol = ncol(d))
    for (col in 1:ncol(d)) {
      stats <- boxplot.stats(x = d[,col], coef = getMultiplier())
      if (length(stats$out) > 0) {
        outlier[d[, col] %in% stats$out, col] <- TRUE
      }
    }
    colnames(outlier) <- colnames(d)

    if (input$HideIrrelevant) {
      keep <- apply(X = outlier, MARGIN = 2, FUN = any)
      d <- d[, keep, drop = FALSE]
      outlier <- outlier[, keep, drop = FALSE]
    }
    req(ncol(d) > 0)

    if (input$ID != "") {
      d$rownames <- getSomeData()[, input$ID]
    } else {
      d$rownames <- 1:nrow(d)
    }

    d <- reshape2::melt(data = d, id.vars = "rownames")
    req(ncol(d) == 3)
    colnames(d) <- c("ID", "Variable", "value")
    for (col in 1:ncol(outlier)) {
      d[ d$Variable == (colnames(outlier)[col]) & !outlier[, col], "ID" ] <- NA   #hide the id for non-outlier cases
    }
    title <- paste("Uni-variable boxplots at IQR multiplier of", getMultiplier())
    if (input$UseYJ) {
      title <- paste(title, "with Yeo-Johnson reshaping")
      if (input$ScaleChart) {
        title <- paste(title, "and with scaling")
      }
    } else {
      if (input$ScaleChart) {
        title <- paste(title, "with scaling")
      }
    }

    ggplot(data = d, mapping = aes(x = Variable, y = value, fill = Variable)) +
      geom_boxplot(coef = getMultiplier(), outlier.colour = "red", ) +
      geom_text_repel(mapping = aes(label = ID)) +
      labs(title = title, y = NULL, x = NULL) +
      coord_flip() +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()
      )
  }, bg = "transparent")

  ###########################################################################
  observe({
    d <- getSomePossiblyContinuousData()
    vars <- colnames(d)
    num <- ncol(d)
    if (isRoleValid(input$Target, d)) {
      num <- num - 1
    }
    if (num < 2) {
      shinyjs::disable(id = "DimReductType")
    } else {
      shinyjs::enable(id = "DimReductType")
      ds <- getDataSummary()
      if (input$Target %in% rownames(ds)) {
        updateSelectizeInput(session = session, inputId = "DimReductType", choices = list("PCA","PLS","MDS","isoMDS","Sammon"), selected = "PLS")
      } else {
        updateSelectizeInput(session = session, inputId = "DimReductType", choices = list("PCA","MDS","isoMDS","Sammon"), selected = "PCA")
      }
    }
  })

  ########################################################################  ###
  getBagplot <- reactive({
    d <- na.omit(getSomeData())
    if (input$ID != "") {
      labels <- d[,input$ID]
    } else {
      labels <- rownames(d)
    }
    ds <- getDataSummary()
    # need to remove special roles and restrict to numeric and use definitely continuous
    d <- d[, colnames(d) == input$Target | (ds$uniqueness >= (getContinuousDebounced()[2]) & ds$numeric), drop = FALSE]
    d <- d[, setdiff(colnames(d), getNonPredictors()), drop = FALSE]

    nvar <- ncol(d)
    if (nvar == 0) {
      showNotification(ui = "There are no continuous numeric variables", type = "warning")
    }
    req(nvar > 0)
    if (input$UseYJ) {
      d <- recipes::recipe(~., data = d) %>%
        recipes::step_YeoJohnson(all_numeric(), num_unique = 10) %>%
        recipes::prep(data = d, retain = TRUE) %>%
        recipes::juice(composition = "data.frame")
    }
    vars <- colnames(d)
    num <- length(setdiff(vars, input$Target))
    req(num >= 2)
    if (input$Target %in% colnames(d)) {
      if (input$DimReductType == "PLS") {
        form <- as.formula(paste0(input$Target, " ~ ."))
      } else {
        d <- d[, colnames(d) != input$Target]
        form <- as.formula(paste0("~. "))
      }
    } else {
      form <- as.formula(paste0("~."))
    }
    data <- switch(
      input$DimReductType,
      "PCA"    = stats::prcomp(formula = form, data = d, center = TRUE, rank. = 2, scale. = input$ScaleChart, na.action = na.exclude, retx = TRUE)$x[,1:2],
      "PLS"    = {
        rec <- recipe(formula = form, data = d) %>%
          step_naomit(all_predictors(), all_outcomes(), skip = TRUE) %>%
          step_center(all_predictors())
        if (input$ScaleChart) {
          rec <- rec %>% step_scale(all_predictors())
        }
        target <- input$Target
        rec %>%
          step_pls(all_predictors(), num_comp = 2, outcome = target) %>%
          prep(data = data) %>%
          juice(all_predictors(), composition = "data.frame")
      },
      "MDS"    = stats::cmdscale(dist(scale(d, center = TRUE, scale = input$ScaleChart), method = input$DistanceMethod), k = 2),
      "isoMDS" = MASS::isoMDS(dist(scale(d, center = TRUE, scale = input$ScaleChart), method = input$DistanceMethod), k = 2)$points,
      "Sammon" = MASS::sammon(dist(scale(d, center = TRUE, scale = input$ScaleChart), method = input$DistanceMethod), k = 2)$points
    )
    x <- data[,1, drop = TRUE]
    y <- data[,2, drop = TRUE]
    names(x) <- labels
    names(y) <- labels
    aplpack::compute.bagplot(x, y, factor = (getMultiplier() + 1), na.rm = FALSE) # using +1 because multiplier is defined differently for boxplot
  })

  ###########################################################################
  output$MultiDimNovelties <- renderPlot({
    bag <- getBagplot()
    plot(bag, show.bagpoints = FALSE)
    if (!is.null(bag$pxy.outlier) && nrow(bag$pxy.outlier) > 0) {
      text(bag$pxy.outlier, labels = rownames(bag$pxy.outlier), pos = 4)
    }
  }, bg = "transparent")

  ###########################################################################
  output$MultiDimNovText <- renderText({
    bag <- getBagplot()
    if (!is.null(bag$pxy.outlier) && nrow(bag$pxy.outlier) > 0) {
      count <- nrow(bag$pxy.outlier)
    } else {
      count <- 0
    }
    text <- paste(count, "bagplot outliers using an equivalent multipler of", getMultiplier())
    text <- paste(text, paste(collapse = " and ", na.omit(c(ifelse(input$ScaleChart, "with standardising", NA), ifelse(input$UseYJ, "with reshaping", NA)))))
    text
  })

  ###########################################################################
  output$MultiDimNovIDs <- DT::renderDataTable({
    bag <- getBagplot()
    req(!is.null(bag$pxy.outlier) & nrow(bag$pxy.outlier) > 0)
    d <- data.frame("ID" = rownames(bag$pxy.outlier))
    DT::datatable(data = d, rownames = FALSE, fillContainer = TRUE, selection = "none",
                  options = list(searching = FALSE,
                                 order = FALSE,
                                 pageLength = nrow(d),
                                 dom = "t")
    )
  })

  ###########################################################################
  getPPS <- reactive({
    data <- getSomePredictorData()
    maxcols <- getColsDebounced()
    if (ncol(data) > maxcols) {
      ds <- getDataSummary()
      cols <- rownames(ds)[!ds[,"nzv", drop = TRUE]]
      data <- data[, colnames(data) %in% cols]  # remove near zero variance variables
    }
    if (ncol(data) > maxcols) {
      vi <- getVarImp()
      vi <- vi[names(vi) %in% colnames(data)]
      vi <- vi[1:min(c(maxcols, length(vi)))]
      data <- data[, colnames(data) %in% c(input$Target,names(vi))]
    }
    data <- na.omit(data)
    par <- isolate(input$Parallel)
    obj <- NA
    if (par) {
      obj <- startParallel("pps")
    }
    showNotification(id = "pps", ui = "Calculating Predictive Power Scores", duration = NULL)
    cor <- pps(data, allowParallel = par, limit = getMaxRowsDebounced())
    removeNotification(id = "pps")
    if (par) {
      stopParallel(obj)
    }
    cor
  })

  ###########################################################################
  output$Correlation <- renderPlotly({
    data = getSomePredictorData()
    ds <- getDataSummary()
    cols <- rownames(ds)[ds[,"numeric", drop = TRUE]]
    data <- data[, colnames(data) %in% cols]  # remove non-numeric variables
    cols <- rownames(ds)[ds[,"uniqueness", drop = TRUE] > 1]
    data <- data[, colnames(data) %in% cols]  # remove zero variance variables
    maxcols <- getColsDebounced()
    if (ncol(data) > maxcols) {
      cols <- rownames(ds)[ds[,"uniqueness", drop = TRUE] > getContinuousDebounced()[1]]
      data <- data[, colnames(data) %in% cols]  # remove non continuous  variables
    }
    if (ncol(data) > maxcols) {
      cols <- rownames(ds)[!ds[,"nzv", drop = TRUE]]
      data <- data[, colnames(data) %in% cols]  # remove near zero variance variables
    }
    if (ncol(data) > maxcols) {
      vi <- getVarImp()
      vi <- vi[names(vi) %in% colnames(data)]
      vi <- vi[1:min(c(maxcols, length(vi)))]
      if (ds[input$Target, "numeric"]) {
        vi <- c(100,vi)
        names(vi)[1] <- input$Target
      }
      data <- data[, colnames(data) %in% names(vi)] # remove low variable importance
    }
    req(ncol(data) > 0)
    if (input$CorMethod == "Distance") {
      corr <- cor.distance(data)
      title <- "Distance Correlation"
    } else if (input$CorMethod == "Predictive Power") {
        corr <- getPPS()
        title <- "Predictive Power"
    } else {
      corr <- cor(data, method = tolower(input$CorMethod), use = "pairwise.complete.obs")
      if (input$CorAbs) {
        corr <- abs(corr)
        title <- paste0(input$CorMethod," (absolute) Correlation")

      } else {
        title <- paste0(input$CorMethod, " Correlation")
      }
    }
    p <- if (input$CorGrouping == "none") {
      ggcorrplot(corr = corr, method = "square", hc.order = FALSE, type = "full", lab = TRUE,
                 title = title, show.diag = FALSE, show.legend = FALSE)
    } else {
      title <- paste(title,"Ordered by", input$CorGrouping)
      ggcorrplot(corr = corr, method = "square", hc.order = TRUE, type = "full", lab = TRUE,
                 title = title, show.diag = FALSE, show.legend = FALSE, hc.method = input$CorGrouping)
    }
    plotly(p, tooltip = c("Var1","Var2","value"))
  })

  ###########################################################################
  output$Pairs <- renderPlot({
    maxcols <- getColsDebounced()
    data = getSomeData()
    ds <- getDataSummary()
    #turn numeric with possibly few levels into factors
    cont <- getContinuousDebounced()
    toFactor <- rownames(ds)[ds$uniqueness <= (cont[1]) & ds$numeric]
    for (col in toFactor) {
      data[, col] <- as.factor(data[, col, drop = TRUE])
    }
    subset <- colnames(data)

    #reject factors with possibly too many levels - unless it is the target
    bad <- rownames(ds)[ds$factor & ds$uniqueness > cont[1]]
    bad <- setdiff(bad, input$Target)
    if (length(bad) > 0) {
      subset <- setdiff(subset, bad)
    }

    #reject date and character variables
    bad <- rownames(ds)[ds$text | ds$date]
    if (length(bad) > 0) {
      subset <- setdiff(subset, bad)
    }

    subset <- setdiff(subset, getNonPredictors())
    data <- data[,subset]

    if (ncol(data) > maxcols) {
      vi <- getVarImp()
      vi <- vi[1:min(c(maxcols, length(vi)))]
      data <- data[, subset %in% c(input$Target,names(vi))]
    }
    req(ncol(data) > 1)
    showNotification(id = "plotpairs", ui = "Calculating pairs plots", duration = NULL)
    if (input$ProbType == "Classification" && isRoleValid(input$Target, data)) {
      plot <- GGally::ggpairs(data = data, mapping = ggplot2::aes(colour = data[, input$Target]), title = "Pairs plot of most important variables",
                              cardinality_threshold = getContinuousDebounced()[1])
    } else {
      plot <- GGally::ggpairs(data = data, cardinality_threshold = getContinuousDebounced()[1])
    }
    removeNotification(id = "plotpairs")
    plot
  }, bg = "transparent")

  # ###########################################################################
  # output$MissCorr <- renderPlot({
  #   d <- getSomeData()
  #   m <- ifelse(is.na(d), 1, 0)
  #   cm <- colMeans(m)
  #   m <- m[, cm > 0 & cm < 1, drop = FALSE]
  #   req(ncol(m) > 0)
  #   corrgram::corrgram(cor(m), order = "OLO", abs = TRUE)  #TODO change to ggplot as per other correlation chart
  #   title(main = "Variable missing value correlation",
  #         sub = "Notice whether variables are missing in sets")
  # }, bg = "transparent")

  output$MissCorr <- renderPlotly({
    d = getSomeData()
    m <- ifelse(is.na(d), 1, 0)
    cm <- colMeans(m)
    data <- m[, cm > 0 & cm < 1, drop = FALSE]
    req(ncol(data) > 0)

    if (input$CorMethod == "Distance") {
      corr <- cor.distance(data)
      title <- "Variable Distance Correlation"
    } else if (input$CorMethod == "Predictive Power") {
      corr <- getPPS()
      title <- "Variable Predictive Power Correlation"
    } else {
      corr <- cor(data, method = tolower(input$CorMethod), use = "pairwise.complete.obs")
      if (input$CorAbs) {
        corr <- abs(corr)
        title <- paste0("Variable ",input$CorMethod," (absolute) Correlation")
        
      } else {
        title <- paste0("Variable ", input$CorMethod, " Correlation")
      }
    }
    p <- if (input$CorGrouping == "none") {
      ggcorrplot(corr = corr, method = "square", hc.order = FALSE, type = "full", lab = TRUE,
                 title = title, show.diag = FALSE, show.legend = FALSE)
    } else {
      title <- paste(title,"Ordered by", input$CorGrouping)
      ggcorrplot(corr = corr, method = "square", hc.order = TRUE, type = "full", lab = TRUE,
                 title = title, show.diag = FALSE, show.legend = FALSE, hc.method = input$CorGrouping)
    }
    plotly(p, tooltip = c("Var1","Var2","value"))
  })
  
  ###########################################################################
  getMissingnessTree <- reactive({
    d <- getData()
    req(ncol(d) > 0, nrow(d) > 0)
    d$missingness <- base::apply(X = is.na(d), MARGIN = 1, FUN = sum)
    req(any(d$missingness > 0))
    model <- rpart(formula = missingness ~ ., data = d)
  })

  ###########################################################################
  output$MissingnessInformationPlot <- renderPlot({
    tree <- getMissingnessTree()
    rpart.plot::rpart.plot(tree, shadow.col = "gray", main = "Predicting the number of missing variables in an observation",
                           sub = "Check whether the outcome variable is an important variable", roundint = FALSE)
  }, bg = "transparent")

  ###########################################################################
  output$MissingnessVariables <- renderPlotly({
    vi <- getMissingnessTree()$variable.importance
    varImp <- vi / sum(vi)
    varImp <- sort(varImp, decreasing = TRUE)[1:min(c(15,length(varImp)))]
    interesting <- names(varImp) %in% c(input$Target, input$ID, input$Weights, input$Groups, input$PreSplit)
    d <- data.frame(varImp, variables = names(varImp), interesting)
    d <- transform(d, variables = reorder(variables, -varImp) )
    p <- ggplot(data = d) +
      geom_col(aes(y = varImp, x = variables, fill = interesting)) +
      labs(title = "Variable Importance for predicting missing variables per observation", y = "Relative Importance")
    plotly(p)
  })

  ###########################################################################
  getPartition <- reactive({
    d <- getRawData()
    if (!is.null(input$PreSplit) && length(input$PreSplit) > 0 && input$PreSplit != "") {
      splitVar <- d[ ,input$PreSplit, drop = TRUE]
      req(!any(is.na(splitVar)))
      lvs <- unique(splitVar)
      req(length(lvs) == 2)
      shinyjs::hideElement(id = "Ratio")
      train <- 2
      if (toupper(as.character(lvs[1])) == "TRAIN")  {
        train <- 1
      } else if (sum(splitVar == lvs[1]) > nrow(d)) {
        train <- 1
      }
      split <- splitVar == lvs[train]
      split <- (1:nrow(d))[split] # want this as row numbers
    } else if (input$Groups != "") {
      req(isRoleValid(input$Groups, d))
      shinyjs::showElement(id = "Ratio")
      grouped <- d[,input$Groups, drop = FALSE]
      req(!any(is.na(grouped)))
      cumFreq <- cumsum(sort(table(grouped), decreasing = FALSE))
      cumProp <- cumFreq / max(cumFreq)
      trainLevels <- names(cumProp[cumProp <= input$Ratio])
      split <- (1:nrow(d))[d[,input$Groups, drop = TRUE] %in% trainLevels]
    } else {
      req(isRoleValid(input$Target, d))
      shinyjs::showElement(id = "Ratio")
      y <- d[, which(colnames(d) == input$Target), drop = TRUE]
      req(length(y) > 1)
      if (class(y) == "character" || any(is.na(y))) {
        split <- sample(x = 1:nrow(d), size = floor(input$Ratio * nrow(d)))
      } else {
        split <- caret::createDataPartition(y = y, p = input$Ratio, list = FALSE)
      }
    }
    split
  })

  ###########################################################################
  output$Splitter <- renderText({
    if (!is.null(input$PreSplit) & input$PreSplit != "") {
      paste("The data was partitioned using variable", input$PreSplit)
    } else if (!is.null(input$Groups) & input$Groups != "") {
      paste("The data was group-stratified using variable", input$Group)
    } else {
      paste("The data was target-stratified using variable", input$Target)
    }
  })

  ###########################################################################
  getTrainData <- reactive({
    d <- getData()[getPartition(), ]
    req(nrow(d) > 0)
    d
  })

  ###########################################################################
  getTestData <- reactive({
    d <- getData()[-getPartition(), ]
    req(nrow(d) > 0)
    d
  })

  ###########################################################################
  output$SplitCountTrain <- renderText({
    d <- getTrainData()
    req(d)
    nrow(d)
  })

  ###########################################################################
  output$SplitCountTest <- renderText({
    d <- getTestData()
    req(d)
    nrow(d)
  })

  ###########################################################################
  output$TestUncertainty <- renderInfoBox({
    d <- getTestData()
    req(d)
    alpha = input$Probability/100
    N <- nrow(d)
    req(N > 30)
    if (input$ProbType == "Regression") {
      lower <- 100 - sqrt(N / qchisq(p = (1 - alpha)/2, df = N)) * 100
      upper <- 100 - sqrt(N / qchisq(p = (1 + alpha)/2, df = N)) * 100
      text <- paste0(round(lower), "%  to  +", round(upper),"% of the RMSE to cover ", alpha*100,"% of cases")
    } else if (input$ProbType == "Classification") {
      s <- qnorm(1 - (1 - alpha)/2) * 0.5 / sqrt(N)
      text <- paste0("Worst case accuracy uncertainty +/- ",round(s, 3), " to cover ", alpha*100,"% of cases")
    } else {
      req(NULL)
    }
    infoBox(title = paste0(input$Probability,"% uncertainty of test metric"), value = text, icon = icon("question"), color = "yellow", fill = TRUE)
  })

  ###########################################################################
  observeEvent({
    input$Evaluate
  },
  {
    data <- getData()
    predictors <- setdiff(colnames(data), c(getNonPredictors(), input$Target))
    if (input$PreSplit != "") {
      part <- input$PreSplit
    } else {
      data$partition <- "Test"
      data$partition[getPartition()] <- "Train"
      part <- "partition"
    }
    form <- as.formula(paste0(part,"~",paste(collapse = "+", predictors)))

    showNotification(id = "fair", ui = "Calculating partition fairness using Random Forest", duration = NULL)
    parallelMode <- isolate(input$Parallel)
    obj <- NA
    if (parallelMode) {
      obj <- startParallel("fair")
    }
    trControl <- caret::trainControl(method = "boot", number = 25, trim = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)  # for performance use oob which is specific to rf
    rf <- caret::train(form, data = data, method = "rf", metric = "ROC", maximise = TRUE, trControl = trControl, na.action = na.exclude)
    if (parallelMode) {
      response <- stopParallel(obj)
    }
    if (is(rf, "error")) {
      shinyalert::shinyalert(title = "Partition fairness", text = response)
      return(NULL)
    }
    removeNotification(id = "fair")
    react$ROC <- max(rf$results$ROC)
  }, ignoreInit = FALSE)

  ###########################################################################
  output$SplitTest <- renderInfoBox({
    req(react$ROC)
    text <- paste("Predicting the partition using rf has a AuC of", round(react$ROC, 2), " compared to an expected value of 0.5")
    infoBox(title = "Partition Fairness", value = text, icon = icon("balance-scale"), color = "yellow", fill = TRUE)
  })

  ###########################################################################
  getBaseRecipe <- reactive({
    data <- getTrainData()
    req(isRoleValid(input$Target, data))
    input$Restart
    # Need to remove special roles ???
    frecipe <- recipes::recipe(formula = as.formula(paste(input$Target, "~ .")), data = data)
    if (input$Weights %in% colnames(data)) {
      w <- input$Weights
      frecipe <- update_role(frecipe, one_of(w), new_role = "case weight")
    }
    if (!is.null(input$ID) && all(input$ID %in% colnames(data))) {
      w <- input$ID
      frecipe <- update_role(frecipe, one_of(w), new_role = "case id")
    }
    if (length(react$HighCardinality) > 0) {
      hc <- react$HighCardinality
       frecipe <- recipes::add_role(frecipe, !!!hc, new_role = "high cardinality")
    }
    if (input$PreSplit %in% colnames(data)) {
      vars <- input$PreSplit
      frecipe <- update_role(frecipe, one_of(vars), new_role = "presplit")
    }
    if (all(input$Groups %in% colnames(data))) {
      vars <- input$Groups
      frecipe <- add_role(frecipe, one_of(vars), new_role = "group")
      frecipe <- remove_role(frecipe, one_of(vars), old_role = "predictor")
    }
    if (!is.null(input$HideCol) && all(input$HideCol %in% colnames(data))) {
      w <- input$HideCol
      frecipe <- step_rm(frecipe, one_of(w))
    }
    frecipe
  })
  ###########################################################################
  observe({
    react$Recipe <- getBaseRecipe()
    updateCheckboxInput(session = session, inputId = "MissingVar",       value = FALSE)
    updateCheckboxInput(session = session, inputId = "MissingObs",       value = FALSE)
    updateCheckboxInput(session = session, inputId = "Shadow",           value = FALSE)
    updateCheckboxInput(session = session, inputId = "Unknown",          value = FALSE)
    updateSelectizeInput(session = session, inputId = "Impute",           selected = "none")
    updateSelectizeInput(session = session, inputId = "Balance",          selected = "none")
    updateSelectizeInput(session = session, inputId = "Variance",         selected = "none")
    updateCheckboxInput(session = session, inputId = "LinComb",          value = FALSE)
    updateCheckboxInput(session = session, inputId = "YJ",               value = FALSE)
    updateCheckboxInput(session = session, inputId = "Text",             value = FALSE)
    updateCheckboxInput(session = session, inputId = "Other",            value = FALSE)
    updateCheckboxInput(session = session, inputId = "Convert",          value = FALSE)
    updateSelectizeInput( session = session, inputId = "DateFeatures",     selected = NULL)
    updateCheckboxInput(session = session, inputId = "Cyclic",           value = FALSE)
    updateCheckboxInput(session = session, inputId = "Center",           value = FALSE)
    updateCheckboxInput(session = session, inputId = "Scale",            value = FALSE)
    updateSelectizeInput(session = session, inputId = "DimReduce",        selected = "none")
    updateCheckboxInput(session = session, inputId = "FeatureSelection", value = FALSE)
    updateCheckboxInput(session = session, inputId = "Clusters",         value = FALSE)
    updateCheckboxInput(session = session, inputId = "Poly",             value = FALSE)
    isolate({
      react$RecipeChanged <- react$RecipeChanged + 1
    })
  }, priority = 10)

  ###########################################################################
  observe({
    req(react$Recipe)
    try({
      tib <- react$Recipe$term_info
      dtePresent <- any(tib$type == "date")   # & tib$role == "predictor")
      nomPresent <- any(tib$type == "nominal" & tib$role == "predictor")
      shinyjs::toggleElement(id = "DateFeatures", condition = dtePresent)
      shinyjs::toggleElement(id = "Cyclic", condition = dtePresent)
      shinyjs::toggleElement(id = "Convert", condition = nomPresent)
      shinyjs::toggleElement(id = "Other", condition = nomPresent)
    }, silent = TRUE)
  })

  ###########################################################################
  ##### Imputation #####
  observeEvent(
    input$Impute,
    {
      d <- getSomeData()
      react$Recipe <- remove_step(react$Recipe, c("naomit", "knnimpute", "bagimpute", "medianimpute", "modeimpute", "rollimpute", "meanimpute"))
      if (input$Impute != "none") {
        react$Recipe <- switch(
          input$Impute,
          "omit" = {
            step_naomit(react$Recipe, all_predictors(), all_outcomes(), skip = TRUE)
          },
          "knnImpute" = {
            step_knnimpute(react$Recipe, all_predictors(), neighbors = 5)
          },
          "Omit/knnImpute" = {
            step_naomit(react$Recipe, all_predictors(), skip = TRUE) %>%
              step_knnimpute(all_predictors(), neighbors = 5)
          },
          "bagImpute" = {
            step_bagimpute(react$Recipe, all_predictors(), impute_with = imp_vars(all_predictors()), trees = 25)
          },
          "Omit/bagImpute" = {
            step_naomit(react$Recipe, all_predictors(), skip = TRUE) %>%
              step_bagimpute(all_predictors(), impute_with = imp_vars(all_predictors()), trees = 25)
          },
          "median.mode" = {
            step_medianimpute(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group")) %>%
              step_modeimpute(all_nominal(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
          },
          "mean.mode" = {
            step_meanimpute(all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group")) %>%
              step_modeimpute(react$Recipe, all_nominal(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
          }
        )
      }
    }
  )

  ###########################################################################
  observeEvent(
    input$Balance,
    ##### Balance Y column #####
    {
      react$Recipe <- remove_step(react$Recipe, c("smote", "nearmiss"))
      if (input$ProbType == "Classification" && input$Balance != "none") {
        react$Recipe <- switch(
          input$Balance,
          "up"      = themis::step_smote(react$Recipe, all_outcomes(), over_ratio = 1, neighbors = 5, skip = TRUE),
          "down"    = themis::step_nearmiss(react$Recipe, all_outcomes(), under_ratio = 1, neighbors = 5, skip = TRUE),
          "up-down" = themis::step_nearmiss(react$Recipe, all_outcomes(), under_ratio = 2, neighbors = 5, skip = TRUE) %>%
            themis::step_smote(all_outcomes(), over_ratio = 1, neighbors = 5, skip = TRUE)
        )
      }
    }
  )

  ###########################################################################
  ##### Add shadow Variables #####
  observeEvent(
    input$Shadow,
    {
      if (input$Shadow) {
        react$Recipe <- step_shadow_missing(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
      } else {
        react$Recipe <- remove_step(react$Recipe, c("shadow_missing"))
      }
    }
  )

  ###########################################################################
  ##### Remove Linear Combinations (one of) #####
  observeEvent(
    input$LinComb,
    {
      if (input$LinComb) {
        react$Recipe <- step_lincomb(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
      } else {
        react$Recipe <- remove_step(react$Recipe, c("lincomb","lincomp"))  ##TODO this is temp work around
      }
    }
  )

  ###########################################################################
  ##### Yeo-Johnson reshaping #####
  observeEvent(
    input$YJ,
    {
      if (input$YJ) {
        react$Recipe <- step_YeoJohnson(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
      } else {
        react$Recipe <- remove_step(react$Recipe, c("YeoJohnson"))
      }
    }
  )

  ###########################################################################
  ##### Drop heavily missing variables #####
  observeEvent(
    {
      input$MissingVar
      input$MissVarThreshold
    },
    {
      name <- "missingVar"
      step <- get_step(react$Recipe, name)
      if (input$MissingVar) {
        if (is.null(step)) {
          react$Recipe <- step_missingVar(react$Recipe, all_predictors(), ratio = input$MissVarThreshold/100)
        } else {
          rec <- react$Recipe
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], ratio = input$MissVarThreshold/100)
          react$Recipe <- rec
        }
      } else
        react$Recipe <- remove_step(react$Recipe, name)
    }
  )

  ###########################################################################
  ##### Drop heavily missing observations #####
  observeEvent(
    {
      input$MissingObs
      input$MissRatio
    },
    {
      name <- "missingObs"
      step <- get_step(react$Recipe, name)
      if (input$MissingObs) {
        if (is.null(step)) {
          react$Recipe <- step_missingObs(react$Recipe, all_predictors(), ratio = input$MissRatio/100)
        } else {
          rec <- react$Recipe
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], ratio = input$MissRatio/100)
          react$Recipe <- rec
        }
      } else
        react$Recipe <- remove_step(react$Recipe, name)
    }
  )

  ###########################################################################
  ##### Assign missing factor levels to "Unknown" #####
  observeEvent(
    {
      input$Unknown
    },
    {
      name <- "unknown"
      step <- get_step(react$Recipe, name)
      if (input$Unknown) {
        if (is.null(step)) {
          react$Recipe <- step_unknown(react$Recipe, all_nominal(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), new_level = "unknown")
          }
      } else
        react$Recipe <- remove_step(react$Recipe, name)
    }
  )

  ###########################################################################
  ##### Drop problem variance columns #####
  observeEvent(
    input$Variance,
    {
      react$Recipe <- remove_step(react$Recipe, c("nzv", "zv"))
      if (input$Variance != "none") {
        react$Recipe <- switch(
          input$Variance,
          "zv"    = step_zv(react$Recipe, all_predictors()),
          "nzv"   = step_nzv(react$Recipe, all_predictors(), freq_cut = 95/5, unique_cut = 10)
        )
      }
    }
  )

  ###########################################################################
  ##### Accumulate Other level #####
  observeEvent(
    {
      input$Other
      input$OtherThreshold
    },
    {
      name <- "other"
      step <- get_step(react$Recipe, name)
      if (input$Other) {
        if (is.null(step)) {
          react$Recipe <- step_other(react$Recipe, all_nominal(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), threshold = input$OtherThreshold/100, other = "other")
        } else {
          rec <- react$Recipe
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], threshold = input$OtherThreshold/100)
          react$Recipe <- rec
        }
      } else {
        react$Recipe <- remove_step(react$Recipe, name)
      }
    }
  )

  ###########################################################################
  ##### Create new columns through conversion #####
  observeEvent(
    {
      input$Convert
      input$OneHot
    },
    {
      name <- "dummy"
      step <- get_step(react$Recipe, name)
      hc <- react$HighCardinality
      if (input$Convert) {
        if (is.null(step)) {
          react$Recipe <- step_dummy(react$Recipe, all_nominal(), -has_role("high cardinality"), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), one_hot = input$OneHot)
        } else {
          rec <- react$Recipe
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], one_hot = input$OneHot)
          react$Recipe <- rec
        }
      } else
        react$Recipe <- remove_step(react$Recipe, name)
    }
  )

  ###########################################################################
  ##### Center numeric columns #####
  observeEvent(
    input$Center,
    {
      if (input$Center) {
        react$Recipe <- step_center(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
      } else {
        react$Recipe <- remove_step(react$Recipe, "center")
      }
    }
  )

  ###########################################################################
  ##### Scale numeric columns #####
  observeEvent(
    input$Scale,
    {
      if (input$Scale) {
        react$Recipe <- step_scale(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
      } else {
        react$Recipe <- remove_step(react$Recipe, "scale")
      }
    }
  )

  ###########################################################################
  ##### Text Features #####
  observeEvent(
    {
      input$text
    },
    {
      name <- "textfeature"
      step <- get_step(react$Recipe, name)
      if (input$text) {
        if (is.null(step)) {
          react$Recipe <- step_textfeature(react$Recipe, has_type("character"), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), extract_functions = textfeatures::count_functions)
        }
      } else {
        react$Recipe <- remove_step(react$Recipe, name)
      }
    }
  )

  ###########################################################################
  ##### Encode text features #####
  observeEvent(
    {
      getDataSummary()
      react$RecipeChanged
    },
    {
      ds <- getDataSummary()
      cols <- rownames(ds)[ds$text]
      colsx <- cols[!cols %in% c(getNonPredictors(), input$Target)]
      toggleElement(id = "text", condition = length(colsx) > 0)
    }
  )

  #####
  observeEvent(
    {
      getContinuousDebounced()
      input$Target
      getDataSummary()
      react$RecipeChanged
    },
    {
      req(input$Target != "")
      ds <- getDataSummary()
      req(input$Target %in% rownames(ds))
      cols <- rownames(ds)[ds$text | (ds$factor & ds$uniqueness > getContinuousDebounced()[1])]
      colsx <- cols[!cols %in% c(getNonPredictors(), input$Target)]
      highCard <- length(colsx) > 0
      react$HighCardinality <- colsx
      toggleElement(id = "String", condition = highCard)
      if (!highCard) {
        choices = list("None" = "none")
      } else if (!ds[input$Target, "numeric"] & ds[input$Target, "uniqueness"] != 2) {
        choices = list("None" = "none", "Omit" = "omit", "Hash encoding" = "hash")
      } else if (ds[input$Target, "uniqueness"] != 2) {
        choices = list("None" = "none", "Omit" = "omit", "Mean encoding" = "mean", "Hash encoding" = "hash", "Embedded encoding" = "embed", "Binary encoding" = "binary")
      } else {
        choices = list("None" = "none", "Omit" = "omit", "Weight-of-evidence encoding" = "woe", "Mean encoding" = "mean", "Hash encoding" = "hash", "Embedded encoding" = "embed", "Binary encoding" = "binary")
      }
      updateSelectizeInput(session = session, inputId = "String", choices = choices, selected = choices[1])
    }
  )

  ##### String encoding #####
  observeEvent(
    {
      input$String
      react$HighCardinality
      input$Target
      react$RecipeChanged
      # getContinuousDebounced()
    },
    {
      colsx <- react$HighCardinality
      req(length(colsx) > 0)
      react$Recipe <- remove_step(react$Recipe, c("rm_HC", "woe", "lencode_mixed", "tokenize","tokenfilter", "texthash", "zv_HC", "embed","binary"))
      if (input$String != "none") {
        react$Recipe <- switch(
          input$String,
          "omit" = {
            step_rm(react$Recipe, has_role("high cardinality"), id = rand_id("rm_HC"))
          },
          "woe" = {
            step_woe(react$Recipe, has_role("high cardinality"), outcome = input$Target)
          },
          "mean" = {
            step_lencode_mixed(react$Recipe, has_role("high cardinality"), outcome = input$Target, id = rand_id("lencode_mixed") ) #bug has wrong id default
          },
          # "****hash" = {
          #   name <- "texthash"
          #   step <- get_step(react$Recipe, name)
          #   if (is.null(step)) {
          #     pattern <- paste0(colsx, "_hash")
          #     react$Recipe %>%
          #       step_tokenize(has_role("high cardinality")) %>%
          #       #step_stem(colsx) %>%
          #       #step_stopwords(colsx) %>%
          #       step_tokenfilter(has_role("high cardinality"), max_tokens = 50) %>%
          #       ######step_tf(colsx) %>%
          #       ######step_tfidf(colsx) %>%
          #       step_texthash(has_role("high cardinality"), num_terms = input$NumTerms) %>%
          #       step_zv(tidyselect::starts_with(pattern), id = rand_id("zv_HC"))   ##some new vars can be constant
          #   } else {
          #     rec <- react$Recipe
          #     rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_terms = input$NumTerms)
          #     rec
          #   }
          # },
          "hash" = {
            name <- "feature_hash"
            step <- get_step(react$Recipe, name)
            if (is.null(step)) {
              react$Recipe %>%
                embed::step_feature_hash(has_role("high cardinality"), num_hash = input$NumTerms)
            } else {
              rec <- react$Recipe
              rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_hash = input$NumTerms)
              rec
            }
          },
          "embed" = {
            name <- "embed"
            step <- get_step(react$Recipe, name)
            if (is.null(step)) {
              step_embed(react$Recipe, has_role("high cardinality"), outcome = input$Target, num_terms = input$NumTerms) #TODO assign embed_control
            } else {
              rec <- react$Recipe
              rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_terms = input$NumTerms)
              rec
            }
          },
          "binary" = {
            name <- "binary"
            step <- get_step(react$Recipe, name)
            if (is.null(step)) {
              step_binary(react$Recipe, has_role("high cardinality"), limit = 1000)
            }
          }
        )
      }
    }
  )

  observeEvent(
    {
      input$MissingObs
      input$MissObsThreshold
    },
    {
      name <- "missingObs"
      step <- get_step(react$Recipe, name)
      if (input$MissingObs) {
        if (is.null(step)) {
          react$Recipe <- step_missingObs(react$Recipe, all_predictors(), ratio = input$MissObsThreshold/100)
        } else {
          rec <- react$Recipe
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], ratio = input$MissObsThreshold/100)
          react$Recipe <- rec
        }
      } else
        react$Recipe <- remove_step(react$Recipe, name)
    }
  )

  ###########################################################################
  # hide/show recipe controls and set choices
    observe({
    shinyjs::toggleElement(id = "Balance", condition = input$ProbType == "Classification" & !input$AddWeights)
    shinyjs::toggleElement(id = "FeatureSelection", condition = input$Target != "")
    if (input$Target == "") {
      choices = list("None" = "none", "Correlation" = "corr", "PCA" = "pca", "ICA" = "ica", "IsoMap" = "isomap")
    } else {
      choices <- list("None" = "none", "PLS (sup)" = "pls", "UMAP (sup)" = "umap", "Correlation" = "corr", "PCA" = "pca", "ICA" = "ica", "IsoMap" = "isomap", "kPCA" = "kpca")
    }
    updateSelectizeInput(session = session, inputId = "DimReduce", choices = choices, selected = input$DimReduce)
    if (input$ProbType ==  "Classification") {
      shinyjs::showElement(id = "Centers")
    } else {
      shinyjs::hideElement(id = "Centers")
    }
  })

  ###########################################################################
  ##### Dimensional Reduction #####

  observeEvent(
    {
      input$DimReduce
      input$CorrThresh
      input$NumComp
      input$VarThresh
    },
    {
      name <- paste(sep = "_", "step", input$DimReduce)
      if (input$DimReduce != "none") {
        step <- get_step(react$Recipe, name)
        if (is.null(step)) {
          react$Recipe <- remove_step(react$Recipe, c("corr", "ica", "pls", "umap", "kpca", "isomap", "pca"))
          target <- input$Target
          react$Recipe <- switch(input$DimReduce,
                                 "corr"   = step_corr(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), threshold = input$CorrThresh),
                                 "ica"    = step_ica(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), num_comp = input$NumComp),
                                 "pls"    = step_pls(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), outcome = target, num_comp = input$NumComp),
                                 "umap"   = step_umap(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), outcome = target, num_comp = input$NumComp),
                                 "kpca"   = step_kpca(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), num_comp = input$NumComp, options = list(kernel = "rbfdot", kpar = list(sigma = 0.2))),
                                 "isomap" = step_isomap(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), num_terms = input$NumComp),
                                 "pca"    = step_pca(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), threshold = input$VarThresh, options = list(retx = FALSE, center = TRUE, scale. = input$Scale, tol = NULL))
          )
        } else {
          rec <- react$Recipe
          react$Recipe <- switch(input$DimReduce,
                                 "corr"   = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], threshold = input$CorrThresh),
                                 "ica"    = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_comp = input$NumComp),
                                 "pls"    = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_comp = input$NumComp),
                                 "umap"   = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_comp = input$NumComp),
                                 "kpca"   = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_comp = input$NumComp),
                                 "isomap" = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], num_comp = input$NumComp),
                                 "pca"    = rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], threshold = input$VarThresh)
          )
          react$Recipe <- rec
        }
      } else
        react$Recipe <- remove_step(react$Recipe, c("corr", "ica", "pls", "umap", "kpca", "isomap", "pca"))
    }
  )

  ###########################################################################
  ##### Polynomial expansion #####
  observeEvent(
    {
      input$Poly
      input$PolyDegree
    },
    {
      react$Recipe <- remove_step(react$Recipe, c("poly"))
      if (input$Poly) {
        react$Recipe <- step_poly(react$Recipe, all_predictors(), -all_nominal(), degree = input$PolyDegree)
      }
    }
  )
  observeEvent(
    input$PolyDegree,
    {
      if (input$Poly) {
        rec <- react$Recipe
        if (!is.null(input$PolyDegree)) {
          step <- get_step(react$Recipe, "poly")
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], degree = input$PolyDegree)
        }
        react$Recipe <- rec
      }
    }
  )

  ##########################################################################
  ##### Feature select columns #####
  observeEvent(
    input$FeatureSelection,
    {
      if (input$FeatureSelection) {
        react$Recipe <- step_select_boruta(react$Recipe, all_predictors(), outcome = input$Target, parallel = input$Parallel)
      } else {
        react$Recipe <- remove_step(react$Recipe, "select_boruta")
      }
    }
  )
  observeEvent(
    input$Parallel,
    {
      if (input$FeatureSelection) {
        rec <- react$Recipe
        if (!is.null(input$Parallel)) {
          step <- get_step(react$Recipe, "FeatureSelection")
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], parallel = input$Parallel)
        }
        react$Recipe <- rec
      }
    }
  )


  ##########################################################################
  ##### Clusters #####
  observeEvent(
    input$Clusters,
    {
      if (input$Clusters) {
        react$Recipe <- step_dbscan(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), eps = input$Eps)
      } else {
        react$Recipe <- remove_step(react$Recipe, "dbscan")
      }
    }
  )
  ##########################################################################
  observeEvent(
    input$Eps,
    {
      if (input$Clusters) {
        rec <- react$Recipe
        if (!is.null(input$Eps)) {
          step <- get_step(react$Recipe, "dbscan")
          rec$steps[[step]] <- recipes:::update.step(rec$steps[[step]], eps = input$Eps)
        }
        react$Recipe <- rec
      }
    }
  )

  ##########################################################################
  ##### Clusters #####
  observeEvent(
    input$Centers,
    {
      if (input$Centers) {
        react$Recipe <- step_classdist(react$Recipe, all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"), class = input$Target)
      } else {
        react$Recipe <- remove_step(react$Recipe, "classdist")
      }
    }
  )

  ###########################################################################
  ##### Create new columns from Date columns #####
  observeEvent(
    {
      input$DateFeatures
      input$Cyclic
    },
    {
      react$Recipe <- remove_step(react$Recipe, c("cyclic"))
      react$Recipe <- remove_step(react$Recipe, c("date"))
      if (length(input$DateFeatures) > 0) {
        react$Recipe <- step_date(react$Recipe, has_type("date"), -has_role("case weight"), -has_role("presplit"), -has_role("group"), features = input$DateFeatures, label = TRUE, ordinal = TRUE)
        if (input$Cyclic) {
          react$Recipe <- step_cyclic(react$Recipe, ends_with("_dow"), ends_with("_month"), -all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
        }
      }
    },
    ignoreNULL = FALSE
  )

  ###########################################################################
  ##### Create new columns from ordered factor columns #####
  observeEvent(
    input$Cyclic,
    {
      react$Recipe <- remove_step(react$Recipe, c("cyclic"))
      if (input$Cyclic & length(input$DateFeatures) > 0) {
        react$Recipe <- step_cyclic(react$Recipe, ends_with("_dow"), ends_with("_month"), -all_numeric(), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
      }
    }
  )
  ###########################################################################
  getSomeTrainData <- reactive({
    d <- getTrainData()
    if (input$ID != "" & input$ID %in% colnames(d)) {
      rownames(d) <- d[, input$ID]
    }
    mrows <- getMaxRowsDebounced()
    if (nrow(d) <= mrows) {
      return(d)
    } else {
      rows <- sample(nrow(d), mrows)
      #preserve order
      rows <- sort(rows, decreasing = FALSE)
      return(d[rows,])
    }
  })

  ###########################################################################
  getPrepRecipe <- reactive({
    req(is(react$Recipe, "recipe"))
    tempRec <- step_naomit(react$Recipe, all_outcomes(), skip = TRUE)
    #safe guard the recipe
    if (any(tempRec$var_info$type == "date" & tempRec$var_info$role == "predictor")) {
      tempRec <- step_rm(tempRec, has_type("date"),-has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group"))
    }
    tryCatch({
      recipes::prep(tempRec, verbose = Verbose, retain = TRUE, training = getSomeTrainData())
    }, error = function(e) {
      print(e$message)
    })
  })

  ###########################################################################
  getPreProcTrain <- reactive({
    prepRecipe <- getPrepRecipe()
    req(class(prepRecipe) == "recipe")
    d <- recipes::juice(prepRecipe, everything(), composition = "data.frame")
    # converts >= 15 unique levels to character
    charCols <- which(allClass(d) == "factor")
    max <- nrow(d)
    for (col in charCols) {
      if (length(unique(d[,col])) >  getContinuousDebounced()[1]) {
        d[,col] <- as.character(d[,col])
      }
    }
    d
  })

  ###########################################################################
  output$RecipeErr <- renderInfoBox({
    prepRecipe <- getPrepRecipe()
    req(is(prepRecipe, "character")) 
    infoBox(title = "Recipe Error", value = prepRecipe, icon = icon("exclamation-circle"), color = "olive", fill = TRUE)
  })
  
  ###########################################################################
  getMissingCount <- reactive({
    d <- getPreProcTrain()
    d <- d[,!colnames(d) %in% getNonPredictors()]
    sum(is.na(d))
  })

  ###########################################################################
  output$MissPresent <- renderInfoBox({
    req(getMissingCount() > 0)
    text <- "Missing values are not all resolved"
    infoBox(title = "Missing values", value = text, icon = icon("stethoscope"), color = "olive", fill = TRUE)
  })
  
  getNominalCount <- reactive({
    d <- getPreProcTrain()
    d <- d[,!colnames(d) %in% c(input$Target, getNonPredictors())]
    noms <- unlist(lapply(d, FUN = is.factor))
    sum(noms)
  })
  
  ###########################################################################
  output$NominalPresent <- renderInfoBox({
    req(getNominalCount() > 0)
    text <- "Nominal variables are not all resolved"
    infoBox(title = "Nominal variables", value = text, icon = icon("calendar-alt"), color = "olive", fill = TRUE)
  })

  ###########################################################################
  getStringCount <- reactive({
    d <- getPreProcTrain()
    d <- d[,!colnames(d) %in% getNonPredictors()]
    chars <- unlist(lapply(d, FUN = is.character))
    sum(chars)
  })
  
  ###########################################################################
  output$StringPresent <- renderInfoBox({
    req(getStringCount() > 0)
    text <- "Character variables are not all resolved"
    infoBox(title = "Character variables", value = text, icon = icon("fire-extinguisher"), color = "olive", fill = TRUE)
  })


  ###########################################################################
  getNZVCount <- reactive({
    d <- getPreProcTrain()
    d <- d[,!colnames(d) %in% getNonPredictors()]
    cols <- caret::nearZeroVar(d, freqCut = 95/5, uniqueCut = 10, names = TRUE)
    length(cols)
  })  
  
  ###########################################################################
  output$NZVPresent <- renderInfoBox({
    cols <- getNZVCount()
    req(cols > 0)
    text <- paste(cols, collapse = ", ")
    infoBox(title = "Near zero variance present", value = text, icon = icon("bell"), color = "olive", fill = TRUE)
  })

  ###########################################################################
  getLinCombCount <- reactive({
    d <- getPreProcTrain()
    d <- d[, numericNames(d)]
    req(ncol(d) > 0)
    d <- d[,!colnames(d) %in% getNonPredictors()]
    d <- na.omit(d)
    lc <- list(remove = c())
    tryCatch(
      {
        zerovar <- diag(var(d)) == 0
        lc$remove <- zerovar
        lc <- caret::findLinearCombos(d)
      },
      error = function(e) {}
    )
    cols <- colnames(d)[lc$remove]
    length(cols)
  })
  
  ###########################################################################
  output$LinCombPresent <- renderInfoBox({
    cols <- getLinCombCount()
    req(cols > 0)
    text <- paste(cols, collapse = ", ")
    infoBox(title = "Linear combinations present", value = text, icon = icon("bell"), color = "olive", fill = TRUE)
  })
  
  ###########################################################################
  output$Imbalanced <- renderInfoBox({
    req(input$ProbType == "Classification")
    d <- getPreProcTrain()
    req(ncol(d) > 0, input$Target != "")
    d2 <- d[, input$Target, drop = TRUE]
    if (input$Weights != "") {
      dd <- data.frame( target = d[, input$Target, drop = TRUE], weight = d[, input$Weights, drop = TRUE])
      tab <- dplyr::count(x = dd, target, wt = weight)
      t <- tab$n
      names(t) <- as.character(tab$target)
    } else {
      t <- base::table(d2)
    }
    percRange <- (max(t) - min(t)) / sum(t) * 100
    req(percRange > 5)
    text <- paste0(input$Target, " is ", round(percRange), "% imbalanced")
    infoBox(title = "Outcome imbalance", value = text, icon = icon("balance-scale-right"), color = "olive", fill = TRUE)
  })

  getVORatio <- reactive({
    leaveOut <- c(getNonPredictors(), input$Target)
    p <- getPreProcTrain()
    p <- p[, !colnames(p) %in% leaveOut]
    d <- getTrainData()
    nrow(d) / ncol(p)
  })
  
  ###########################################################################
  output$VariablesRatio <- renderInfoBox({
    ratio <- getVORatio()
    req(ratio < 50)
    text <- paste("The ratio of observations to variables is", round(ratio))
    infoBox(title = "Inadequate observations", value = text, icon = icon("balance-scale-left"), color = "olive", fill = TRUE)
  })
  
  ###########################################################################
  output$VariablesPresent <- renderInfoBox({
    d <- getPreProcTrain()
    v <- colnames(d)
    v <- setdiff(v, c(input$Target, getNonPredictors()))
    text <- paste("There are", length(v), "predictor variables")
    infoBox(title = "After recipe processing", value = text, icon = icon("sort-amount-down-alt"), color = "olive", fill = TRUE)
  })
  
  ###########################################################################
  output$PreProcSummary <- renderPrint({
    print(getPrepRecipe(), form_width = 150)
  })

  ###########################################################################
  output$ProcessedDataSummary <- renderUI({
    summary <- summarytools::dfSummary(getPreProcTrain(), graph.magnif = 1.5)
    summarytools::view(summary,
                       method = 'render',
                       report.title = NA,
                       headings = FALSE,
                       bootstrap.css = TRUE,
                       footnote = NA,
                       max.tbl.height = 600,
                       collapse = 1,
                       silent = TRUE
    )
  })

  ###########################################################################
  output$PreProcTable <- DT::renderDataTable({
    d <- getPreProcTrain()
    dt <- DT::datatable(data = d, rownames = TRUE, selection = "none",
                        extensions = c('Scroller','FixedHeader'),
                        options = list(
                          scrollX = TRUE,
                          deferRender = TRUE,
                          scrollY = 540,
                          scroller = TRUE
                        ))
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
  getMethods <- reactive({
    input$RefreshModels
    updateCheckboxInput(session = session, inputId = "OutliersTag", value = FALSE)
    mi <- getModelInfo()

    # corrections to model info
    mi[["chaid"]]$tags <- c(mi[["chaid"]]$tags, "Categorical Predictors Only")
    mi[["plsRglm"]]$tags <- gsub(pattern  = "Generalized Linear Models", replacement = "Generalized Linear Model", x = mi[["plsRglm"]]$tags)
    Label <- vector(mode = "character", length = length(mi))
    Packages <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Packages[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    m <- data.frame(Model = names(mi), Label, Packages, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
    m[sample(nrow(m)),] # randomise the rows so the same methods are not always at the top
  })

  ###########################################################################
  getTags <- reactive({
    Tags <- getMethods()$Tags
    unique(unlist(strsplit(x = Tags, split = "<br/>", fixed = TRUE)))
  })

  ###########################################################################
  observe({
    tags <- getTags()
    updateSelectizeInput(session = session, inputId = "IncFeatures", choices = setdiff(tags, input$ExcFeatures), selected = input$IncFeatures)
    updateSelectizeInput(session = session, inputId = "ExcFeatures", choices = setdiff(tags, input$IncFeatures), selected = input$ExcFeatures)
  })

  ###########################################################################
  observe({
    req(input$ProbType)
    if (input$ProbType == "Classification") {
      showElement(id = "BiClass")
      showElement(id = "Probs")
    } else {
      hideElement(id = "BiClass")
      hideElement(id = "Probs")
    }
  })

  ###########################################################################
  observe({
    req(input$ProbType)
    req(!is.null(react$Prob2Class), length(react$Prob2Class) > 0)
    if (input$ProbType == "Classification" && react$Prob2Class) {
      updateSelectizeInput(session = session, inputId = "HypMetric", choices = biClassChoices, selected = biClassChoices[1])
    } else if (input$ProbType == "Classification") {
      updateSelectizeInput(session = session, inputId = "HypMetric", choices = multiClassChoices, selected = multiClassChoices[1])
    } else {
      updateSelectizeInput(session = session, inputId = "HypMetric", choices = regChoices, selected = regChoices[1])
    }
  })

  ###########################################################################
  getFiltMethods <- reactive({
    req(input$ProbType)
    d <- getMethods()
    if (input$AvailPackages) {
      d <- d[ !grepl(pattern = "<i class", d$Packages), ]
    }

    d <- switch(input$ProbType,
                "Classification" = d[ d$Classification != "", ],
                "Regression" = d[ d$Regression != "", ],
                "Any" = d
    )
    if (input$TwoClassTag) {
      d <- d[ !grepl(pattern = "Ordinal Outcomes", d$Tags, ignore.case = TRUE), ]
    } else {
      d <- d[ !grepl(pattern = "Two Class Only", d$Tags, ignore.case = TRUE), ]
    }

    d <- switch(input$PredictorsTag,
                "All Nominal" = d[ !grepl(pattern = "Binary Predictors Only", d$Tags, ignore.case = TRUE), ],
                "All Binary" = d[ !grepl(pattern = "Categorical Predictors Only", d$Tags, ignore.case = TRUE), ],
                "Mixture" = d[ !grepl(pattern = "Categorical Predictors Only|Binary Predictors Only", d$Tags, ignore.case = TRUE), ]
    )
    if (input$WeightingTag) {
      d <- d[ grepl(pattern = "Accepts Case Weights", d$Tags, ignore.case = TRUE), ]
    }
    if (input$OutliersTag) {
      d <- d[ grepl(pattern = "Robust", d$Tags, ignore.case = TRUE), ]
    }
    if (input$MissingTag) {
      d <- d[ grepl(pattern = "Handle Missing Predictor Data", d$Tags, ignore.case = TRUE), ]
    }
    if (input$NominalsTag) {
      d <- d[ grepl(pattern = "Tree-Based Model", d$Tags, ignore.case = TRUE), ]
    }
    if (input$LinCombTag | input$NZVTag) {
      if (input$ProbType == "Regression") {
        ols <- "Linear Regression"
      } else {
        ols <- "Linear Classify"
      }
      d <- d[ !grepl(pattern = ols, d$Tags, ignore.case = TRUE), ]
    }
    if (input$RatioTag) {
      d <- d[ grepl(pattern = "Implicit Feature Selection|Feature Selection Wrapper", d$Tags, ignore.case = TRUE), ]
    }
    
    if (input$ProbabilityTag) {
        d <- d[ d$ClassProb != "", ]
    }

    if (length(input$IncFeatures) > 0) {
      pat1 <- paste(sep = "", collapse = "|", input$IncFeatures, "(<br/>|$)")
      d <- d[ grepl(pattern = pat1, d$Tags), ]
    }
    if (length(input$ExcFeatures) > 0) {
      pat2 <- paste(collapse = "|", input$ExcFeatures, "(<br/>|$)")
      d <- d[ !grepl(pattern = pat2, d$Tags), ]
    }
    d
  })

  ###########################################################################
  observe({
    input$RefreshModels
    req(input$ProbType)
    if (input$ProbType == "Classification") {
      showElement(id = "ProbabilityTag")
      showElement(id = "TwoClassTag")
      ds <- getDataSummary()
      uniqCnt <- ds[rownames(ds) == input$Target, "uniqueness"]
      react$Prob2Class <- uniqCnt == 2
      updateCheckboxInput(session = session, inputId = "TwoClassTag", value = uniqCnt == 2)
      leaveOut <- c(input$Target, getNonPredictors())
      factors <- ds[!rownames(ds) %in% leaveOut, "factor"]
      binaries <- ds[!rownames(ds) %in% leaveOut, "binary"]
      if (all(factors)) {
        updateRadioButtons(session = session, inputId = "PredictorsTag", selected = "All Nominal")
      } else if (all(binaries)) {
        updateRadioButtons(session = session, inputId = "PredictorsTag", selected = "All Binary")
      } else {
        updateRadioButtons(session = session, inputId = "PredictorsTag", selected = "Mixture")
      }
    } else if (input$ProbType == "Regression") {
      hideElement(id = "ProbabilityTag")
      hideElement(id = "TwoClassTag")
      updateRadioButtons(session = session, inputId = "BiClass", selected = "Neutral")
      updateRadioButtons(session = session, inputId = "CatPredictors", selected = "Neutral")
    } else {
      showElement(id = "ProbabilityTag")
      showElement(id = "TwoClassTag")
    }
    # Weightings
    updateCheckboxInput(session = session, inputId = "WeightingTag", value = input$Weights != "")
    # missing values
    updateCheckboxInput(session = session, inputId = "MissingTag", value = (getMissingCount() > 0))
    # Nominals present
    updateCheckboxInput(session = session, inputId = "NominalsTag", value = (getNominalCount() > 0))
    # NZV present
    updateCheckboxInput(session = session, inputId = "NZVTag", value = (getNZVCount() > 0))
    # Linear Combinations present
    updateCheckboxInput(session = session, inputId = "LinCombTag", value = (getLinCombCount() > 0))
    # Var to Obs ratio
    updateCheckboxInput(session = session, inputId = "RatioTag", value = (getVORatio() < 50))
  })

  ###########################################################################
  output$Methods <- renderInfoBox({
    d <- getFiltMethods()
    infoBox(title = "Available Methods", value = nrow(d), icon = icon("align-justify"), color = "red", fill = TRUE)
  })

  ###########################################################################
  observeEvent(input$ChooseAll, {
    tableProxy <- dataTableProxy(outputId = "MethodsTable", session = session)
    if (length(react$MethodSet) > 0) {
      selectRows(proxy = tableProxy, selected = NULL) #this does not trigger the *_rows_selected event
      react$MethodSet <- c()
    } else {
      selectRows(proxy = tableProxy, selected = 1:nrow(getFiltMethods()))
    }
  })

  ###########################################################################
  output$MethodsTable <- DT::renderDataTable({
    d <- getFiltMethods()
    if (input$ProbType %in% c("Classification","Regression")) {
      d <- d[, !colnames(d) %in% c("Classification","Regression")]
    }
    if (input$ProbType == "Regression") {
      d$ClassProbs <- NULL
    }
    d$Label <- str_wrap(d$Label, width = 25)
    d$Label <- gsub(d$Label, pattern = "\n", replacement = "<p/>", fixed = TRUE)
    preselection <- which(d[, "Model"] %in% isolate(react$MethodSet)) #use isolate because this is only setting the on-loading selected methods
    DT::datatable(data = d, rownames = FALSE, selection = list(mode = "multiple", selected = preselection, target = "row"),
                  extensions = c('Scroller','FixedHeader'),
                  escape = FALSE,
                  options = list(
                    scrollX = TRUE,
                    deferRender = TRUE,
                    scrollY = 540,
                    scroller = TRUE
                  ))
  })

  ###########################################################################
  observeEvent(input$MethodsTable_rows_selected, {
    d <- getFiltMethods()
    methodSet <- d[input$MethodsTable_rows_selected, "Model"]
    if (!setequal(react$MethodSet, methodSet)) {
      react$MethodSet <- methodSet
    }
  })

  ###########################################################################
  observeEvent(react$MethodSet, {
    d <- getFiltMethods()
    methodSet <- d[input$MethodsTable_rows_selected, "Model"]
    if (!setequal(react$MethodSet, methodSet)) {
      tableProxy <- dataTableProxy(outputId = "MethodsTable", session = session)
      selected <- which(d[,"Model"] %in% react$MethodSet)
      selectRows(proxy = tableProxy, selected = selected)
    }
  })

  ###########################################################################
  getSelectedMethodTable <- reactive({
    d <- getFiltMethods()
    d[d[, "Model"] %in% react$MethodSet, ]
  })

  ###########################################################################
  output$SelectedMethodsTable <- renderTable({
    getSelectedMethodTable()[, c("Model", "Label")]
  })

  ###########################################################################
  output$SelWarn <- renderInfoBox({
    d <- getSelectedMethodTable()
    if (nrow(d) == 0) {
      text <- "No methods selected yet"
    } else if (nrow(d[grepl(pattern = "<i class", d$Packages),]) > 0) {
      text <- "There are packages that need to be installed"
    } else {
      req(FALSE)
    }
    infoBox(title = "Method Selection", value = text, icon = icon("exclamation-circle"), fill = TRUE, color = "red")
  })

  ###########################################################################
  getMethodsDummyVars <- reactive({
    data <- getFiltMethods()
    req(nrow(data) > 1)
    mat1 <- matrix(0, nrow = nrow(data), ncol = 3)
    colnames(mat1) <- c("Regression", "Classification","ClassProbs")
    mat1[,1] <- ifelse(data$Regression != "", 1, 0)
    mat1[,2] <- ifelse(data$Classification != "", 1, 0)
    mat1[,3] <- ifelse(data$ClassProbs == "", 0, 1)
    tags <- unique(unlist(strsplit(x = data$Tags, split = "<br/>")))
    mat2 <- matrix(0, nrow = nrow(data), ncol = length(tags))
    colnames(mat2) <- tags
    for (t in 1:length(tags)) {
      mat2[,t] <- ifelse( grepl(pattern = tags[t], x = data$Tags, fixed = TRUE), 1, 0 )
    }
    mat <- cbind(mat1,mat2)
    rownames(mat) <- data$Model
    mat
  })

  ###########################################################################
  observe({
    d <- getSelectedMethodTable()
    shinyjs::toggle(id = "Suggest", condition = nrow(d) >= 3)
    if (nrow(d) == 0) {
      shinyjs::hideElement(id = "Install")
    } else if (nrow(d[grepl(pattern = "<i class", d$Package),]) > 0) {
      shinyjs::showElement(id = "Install")
    } else {
      shinyjs::hideElement(id = "Install")
    }
  })

  ###########################################################################
  getSelectedMethods <- reactive({
    ml <- c()
    if (length(react$MethodSet) > 0) {
      ml <- react$MethodSet
    }
    if (input$NullModel) {
      ml <- union("null", ml) # Add the null model
    }
    ml
  })

  ###########################################################################
  observeEvent(input$Install, {
    d <- getSelectedMethods()
    req(nrow(d) > 0)
    shinyjs::hideElement(id = "Install")
    librs <- unlist(strsplit(d$Library, split = "<br/>"))
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
    req(length(react$MethodSet) > 2)
    b <- as.data.frame(getMethodsDummyVars())
    picked <- b[rownames(b) %in% react$MethodSet,]
    suggested <- caret::maxDissim(a = picked, b = b, method = "euclidean", n = 1)
    react$MethodSet <- union(react$MethodSet, rownames(b)[suggested])
  })

  ###########################################################################
  get2dData0 <- reactive({
    data <- getMethodsDummyVars()
    #calculate the distance of the tags (as binary)
    d1 <- proxy::dist(data, method = "euclidean")
    #calculate the distance between the names (too avoid overlap)
    d2 <- stringdist::stringdistmatrix(a = rownames(data), method = "jw")
    d <- d1 + 3 * d2
    # reduce to 2 PC and plot in xy
    d2d <- cmdscale(d, k = 2)
    d2d <- as.data.frame(d2d)
    req(ncol(d2d) == 2) # can be less than 2 so check is needed
    colnames(d2d) <- c("PC1", "PC2")
    d2d$Method <- rownames(d2d)
    d2d$Type <- ifelse(data[, "Regression"] == 1 & data[,"Classification"] == 1, "Both", ifelse(data[,"Regression"] == 1, "Regression", "Classification"))
    d2d
  })

  ###########################################################################
  get2Ddata <- reactive({
    d2d <- get2dData0()
    if (input$ProbType != "Any") {
      d2d$Selected <- d2d$Method %in% react$MethodSet
    }
    d2d
  })

  ###########################################################################
  output$D2plot <- renderPlot({
    data <- get2Ddata()
    P <- ggplot(data = data, mapping = aes(x = PC1, y = PC2)) +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    if (input$ProbType == "Any") {
      P <- P + geom_text(mapping = aes(label = Method, colour = Type), check_overlap = FALSE)
    } else {
      P <- P + geom_text(mapping = aes(label = Method, colour = Selected), check_overlap = FALSE)
    }
    P
  }, bg = "transparent")

  ###########################################################################
  output$MapInfo <- renderUI({
    # This code suits Chrome (and other) browser. It does NOT suit the RStudio browser
    d2d <- get2Ddata()
    hover <- input$MapHover
    point <- nearPoints(d2d, hover, threshold = 15, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    top_px <- (top_px + 60)
    style <- paste0("position:absolute; z-index:100; background-color: rgba(225, 0, 0, 0.70); left:", left_px + 15, "px; top:", top_px + 2, "px;")
    # actual tooltip created as wellPanel
    data <- getFiltMethods()
    method <- data[data$Model == point$Method,]
    wellPanel(
      style = style,
      HTML(paste0(
        tags$b(point$Method), "<br/>",
        method$Label,
        "<ul><li>", gsub(method$Tags, pattern = "<br/>", replacement = "</li><li>", fixed = TRUE), "</li></ul>")
      )
    )
  })
  
  ###########################################################################
  # output$MapPos <- renderUI({
  #   hover <- input$MapClick
  #   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  #   left_px <- (left_px + 8) * 0.8
  #   top_px <- (top_px + 60) * 0.8
  #   style <- paste0("position:absolute; z-index:100; background-color: rgba(225, 0, 0, 0.70); left:", left_px, "px; top:", top_px, "px;")
  #   wellPanel(style = style, paste(left_px, top_px))
  # })

  ###########################################################################
  observeEvent(input$MapDblClick, {
    d2d <- get2Ddata()
    point <- nearPoints(d2d, input$MapDblClick, threshold = 15, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)
    isolate({
      if (point$Method %in% react$MethodSet) {
        # remove the method
        methodset <- setdiff(react$MethodSet, point$Method)
      } else {
        # add the method
        methodset <- c(react$MethodSet, point$Method)
      }
    })
    react$MethodSet <- methodset
  })

  ###########################################################################
  observe({
    shinyjs::toggle(id = "Number", condition = input$Method != "none")
    shinyjs::toggle(id = "Repeats", condition = grepl("[d_]cv$", input$Method))
    shinyjs::toggle(id = "Balance", condition = input$ProbType == "Classification")
  })

  ###########################################################################
  observe({
    req(input$Method)
    if (grepl("cv", input$Method, ignore.case = TRUE)) {
      updateNumericInput(session = session, inputId = "Number", label = "Number of CV folds", value = 10)
    } else {
      updateNumericInput(session = session, inputId = "Number", label = "Number of iterations", value = 25)
    }
  })

  ###########################################################################
  observeEvent(input$Groups, {
    if (input$Groups != "") {
      updateSelectizeInput(session = session, inputId = "Method", label = "Method", choices = list("LGOCV",'adaptive_LGOCV'), selected = "LGOCV")
    } else {
      updateSelectizeInput(session = session, inputId = "Method", label = "Method", choices = list("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "adaptive_cv", "adaptive_boot"), selected = "boot")
    }
  })

  ###########################################################################
  getIndex <- reactive({
    req(input$Method, input$ProbType)
    data <- getTrainData()
    req(isRoleValid(input$Target, data))
    y <- data[,input$Target]
    req(length(y) > 0)
    #Prepare the resampling index (considering the effect on ensembling)
    if (any(input$Method %in% c("boot632","boot","adaptive_boot"))) {
      index <- caret::createResample(y, times = input$Number, list = TRUE)
      react$AllowEnsemble <- TRUE
    } else if (any(input$Method %in% c("cv","adaptive_cv"))) {
      index  <- caret::createFolds(y, k = input$Number, list = TRUE, returnTrain = TRUE)
      react$AllowEnsemble <- TRUE
    } else if (input$Method == "repeatedcv") {
      index <- caret::createMultiFolds(y, k = input$Number, times = input$Repeats)
      react$AllowEnsemble <- TRUE
    } else if (any(input$Method %in% c("LGOCV","adaptive_LGOCV"))) {
      req(input$Groups != "")
      group <- as.factor(data[, input$Groups])
      index <- caret::groupKFold(group = group, k = min( c(input$Number, length(unique(group))) ))
      react$AllowEnsemble <- TRUE
    } else {
      #c("optimism_boot", "boot_all", "LOOCV")
      react$AllowEnsemble <- FALSE
      index <- NULL
    }
    index
  })

  ###########################################################################
  output$NearZeroVarTest <- renderInfoBox({
    data <- getData()
    index <- getIndex()
    
    wrap2 <- function(index, x) {
      x <- (x[index, , drop = FALSE])
      colnames(x)[apply(x, 2, function(x) length(unique(na.omit(x))) < 2)]
    }
    
    columns <- unique(unlist(lapply(index, wrap2, x = data)))
    req(length(columns) > 0)
    text <- paste(columns, collapse = ", ")
    infoBox(title = "Zero variance variables in one or more resample", value = text, icon = icon("dice"), color = "yellow", fill = TRUE)
  })
  
  ###########################################################################
  getTrainControl <- reactive({
    req(input$Method)
    req(input$ProbType)
    req(input$Target)
    if ((input$ProbType == "Regression" || react$Prob2Class) && input$HypMetric %in% c("Accuracy", "Kappa","RMSE", "Rsquared", "MAE")) {
      summFunc <- defaultSummary
    } else if (input$HypMetric %in% c("ROC","Sens","Spec")) {
      summFunc <- twoClassSummary
    } else if (react$Prob2Class && input$HypMetric %in% c("AUC","Precision","Recall", "F")) {
      summFunc <- prSummary
    } else if (input$HypMetric %in% c("Accuracy","Kappa","Mean_F1", "Mean_Sensitivity",
                                      "Mean_Specificity", "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value",
                                      "Mean_Precision", "Mean_Recall", "Mean_Detection_Rate",
                                      "Mean_Balanced_Accuracy","logLoss", "AUC", "prAUC")) {
      summFunc <- multiClassSummary
    } else {
      req(FALSE)
    }
    trainControl(
      method = input$Method,
      number = input$Number,
      repeats = ifelse(grepl("[d_]cv$", input$Method), input$Repeats, NA),
      search = input$Search,
      verboseIter = TRUE,
      returnData = TRUE,
      returnResamp = "final",
      savePredictions = TRUE,
      classProbs = input$ProbType == "Classification",
      summaryFunction = summFunc,
      selectionFunction = input$SelectionFunc,
      preProcOptions = NULL, # Not allowed with Recipes
      sampling = NULL,
      index = getIndex(),
      #   timingSamps = 1000,  # this causes crashes relating to unknown variable names being supplied to predict()
      trim = FALSE,
      allowParallel = input$Parallel
    )
  })

  ###########################################################################
  observe({
    shinyjs::toggle(id = "ResultsFor", condition = input$ResultsFor != "")
  })

  ###########################################################################
  # Reset any trained models if the train-data or reset button changes
  observeEvent( #TODO review this
    {
      getTrainData()
      input$Reset
    },
    {
      methods <- getSelectedMethods()
      req(length(methods) > 0)
      d <- getTrainData()
      req(nrow(d) > 0)
      modelSet <- react$ModelSet
      req(length(modelSet) > 0)
      showNotification("Resetting trained models", duration = 5, session = session)
      updateRadioButtons(session = session, inputId = "ResultsFor", choices = list(""), selected = "")
      for (m in 1:length(modelSet)) {
        modelSet[[m]] <- NULL
      }
      react$ModelSet <- modelSet
    })

  ###########################################################################
  observeEvent(input$UndoModel, {
    req(length(react$ModelSet) > 0)
    req(input$ResultsFor)
    react$ModelSet[[input$ResultsFor]] <- NULL
    showNotification(paste("Resetting model", input$ResultsFor), duration = 5, session = session)
  })

  ###########################################################################
  observe({
    if (length(react$MethodSet) == 0) {
      shinyjs::disable(id = "Train")
    } else {
      shinyjs::enable(id = "Train")
    }
  })

  ###########################################################################
  observeEvent(
    input$Train, 
    {
      req(input$Target)
      methods <- getSelectedMethods()
      req(length(methods) > 0)
      recipe <- react$Recipe
      TrainData <- getTrainData()
      # remove any blank outcomes as the last operation in order to benefit from semi-supervised learning
      recipe <- step_naomit(recipe, all_outcomes(), skip = TRUE)
      
      if (any(recipe$var_info$type == "date" & recipe$var_info$role == "predictor")) {
        recipe <- step_rm(recipe, has_type("date"), -has_role("outcome"), -has_role("case weight"), -has_role("case id"), -has_role("presplit"), -has_role("group")) # remove any date variables (only their transforms should exist)
      }
      req(length(methods) > 0)
      count <- 0
      done <- c()
      assign("last.warning", NULL, envir = baseenv())
      for (method in methods) {
        if (input$Pause) {
          showNotification("Training was stopped early", duration = 10, session = session)
          shinyBS::updateButton(session = session, inputId = "Pause", value = FALSE)
          break
        }
        if (!is.null(isolate(react$ModelSet[[method]]))) {
          count <- count + 1
          done <- c(done, method)
          next
        }
        shinyjs::disable(id = "Train")
        prog <- (count + 0.2) / length(methods)
        assign("last.warning", NULL, envir = baseenv())
        parallelMode <- isolate(input$Parallel)
        withProgress({
          setProgress(value = prog, message = paste("Training method", method) )
          obj <- NA
          if (parallelMode) {
            obj <- startParallel(method)
            result <- tryCatch({
              caret::train(x = recipe, data = TrainData, method = method, metric = input$HypMetric,
                           trControl = getTrainControl(), tuneLength = ifelse(input$Method == "none", 1, input$TuneLength),
                           maximize = !(input$HypMetric %in% minimiseMetric))
            },
            error = function(e) {
              return(e)
            })
            react$Log[[method]] <- stopParallel(obj)
            if (is(result, "error")) {
              react$ModelSet[[method]] <- "Model failed to train"
              react$Warn[[method]] <- c(result$message, warnings()) 
            } else {
              react$ModelSet[[method]] <- result
              react$Warn[[method]] <- NULL
            }
          } else {
            outputStream <- utils::capture.output( {
              result <- tryCatch({
                caret::train(x = recipe, data = TrainData, method = method, metric = input$HypMetric,
                             trControl = getTrainControl(), tuneLength = ifelse(input$Method == "none", 1, input$TuneLength),
                             maximize = !(input$HypMetric %in% minimiseMetric))
              },
              error = function(e) {
                return(e)
              })
            }, type = "message")
            if (is(result, "error")) {
              react$ModelSet[[method]] <- "Model failed to train"
              react$Log[[method]] <- ifelse(length(outputStream) == 0, "", outputStream)
              react$Warn[[method]] <- c(result$message, warnings()) 
            } else {
              react$ModelSet[[method]] <- result
              react$Log[[method]] <- outputStream
              react$Warn[[method]] <- warnings()
            }
          }
        })
        
        done <- c(done, method)
        shinyjs::showElement(id = "ResultsFor")
        updateRadioButtons(session = session, inputId = "ResultsFor", choices = done, selected = method)
        shinyjs::enable(id = "Train")
        shinyjs::click(id = "Train")
        return()  # early exit if something was trained (successfully or otherwise)
      } #end of for loop
      # to get here there must be no models to train in this pass
      updateRadioButtons(session = session, inputId = "ResultsFor", choices = done, selected = done[length(done)])
      shinyjs::enable(id = "Train")
      showNotification("Finished training", duration = 10, session = session)
    }, 
    priority = -10, 
    ignoreInit = TRUE, 
    ignoreNULL = TRUE
  )
  
  ###########################################################################
  output$CurrentModel <- renderText({
    req(input$ResultsFor)
    input$ResultsFor
  })

  ###########################################################################
  output$TrainLog <- renderText({
    req(input$ResultsFor, react$Log)
    mess <- react$Log[[input$ResultsFor]]
    req(mess)
    paste(collapse = "\n", mess)
  })
  
  ###########################################################################
  output$TrainWarn <- renderText({
    req(input$ResultsFor, react$Warn)
    mess <- react$Warn[[input$ResultsFor]]
    req(mess)
    paste(collapse = "\n", mess)
  })
  
  ###########################################################################
  output$TrainModelRawPlot <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, is(mod, "train") | is(mod, "caretStack"))
    req(mod$method != "null")
    plot(mod$finalModel)
  }, bg = "transparent")

  ###########################################################################
  getTrainedRecipe <- reactive({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    req(mod$method != "null")
    mod$recipe
  })

  ###########################################################################
  output$TrainedRecipe <- renderPrint({
    getTrainedRecipe()
  })

  ###########################################################################
  output$RecipeSummary <- renderTable({
    recip <- getTrainedRecipe()
    req(recip)
    as.data.frame(summary(recip))
  })

  # ###########################################################################
  # output$Breakdown <- renderPlot({
  #   req(input$ResultsFor)
  #   mod <- react$ModelSet[[input$ResultsFor]]
  #   req(mod, !inherits(mod, c("character")))
  #   if (is(mod, "caretStack")) {
  #     mod <- mod$ens_model
  #   }
  #   req(mod$method != "null")
  #   testCase <- na.omit(getTestData())[1,]
  #   explain <- breakDown::broken(mod, new_observation = testCase, data = getTrainData())
  #   plot(explain) + ggtitle(paste("Breakdown plot for", react$ModelSet[[input$ResultsFor]]))
  # })

  ###########################################################################
  output$VarImp <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
    req(mod$method != "null")
    trellis.par.set(caretTheme())
    # Not every model can produce a Variable-Importance plot so handle silently
    try({
      plot(varImp(mod), main = paste(sep = " - ", "Variable Importance", input$ResultsFor))
    }, silent = TRUE)
  }, bg = "transparent")

  ###########################################################################
  output$Optimize <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    trellis.par.set(caretTheme())
    try({
      plot(mod, main = paste(sep = " - ", "Hyper-parameter optimisation", input$ResultsFor))
    }, silent = TRUE)
  }, bg = "transparent")

  ###########################################################################
  output$Hyperparams <- renderTable({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(is(mod, "train"))
    xtable(mod$bestTune, caption = "Optimum hyper-parameters")
  })

  ###########################################################################
  output$Hyperparams2 <- renderTable({
    req(input$SelectedModel )
    mod <- react$ModelSet[[input$SelectedModel]]
    req(is(mod, "train"))
    xtable(mod$bestTune, caption = "Optimum hyper-parameters")
  }, digits = 4)
  
  ###########################################################################
  output$Characteristics <- renderTable({
    req(input$ResultsFor, input$ResultsFor != "")
    m <- getMethods()
    req(!is(m, "character"))
    if (grepl(pattern = "+", input$ResultsFor, fixed = TRUE)) {
      xtable(data.frame(tags = "Ensemble model"), caption = "Method characteristics")
    } else {
      m <- getMethods()
      Tags <- m[m$Model ==  input$ResultsFor, "Tags", drop = TRUE]
      tags <- unlist(strsplit(x = Tags, split = "<br/>", fixed = TRUE))
      xtable(data.frame(tags), caption = "Method characteristics")
    }
  })

  ###########################################################################
  output$ModelCoef <- renderPrint({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, c("character")))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
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
  observe({
    req(input$Target != "")
    d <- getSomeData()
    req(d)
    req(input$Target %in% colnames(d))
    if (input$ProbType == "Classification") {
      Levfreq <- sort(table(d[, input$Target]), decreasing = FALSE)
      updateSelectizeInput(session = session, inputId = "Positive", choices = as.list(names(Levfreq)), selected = as.list(names(Levfreq[1])))
    }
  })

  ###########################################################################
  getConfusionMatrix <- reactive({
    req(input$ProbType == "Classification", input$Target, input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !is(mod, "character"))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
    data <- getModelHoldOutResults(mod)
    xtab <- table(data$obs, data$pred)
    caret::confusionMatrix(xtab, positive = input$Positive, mode = "everything")
  })

  ###########################################################################
  output$ModelTrainPlot <- renderPlot({
    req(input$Target, input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !is(mod, "character"))
    if (input$ProbType == "Regression") {
      if (is(mod, "caretStack")) {
        mod <- mod$ens_model
      }
      d <- getModelHoldOutResults(mod)
      rang <- range(c(d$obs, d$pred))
      plot <- ggplot(data = d) +
        geom_point(mapping = aes(x = pred, y = obs)) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "Resampled results", y = "actual", x = "predicted") +
        coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
    } else if (input$ProbType == "Classification") {
      cm <- getConfusionMatrix()$table
      data <- as.data.frame(cm)
      colnames(data) <- c("Prediction","Reference", "Freq")
      data$missclassified <- data$Prediction != data$Reference
      plot <- ggplot(data = data, mapping = aes(y = Freq, axis1 = Prediction, axis2 = Reference, label = after_stat(stratum))) +
        ggalluvial::geom_alluvium(aes(fill = missclassified, colour = missclassified), show.legend = TRUE) +
        ggalluvial::geom_stratum(width = 0.2) +
        geom_text(stat = "stratum", reverse = TRUE) +
        scale_x_discrete(limits = c("Prediction", "Actual"), expand = c(0.0, 0.0)) +
        ggtitle("Resampled confusion matrix") +
        scale_fill_manual(values = c("green","red")) +
        theme(plot.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              legend.position = "bottom")
    }
    #plotly(plot) #GGalluvial is not plotly ready
    plot
  }, bg = "transparent")

  ###########################################################################
  output$RocCurvePlot <- renderPlot({
    ds <- getDataSummary()
    req(input$ResultsFor, input$Target != "")
    shinyjs::toggle(id = "ROCPlot", condition = input$ProbType == "Classification" & any(rownames(ds)[ds$uniqueness == 2] == input$Target))
    req(input$ProbType == "Classification" & any(rownames(ds)[ds$uniqueness == 2] == input$Target))
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod)
    req(!is(mod, "character"))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
    data <- getModelHoldOutResults(mod)
    req(input$Positive %in% colnames(data))
    req(nlevels(data$obs) == 2)
    data$M <- data[, input$Positive]
    data$D <- as.numeric(data$obs == input$Positive)
    g <- ggplot(data, aes(m = M, d = D)) +
      geom_roc(n.cuts = 0) + #not plotly compatible yet
      geom_rocci() + # not plotly compatible yet
      coord_equal() + 
      style_roc() +
      labs(title = "Receiver Operating Characteristic (ROC) Curve") +
      theme(plot.background = element_blank())
    try({
      g <- g + annotate("text", x = 0.75, y = 0.25, label = paste("AuC =", round((calc_auc(g))$AUC, 2)))
    }, silent = TRUE)
    g 
  }, bg = "transparent")

  ###########################################################################
  output$ClassStats <- renderPrint({
    req(input$ProbType == "Classification")
    cm <- getConfusionMatrix()
    req(cm)
    print(cm)
  })

  ###########################################################################
  output$Resamples <- renderTable({
    req(length(react$ModelSet) > 0, input$Target, input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(mod, !inherits(mod, "character"))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
    results <- dplyr::inner_join(mod$results, mod$bestTune, by = colnames(mod$bestTune))
    results <- results[, colnames(mod$results) %in% mod$perfNames]
    good <- !apply(results, MARGIN = 2, FUN = is.na)
    results2 <- results[, good, drop = FALSE]
    miss <- sum(is.na(mod$pred$pred))
    if (miss > 0) {
      results2$`Missing Pred(%)` <- miss / length(mod$pred$pred) * 100
    }
    tryCatch({
      results2$`Train Obs` <- length(fitted(mod))
    }, error = function(e) {
      results2$`Train Obs` <- length(fitted(mod$finalModel))
    })
    req(ncol(results2) > 0)
    results2
  }, digits = 2, width = "100%")

  ###########################################################################
  output$Resamples2 <- renderTable({
    req(length(react$ModelSet) > 0, input$Target, input$SelectedModel)
    mod <- react$ModelSet[[input$SelectedModel]]
    req(mod, !inherits(mod, "character"))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
    results <- dplyr::inner_join(mod$results, mod$bestTune, by = colnames(mod$bestTune))
    results <- results[, colnames(mod$results) %in% mod$perfNames]
    good <- !apply(results, MARGIN = 2, FUN = is.na)
    results2 <- results[, good, drop = FALSE]
    miss <- sum(is.na(mod$pred$pred))
    if (miss > 0) {
      results2$`Missing Pred(%)` <- miss / length(mod$pred$pred) * 100
    }
    tryCatch({
      results2$`Train Obs` <- length(fitted(mod))
    }, error = function(e) {
      results2$`Train Obs` <- length(fitted(mod$finalModel))
    })
    req(ncol(results2) > 0)
    results2
  }, digits = 2, width = "100%")
  
  ###########################################################################
  output$Selection <- renderPlot({
    mods <- getResampledModels()
    if (length(mods$metrics) == 0) {
      alert("No metrics available to plot")
      req(FALSE)
    }
    if (!input$NullModel && any(mods$models == "null")) {
      mods$models <- mods$models[mods$models != "null"]
      nullCols <- grepl(pattern = "^null~", x = colnames(mods$values))
      mods$values <- mods$values[, !nullCols]
    }
    req(length(mods$models) > 0)
    if (input$NullNormalise & "null" %in% mods$models) {
      actualNames <- colnames(mods$values)
      # Normalise the various hyper-metrics using null model
      for (metric in unique(c(regChoices, multiClassChoices, biClassChoices))) {
        col <- paste(sep = "~", "null", metric)
        if (col %in% actualNames) {
          nullMetric <- mean(mods$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in mods$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                mods$values[, mcol] <- mods$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
      if (!input$HideTimings) {
        # Normalise the prediction timings per 1000
        nullMetric <- mean(mods$values[, "null~logTiming"], na.rm = TRUE)
        for (model in mods$models) {
          mcol <- paste(sep = "~", model, "logTiming")
          if (mcol %in% actualNames) {
            mcol <- paste(sep = "~", model, metric)
            if (mcol %in% actualNames) {
              mods$values[, mcol] <- mods$values[, mcol] / nullMetric
            }
          }
        }
      }
    }

    #hide results worse than null model
    subset <- rep(TRUE, length(mods$models))
    if (input$HideWorse & "null" %in% mods$models) {
       actualNames <- colnames(mods$values)
       col <- paste(sep = "~", "null", input$HypMetric)
       if (col %in% actualNames) {
         nullMetric <- mean(mods$values[, col], na.rm = TRUE)
         if (!is.na(nullMetric)) {
           m <- 0
           for (model in mods$models) {
             m <- m + 1
             mcol <- paste(sep = "~", model, input$HypMetric)
             if (mcol %in% actualNames) {
               subset[m] <- ifelse(input$HypMetric %in% minimiseMetric, mean(mods$values[, mcol], na.rm = TRUE) <= nullMetric, mean(mods$values[, mcol], na.rm = TRUE) >= nullMetric)
             }
           }
         }
       }
    }

    if (input$ExcludeNull) {
      subset <- subset & mods$models != "null"
    }

    # plot metrics
    req(any(subset))
    trellis.par.set(caretTheme())
    bwplot(mods, models = mods$models[subset], notch = input$ShowNotches)   #metric = "RMSE", main =""
  }, bg = "transparent")

  
  ###########################################################################
  output$ResidualCorr <- renderPlot({
    rmodels <- getResampledModels()
    # remove any ensembles (i.e. not Recipe classes) or null model
    for (name in rmodels$models) {
      if (is(react$ModelSet[[name]], "caretStack") | (name == "null" & input$ExcludeNull)) {
        rmodels$models <- rmodels$models[rmodels$models != name]
        rmodels$values <- rmodels$values[, !grepl(pattern = paste0("^", name, "~"), x = colnames(rmodels$values))]
      }
    }
    
    req(length(rmodels$models) > 1)
    data <- caret::resamples(react$ModelSet[rmodels$models])
    data <- data$values[, grep(paste("~", input$HypMetric, sep = ""), names(x$values))]
    colnames(data) <- gsub(paste("~", input$HypMetric, sep = ""), "", colnames(data))
    if (input$CorMethod == "Distance") {
      corr <- cor.distance(data)
      title <- "Distance Correlation"
    } else if (input$CorMethod == "Predictive Power") {
      corr <- getPPS()
      title <- "Predictive Power"
    } else {
      corr <- cor(data, method = tolower(input$CorMethod), use = "pairwise.complete.obs")
      if (input$CorAbs) {
        corr <- abs(corr)
        title <- paste0(input$CorMethod," (absolute) Correlation")
      } else {
        title <- paste0(input$CorMethod, " Correlation")
      }
    }
    p <- if (input$CorGrouping == "none") {
      ggcorrplot(corr = corr, method = "square", hc.order = FALSE, type = "full", lab = TRUE,
                 title = title, show.diag = FALSE, show.legend = FALSE)
    } else {
      title <- paste(title,"Ordered by", input$CorGrouping)
      ggcorrplot(corr = corr, method = "square", hc.order = TRUE, type = "full", lab = TRUE,
                 title = title, show.diag = FALSE, show.legend = FALSE, hc.method = input$CorGrouping)
    }
    plotly(p, tooltip = c("Model1","Model2","value"))
  })
  
  ###########################################################################
  output$ModelTree <- renderPlot({
    rmodels <- getResampledModels()
    # remove any ensembles (i.e. not Recipe classes) or null model
    for (name in rmodels$models) {
      if (is(react$ModelSet[[name]], "caretStack") | (name == "null" & input$ExcludeNull)) {
        rmodels$models <- rmodels$models[rmodels$models != name]
        rmodels$values <- rmodels$values[, !grepl(pattern = paste0("^", name, "~"), x = colnames(rmodels$values))]
      }
    }
    req(length(rmodels$models) > 2)
    plot(caret::cluster(rmodels), sub = "")
  }, bg = "transparent")

  ###########################################################################
  observe({
    input$Reset
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
    updateRadioButtons(session = session, inputId = "SelectedModel", choices = as.list(mNames), selected = mNames[[1]])
    shinyjs::toggle(id = "SelectedModel", condition = mNames[[1]] != "")
  })

  ###########################################################################
  isTrainValid <- function(x) {
    !is.null(x) && (is(x, "train") || is(x, "caretStack")) && is.function(x$modelInfo$prob)
  }

  ###########################################################################
  observe({
    if (length(react$ModelSet) == 0) {
      shinyjs::hideElement(id = "Ensemble")
      updateSelectizeInput(session = session, inputId = "Ensemble", choices = list(""), selected = "")
    } else if (input$ProbType == "Classification") {
      ok <- sapply(react$ModelSet, isTrainValid)
      mNames <- names(react$ModelSet)[ok] # Should not be able to select models that have no class probs
      mNames <- mNames[mNames != "null"]
      shinyjs::toggleElement(id = "Ensemble", condition = length(mNames) > 1 & react$AllowEnsemble)
      updateSelectizeInput(session = session, inputId = "Ensemble", choices = as.list(mNames), selected = "")
    } else {
      mNames <- names(react$ModelSet)
      mNames <- mNames[mNames != "null"]
      mNames <- mNames[!grepl(pattern = "\\+",x = mNames)]
      shinyjs::toggleElement(id = "Ensemble", condition = length(mNames) > 1 & react$AllowEnsemble)
      updateSelectizeInput(session = session, inputId = "Ensemble", choices = as.list(mNames), selected = "")
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
    prog <- (length(react$ModelSet) + 0.2) / (length(react$ModelSet) + 1)
    method <- paste(collapse = "+", input$Ensemble)
    ensembModels <- as.caretList(react$ModelSet[input$Ensemble])
    updateTabItems(session = session, inputId = "Navbar", selected = "Train")
    assign("last.warning", NULL, envir = baseenv())
    showNotification(id = method, ui = paste("Training ensemble", method), duration = NULL)
    warns <- utils::capture.output( {
      output <- utils::capture.output( {
        result <- tryCatch({
          ensControl <- getTrainControl()
          ensControl$index <- NULL  ## documented as necessary. See https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
          caretEnsemble::caretStack(all.models = ensembModels, method = "glm", trControl = ensControl, metric = input$HypMetric, tuneLength = input$TuneLength)
        }, error = function(e) {return(e)})
      }, type = "output")
    }, type = "message")

    if (is(result, "error")) {
      react$ModelSet[[method]] <- "Model failed to train"
      react$Log[[method]] <- c(output, unique(warns), result$message) #, rlang::last_error())
    } else {
      react$ModelSet[[method]] <- result
      react$Log[[method]] <- c(output, unique(warns))
    }
    updateRadioButtons(session = session, inputId = "ResultsFor", choices = as.list(names(react$ModelSet)), selected = method)
    removeNotification(id = method)
    showNotification(ui = "Finished training")
    shinyjs::enable(id = "Train")
  })

  # ###########################################################################
  # getExplanation <- reactive({
  #   req(input$Target, input$SelectedModel)
  #   mod <- react$ModelSet[[input$SelectedModel]]
  #   req(!inherits(mod, "character"))
  #   data <- getTestData()
  #   isY <- colnames(data) == input$Target
  #   DALEX::explain(model = mod, data = data[, !isY], y = data[,isY])
  # })

  ##########################################################################
  getGraduatingModel <- reactive({
    mod <- react$ModelSet[[input$SelectedModel]]
    req(mod, !inherits(mod, "character"))
    mod
  }) 
  ###########################################################################
  output$GradLabel <- renderText({
    paste("Method:", getGraduatingModel()$modelInfo$label)
  })
  
  ###########################################################################
  getTestConfusionMatrix <- reactive({
    req(input$ProbType == "Classification")
    req(input$Target, input$SelectedModel)
    mod <- react$ModelSet[[input$SelectedModel]]
    req(!inherits(mod, "character"))
    data <- getTestData()
    isY <- colnames(data) == input$Target
    caret::confusionMatrix(
      data = predict(mod, newdata = data[, !isY]),
      reference = data[,isY],
      dnn = c("Prediction", "Observed"),
      mode = "everything"
    )
  })

  ###########################################################################
  getFinalIndex <- reactive({
    # In this snippet of code the holdouts sets remain what they were.
    # The new train sets are a combination of the original train data union-ed 
    # with the "test" data.
    # This makes a comparison to the previous hold-out results fair.
    req(input$Method, input$ProbType, input$Target)
    tindex <- (1:ncol(getData()))[-getPartition()]
    indexes <- getTrainControl()$index
    # Add these train indices to the test indices
    for (l in names(index)) {
      indexes[[l]] <- c( indexes[[l]], tindex)
    }
    indexes
  })

  ###########################################################################
  getFinalRecipe <- reactive({
    if (input$LockRecipe) {
      data <- getFinalData()
      recipe <- recipes::recipe(as.formula(paste0(input$Target, " ~ .")), data = data)
      if (input$Weights %in% colnames(data)) {
        w <- input$Weights
        recipe <- update_role(recipe, one_of(w), new_role = "case weight")
      }
      if (!is.null(input$ID) && all(input$ID %in% colnames(data))) {
        w <- input$ID
        recipe <- update_role(recipe, one_of(w), new_role = "case id")
      }
      if (length(react$HighCardinality) > 0) {
        hc <- react$HighCardinality
        recipe <- recipes::add_role(recipe, !!!hc, new_role = "high cardinality")
      }
      if (input$PreSplit %in% colnames(data)) {
        vars <- input$PreSplit
        recipe <- update_role(recipe, one_of(vars), new_role = "presplit")
      }
      if (all(input$Groups %in% colnames(data))) {
        vars <- input$Groups
        recipe <- add_role(recipe, one_of(vars), new_role = "group")
        recipe <- remove_role(recipe, one_of(vars), old_role = "predictor")
      }
      if (!is.null(input$HideCol) && all(input$HideCol %in% colnames(data))) {
        w <- input$HideCol
        recipe <- step_rm(recipe, one_of(w))
      }
      recipe
    } else {
      react$Recipe
    }
  })  

  ###########################################################################
  getFinalData <- reactive({
    if (input$LockRecipe) {
      mod <- getGraduatingModel()
      tryCatch({
        rec <- recipes::prep(mod$recipe, training = getTrainData(), verbose = Verbose, retain = TRUE, strings_as_factors = FALSE) #Use the training data %>% 
        fdata <- recipes::bake(rec, everything(), new_data = getData(), composition = "data.frame")
      }, error = function(e) {
        print(e)
        req(FALSE)
      })
      # # converts >= 15 unique levels to character
      # charCols <- which(allClass(fdata) == "factor")
      # max <- nrow(fdata)
      # for (col in charCols) {
      #   if (length(unique(fdata[,col])) >  getContinuousDebounced()[1]) {
      #     fdata[,col] <- as.character(fdata[,col])
      #   }
      # }
    } else {
      fdata <- getData()  
    }
    fdata
  })
  
  ###########################################################################
  observeEvent(
    input$LockModel,
    {
      if (input$LockModel) {
        shinyjs::disable(id = "LockHyper") 
        shinyjs::disable(id = "LockRecipe") 
        shinyjs::disable(id = "GradTrain") 
      } else {
        shinyjs::enable(id = "LockHyper") 
        shinyjs::enable(id = "LockRecipe") 
        shinyjs::enable(id = "GradTrain") 
      }
    }
  )
  
  
  ###########################################################################
  getTrainedFinalisedModel <- reactive({
    if (input$LockModel) {
      getGraduatingModel()
    } else {
      gradTrainedModel()  
    }
  })
    
  ###########################################################################
  gradTrainedModel <- eventReactive(
    input$GradTrain,
    {
      mod <- getGraduatingModel()
      method <- mod$method
      showNotification(session = session, paste("Final training method:", method), duration = NULL, id = "Final")
      shinyjs::disable((id = "GradTrain"))
      trcontrol <- getTrainControl()
      trcontrol$index <- getFinalIndex()
      if (input$LockHyper) {
        grid <- mod$bestTune
        tuneLength <- 1
        trcontrol$search <- "grid"
      } else {
        grid <- NULL
        tuneLength <- isolate(input$TuneLength)
        trcontrol$search <- isolate(input$Search)
      }
      assign("last.warning", NULL, envir = baseenv())
      parallelMode <- isolate(input$Parallel)
      obj <- NA
      output <- NULL
      fdata <- getFinalData()
      frecipe <- getFinalRecipe()
      if (parallelMode) {
        obj <- startParallel(method)
        result <- tryCatch({
          caret::train(x = frecipe, data = fdata, method = method, trControl = trcontrol,
                       metric = input$HypMetric, tuneGrid = grid, tuneLength = tuneLength)
        },
        error = function(e) {
          e
        })
        output <- stopParallel(obj)
      } else {
        output <- utils::capture.output( {
          result <- tryCatch({   
            caret::train(x = frecipe, data = fdata, method = method, trControl = trcontrol,
                         metric = input$HypMetric, tuneGrid = grid, tuneLength = tuneLength)
          },
          error = function(e) {
            e
          })
        }, type = "message")
      }
      
      if (is(result, "error")) {
        print(result$message)
        print(output)
      }
      
      if (input$LockRecipe) {
        gmod <- getGraduatingModel()
        gmod$results <- result$results
        gmod$bestTune <- result$bestTune
        gmod$resample <- result$resample
        gmod$finalModel <- result$finalModel
        result <- gmod
      }
      shinyjs::enable((id = "GradTrain"))
      removeNotification(session = session, id = "Final")
      result
    },
    ignoreInit = TRUE
  )
  
  ###########################################################################
  output$GradModelPlot1 <- renderPlot({
    mod <- getGraduatingModel()
    req(mod)
    req(!inherits(mod, "error"))
    if (input$ProbType == "Classification") {
      cm <- getTestConfusionMatrix()$table
      cmdf <- melt(data = cm)
      colnames(cmdf) <- c("Prediction", "Observation", "value")
      cmdf$value <- cmdf$value / sum(cmdf$value)
      par(pty = "s")
      alluvial::alluvial(
        cmdf[,1:2],
        freq = cmdf$value,
        col = ifelse(cmdf[, 1] == cmdf[, 2], "green", "red"),
        alpha = 0.5,
        hide  = cmdf$value == 0
      )
      mtext("Confusion matrix for test data", 3, line = 3, font = 2)
    } else if (input$ProbType == "Regression") {
      req(input$ProbType == "Regression")
      test <- getTestData()
      isY <- colnames(test) == input$Target
      predictions <- predict(mod, newdata = test[, !isY])
      d <- data.frame(predictions, test[, isY])
      colnames(d) <- c("pred", "obs")
      range <- range(c(d$pred, d$obs), na.rm = TRUE)
      ggplot2::ggplot(data = d) +
        geom_point(mapping = aes(x = pred, y = obs)) +
        geom_abline(slope = 1, intercept = 0, colour = "blue") +
        labs(title = "Test results",  y = "Observed", x = "Predicted") +
        coord_fixed(ratio = 1, xlim = range, ylim = range, expand = TRUE)
    }
  }, bg = "transparent")
  
  ###########################################################################
  output$GradModelPlot2 <- renderPlot({
    mod <- getTrainedFinalisedModel()
    req(mod)
    req(!inherits(mod, "error"))
    if (input$ProbType == "Classification") {
      cm <- getTestConfusionMatrix()$table
      cmdf <- melt(data = cm)
      colnames(cmdf) <- c("Prediction", "Observation", "value")
      cmdf$value <- cmdf$value / sum(cmdf$value)
      par(pty = "s")
      alluvial::alluvial(
        cmdf[,1:2],
        freq = cmdf$value,
        col = ifelse(cmdf[, 1] == cmdf[, 2], "green", "red"),
        alpha = 0.5,
        hide  = cmdf$value == 0
      )
      mtext("Confusion matrix for test data", 3, line = 3, font = 2)
    } else if (input$ProbType == "Regression") {
      req(input$ProbType == "Regression")
      test <- getTestData()
      isY <- colnames(test) == input$Target
      predictions <- predict(mod, newdata = test[, !isY])
      d <- data.frame(predictions, test[, isY])
      colnames(d) <- c("pred", "obs")
      range <- range(c(d$pred, d$obs), na.rm = TRUE)
      ggplot2::ggplot(data = d) +
        geom_point(mapping = aes(x = pred, y = obs)) +
        geom_abline(slope = 1, intercept = 0, colour = "blue") +
        labs(title = "Predicted versus Observed for test data",  y = "Observed", x = "Predicted") +
        coord_fixed(ratio = 1, xlim = range, ylim = range, expand = TRUE)
    }
  }, bg = "transparent")
  
  ###########################################################################
  output$FinalResamples <- renderTable({
    mod <- getTrainedFinalisedModel()
    req(mod, !inherits(mod, "error"))
    if (is(mod, "caretStack")) {
      mod <- mod$ens_model
    }
    req(mod$results)
    results <- dplyr::inner_join(x = mod$results, y = mod$bestTune, by = colnames(mod$bestTune))
    results <- results[, colnames(mod$results) %in% mod$perfNames]
    good <- !apply(results, MARGIN = 2, FUN = is.na)
    results2 <- results[, good, drop = FALSE]
    miss <- sum(is.na(mod$pred$pred))
    if (miss > 0) {
      results2$`Missing Pred(%)` <- miss / length(mod$pred$pred) * 100
    }
    tryCatch({
      results2$`Train Obs` <- length(fitted(mod))
    }, error = function(e) {
      results2$`Train Obs` <- length(fitted(mod$finalModel))
    })
    req(ncol(results2) > 0)
    results2
  }, digits = 2, width = "100%")
  
  ###########################################################################
  output$FinalHyperparams <- renderTable({
    mod <- getTrainedFinalisedModel()
    req(mod)
    req(is(mod, "train"))
    req(mod$bestTune)
    xtable(mod$bestTune, caption = "Optimum hyper-parameters")
  }, digits = 4)

  ###########################################################################
  output$GradOptimize <- renderPlot({
    if (input$LockHyper) {
      req(input$ResultsFor)
      mod <- react$ModelSet[[input$ResultsFor]]
    } else {
      mod <- getTrainedFinalisedModel()
    }
    req(mod)
    req(mod, is(mod, "train") | is(mod, "caretStack"))
    trellis.par.set(caretTheme())
    try({
      plot(mod, main = paste(sep = " - ", "Hyper-parameter optimisation", input$ResultsFor))
    }, silent = TRUE)
}, bg = "transparent")

  ###########################################################################
  output$GradResiduals <- renderPlot({
    req(input$ResultsFor)
    mod <- react$ModelSet[[input$ResultsFor]]
    req(!inherits(mod, "character"))
    modf <- getTrainedFinalisedModel()
    req(is(mod,"train"), is(modf,"train"))
    if (input$ProbType == "Classification") {
      test <- getTestData()
      isY <- colnames(test) == input$Target
      y <- test[, isY, drop = FALSE]
      Before <- as.matrix(predict.train(mod, newdata = test, type = "prob", na.action = na.pass))
      After <- as.matrix(predict(modf, newdata = test, type = "prob", na.action = na.pass))
      if (all(is.na(Before))) {
        # No class probabilities
      } else {
        # Class probabilities
        yy <- as.matrix(predict(caret::dummyVars(~., data = y, fullRank = FALSE, levelsOnly = TRUE), newdata = y, na.action = na.pass))
        residualB <- yy - Before
        residualA <- yy - After
        m <- 1
        range <- c(-m,m)
        Triangles <- data.frame(x1 = c(m, m, 0, -m, -m, 0), y1 = c(m, 0, 0, 0, -m, 0))
        plotDataA <- c()
        plotDataB <- c()
        for (l in 1:ncol(residualB)) {
          plotDataA <- c(plotDataA, residualA[,l, drop = TRUE])
          plotDataB <- c(plotDataB, residualB[,l, drop = TRUE])
        }
        plotData <- data.frame(After = plotDataA, Before = plotDataB, Type = test[, isY, drop = TRUE])
        ggplot2::ggplot() +
          geom_point(data = plotData, mapping = aes(x = Before, y = After, colour = Type)) +
          geom_abline(slope = 1, intercept = 0, colour = "blue") +
          geom_hline(yintercept = 0, colour = "blue") +
          geom_polygon(data = Triangles, mapping = aes(x = x1, y = y1, fill = "green"), alpha = 0.1, show.legend = FALSE) +
          scale_fill_manual(values = c("green","red")) +
          labs(title = "Class Probability Residuals for test data", subtitle = "(before and after finalisation)", y = "After", x = "Before") +
          coord_fixed(ratio = 1, xlim = range, ylim = range, expand = TRUE)
      }

    } else if (input$ProbType == "Regression") {
      test <- getTestData()
      isY <- colnames(test) == input$Target
      y <- test[, isY]
      Before <- y - predict(mod, newdata = test, na.action = na.exclude)
      After <- y - predict(modf, newdata = test, na.action = na.exclude)
      d <- data.frame(Before, After)
      m <- max(abs(c(d$Before, d$After)), na.rm = TRUE)
      range <- c(-m,m)
      Triangles <- data.frame(x1 = c(m, m, 0, -m, -m, 0), y1 = c(m, 0, 0, 0, -m, 0))
      ggplot2::ggplot(data = d) +
        geom_point(mapping = aes(x = Before, y = After)) +
        geom_abline(slope = 1, intercept = 0, colour = "blue") +
        geom_hline(yintercept = 0, colour = "blue") +
        geom_polygon(data = Triangles, mapping = aes(x = x1, y = y1, fill = "green"), alpha = 0.1, show.legend = FALSE) +
        scale_fill_manual(values = c("green","red")) +
        labs(title = "Residuals (before and after finalisation) for test data", y = "After", x = "Before") +
        coord_fixed(ratio = 1, xlim = range, ylim = range, expand = TRUE)
    }
  }, bg = "transparent")
  
  ###########################################################################
  output$Performance1 <- renderPrint({
    req(input$SelectedModel)
    mod <- getGraduatingModel()
    req(!is(mod, "character"))
    summary(mod$finalModel)
  })
  ###########################################################################
  output$Performance1Plus <- renderTable({
    req(input$SelectedModel)
    mod <- getGraduatingModel()
    req(!is(mod, "character"))
    test <- getTestData()
    isY <- colnames(test) == input$Target
    predictions <- predict(mod, newdata = test[, !isY])
    d <- data.frame(test[, isY], predictions)
    colnames(d) <- c("obs", "pred")
    if (input$ProbType == "Classification") {
      cm <- caret::confusionMatrix(data = d$pred, reference = d$obs, positive = input$Positive)
      results <- as.matrix(cm$byClass)
      results <- t(results)
      rownames(results) <- NULL
    } else {
      ds <- defaultSummary(data = d)
      RMSE <- ds[["RMSE"]]
      R2 <- ds[["Rsquared"]]
      MAE <- ds[["MAE"]]
      N <- sum(!is.na(d$pred))
      n <- sum(is.na(d$pred))
      upperlim <- ds[["RMSE"]] * sqrt(N / qchisq(1 - input$Probability/100,N))
      nMod <- react$ModelSet[["null"]]
      if (!is.null(nMod) && !inherits(nMod, "character")) {
        predictions <- predict(nMod, newdata = test[, !isY])
        d <- data.frame(test[, isY], predictions)
        colnames(d) <- c("obs", "pred")
        ns <- defaultSummary(data = d)
        RelRMSE <- ds[["RMSE"]] / ns[["RMSE"]]
        RelMAE <- ds[["MAE"]] / ns[["MAE"]]
        results <- data.frame(signif(RMSE,4), signif(R2,6), signif(MAE,4), signif(upperlim,4), signif(RelRMSE*100,3), signif(RelMAE*100,3), N, n)
        colnames(results) <- c("RMSE", "Rsquared", "Mean Absol Err", paste0(input$Probability,"% conf. RMSE below"), "RMSE %", "MAE %", "Num predicted", "Num pred Failed")
      } else {
        results <- data.frame(signif(RMSE,4), signif(R2,6), signif(MAE,4), signif(upperlim,4), N, n)
        colnames(results) <- c("RMSE", "Rsquared", "Mean Absol Err", paste0(input$Probability,"% conf. RMSE below"), "Num predicted", "Num pred Failed")
      }
    }
    results
  })
  
  ###########################################################################
  output$Performance2 <- renderPrint({
    req(input$SelectedModel)
    mod <- getTrainedFinalisedModel()
    req(!is(mod, "character"))
    print(summary(mod$finalModel))
    # test <- getTestData2()
    # isY <- colnames(test) == input$Target
    # predictions <- predict(mod, newdata = test[, !isY])
    # d <- data.frame(test[, isY], predictions)
    # colnames(d) <- c("obs", "pred")
    # if (input$ProbType == "Classification") {
    #   N <- sum(!is.na(d$pred))
    #   n <- sum(is.na(d$pred))
    #   results <- c("Num predicted" = N, "Num pred Failed" = n)
    #   print(results, digits = 1)
    # } else {
    #   ds <- defaultSummary(data = d)
    #   RMSE <- ds[["RMSE"]]
    #   R2 <- ds[["Rsquared"]]
    #   MAE <- ds[["MAE"]]
    #   N <- sum(!is.na(d$pred))
    #   n <- sum(is.na(d$pred))
    #   upperlim <- ds[["RMSE"]] * sqrt(N / qchisq(1 - input$Probability/100,N))
    #   nMod <- react$ModelSet[["null"]]
    #   if (!is.null(nMod) && !inherits(nMod, "character")) {
    #     predictions <- predict(nMod, newdata = test[, !isY])
    #     d <- data.frame(test[, isY], predictions)
    #     colnames(d) <- c("obs", "pred")
    #     ns <- defaultSummary(data = d, lev = lev)
    #     RelRMSE <- ds[["RMSE"]] / ns[["RMSE"]]
    #     RelMAE <- ds[["MAE"]] / ns[["MAE"]]
    #     results <- c(RMSE, R2, MAE, upperlim)
    #     names(results) <- c("RMSE", "Rsquared", "Mean Absol Err", paste0(input$Probability,"% conf. RMSE below"))
    #     print(results, digits = 6)
    #     results <- c("RMSE %" = RelRMSE*100, "MAE %" = RelMAE*100)
    #     print(results, digits = 3)
    #   } else {
    #     results <- c(RMSE, R2, MAE, upperlim)
    #     names(results) <- c("RMSE", "Rsquared", "Mean Absol Err", paste0(input$Probability,"% conf. RMSE below"))
    #     print(results, digits = 6)
    #   }
    #   results <- c("Num predicted" = N, "Num pred Failed" = n)
    #   print(results, digits = 1)
    # }
  })
  
  # reactive getResampleIndices ---------------------------------------------------
  getResampleIndices <- reactive({
    tc <- getTrainControl()
    index <- tc$index
    data <- getSomeTrainData()
    rows <- nrow(data)
    #set the correct row order
    if (input$Groups != "") {
      strat <- input$Groups
    } else {
      strat <- input$Target
    }
    stratifier <- data[,strat, drop = TRUE]
    ord <- order(stratifier)
    if (is.numeric(stratifier) & length(unique(stratifier)) > getContinuousDebounced()[1]) {
      stratifier <- dplyr::ntile(x = stratifier, n = min(5, length(unique(stratifier))))
    } else {
      stratifier <- as.integer(as.factor(stratifier))
    }
    d <- data.frame(row = 1:rows)
    # TODO make this show counts in order to see the effect of "with replacement"
    props <- vector(mode = "numeric", length(index))
    for (i in 1:length(index)) {
      vec <- ifelse(d$row %in% index[[i]], 0, max(stratifier) + 1)
      d <- cbind(d, vec)
      props[i] <- sum(vec == 0)/rows
    }
    d <- cbind(d, stratifier)
    d <- d[ord,]
    d$row <- (1:rows)*100/rows
    names(index) <- paste0(names(index), " ", round(props*100), "%")
    colnames(d) <- c("Observation", names(index), paste0("-",strat))
    tidyr::pivot_longer(data = d, cols = -Observation, names_to = "Resample", values_to = "Bag_Type")
  })
  
  # render plot ResampleChart ---------------------------------------------------
  output$ResampleChart <- renderPlotly({
    cbp <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    pivot <- getResampleIndices()
    plot <- ggplot(data = pivot, aes(x = Resample, y = Observation, text = Bag_Type)) +
      geom_raster(aes_string(fill = "Bag_Type")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none") +
      scale_y_reverse() +
      scale_fill_gradientn(colours = cbp) +
      labs(x = "", y = "Observations", title = paste(input$Method, "resampling pattern"))
    plotly(plot)
  })

  
  # reactive getFinalPredictions ---------------------------------------------------------------------------------------------
  getFinalPredictions <- reactive({
    mod <- getTrainedFinalisedModel()
    req(mod, !inherits(mod, "error"))
    predict(mod, getData(), type = "raw", na.action = na.exclude())
  })


  
  # render Infobox NullHandling ---------------------------------------------------
  output$NullHandling <- renderInfoBox({
    p <- getFinalPredictions()
    count <- sum(is.na(p))
    prop <- sum(is.na(p)) / length(p) * 100
    infoBox(title = "Missing predictions", subtitle = "Due to missing predictors", icon = icon("skull"), value = paste0(count, " (",signif(prop, digits = 3),"%)"), color = ANAColour, fill = TRUE)
  })
  
  # render Infobox PredInterval ---------------------------------------------------
  output$PredInterval <- renderInfoBox({
    d <- getData()
    req(isRoleValid(input$Target, d))
    predi <- getFinalPredictions()
    if (input$ProbType == "Regression") {
      obse <- d[, input$Target, drop = TRUE]
      absError <- abs(obse - predi)
      relError <- abs(absError / predi)
      pi <- quantile(absError, probs = input$Probability/100, na.rm = TRUE)
      rpi <- 100 * quantile(relError, probs = input$Probability/100, na.rm = TRUE)
      infoBox(title = paste0("Empirical (", input$Probability, "%) prediction interval overall"), icon = icon("exchange-alt"), value = paste0("+/- ",signif(pi, digits = 3), " (",signif(rpi, digits = 3),"%)"), color = ANAColour, fill = TRUE)
    } else if (input$ProbType == "Classification") {
      #TODO 
    }
  })
  
  
  
  
    
  ###########################################################################
  observeEvent(input$Next, {
    if (input$Navbar == "DataLoad") {
      updateTabItems(session = session, inputId = "Navbar", selected = "DataColumns")
    } else if (input$Navbar == "DataColumns") {
      updateTabItems(session = session, inputId = "Navbar", selected = "RolesChart")
    } else if (input$Navbar == "RolesChart") {
      updateTabItems(session = session, inputId = "Navbar", selected = "DataSummary")
    } else if (input$Navbar == "DataSummary") {
      updateTabItems(session = session, inputId = "Navbar", selected = "DataTable")
    } else if (input$Navbar == "DataTable") {
      updateTabItems(session = session, inputId = "Navbar", selected = "SequenceChart")
    } else if (input$Navbar == "SequenceChart") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MissingPattern")
    } else if (input$Navbar == "MissingPattern") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Outliers")
    } else if (input$Navbar == "Outliers") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Continuity")
    } else if (input$Navbar == "Continuity") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Observations")
    } else if (input$Navbar == "Observations") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Variables")
    } else if (input$Navbar == "Variables") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Boxplots")
    } else if (input$Navbar == "Boxplots") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Bagplot")
    } else if (input$Navbar == "Bagplot") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Correlation")
    } else if (input$Navbar == "Correlation") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Pairs")
    } else if (input$Navbar == "Pairs") {
      updateTabItems(session = session, inputId = "Navbar", selected = "DistPlot")
    } else if (input$Navbar == "DistPlot") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Clusters")
    } else if (input$Navbar == "Clusters") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MissSummary")
    } else if (input$Navbar == "MissSummary") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MissCorrelation")
    } else if (input$Navbar == "MissCorrelation") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MissPattern")
    } else if (input$Navbar == "MissPattern") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MissExplain")
    } else if (input$Navbar == "MissExplain") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MissVariables")
    } else if (input$Navbar == "MissVariables") {
    } else if (input$Navbar == "Sampling") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ProcessingRecipe")
    } else if (input$Navbar == "ProcessingRecipe") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ProcessingSummary")
    } else if (input$Navbar == "ProcessingSummary") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ProcessingTable")
    } else if (input$Navbar == "ProcessingTable") {
      #TODO cannot currently select menu & expand sub-menu
      updateTabItems(session = session, inputId = "Navbar", selected = "MethodCriteria")
    } else if (input$Navbar == "MethodCriteria") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MethodSimilarity")
    } else if (input$Navbar == "MethodSimilarity") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MethodTable")
    } else if (input$Navbar == "MethodTable") {
      updateTabItems(session = session, inputId = "Navbar", selected = "MethodSummary")
    } else if (input$Navbar == "MethodSummary") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Train")
    } else if (input$Navbar == "Train") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ModelMetrics")
    } else if (input$Navbar == "ModelMetrics") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ModelCorrelation")
    } else if (input$Navbar == "ModelCorrelation") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ModelHierarchy")
    } else if (input$Navbar == "ModelHierarchy") {
      updateTabItems(session = session, inputId = "Navbar", selected = "ModelSelection")
    } else if (input$Navbar == "ModelSelection") {
      updateTabItems(session = session, inputId = "Navbar", selected = "GradSummary")
    } else if (input$Navbar == "GradSummary") {
      updateTabItems(session = session, inputId = "Navbar", selected = "GradFit")
    } else if (input$Navbar == "GradFit") {
      updateTabItems(session = session, inputId = "Navbar", selected = "GradParameters")
    } else if (input$Navbar == "GradParameters") {
      updateTabItems(session = session, inputId = "Navbar", selected = "GradResiduals")
    } else if (input$Navbar == "GradResiduals") {
      updateTabItems(session = session, inputId = "Navbar", selected = "Analyse")
    }
  })
  
# * Help ----------------------------------------------------------------------

  # reactive getHelp2 ----------------------------------------------------------------------
  getHelp2 <- reactive({
    showNotification("The help information is loading", id = "HelpLoad", duration = 5)
    df <- getHelp()
    # df$content <- as.character(df$content)
    # df$placement <- as.character(df$placement)
    # df$type <- as.character(df$type)
    # df$placement[is.na(df$placement)] <- "bottom"
    df
  })
  
  
# observe -----------------------------------------------------------------
  observeEvent(input$Help, {
    helpId <- "Help"
    if (input$Help %% 2 == 0) {
      updateActionButton(session = session, inputId = helpId, icon = icon("question-circle"))
    } else {
      updateActionButton(session = session, inputId = helpId, icon = icon("question"))
    }
    
    df <- getHelp2()
    if (input$Help %% 2 == 0) {
      for (row in 1:nrow(df)) {
        content <- as.character(df[row, "content"])
        if (is.na(content)) next
        shinyBS::removeTooltip(session = session, id = as.character(df[row, "id"]))
      }
      showNotification("Hover tooltips are disabled", duration = 5)
    } else {
      for (row in 1:nrow(df)) {
        content <- as.character(df[row, "content"])
        if (is.na(content)) next
        shinyBS::addTooltip(session = session, id = as.character(df[row, "id"]), title = content, placement = as.character(df[row, "placement"]))
      }
      showNotification("Hover tooltips are enabled", duration = 5)
    }
  })
  
  # Observe -------------------------------------------------------------------------------------------
  # Overwrites the help spreadsheet (button is commented out)
  observeEvent(
    input$HelpExport,
    {
      inputs <- names(input)
      df1 <- data.frame(id = inputs, content = paste("Tooltip for",inputs), placement = "bottom", type = "input", stringsAsFactors = FALSE)
      df2 <- getHelp2()
      df3 <- anti_join(df1, df2, by = "id")
      df <- rbind(df2, df3)
      xlxs::write.xlsx(df, file = "help.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = FALSE, append = FALSE)
      showNotification("Help spreadsheet written")
    }
  )
  
  
  
  ###########################################################################
  observe({
    req(input$Navbar)
    hideElement(id = "Header")
    hideElement(id = "DateFormat")
    hideElement(id = "Sep")
    hideElement(id = "Quote")
    hideElement(id = "Decimal")
    hideElement(id = "MissingStrings")
    hideElement(id = "MaxRows")
    hideElement(id = "Continuous")
    hideElement(id = "HideIrrelevant")
    hideElement(id = "ScaleChart")
    hideElement(id = "UseYJ")
    hideElement(id = "CorAbs")
    hideElement(id = "CorGrouping")
    hideElement(id = "CorMethod")
    hideElement(id = "UseDummy")
    hideElement(id = "SortOrder")
    hideElement(id = "Cols")
    hideElement(id = "Multiplier")
    hideElement(id = "NullNormalise")
    hideElement(id = "HideWorse")
    hideElement(id = "ExcludeNull")
    hideElement(id = "HideTimings")
    hideElement(id = "ShowNotches")
    hideElement(id = "Probability")
    hideElement(id = "ObserveMvOut")
    hideElement(id = "Observe1DOut")
    hideElement(id = "ObserveMiss")
    hideElement(id = "MergeWeighting")
    hideElement(id = "ConsiderCont")
    hideElement(id = "ConsiderNom")
    # hideElement(id = "ConsiderImbal")
    hideElement(id = "ConsiderOut")
    hideElement(id = "ConsiderNZV")
    hideElement(id = "MergeVarImp")
    hideElement(id = "OneHot")
    hideElement(id = "OtherThreshold")
    hideElement(id = "NumTerms")
    hideElement(id = "Eps")
    hideElement(id = "MinPts")
    hideElement(id = "Parallel")
    hideElement(id = "NullModel")
    hideElement(id = "Positive")
    hideElement(id = "VarThresh")
    hideElement(id = "CorrThresh")
    hideElement(id = "NumComp")
    hideElement(id = "PolyDegree")
    hideElement(id = "Klof")
    hideElement(id = "DimReductType")
    
    if (input$Navbar == "DataLoad") {
      showElement(id = "Header")
      showElement(id = "DateFormat")
      showElement(id = "Sep")
      showElement(id = "Quote")
      showElement(id = "Decimal")
      showElement(id = "MissingStrings")
    } else if (input$Navbar == "DataColumns") {
      showElement(id = "Continuous")
      showElement(id = "MaxRows")
    } else if (input$Navbar == "DataChart") {
      showElement(id = "Continuous")
      showElement(id = "MaxRows")
    } else if (input$Navbar == "DataSummary") {
      showElement(id = "Continuous")
      showElement(id = "MaxRows")
    } else if (input$Navbar == "DataTable") {
      showElement(id = "Continuous")
      showElement(id = "MaxRows")
    } else if (input$Navbar ==  "SequenceChart") {
      showElement(id = "MaxRows")
      showElement(id = "SortOrder")
      showElement(id = "Cols")
    } else if (input$Navbar == "MissingPattern") {
      showElement(id = "MaxRows")
      showElement(id = "HideIrrelevant")
      showElement(id = "SortOrder")
    } else if (input$Navbar == "Outliers") {
      showElement(id = "MaxRows")
      showElement(id = "Continuous")
      showElement(id = "UseYJ")
      showElement(id = "SortOrder")
    } else if (input$Navbar == "Continuity") {
      showElement(id = "MaxRows")
      showElement(id = "Continuous")
      showElement(id = "ScaleChart")
    } else if (input$Navbar == "Observations") {
      showElement(id = "MaxRows")
      showElement(id = "UseYJ")
      showElement(id = "Multiplier")
      showElement(id = "ObserveMvOut")
      showElement(id = "Observe1DOut")
      showElement(id = "ObserveMiss")
      if (input$Weights != "") {
        showElement(id = "MergeWeighting")
      }
      showElement(id = "Klof")
    } else if (input$Navbar == "Variables") {
      showElement(id = "MaxRows")
      showElement(id = "Continuous")
      showElement(id = "ScaleChart")
      showElement(id = "Multiplier")
      showElement(id = "UseYJ")
      showElement(id = "ConsiderCont")
      showElement(id = "ConsiderNom")
      showElement(id = "ObserveMiss")
      # showElement(id = "ConsiderImbal")
      showElement(id = "ConsiderOut")
      showElement(id = "ConsiderNZV")
      if (input$Target != "") {
        showElement(id = "MergeVarImp")
      }
    } else if (input$Navbar == "Boxplots") {
      showElement(id = "MaxRows")
      showElement(id = "ScaleChart")
      showElement(id = "Continuous")
      showElement(id = "HideIrrelevant")
      showElement(id = "DimReductType")
      showElement(id = "UseYJ")
      showElement(id = "Multiplier")
    } else if (input$Navbar == "Bagplot") {
      showElement(id = "MaxRows")
      showElement(id = "Continuous")
      showElement(id = "ScaleChart")
      showElement(id = "UseYJ")
      showElement(id = "Multiplier")
    } else if (input$Navbar == "Correlation") {
      showElement(id = "Parallel")
      showElement(id = "MaxRows")
      showElement(id = "Continuous")
      showElement(id = "CorAbs")
      showElement(id = "CorMethod")
      showElement(id = "CorGrouping")
      showElement(id = "Cols")
    } else if (input$Navbar == "Pairs") {
      showElement(id = "Parallel")
      showElement(id = "MaxRows")
      showElement(id = "Continuous")
      showElement(id = "Cols")
    } else if (input$Navbar ==  "DistPlot") {
      showElement(id = "MaxRows")
      showElement(id = "Eps")
      showElement(id = "MinPts")
      showElement(id = "ScaleChart")
    } else if (input$Navbar ==  "Clusters") {
      showElement(id = "MaxRows")
      showElement(id = "MinPts")
      showElement(id = "ScaleChart")
    } else if (input$Navbar ==  "MissSummary") {
      showElement(id = "MaxRows")
    } else if (input$Navbar ==  "MissCorrelation") {
      showElement(id = "MaxRows")
      showElement(id = "CorAbs")
      showElement(id = "CorMethod")
      showElement(id = "CorGrouping")
    } else if (input$Navbar ==  "MissPattern") {
      showElement(id = "MaxRows")
    } else if (input$Navbar ==  "MissExplain") {
      showElement(id = "MaxRows")
      showElement(id = "Parallel")
    } else if (input$Navbar ==  "MissVariables") {
      showElement(id = "MaxRows")
    } else if (input$Navbar ==  "Sampling") {
      showElement(id = "Probability")
    } else if (input$Navbar ==  "ProcessingSteps" | input$Navbar ==  "ProcessingRecipe" | input$Navbar == "ProcessingSummary") {
      showElement(id = "Continuous")
      showElement(id = "MaxRows")
      toggleElement(id = "OneHot", condition = input$Convert)
      toggleElement(id = "OtherThreshold", condition = input$Other)
      toggleElement(id = "Eps", condition = input$Clusters)
      toggleElement(id = "MinPts", condition = input$Clusters)
      toggleElement(id = "PolyDegree", condition = input$Poly)
      toggleElement(id = "CorrThresh", condition = input$DimReduce == "corr")
      toggleElement(id = "VarThresh", condition = input$DimReduce == "pca")
      toggleElement(id = "NumComp", condition = input$DimReduce %in% c("ica","pls", "umap", "kpca","isomap"))
      toggleElement(id = "NumTerms", condition = input$String %in% c("hash", "embed"))
    } else if (input$Navbar ==  "Train") {
      showElement(id = "Parallel")
      showElement(id = "NullModel")
      if (input$ProbType == "Classification") {
        showElement(id = "Positive")
      }
    } else if (input$Navbar ==  "ModelMetrics") {
      mods <- getResampledModels()
      shinyjs::toggle(id = "NullNormalise", condition = length(mods$models) > 1 & input$NullModel)
      shinyjs::toggle(id = "HideWorse", condition = length(mods$models) > 1 & input$NullModel)
      shinyjs::toggle(id = "SelectedModelCM", condition = input$ProbType == "Classification")
      shinyjs::toggle(id = "FinalModelCM", condition = input$ProbType == "Classification")
      shinyjs::toggle(id = "ExcludeNull", condition = input$NullModel)
      shinyjs::showElement(id = "ShowNotches")
      shinyjs::showElement(id = "HideTimings")
    } else if (input$Navbar ==  "ModelCorrelation") {
      mods <- getResampledModels()
      shinyjs::toggle(id = "HideWorse", condition = length(mods$models) > 1 & input$NullModel)
      shinyjs::toggle(id = "ExcludeNull", condition = input$NullModel)
    } else if (input$Navbar ==  "ModelHierarchy") {
      mods <- getResampledModels()
      shinyjs::toggle(id = "HideWorse", condition = length(mods$models) > 1 & input$NullModel)
      shinyjs::toggle(id = "ExcludeNull", condition = input$NullModel)
    } else if (input$Navbar ==  "GradSummary") {
      showElement(id = "Parallel")
    } else if (input$Navbar ==  "GradFit") {
    } else if (input$Navbar ==  "GradParameters") {
    }
  })
  
})