# Shiny Caret is an interactive interface for using various machine learning methods from the caret package ----
# (c) Nick Ward (University of Canterbury) 2018-2020 ----
# UI code -------------------------------------------------------------------------------------------------


# header ----
header <- shinydashboardPlus::dashboardHeaderPlus(
  title = tagList(span(class = "logo-lg", "Shiny Caret Laboratory"), icon("diamond")),
  left_menu = tagList(
    shinyWidgets::dropdownButton(inputId = "Submenu", 
                                 tags$h3("Parameters"),
                                 checkboxInput(inputId = "Header", label = "Has header", value = TRUE),
                                 textInput(inputId = "DateFormat", label = "Date format", value = "%Y-%m-%d"),
                                 radioButtons(inputId = "Sep", label = "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                                 radioButtons(inputId = "Quote", label = "Quote character", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'),
                                 radioButtons(inputId = "Decimal", label = "Decimal separator", choices = c("Dot" = ".", "Comma" = ","), selected = '.'),
                                 selectizeInput(inputId = "MissingStrings", label = "Missing placeholders (text)", options = list(create = TRUE), multiple = TRUE, choices = missingStrings, selected = missingStrings),
                                 sliderInput(inputId = "MaxRows", label = "Maximum initial sample size", min = 500, max = 3000, step = 100, value = maxRows),
                                 checkboxInput(inputId = "HideIrrelevant", label = "Hide Irrelevant", value = FALSE ),
                                 sliderInput(inputId = "Continuous", label = "Discont.-Continuous", min = 2, max = 100, step = 1, value = c(15,50)),
                                 checkboxInput(inputId = "ScaleChart", label = "Standarise charts", value = TRUE),
                                 checkboxInput(inputId = "UseYJ", label = "Reshape numeric variables?", value = TRUE),
                                 checkboxInput(inputId = "UseDummy", label = "Convert everything to numeric?", value = TRUE),
                                 selectizeInput(inputId = "SortOrder", label = "Display Order", choices = "", selected = "", multiple = FALSE, options = list(placeholder = 'Ensure an ID column is available')),
                                 sliderInput(inputId = "Cols", label = "Number of variables to chart", min = 5, max = 30, step = 1, value = 20), 
                                 sliderInput(inputId = "Multiplier", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5), #debounced up to here
                                 checkboxInput(inputId = "NullNormalise", label = "Make relative chart", value = TRUE),
                                 checkboxInput(inputId = "HideWorse", label = "Hide worse models", value = TRUE),
                                 checkboxInput(inputId = "ExcludeNull", label = "Exclude Null model", value = FALSE),
                                 checkboxInput(inputId = "HideTimings", label = "Hide prediction timing metrics", value = TRUE),
                                 checkboxInput(inputId = "ShowNotches", label = "ShowNotches", value = FALSE),
                                 sliderInput(inputId = "Probability", label = "Probability", min = 68, max = 99, step = 1, value = 95, post = "%"),
                                 sliderInput(inputId = "ObserveMvOut", label = "Rate multivar outliers", min = 0, max = 3, step = 0.1, value = 1),
                                 sliderInput(inputId = "Observe1DOut", label = "Rate univar outliers", min = 0, max = 3, step = 0.1, value = 1),
                                 sliderInput(inputId = "ObserveMiss", label = "Rate missingness", min = 0, max = 3, step = 0.1, value = 1),
                                 checkboxInput(inputId = "MergeWeighting", label = "Scale by observation weighting", value = FALSE),
                                 sliderInput(inputId = "ConsiderCont", label = "Rate continuous levels", min = 0, max = 3, step = 0.1, value = 1),
                                 sliderInput(inputId = "ConsiderNom", label = "Rate nominal levels", min = 0, max = 3, step = 0.1, value = 1),
                                 # sliderInput(inputId = "ConsiderImbal", label = "Rate nominal imbalance", min = 0, max = 3, step = 0.1, value = 1),
                                 sliderInput(inputId = "ConsiderOut", label = "Rate continuous outliers", min = 0, max = 3, step = 0.1, value = 1),
                                 sliderInput(inputId = "ConsiderNZV", label = "Rate near-zero-variance", min = 0, max = 3, step = 0.1, value = 1),
                                 checkboxInput(inputId = "MergeVarImp", label = "Scale by variable importance", value = FALSE),
                                 selectizeInput(inputId = "CorMethod", label = "Correlation method", choices = c("Pearson", "Spearman" , "Kendal", "Distance" , "Predictive Power"), selected = "pearson"),
                                 checkboxInput(inputId = "CorAbs", label = "Use absolute correlation?", value = TRUE),
                                 selectizeInput(inputId = "CorGrouping", label = "Correlation grouping", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "none"), selected = "none"),
                                 sliderInput(inputId = "NumTerms", label = "Number of encoded features", min = 1, max = 10, value = 5, step = 1),
                                 #sliderInput(inputId = "MissRatio", label = "Missingness ratio", min = 10, max = 100, value = 50, step = 5),
                                 sliderInput(inputId = "VarThresh", label = "Proportion of explained variance", min = 0.8, max = 0.99, step = 0.01, value = 0.95),
                                 sliderInput(inputId = "CorrThresh", label = "Correlation threshold", min = 0.8, max = 0.99, step = 0.01, value = 0.95),
                                  sliderInput(inputId = "NumComp", label = "Number of latent components", min = 2, max = 50, step = 1, value = 15),
                                 sliderInput(inputId = "PolyDegree", label = "Polynomial degree", min = 1, max = 3, value = 2, step = 1),
                                 checkboxInput(inputId = "OneHot", label = "One-Hot style encoding", value = FALSE),
                                 sliderInput(inputId = "OtherThreshold", label = "Threshhold for \"Other\" level", min = 1, max = 100, step = 1, value = 5, post = "%"),
                                 sliderInput(inputId = "Eps", label = "Neighbourhood radius", min = 0.0, max = 1, value = 0),
                                 sliderInput(inputId = "MinPts", label = "Minimum number of points", min = 2, max = 30, step = 1, value = 4),
                                 sliderInput(inputId = "Klof", label = "Size of neighbourhood", min = 2, max = 30, step = 1, value = 4),
                                 checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
                                 checkboxInput(inputId = "NullModel", label = "Add a null model", value = TRUE),
                                 selectizeInput(inputId = "Positive", label = "Positive class", choices = c(), selected = NULL),
                                 selectizeInput(inputId = "DimReductType", label = "Chart-based dimensional reduction", choices = c(), selected = NULL),
                                 circle = FALSE, status = "primary", icon = icon("wrench"), width = "600px"
    ),
    actionLink(inputId = "Save", label = "", icon = icon("save")),
    actionLink(inputId = "Help", label = "", icon = icon("question-circle")),
    shinyBS::bsTooltip(id = "Help", title = "Activate/Deactivate tool-tips"),
    actionLink(inputId = "Next", label = "", icon = icon("step-forward"))
    #actionLink(inputId = "HelpExport", label = "", icon = icon("file-export")) 
  )
)


# sidebar ----
sidebar <- shinydashboard::dashboardSidebar(
  sidebarMenu(id = "Navbar",
               menuItem(text = "Raw Data", icon = icon("table"), startExpanded = TRUE, expandedName = "RawData",
                        menuSubItem(text = "Load", tabName = "DataLoad", selected = TRUE, icon = icon("file-download")),
                        menuSubItem(text = "Roles", tabName = "DataColumns", icon = icon("user-secret")),
                        menuSubItem(text = "Chart", tabName = "RolesChart", icon = icon("chart-area")),
                        menuSubItem(text = "Summary", tabName = "DataSummary", icon = icon("newspaper")),
                        menuSubItem(text = "Table", tabName = "DataTable", icon = icon("table"))
               ),
              menuItem(text = "Exploration", icon = icon("search"), expandedName = "Exploration",
                       menuItem(text = "Homogeneity", icon = icon("chess-board"),
                                menuSubItem(text = "Sequence chart", tabName = "SequenceChart"),
                                menuSubItem(text = "Missing pattern", tabName = "MissingPattern"),
                                menuSubItem(text = "Outliers", tabName = "Outliers"),
                                menuSubItem(text = "Continuity", tabName = "Continuity")
                       ),
                       menuItem(text = "Novelties", icon = icon("eye"),
                                menuSubItem(text = "Observations", tabName = "Observations"),
                                menuSubItem(text = "Variables", tabName = "Variables"),
                                menuSubItem(text = "Box plots", tabName = "Boxplots"),
                                menuSubItem(text = "Bag plots", tabName = "Bagplot")
                       ),
                       menuItem(text = "Charts", icon = icon("bar-chart-o"),
                                menuSubItem(text = "Correlation", tabName = "Correlation"),
                                menuSubItem(text = "Pairs", tabName = "Pairs")
                       ),
                       menuItem(text = "Diversity", icon = icon("fingerprint"),
                                menuSubItem(text = "DistPlot", tabName = "DistPlot"),
                                menuSubItem(text = "Clusters", tabName = "Clusters")
                       ),
                       menuItem(text = "Missingness", icon = icon("braille"), 
                                menuSubItem(text = "Summary", tabName = "MissSummary"),
                                menuSubItem(text = "Correlation", tabName = "MissCorrelation"),
                                menuSubItem(text = "Pattern", tabName = "MissPattern"),
                                menuSubItem(text = "Explanation", tabName = "MissExplain"),
                                menuSubItem(text = "Variables", tabName = "MissVariables")
                       )
              ),
               
              menuItem(text = "Sampling", icon = icon("random"), tabName = "Sampling", 
                       menuSubItem(text = "Test Cases", tabName = "TestCases", icon = icon("vials")),
                       menuSubItem(text = "Resampling", tabName = "Resampling", icon = icon("redo"))
              ),
               
              menuItem(text = "Data Processing", icon = icon("cogs"), expandedName = "DataProcessing",
                       menuSubItem(text = "Steps", tabName = "ProcessingSteps", icon = icon("shoe-prints")),
                       menuSubItem(text = "Recipe", tabName = "ProcessingRecipe", icon = icon("list-ol")),
                       menuSubItem(text = "Summary", tabName = "ProcessingSummary", icon = icon("clipboard-list")),
                       menuSubItem(text = "Processed", tabName = "ProcessingTable", icon = icon("table"))
              ),

              menuItem(text = "Methods", tabName = "Methods", icon = icon("check-double"),
                       menuSubItem(text = "Criteria", tabName = "MethodCriteria", icon = icon("sliders")),
                       menuSubItem(text = "Map", tabName = "MethodSimilarity", icon = icon("map-marked-alt")),
                       menuSubItem(text = "List", tabName = "MethodTable", icon = icon("check-square")),
                       menuSubItem(text = "Summary", tabName = "MethodSummary", icon = icon("table"))
              ),

              menuItem(text = "Train Models", tabName = "Train", icon = icon("dumbbell")),

              menuItem(text = "Model Selection", tabName = "Select", icon = icon("balance-scale"),
                       menuSubItem(text = "Metrics", tabName = "ModelMetrics", icon = icon("tachometer-alt")),
                       menuSubItem(text = "Correlation", tabName = "ModelCorrelation", icon = icon("th")),
                       menuSubItem(text = "Hierarchy", tabName = "ModelHierarchy", icon = icon("sitemap")),
                       menuSubItem(text = "Selection", tabName = "ModelSelection", icon = icon("clipboard-check"))
              ),

              menuItem(text = "Graduate", tabName = "Graduate", icon = icon("graduation-cap"),
                       menuSubItem(text = "Fit", tabName = "GradFit1", icon = icon("th")),
                       menuSubItem(text = "Summary", tabName = "GradSummary", icon = icon("tachometer-alt")),
                       menuSubItem(text = "Fit", tabName = "GradFit2", icon = icon("th")),
                       menuSubItem(text = "Hyperparameters", tabName = "GradParameters", icon = icon("sitemap")),
                       menuSubItem(text = "Residuals", tabName = "GradResiduals", icon = icon("clipboard-check"))
              ),
              
              menuItem(text = "Analyse", tabName = "Analyse", icon = icon("stethoscope"))
  ))


# body ----
body <- shinydashboard::dashboardBody(
  tagList(
    tags$style(type = "text/css", value = "#value{ width: 200px; word-break: break-all; white-space: normal;}"),
    ifelse(DEV, shinythemes::themeSelector(), ""), 
    chooseSliderSkin("Modern", color = NULL), 
    # The CSS below configures the tabBox colour scheme
    tags$style("
                .nav-tabs { background-color: lightblue }
                .sidebar-menu .treeview-menu { padding: 0 0 0 30px;  }
    ")
  ),
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  
  tabItems(
    tabItem(tabName = "DataLoad",
            tabBox(id = "DataSource", title = "Load", width = 12, side = "right", height = "300px", 
                   tabPanel(title = "Server Data File", icon = icon("file-excel"), 
                            fluidRow(
                              column(
                                width = 6, offset = 3,
                                shinyFiles::shinyFilesButton(id = "ServerFile", label = "Choose a server file", title = "Choose a data file that resides where R is running", multiple = FALSE),
                                disabled(shiny::textInput(inputId = "ServerFileName", label = "", value = ""))
                              )
                            )
                   ),
                   tabPanel(title = "Local Data File", icon = icon("file-alt"),
                            fluidRow(
                              column(
                                width = 4, offset = 3, 
                                tags$div(id = "LocalFileDiv",  # the div takes a tool tip
                                         fileInput(inputId = "LocalFile", label = "Choose a local file", multiple = FALSE),
                                         hidden(shiny::textInput(inputId = "LocalFileName", label = "", value = ""))
                                )
                              )
                            )
                   ),
                   tabPanel(title = "Remote Data Resource", icon = icon("cloud-download-alt"),
                            fluidRow(
                              column(
                                width = 10, offset = 1,
                                textInput(inputId = "URL", label = "URL", value = "http://"))
                            )
                   ),
                   tabPanel(title = "Package Dataset", icon = icon("r-project"),
                            fluidRow(
                              column(
                                width = 10, offset = 1,
                                selectizeInput(inputId = "Package", label = "Choose an available package containing data", choices = c(.packages(all.available = TRUE)), selected = "caret"),
                                selectizeInput(inputId = "DataSet", label = "Choose a data set", choices = c(), selected = NULL, width = "100%")
                              )
                            )
                   )
            ),
            fluidRow(
              column(
                width = 9, offset = 3,
                box(title = "Project", background = DATAColour,
                    selectizeInput(inputId = "Project", label = "Input a project name", choices = c(), multiple = FALSE, selected = NULL, options = list(create = TRUE)),
                    actionButton(inputId = "LoadProjectReq", label = "Load Project", icon = icon("play"))
                )
              )
            ),
            fluidRow(
              infoBoxOutput(outputId = "ObservationCount", width = 6),
              infoBoxOutput(outputId = "VariableCount", width = 6)
            )
    ),
    
    tabItem(tabName = "DataColumns",
            box(title = "Data Roles", solidHeader = TRUE, background = DATAColour, width = 12, collapsible = TRUE, 
                fluidRow(
                  column(
                    width = 2,
                    selectizeInput(inputId = "Target", label = "Outcome", choices = "", selected = "", multiple = FALSE),
                    plotOutput("ClassPieChart", height = "50px")
                  ),
                  
                  column(
                    width = 2,
                    selectizeInput(inputId = "ID", label = "Identifier", choices = c(), selected = c(), options = list(placeholder = 'Assign an ID column')),
                    checkboxInput(inputId = "AddIds", label = "Convert row names to a variable", value = FALSE)
                  ),
                  column(
                    width = 2,
                    selectizeInput(inputId = "HideCol", label = "Unused", choices = "", selected = "", multiple = TRUE)
                  ),
                  column(
                    width = 2,
                    selectizeInput(inputId = "PreSplit", label = "Train-Test Partition", choices = "", selected = "", multiple = FALSE),
                    plotOutput("SplitPieChart", height = "50px")
                  ),
                  column(
                    width = 2,
                    selectizeInput(inputId = "Weights", label = "Observation Weighting", choices = "", selected = "", multiple = FALSE),
                    checkboxInput(inputId = "AddWeights", label = "Auto Balance", value = FALSE),
                    selectizeInput(inputId = "BalanceFactors", label = "Balance Factors", choices = "", selected = "", multiple = TRUE)
                  ),
                  column(
                    width = 2,
                    selectizeInput(inputId = "Groups", label = "Observation Stratifier", choices = c(), selected = c(), multiple = FALSE),
                    plotOutput(outputId = "GroupPieChart", height = "50px")
                  )
                )
            ),
            fluidRow(
              infoBoxOutput(outputId = "StringCount", width = 4),
              infoBoxOutput(outputId = "NZVCount", width = 4),
              infoBoxOutput(outputId = "ContCount", width = 4)
            ),
            fluidRow(
              infoBoxOutput(outputId = "MissVar", width = 4),
              infoBoxOutput(outputId = "MissObs", width = 4),
              infoBoxOutput(outputId = "MissTarget", width = 4)
            ),
            fluidRow(
              infoBoxOutput(outputId = "TargetCheck", width = 6),
              infoBoxOutput(outputId = "RatioObs", width = 6)
            )
    ),
    
    tabItem(tabName = "RolesChart",
            box(title = "Data Roles", solidHeader = TRUE, collapsible = FALSE, width = 12, background = DATAColour,
                withSpinner(ui_element = plotlyOutput(outputId = "RoleTypeChart", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),            
    
    tabItem(tabName = "DataSummary",
            box(title = "Raw Data Summary", solidHeader = TRUE, collapsible = FALSE, width = 12, background = DATAColour, 
                withSpinner(ui_element = htmlOutput(outputId = "DataSummary", style = "color:black"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "DataTable",
            box(title = "Raw Data Table", solidHeader = TRUE, collapsible = FALSE, width = 12, background = DATAColour, 
                withSpinner(ui_element = DT::dataTableOutput(outputId = "Data"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "SequenceChart",
            box(title = "Sequence chart", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "SequenceChart1", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                ),
                withSpinner(ui_element = plotOutput(outputId = "SequenceChart2", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                ),
                withSpinner(ui_element = plotOutput(outputId = "SequenceChart3", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                            
                ),
                withSpinner(ui_element = plotOutput(outputId = "SequenceChart4", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                ),
                withSpinner(ui_element = plotOutput(outputId = "SequenceChart5", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "MissingPattern",
            box(title = "Missing pattern", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "MissingChart1", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Outliers", 
            box(title = "Outliers", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "OutlierPlots", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Continuity",
            box(title = "Continuity", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "RisingChart", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Observations",
            box(title = "Observations", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "NoveltyObservations", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Variables",
            box(title = "Variables", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "NoveltyColumns", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Boxplots", width = 12,
            box(title = "Box Plots", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "NumericNovelties", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Bagplot", 
            box(title = "Bag Plot", solidHeader = TRUE, background = EDAColour, width = 12,
                fluidRow(
                  column(width = 10,
                         plotOutput(outputId = "MultiDimNovelties", height = FullHeight),
                         box(title = "Parameters", collapsible = TRUE, width = 12, solidHeader = TRUE, background = EDAColour, collapsed = TRUE,
                             fluidRow(
                               column(width = 6, 
                                      selectizeInput(inputId = "DimReductType", label = "Style of Dimensional Reduction", choices = c(""), selected = "")
                               ),
                               column(width = 6,
                                      selectizeInput(inputId = "DistanceMethod", label = "Distance metric", choices = c("euclidean","manhattan"), selected = "euclidean")
                               )
                             )
                         )
                  ),
                  column(width = 2,
                         box(title = "Outliers", width = 12, collapsible = TRUE, solidHeader = TRUE, background = EDAColour,
                             textOutput(outputId = "MultiDimNovText"),
                             DT::dataTableOutput(outputId = "MultiDimNovIDs", height = "500px")
                         )
                  )
                )
            )
    ),
    
    tabItem(tabName = "Correlation",  
            box(title = "Variable Correlation", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "Correlation", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Pairs",
            box(title = "Pairs", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "Pairs", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    
    tabItem(tabName = "DistPlot", 
            box(title = "Observation spread", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotOutput("KnnDistPlot", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Clusters",
            panel(
              box(title = "Clusters", solidHeader = TRUE, background = EDAColour, width = 12, collapsible = TRUE,
                  withSpinner(ui_element = plotOutput("ClusterPlot", height = FullHeight),
                              color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                  )
              ),
              valueBoxOutput(outputId = "ClusterCount", width = 6),
              valueBoxOutput(outputId = "Outliers", width = 6),
              box(title = "Cluster outliers", solidHeader = TRUE, background = EDAColour, width = 12, collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "ClusterPrint")
              )
            )
    ),
    
    tabItem(tabName = "MissSummary", width = 12,
            panel(heading = "Summary",
                  infoBox(title = "Missing Observations", width = 4, color = EDAColour, fill = TRUE, icon = icon("align-justify"), textOutput(outputId = "MissObservations")),
                  infoBox(title = "Missing Variables", width = 4, color = EDAColour, fill = TRUE, icon = icon("columns"), textOutput(outputId = "MissVariables")),
                  infoBox(title = "Missing Values", width = 4, color = EDAColour, fill = TRUE, icon = icon("border-all"), textOutput(outputId = "MissValues")),
                  box(title = "Variables", solidHeader = TRUE, background = EDAColour, width = 6,
                      sliderInput(inputId = "MissVarThreshold", label = "Missing variable threshold", min = 0, max = 100, step = 1, value = 50, post = "%", width = "100%")
                  ),
                  box(title = "Observations", solidHeader = TRUE, background = EDAColour, width = 6,
                      sliderInput(inputId = "MissObsThreshold", label = "Missing observation threshold", min = 0, max = 100, step = 1, value = 50, post = "%", width = "100%")
                  ),
                  infoBox(title = "Heavily Missing Variables", width = 6, color = EDAColour, fill = TRUE, icon = icon("columns"), textOutput(outputId = "HeavyMissVar")),
                  infoBox(title = "Heavily Missing Observations", width = 6, color = EDAColour, fill = TRUE, icon = icon("align-justify"), textOutput(outputId = "HeavyMissObs"))
            )
    ),
    
    tabItem(tabName = "MissCorrelation", 
            box(title = "Missingness correlation", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "MissCorr", height = FullHeight, width = FullHeight), # make square
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "MissPattern",
            box(title = "Missingness pattern", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "MissingChart2", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "MissExplain", width = 12,
            box(title = "Missingness explanation", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "MissingnessInformationPlot", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "MissVariables", width = 12,
            box(title = "Missingness variables", solidHeader = TRUE, background = EDAColour, width = 12,
                withSpinner(ui_element = plotlyOutput(outputId = "MissingnessVariables", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "TestCases",
            box(title = "Partitioning", solidHeader = TRUE, width = 6, background = SAMPColour, 
                fluidRow(
                  column(width = 6,
                         sliderInput(inputId = "Ratio", label = "Partition ratio", min = 0.3, max = 1.0, value = 0.8, step = 0.01)
                  ),
                  column(width = 6,
                         textOutput(outputId = "Splitter"),
                         actionButton(inputId = "Evaluate", label = "Evaluate Fairness", icon = icon("play"))
                  )
                )
            ),
            infoBox(title = "Train Count", width = 4, icon = icon("plus"), color = SAMPColour, fill = TRUE,
                    value = textOutput(outputId = "SplitCountTrain")
            ),
            infoBox(title = "Test Count", width = 4, icon = icon("minus"), color = SAMPColour, fill = TRUE,
                    value = textOutput(outputId = "SplitCountTest")
            ),
            infoBoxOutput(outputId = "TestUncertainty", width = 8),
            infoBoxOutput(outputId = "SplitTest", width = 10)
    ),
    
    tabItem(tabName = "Resampling",
            box(title = "Train Resampling", solidHeader = TRUE, width = 6, background = SAMPColour,  
                fluidRow(
                  column(width = 6,
                         # hiding methods "oob" & "timeslice"
                         selectizeInput(inputId = "Method", label = "Method", choices = c("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "adaptive_cv", "adaptive_boot", "LGOCV", "adaptive_LGOCV"), selected = "boot")
                  ),
                  column(width = 6, 
                         sliderInput(inputId = "Number", label = "Number of folds or iterations", min = 1, max = 30, step = 1, value = 10, ticks = FALSE),
                         sliderInput(inputId = "Repeats", label = "Number of CV repeats", min = 1, max = 10, step = 1, value = 1)  #For repeated k-fold cross-validation only
                  )
                )
            ),
            infoBoxOutput(outputId = "NearZeroVarTest", width = 8),
            box(title = "Resampling map", width = 12, collapsible = TRUE, background = SAMPColour, solidHeader = TRUE, 
                withSpinner(ui_element = plotlyOutput(outputId = "ResampleChart", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "ProcessingSteps",
            box(title = tagList("Processing Steps", actionButton(inputId = "Restart", label = "Restart", icon = icon("repeat"))),
                width = 12,
                collapsible = TRUE,
                fluidRow(
                  column(
                    width = 3,
                    box(title = "Missing values", width = 12, solidHeader = TRUE, background = PROCColour, collapsible = TRUE,
                        checkboxInput(inputId = "MissingVar", label = "Remove heavily missing variables?", value = FALSE),
                        checkboxInput(inputId = "MissingObs", label = "Remove heavily missing observations?", value = FALSE),
                        checkboxInput(inputId = "Unknown", label = "Assign missing nominal levels to Unknown?", value = FALSE),
                        checkboxInput(inputId = "Shadow", label = "Add shadow variables?", value = FALSE),
                        selectizeInput(inputId = "Impute", label = "Missing value processing?", choices = missChoices, selected = "none")
                    )
                  ),
                  column(
                    width = 3,
                    box(title = "Non-numeric", width = 12, solidHeader = TRUE, background = PROCColour, collapsible = TRUE,
                        selectizeInput(inputId = "Balance", label = "Balance outcome processing?", choices = c("None" = "none", "Up" = "up", "Down" = "down", "Up-Down" = "up-down"), selected = "none"),
                        checkboxInput(inputId = "text", label = "Expand text into text features?", value = FALSE),
                        selectizeInput(inputId = "String", label = "High Cardinality processing?", 
                                    choices = c("None" = "none", "Omit" = "omit", "Weight-of-evidence encoding" = "woe", "Mean encoding" = "mean", "Hash encoding" = "hash", "Embedded encoding" = "embed", "Binary encoding" = "binary"),
                                    selected = "none"),
                        checkboxInput(inputId = "Other", label = "Pool rare levels?", value = FALSE),
                        selectizeInput(inputId = "DateFeatures", label = "Expand dates into features", multiple = TRUE, 
                                    choices = c("day of week" = "dow", "day of year" = "doy", "week" = "week", "month" = "month", "decimal", "quarter" = "quarter", "semester", "year"),
                                    selected = c()),
                        checkboxInput(inputId = "Cyclic", label = "Treat dow, month as cyclic?", value = FALSE),
                        checkboxInput(inputId = "Convert", label = "Convert nominal predictors to binary?", value = FALSE)
                    )
                  ),
                  column(
                    width = 3,
                    box(title = "Numeric", width = 12, solidHeader = TRUE, background = PROCColour, collapsible = TRUE,
                        checkboxInput(inputId = "YJ", label = "Reshape numeric distributions?", value = FALSE),
                        checkboxInput(inputId = "Center", label = "Apply mean centering?", value = FALSE),
                        checkboxInput(inputId = "Scale", label = "Apply SD scaling?", value = FALSE),
                        selectizeInput(inputId = "DimReduce", label = "Dimensionally reduce?", choices = c("None" = "none", "Correlation" = "corr", "PCA" = "pca", "ICA" = "ica", "IsoMap" = "isomap", "PLS (sup)" = "pls", "UMAP (sup)" = "umap", "kPLS" = "kpls"), selected = "none"),
                        checkboxInput(inputId = "Poly", label = "Apply polynomial expansion?", value = FALSE)
                    )
                  ),
                  column(
                    width = 3,
                    box(title = "Late steps", width = 12, solidHeader = TRUE, background = PROCColour, collapsible = TRUE,
                        checkboxInput(inputId = "Clusters", label = "Add clusters?", value = FALSE), 
                        checkboxInput(inputId = "Centers", label = "Add Distances to class centers?", value = FALSE),
                        checkboxInput(inputId = "FeatureSelection", label = "Apply Feature Selection?", value = FALSE),
                        selectizeInput(inputId = "Variance", label = "Variance processing?", choices = c("None" = "none", "Zero Variance" = "zv", "Near-zero variance" = "nzv"), selected = "none"),
                        checkboxInput(inputId = "LinComb", label = "Remove variables to make linearly independent?", value = FALSE)
                    )
                  )
                )
            ),
            box(title = "Issues to resolve", width = 12, collapsible = TRUE,
                infoBoxOutput(outputId = "MissPresent", width = 4),
                infoBoxOutput(outputId = "NominalPresent", width = 4),
                infoBoxOutput(outputId = "StringPresent", width = 4),
                infoBoxOutput(outputId = "NZVPresent", width = 4),
                #infoBoxOutput(outputId = "VariablesPresent", width = 4),
                infoBoxOutput(outputId = "VariablesRatio", width = 4),
                infoBoxOutput(outputId = "LinCombPresent", width = 4),
                infoBoxOutput(outputId = "Imbalanced", width = 4),
                infoBoxOutput(outputId = "RecipeErr", width = 8)
            )
    ),
    
    tabItem(tabName = "ProcessingRecipe",
            box(title = "Recipe Ingredients", width = 12, solidHeader = TRUE, background = PROCColour, collapsible = TRUE, 
                verbatimTextOutput(outputId = "PreProcSummary")
            )
    ),
    
    tabItem(tabName = "ProcessingSummary",
            box(title = "Processed Data Summary", solidHeader = TRUE, collapsible = TRUE, width = 12, background = PROCColour, 
                withSpinner(ui_element = htmlOutput(outputId = "ProcessedDataSummary", style = "color:black"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "ProcessingTable",
            box(title = "Processed Train-Data Table", solidHeader = TRUE, collapsible = FALSE, width = 12, background = PROCColour,
                withSpinner(ui_element = DT::dataTableOutput("PreProcTable"), 
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "MethodCriteria",
            box(title = "Criteria by which to restrict the choice of methods", width = 12, background = METHColour,
                box(title  = "Basic criteria", width = 3, background = METHColour,
                    checkboxInput(inputId = "AvailPackages", label = "Available packages only", value = TRUE),
                    actionButton(inputId = "RefreshModels", label = "Reset", icon = icon("refresh"))
                ),
                box(title = "Implied criteria", width = 6, background = METHColour,
                    fluidRow(
                      radioButtons(inputId = "ProbType", label = "Problem Type:",  choices = c("Any", "Classification","Regression"), selected = "Any", inline = TRUE),
                      radioButtons(inputId = "PredictorsTag", label = "Special predictors", choices = c("All Nominal", "All Binary", "Mixture"), selected = "Mixture", inline = TRUE),
                      column(width = 6,
                             checkboxInput(inputId = "TwoClassTag", label = "Two class outcome?", value = TRUE),
                             checkboxInput(inputId = "WeightingTag", label = "Weightings present?", value = FALSE),
                             checkboxInput(inputId = "OutliersTag", label = "Outliers still present?", value = FALSE),
                             checkboxInput(inputId = "MissingTag", label = "Missing data still present?", value = FALSE)
                      ),
                      column(width = 6,
                             checkboxInput(inputId = "ProbabilityTag", label = "Class probabilities required?", value = FALSE),
                             checkboxInput(inputId = "NominalsTag", label = "Nominal data still present?", value = FALSE),
                             checkboxInput(inputId = "NZVTag", label = "Near-Zero Variance still present?", value = FALSE),
                             checkboxInput(inputId = "LinCombTag", label = "Linear Combinations still present?", value = FALSE),
                             checkboxInput(inputId = "RatioTag", label = "Excessive variables cf observations?", value = FALSE)
                      )
                    )
                ),
                box(title = "Subjective criteria", width = 3, background = METHColour,
                    selectizeInput(inputId = "IncFeatures", label = "Essential characteristics", choices = c(), multiple = TRUE, selected = NULL),
                    selectizeInput(inputId = "ExcFeatures", label = "Excluded characteristics", choices = c(), multiple = TRUE, selected = NULL)
                )
            ),
            infoBoxOutput(outputId = "Methods")
    ),
    
    tabItem(tabName = "MethodSimilarity",
            box(title = "Map of methods that meet the criteria", width = 12, background = METHColour, solidHeader = TRUE,
                withSpinner(ui_element = plotOutput(outputId = "D2plot", height = "600px", hover = "MapHover", dblclick = "MapDblClick"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                ),
                uiOutput("MapInfo"),
                box(title = "", width = 11, background = METHColour, solidHeader = TRUE, 
                    actionButton(inputId = "Suggest", label = "Suggest dissimilar", icon = icon(name = "plus-circle"))
                )
            )
    ),
    
    tabItem(tabName = "MethodTable",
            box(title = "List of Methods that meet the criteria", width = 12, background = METHColour, solidHeader = TRUE,
                actionButton(inputId = "ChooseAll", label = "All", icon = icon("align-justify")),
                DT::dataTableOutput(outputId = "MethodsTable")
            )
    ),
    
    tabItem(tabName = "MethodSummary",
            box(title = "Chosen methods", width = 6, solidHeader = TRUE, background = METHColour,
                actionButton(inputId = "Install", label = "Install missing packages", icon = icon(name = "download")),
                tableOutput(outputId = "SelectedMethodsTable")
            ),
            infoBoxOutput(outputId = "SelWarn")
    ),
    
    tabItem(tabName = "Train",
            # "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models)
            # "timeslice" left out because it needs lots of extra parameters
            fluidRow(
              column(width = 2,
                     box(title = "Actions", width = 12, background = TRAINColour, collapsible = TRUE,
                         actionButton(inputId = "Reset", label = "Reset", icon = icon("fast-backward"), width = "100px"),
                         hr(),
                         actionButton(inputId = "Train", label = "Train", icon = icon("play"), width = "100px"),
                         hr(),
                         shinyBS::bsButton(inputId = "Pause", label = "Pause", icon = icon("pause"), type = "toggle", value = FALSE, width = "100px")
                     ),
                     box(title = "Select a model", background = TRAINColour, width = 12,
                         radioButtons(inputId = "ResultsFor", label = "", choices = "", selected = ""),
                         actionButton(inputId = "UndoModel", label = "Untrain", icon = icon("undo-alt"), width = "100px")
                     )
              ),
              column(width = 10,
                     box(title = "Optimisation", width = 12, background = TRAINColour, collapsible = TRUE,
                         fluidRow(
                           column(width = 3, selectizeInput(inputId = "HypMetric", label = "Performance metric", regChoices, selected = "RMSE", width = "140px")),
                           column(width = 3, selectizeInput(inputId = "SelectionFunc", label = "Optimum selection", choices = selectionChoices, selected = selectionChoices[1], width = "140px")),
                           column(width = 3, selectizeInput(inputId = "Search", label = "Search type", choices = searchChoices, selected = searchChoices[1], width = "140px")),
                           column(width = 3, sliderInput(inputId = "TuneLength", label = "Tune attempts", min = 1, max = 15, step = 1, value = 4, ticks = FALSE, width = "140px"))
                         )
                     ),
                     shinydashboard::tabBox(
                       id = "TrainTab",
                       side = "left",
                       width = 12,
                       tabPanel(title = "Summary", 
                                icon = icon("file"),
                                panel(
                                  box(title = "Errors and Warnings", width = 12, verbatimTextOutput(outputId = "TrainWarn"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE),
                                  box(title = "Resampled performance", width = 12, div(style = 'overflow-x: auto', tableOutput(outputId = "Resamples")), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE),
                                  box(title = "Optimum hyper-parameters", width = 6, tableOutput(outputId = "Hyperparams"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE),
                                  box(title = "Method characteristics", width = 6, tableOutput(outputId = "Characteristics"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE)
                                )
                       ),
                       tabPanel(title = "Fit", 
                                icon = icon("tachometer-alt"),
                                panel(
                                  box(title = "Model performance", width = 6, solidHeader = TRUE, background = TRAINColour,
                                      withSpinner(ui_element = plotOutput(outputId = "ModelTrainPlot", height = "400px"),
                                                  color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                                      )
                                  ),
                                  box(id = "ROCPlot", title = "Response Operating Characteristics", width = 6, solidHeader = TRUE, background = TRAINColour,
                                      withSpinner(ui_element = plotOutput(outputId = "RocCurvePlot", height = "400px"),
                                                  color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                                      )
                                  ),
                                  box(title = "Classification Statistics",  width = 6, verbatimTextOutput(outputId = "ClassStats"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE)
                                )
                       ),
                       tabPanel(title = "Tuning",
                                icon = icon("chart-line"),
                                withSpinner(ui_element = plotOutput(outputId = "Optimize", height = "400px"),
                                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                                )
                       ),
                       tabPanel(
                         title = "Recipe",
                         icon = icon("book-open"),
                         panel(
                           box(title = "Recipe Definition", width = 12, verbatimTextOutput(outputId = "TrainedRecipe"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE),
                           box(title = "Variable Roles", width = 12, tableOutput(outputId = "RecipeSummary"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE)
                         )
                       ),
                       # tabPanel(
                       #   title = "Breakdown",
                       #   icon = icon("columns"),
                       #   withSpinner(color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE,
                       #     plotOutput(outputId = "Breakdown", height = "400px")
                       #   ),
                       # ),
                       tabPanel(
                         title = "Variables",
                         icon = icon("columns"),
                         panel(
                           box(title = "Importance", solidHeader = TRUE, background = TRAINColour, collapsible = TRUE, width = 12,
                               plotOutput(outputId = "VarImp", height = "400px")
                           ),
                           box(title = "Coefficents", width = 12, verbatimTextOutput(outputId = "ModelCoef"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE)
                         )
                       ),
                       tabPanel(
                         title = "Log",
                         icon = icon("quote-left"),
                         panel(
                           box(title = "Output", width = 12, verbatimTextOutput(outputId = "TrainLog"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE)
                         )
                       )
                     )
                     #)
              )
            )
    ),
    
    tabItem(tabName = "ModelMetrics",
            box(title = "Model Metrics", icon = icon("tachometer-alt"), solidHeader = TRUE, background = MODELSColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "Selection", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "ModelCorrelation",
            box(title = "Model Correlation", icon = icon("th"), solidHeader = TRUE, background = MODELSColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "ResidualCorr", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "ModelHierarchy", 
            box(title = "Model Hierarchy", icon = icon("sitemap"), solidHeader = TRUE, background = MODELSColour, width = 12, 
                withSpinner(ui_element = plotOutput(outputId = "ModelTree", height = FullHeight),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "ModelSelection",
            box(title = "Model Selection", icon = icon("clipboard-check"), solidHeader = TRUE, background = MODELSColour, width = 12,
                fluidRow(
                  column(width = 8, 
                         radioButtons(inputId = "SelectedModel", label = "Choose the best performing model", choices = "", selected = "", inline = TRUE, width = "100%")
                  ),
                  column(width = 4, 
                         actionButton(inputId = "AddModel", label = "Ensemble", icon = icon("play")),
                         selectizeInput(inputId = "Ensemble", label = "Choose a set to ensemble", multiple = TRUE, choices = "", selected = "")
                  )
                )
            )
    ),
    
    tabItem(tabName = "GradFit1", title = "Fit", icon = icon("heartbeat"),
            panel(
              box(title = "Projected Model performance on unseen data", solidHeader = TRUE, background = GRADColour, width = 12, collapsible = TRUE,
                  withSpinner(ui_element = plotOutput(outputId = "GradModelPlot1", height = "400px"),
                              color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                  )
              ),
              box(title = "Statistics", solidHeader = TRUE, background = GRADColour, width = 12, collapsible = TRUE,
                  div(style = 'overflow-x: auto', tableOutput(outputId = "Performance1Plus"))
              ),
              box(title = "Model Summary", solidHeader = TRUE, background = GRADColour, width = 12, collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "Performance1")
              )
            )
    ),
    tabItem(tabName = "GradSummary", icon = icon("file"),
            box(title = "Graduation Summary", solidHeader = TRUE, width = 12,
                box(fluidRow(column(offset = 1, width = 5, checkboxInput(inputId = "LockModel", label = "Lock Model based upon the training data", value = TRUE))),
                    fluidRow(column(offset = 1, width = 4, checkboxInput(inputId = "LockHyper", label = "Lock existing hyper-parameters", value = TRUE)),
                             column(width = 4, checkboxInput(inputId = "LockRecipe", label = "Lock existing recipe", value = TRUE)),
                             column(width = 1, actionButton(inputId = "GradTrain", label = "Train", icon = icon("play"), width = "100px"))),
                    title = textOutput(outputId = "GradLabel"), width = 12, solidHeader = TRUE, background = GRADColour, collapsible = TRUE
                ),
                box(title = "Initial Resampled performance", width = 12, div(style = 'overflow-x: auto', tableOutput(outputId = "Resamples2")), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE),
                box(title = "Final Resampled performance", width = 12, div(style = 'overflow-x: auto', tableOutput(outputId = "FinalResamples")), solidHeader = TRUE, background = GRADColour, collapsible = TRUE),
                box(title = "Initial hyper-parameters", width = 6, tableOutput(outputId = "Hyperparams2"), solidHeader = TRUE, background = TRAINColour, collapsible = TRUE),
                box(title = "Final hyper-parameters", width = 6, tableOutput(outputId = "FinalHyperparams"), solidHeader = TRUE, background = GRADColour, collapsible = TRUE)
            )
    ),
    tabItem(tabName = "GradFit2", title = "Fit", icon = icon("heartbeat"),
            box(title = "Finalised Model Fit", solidHeader = TRUE, background = GRADColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "GradModelPlot2", height = "400px"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                ),
                verbatimTextOutput(outputId = "Performance2")
            )
    ),
    tabItem(tabName = "GradParameters", icon = icon("chart-line"),
            box(title = "Finalised Hyperparameters", solidHeader = TRUE, background = GRADColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "GradOptimize", height = "400px"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "GradResiduals", icon = icon("chart-area"),
            box(title = "Finalised Residuals", solidHeader = TRUE, background = GRADColour, width = 12,
                withSpinner(ui_element = plotOutput(outputId = "GradResiduals", height = "400px"),
                            color = SPINNER_COLOUR, type = SPINNER_TYPE, size = SPINNER_SIZE
                )
            )
    ),
    
    tabItem(tabName = "Analyse",
            box(title = "Graduation Analysis", solidHeader = TRUE, width = 12,
                infoBoxOutput(outputId = "NullHandling", width = 6),
                infoBoxOutput(outputId = "PredInterval", width = 6)
            )
    )
  )
)


# shinyUI ----
shinyUI(
  shinydashboardPlus::dashboardPagePlus(
    header = header,
    sidebar = sidebar,
    body = body
  )
)
