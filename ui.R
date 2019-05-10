# Shiny Caret is an interactive interface for using various machine
# learning methods from the caret package
# (c) Nick Ward (University of Canterbury) 2018

library(shiny, )
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)

###########################################################################
### UI logic
shinyUI(
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    tags$style(type="text/css", "#value{ width: 200px; word-break: break-all; white-space: normal;}"),
    #shinythemes::themeSelector(), #dev only
    navbarPage(
      id = "Navbar",
      title = "Shiny Caret Lab",
      windowTitle = "Shiny Caret Lab",
      theme = shinythemes::shinytheme("yeti"),
      tabPanel(
        title = "Data", icon = icon("database"),
        h4("Data"),
        tabsetPanel(
          id = "DataTabs",
          tabPanel(
            title = "Load data",
            fluidRow(
              column(
                width = 2,
                radioButtons(inputId = "DataSource", label = "Data source", choices = c("CSV file", "R dataset"), selected = "R dataset")
              ),
              column(
                width = 3, offset = 1,
                # Input: Checkbox if file has header ----
                checkboxInput(inputId = "Header", label = "Header", value = TRUE),
                textInput(inputId = "DateFormat", label = "Supplied Date format", value = "%Y-%m-%d")
              ),
              column(
                width = 3,
                radioButtons(inputId = "Sep", "Separator",
                             choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                             selected = ",")
              ),
              column(
                width = 3,
                radioButtons(inputId = "Quote", label = "Quote character",
                             choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                             selected = '"')
              )
            ),
            fluidRow(
              column(
                width = 6, offset = 3,
                # Input: Select a file ----
                fileInput(inputId = "CSVFile", label = "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                          )
              )
            ),
            fluidRow(
              column(
                width = 10, offset = 1,
                shinyjs::hidden(
                  selectInput(inputId = "Package", label = "Choose an available package containing data", choices = c(.packages(all.available = TRUE)), selected = "caret"),
                  selectInput(inputId = "DataSet", label = "Choose a data set", choices = c(), selected = NULL, width = "100%")
                )
              )
            ),
            dataTableOutput(outputId = "RawData")
          ),
          tabPanel(
            title = "Columns",
            fluidRow(
              column(
                width = 2, offset = 1,
                selectInput(inputId = "Class", label = "Dependent variable", choices = "", selected = "", multiple = FALSE)
              ),
              column(
                width = 2,
                selectInput(inputId = "ID", label = "Identifier variables", choices = "", selected = "", multiple = TRUE)
              ),
              column(
                width = 2,
                selectInput(inputId = "HideCol", label = "Remove variables", choices = "", selected = "", multiple = TRUE)
              ),
              column(
                width = 2,
                selectInput(inputId = "PreSplit", label = "Train-Test indicator", choices = "", selected = "", multiple = FALSE)
              ),
              column(
                width = 2,
                selectInput(inputId = "Weights", label = "Weighting", choices = "", selected = "", multiple = FALSE)
              )
            ),
            panel(
              verbatimTextOutput(outputId = "YSummary"),
              verbatimTextOutput(outputId = "DataIssues")
            ),
            tags$hr() ,     # Horizontal line ----
            htmlOutput(outputId= "RawDataSummary")
          ),
          tabPanel(
            title = "Homogeneity",
            verticalTabsetPanel(
              verticalTabPanel(
                title = "Missing data",
                withSpinner(
                  plotOutput(outputId= "MissingChart", height = "500px")
                ),
                box_height = 2
              ),
              verticalTabPanel(
                title = "Continuity",
                withSpinner(
                  plotOutput(outputId= "RisingChart", height = "500px")
                ),
                box_height = 2
              ),
              verticalTabPanel(
                title = "Control Chart",
                withSpinner(
                  plotOutput(outputId= "NaturalOrderChart", height = "500px")
                ),
                box_height = 2
                ),
              id = "HomogeneityVtabs",
              contentWidth = 10
          )
          ),
          tabPanel(
            title = "Novelties",
            tabsetPanel(
              tabPanel(
                title = "Uni-variable Continuous",
                checkboxInput(inputId = "BoxPlotNovelties", label = "Only show boxplots with novelties", value = TRUE),
                plotOutput(outputId= "NumericNovelties"),
                sliderInput(inputId = "Multiplier", label = "IQR Multiplier", min = 0, max = 10, step = 0.1, value = 1.5),
                dataTableOutput(outputId = "NumNoveltyTable")
              ),
              tabPanel(
                title = "Multi-variable Discontinuous",
                selectizeInput(inputId = "Factors", label = "Factors (in ascending complexity)", choices = NULL, selected = NULL, multiple = TRUE),
                plotOutput(outputId= "FactorNovelties", height = "600px")
              ),
              tabPanel(
                title = "Multi-variable Continuous",
                selectInput(inputId = "DimReductType", label = "Style of Dim Reduction", choices = c(""), selected = ""),
                plotOutput(outputId="MultiDimNovelties", height = "700px")
              ),
              type = "pills"
            )
          ),
          tabPanel(
            title = "Visualization",
            withSpinner(
              plotOutput(outputId = "YBalance")
            ),
            withSpinner(
              plotOutput(outputId= "FeaturePlot", height = "600px")
            )
          ),
          tabPanel(
            title = "Missingness",
            sliderInput(inputId = "ThresholdMissing", label="Threshold missing", min=0, max=0.5, value=0.1, step=0.01),
            withSpinner(
              plotOutput(outputId = "MissCorr")
            ),
            withSpinner(
              plotOutput(outputId = "MissingnessInformation")
            )
          )
        ),
        wellPanel(
          fluidRow(
            column( width = 4,
                    checkboxInput(inputId = "ScaleChart", label = "Normalize charts", value = TRUE),
                    checkboxInput(inputId = "ShowLegend", label = "Show Legends", value = TRUE)
            ),
            column( width = 6, offset = 1,
                    sliderInput(inputId = "Continuous", label = "Min. Proportion of distinct values to be continuous", min = 0, max = 1, step = 0.05, value = 0.5)
            )
          )
        )
      ),
      tabPanel(
        title = "Split", icon = icon("bolt"),
        h4("Train - Test Split"),
        sliderInput(inputId = "Ratio", label = "Split ratio", min = 0.3, max = 0.9, value = 0.8, step = 0.01),
        verbatimTextOutput(outputId = "SplitSummary"),
        wellPanel(
          textOutput(outputId = "SplitWarnings")
        )
      ),
      tabPanel(
        title = "Preproc", icon = icon("cogs"),
        h4("Preprocessing"),
        tabsetPanel(
          id = "PreProc",
          tabPanel(
            title = "Specification",
            fluidRow(
              column(
                width = 3, offset = 1,
                actionButton(inputId = "Restart", label = "Restart", icon = icon("repeat")),
                checkboxInput(inputId = "Missing", label = "Remove near-all-missing variables?", value = FALSE),
                selectInput(inputId = "Impute", label = "Missing values?", choices = c("None"="none", "Omit"="omit", "Impute KNN"="knnImpute", "Impute Bag"="bagImpute", "Impute Median/Mode"="medianImpute", "Impute Roll/Mode"="rollImpute", "Impute Mean"="mean"), selected = "omit"),
                selectInput(inputId = "Balance", label = "Balance dependent?", choices = c("None"="none", "Up"="up", "Down"="down", "Up-Down" = "up-down"), selected = "none"),
                checkboxInput(inputId = "NZV", label = "Remove Near-Zero-Variance variables?", value = FALSE)
              ),
              column(
                width = 3,
                checkboxInput(inputId = "LinComb", label = "Remove variables to make linearly independent?", value = FALSE),
                checkboxInput(inputId = "YJ", label = "Apply YeoJohnson rescaling?", value = FALSE),
                checkboxInput(inputId = "Other", label = "Pool rare levels?", value = FALSE),
                checkboxInput(inputId = "Convert", label = "Convert nominal predictors to binary", value = FALSE),
                checkboxInput(inputId = "OneHot", label = "Use One-Hot style?", value = FALSE)
              ),
              column(
                width = 3,
                selectizeInput(inputId = "DateFeatures", label = "Expand dates into features", selected = "decimal", multiple = TRUE,
                               choices = c("day of week" = "dow", "day of year" = "doy", "week", "month", "decimal", "quarter", "semester", "year")),
                checkboxInput(inputId = "Center", label = "Apply recentering?", value = FALSE),
                checkboxInput(inputId = "Scale", label = "Apply SD Rescaling?", value = FALSE),
                selectInput(inputId = "DimReduce", label = "Dimensionally reduce?", choices = c("None"="none", "Correlation"="corr", "PCA"="pca", "ICA" = "ica", "IsoMap"="isomap"), selected = "none"),
                sliderInput(inputId = "VarThresh", label = "Cummulative Variance threshold", min = 0.8, max = 0.99, step = 0.01, value = 0.95)
              )
            ),
            verbatimTextOutput("PreProcSummary")
          ),

          tabPanel(
            title = "Summary",
            htmlOutput(outputId= "ProcessedDataSummary")
          ),
          tabPanel(
            title = "Data Table",
            DT::dataTableOutput("PreProcTable")
          )
        )
      ),
      tabPanel(
        title = "Methods",  icon = icon("industry"),
        fluidRow(
          column(
            width = 3,
            checkboxInput(inputId = "AvailPackages", label = "Available packages only", value = TRUE),
            actionButton(inputId = "ChooseAll", label = "All", icon = icon("align-justify")),
            actionButton(inputId = "RefreshModels", label = "", icon = icon("refresh")),
            actionButton(inputId = "Suggest", label = "Suggest", icon = icon(name="plus-circle")),
            radioButtons(inputId = "ProbType", label = "Problem Type:",  choices = c("Any", "Classification","Regression"), selected = "Any", inline = TRUE)
          ),
          column(
            width = 3,
            radioButtons(inputId = "Linearity", label = "Model linearity", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE),
            radioButtons(inputId = "Ensembled", label = "Ensembled", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE),
            radioButtons(inputId = "Complexity", label = "Complexity", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE)
          ),
          column(
            width = 3,
            radioButtons(inputId = "Regularisation", label = "Regularisation", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE),
            radioButtons(inputId = "BiClass", label = "Two class", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE),
            radioButtons(inputId = "Probs", label = "Class probabilities", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE)
          ),
          column(
            width = 3,
            radioButtons(inputId = "Robustness", label = "Tolerance to outliers", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE),
            radioButtons(inputId = "Transparency", label = "Transparency", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE),
            radioButtons(inputId = "CatPredictors", label = "Categorical Predictors", choices = c("Avoid", "Neutral", "Only"), selected = "Neutral", inline = TRUE)
          )
        ),
        tabsetPanel(
          id = "Models",
          tabPanel(
            title = "Table",
            tags$h3("Select Candidate Models"),
            DT::dataTableOutput(outputId = "ModelTable")
          ),
          tabPanel(
            title = "Choosen",
            br(),
            actionButton(inputId = "Install", label = "Install missing packages", icon = icon(name="download")),
            hr(),
            tableOutput(outputId = "SelectedModelsTable"),
            textOutput(outputId = "SelWarn")
          ),
          tabPanel(
            title = "Chart",
            radioButtons(inputId = "DistMetric", label = "Distance metric", choices = c("binary","euclidean"), selected = "binary"),
            withSpinner(
              plotOutput("D2D", brush = "D2D_brush")
            ),
            tags$h3("Brushed Model Types"),
            tableOutput(outputId = "D2DTable")
          )
        )

      ),
      tabPanel(
        title = "Training",  icon = icon("dumbbell"),
        h4("Model training"),
        # "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models)
        # "timeslice" left out because it needs lots of extra parameters
        fluidRow(
          column(
            width = 2, offset = 1,
            # hiding methods "oob" & "timeslice"
            selectInput(inputId = "Method", label = "The resampling method", choices = c("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "adaptive_cv", "adaptive_boot", "LGOCV", "adaptive_LGOCV"), selected = "cv")
          ),
          column(
            width = 2,
            sliderInput(inputId = "TuneLength", label = "Tune length", min = 1, max = 15, step = 1, value = 5, ticks = FALSE),
            sliderInput(inputId = "Number", label = "Number of folds or iterations", min = 1, max = 15, step = 1, value = 10, ticks = FALSE)
          ),
          column(
            width = 2,
            selectInput(inputId = "HypMetric", label = "Hyper-parameter metric", regChoices, selected = "RMSE")
          ),
          column(
            width = 2,
            selectInput(inputId = "SelectionFunc", label = "Optimum selection", choices = selectionChoices, selected = selectionChoices[1]),
            selectInput(inputId = "Search", label = "Search type", choices = searchChoices, selected = searchChoices[1])
          ),
          column(
            width = 2,
            checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
            checkboxInput(inputId = "NullModel", label = "Add a null model", value = TRUE)
          ),
          column(
            width = 2,
            numericInput(inputId = "Repeats", label = "Number of CV repeats", min = 1, max = 10, step = 1, value = 1) #For repeated k-fold cross-validation only
          )
        ),
        fluidRow(
          column(
            width = 1, offset = 1,
            actionButton(inputId = "Reset", label = "", icon = icon("fast-backward"))
          ),
          column(
            width = 1, offset = 3,
            actionButton(inputId = "Train", label = "Train", icon = icon("play"))
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 2,
            wellPanel(
              radioButtons(inputId = "ResultsFor", label = "View training log", choices = "", selected = "", inline = TRUE)
            )
          ),
          column(
            width = 10,
            tabsetPanel(
              tabPanel(
                title = "Fit",
                verbatimTextOutput(outputId = "FinalModelCM"),
                plotOutput(outputId = "TrainedModelPlot", height = "600px")
              ),
              tabPanel(
                title = "Hyper-parameters",
                "Optimum Hyper-parameters",
                tableOutput(outputId = "Hyperparams"),
                plotOutput(outputId = "Optimize")
              ),
              tabPanel(
                title = "Variables",
                plotOutput(outputId = "VarImp", height = "600px"),
                verbatimTextOutput(outputId = "ModelCoef")
              ),
              tabPanel(
                title = "Plot",
                plotOutput(outputId = "TrainPlot")
              ),
              tabPanel(
                title = "Log",
                verbatimTextOutput(outputId = "TrainLog")
              )
              #verbatimTextOutput(outputId = "Predictors"),
              #verbatimTextOutput(outputId = "ModelSummary")
            )
          )
        )
      ),
      tabPanel(
        title = "Selection", icon = icon("balance-scale"),
        h4("Model Selection"),
        plotOutput(outputId = "Selection"),
        fluidRow(
          column(
            width = 2,
            checkboxInput(inputId = "NullNormalise", label = "Normalise the chart relative to the null model", value = TRUE)
          ),
          column(
            width = 2, offset = 2,
            wellPanel(
              radioButtons(inputId = "SelectedModel", label = "Choose the best performing model", choices = "", selected = "")
            )
          ),
          column(
            width = 2, offset = 1,
            selectizeInput(inputId = "Ensemble", label = "Choose a set to ensemble", multiple = TRUE, choices = "", selected = "")
          ),
          column(
            width = 2,
            actionButton(inputId = "AddModel", label = "Ensemble", icon = icon("play"))
          )
        ),
        plotOutput(outputId = "ResidualCorr", height = "600px"),
        checkboxInput(inputId = "ExcludeNull", label = "Exclude Null model", value = TRUE),
        plotOutput(outputId = "Cluster", height = "600px")
      ),
      tabPanel(
        title = "Performance", icon = icon("graduation-cap"),
        h4("Model Performance on unseen data"),
        plotOutput(outputId = "SelectedModelPlot", height = "600px"),
        verbatimTextOutput(outputId = "Performance"),
        verbatimTextOutput(outputId = "SelectedModelSummary")
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 1, offset = 5,
        actionButton(inputId = "Next", label = "Proceed", icon = icon("step-forward"))
      )
    )
  ))
