# Shiny Caret ----
# An interactive interface for using various machine learning methods from the caret package
# (c) Nick Ward (University of Canterbury) 2018-2020

# * 1 Packages ----
# * 1 * 1 General ----
library(stats, quietly = TRUE)
library(psych, quietly = TRUE)
library(stringi, quietly = TRUE)
library(rlang, quietly = TRUE)
library(doParallel, quietly = TRUE)
library(RSpectra, quietly = TRUE)
library(xlsx, quietly = TRUE)
library(readxl, quietly = TRUE)
library(energy, quietly = TRUE)
library(lubridate, quietly = TRUE)

# * 1 * 2 Data manipulation ----
library(plyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(reshape2, quietly = TRUE)
library(stringr, quietly = TRUE)
library(summarytools, quietly = TRUE)
library(cluster, quietly = TRUE)

# * 1 * 3 Shiny related ----
library(shiny, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(shinyalert, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyBS, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
library(shinycssloaders, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(shinydashboard, quietly = TRUE)
library(shinydashboardPlus, quietly = TRUE)
library(shinyFiles, quietly = TRUE)

# * 1 * 4 Caret & Recipes ----
library(caret, quietly = TRUE)
library(caretEnsemble, quietly = TRUE)
#library(modelgrid,quietly = TRUE)
library(MLmetrics, quietly = TRUE)
library(recipes, quietly = TRUE)
library(embed, quietly = TRUE)
library(textrecipes, quietly = TRUE)
#library(breakDown, quietly = TRUE)
library(themis, quietly = TRUE)
#library(modeltime)
#library(timetk)   

if (!library(recipeselectors, quietly = TRUE, logical.return = TRUE) ) {
  library(devtools)
  # Follow https://github.com/stevenpawley/recipeselectors
  devtools::install_github("stevenpawley/recipeselectors")
}

# * 1 * 5 Visualisation ----
library(vcd, quietly = TRUE)
library(ellipse, quietly = TRUE)
library(lattice, quietly = TRUE)
library(RANN) # check need
library(PerformanceAnalytics, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(ggalluvial, quietly = TRUE)
library(RColorBrewer, quietly = TRUE)
library(dimRed, quietly = TRUE)
library(rpart.plot, quietly = TRUE)
library(plotly, quietly = TRUE)
library(corrgram, quietly = TRUE)
if (!library(tabplot, quietly = TRUE, logical.return = TRUE)) {
  library(devtools, quietly = TRUE)
  # crashes  devtools::install_github("mtennekes/tabplot")
}
if (!library(mixOmics, quietly = TRUE, logical.return = TRUE)) {
  library(BiocManager, quietly = TRUE)
  BiocManager::install('mixOmics')
}
library(aplpack, quietly = TRUE)
library(plotROC, quietly = TRUE)
library(dbscan, quietly = TRUE)
library(Rlof, quietly = TRUE)
library(fpc, quietly = TRUE)
library(factoextra, quietly = TRUE)
library(xtable, quietly = TRUE)
library(ggcorrplot, quietly = TRUE)
library(cowplot, quietly = TRUE)


# * 2 Meta code ----
# * 2 * 1 Options ----
options(shiny.maxRequestSize = 50*1024^2, na.action = na.exclude)

# * 2 * 2 Constants ----
DEV <- FALSE
MAXCOLS <- 20  #limit to size of correlation matrix
missingStrings <- c("NA","N/A", "--")
regChoices <- c("RMSE", "MAE", "Rsquared")
multiClassChoices <- list("Accuracy","Kappa","Mean_F1", "Mean_Sensitivity", "Mean_Specificity",
                       "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value", "Mean_Precision", "Mean_Recall",
                       "Mean_Detection_Rate", "Mean_Balanced_Accuracy","logLoss", "AUC", "prAUC")
biClassChoices <- list("Accuracy","Kappa","ROC","Sens","Spec", "AUC","Precision","Recall", "F")
minimiseMetric <- list("RMSE", "MAE")
selectionChoices <- list("best","oneSE", "tolerance")
searchChoices <- list("random","grid")
missChoices = c("None" = "none", "Omit Miss Obs" = "omit", "Impute KNN" = "knnImpute", "Omit/Impute KNN" = "Omit/knnImpute", "Impute Bag" = "bagImpute", "Omit/Impute Bag" = "Omit/bagImpute", "Impute Median/Mode" = "median.mode", "Impute Mean/Mode" = "mean.mode")
Verbose <- FALSE
maxRows <- 1000
roots <- c(wd = ".")
FullHeight <- "640px"

# Spinner style
SPINNER_COLOUR <- "#FFFFFF"
SPINNER_TYPE <- 4
SPINNER_SIZE <- 2

PROJECT_FOLDER <- "Projects"
INPUT <- list()
REACT <- list()

# Available Colours: red,yellow,aqua,blue,light-blue,green,navy,teal,olive,lime,orange,fuchsia,purple,maroon,black,
DATAColour <- "light-blue"
EDAColour <- "blue"
SAMPColour <- "yellow"
PROCColour <- "olive"
METHColour <- "red"
TRAINColour <- "aqua"
MODELSColour <- "teal"
GRADColour <- "maroon"
ANAColour <- "orange"
  
# * 2 * 3 Source files ----
source("RecipeSteps.R")
source("pps.R")

# * 3 * Functions  ----
# * 3 * 1 Utility ----

#' plotly
#'
#' @param plot - A ggplot chart
#'
#' @return a plotly chart with full transparency
plotly <- function(plot, tooltip = NULL) {
  plot <- plotly::ggplotly(plot, tooltip = tooltip) %>% 
    plotly::layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
                   modebar = list(orientation = "v", bgcolor = 'rgba(0,0,0,0)'), margin = list(l = 0, r = 50, t = 50, b = 0),
                   legend = list(orientation = "v", y = 0, x = 100)) %>% 
    plotly::config(displaylogo = FALSE, editable = TRUE, edits = list(legendPosition = TRUE)) ##, displayModeBar = "hover")
  plot
}

sideBySide <- function(plots, tooltips = NULL, widths = NULL) {
  for (i in length(plots)) {
    plots[[i]] <- plotly::ggplotly(p = plots[[i]], tooltip = tooltips[i] )
  }
  plotly::subplot(plots, shareY = TRUE, widths = widths/12, titleX = TRUE) %>%
    plotly::layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
                   modebar = list(orientation = "v", bgcolor = 'rgba(0,0,0,0)'), margin = list(l = 0, r = 50, t = 50, b = 0),
                   legend = list(orientation = "h", y = 1.02, x = 1)) %>% 
    plotly::config(displaylogo = FALSE, editable = TRUE, edits = list(legendPosition = TRUE))
}


#' is.data.frame
#'
#' @param data 
#'
#' @return whether a data.frame
#' @export
is.dataframe <- function(data) {
  !is.null(data) && is(get(data),"data.frame")
}
  
  #' is.wholenumber
  #'
  #' @param x - numeric vector
  #' @param tol - tolerance to employ
  #'
  #' @return - whether values are ALL whole numbers
  #' @export
  #'
  is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
    if (!is.numeric(x)) return(FALSE)
    x <- na.omit(x)
    all(x - round(x) < tol)
  }
  
  
  #' is.binary
  #'
  #' @param x - a numeric vector
  #'
  #' @return - whether values are ALL binary (i.e. 0, 1)
  #' @export
  #'
  is.binary <- function(x) {
    if (!is.numeric(x)) return(FALSE)
    x <- unique(x, incomparables = c(NA))
    length(x) == 2 && all(x == 0 | x == 1)
  }
  

#' formattedColNames
#'
#' @param df  - a data.frame object
#'
#' @return - a character vector of <name [class]> 
#' @export
#'
formattedColNames <- function(df) {
  if (length(df) == 0) return(NULL)
  cname <- vector(length = ncol(df), mode = "character")
  for (col in 1:ncol(df)) {
    cname[col] <- class(df[, col])[1]
  }
  fcols <- colnames(df)
  names(fcols) <- paste0(fcols," [", cname,"]")
  fcols
}

equals <- function(x,y) {
  if ( !any(class(x) %in% c("NULL", "NA", "character", "logical", "numeric"))) return(identical(x,y))
  if (length(x) != length(y)) return(FALSE)
  if (all(is.null(x) & is.null(y))) return(TRUE)
  if (any(is.null(x) | is.null(y))) return(FALSE)
  if (all(is.na(x) & is.na(y))) return(TRUE)
  if (any(is.na(x) | is.na(y))) return(FALSE)
  return(all(identical(x,y)))
}

#' allClass
#'
#' @param df - a data.frame object
#'
#' @return a character vector of column classes. If a column has multiple classes only the first is returned.
#' @export
#'
allClass <- function(df) {
  if (length(df) == 0) return(NULL)
  cname <- vector(length = ncol(df), mode = "character")
  for (col in 1:ncol(df)) {
    cname[col] <- class(df[, col])[1]
  }
  cname
}

#' factorNames
#'
#' @param data - a data.frame object
#'
#' @return - a character vector of factor-column names
#' @export
#'
factorNames <- function(data) {
  colnames(data)[unlist(lapply(data, is.factor))]
}

#' numericNames
#'
#' @param data - a data.frame object
#'
#' @return - a character vector of numeric-column names
#' @export
#'
numericNames <- function(data) {
  colnames(data)[unlist(lapply(data, is.numeric))]
}

lowCardardinality <- function(data, cardinality) {
  lc <- function(x, cardinality) {
    nlevels(x) <= cardinality
  }
  colnames(data)[unlist(lapply(data, lc, cardinality))]
}

#' Create observation weights based on balancing a set of factors
#'
#' @param factors 
#' @param data 
#'
#' @return a revised data frame with an extra column called "weightings"
observationWeights <- function(factors = lowCardinality(data, 15), data = data) {
  if (length(factors) == 0) return(data)
  d <- data[, factors, drop = FALSE]
  for (factor in factors) {
    if (!is.factor(d[,factor])) {
      d[, factor] <- as.factor(d[, factor, drop = TRUE])
    }
  }
  form <- as.formula(paste0("~", paste(factors, collapse = ":"), "-1"))
  ZZZweights <- 1/colSums(model.matrix(form, data = d))
  dm <- data.frame(ZZZweights, names = names(ZZZweights))
  data$joiner <- NULL
  for (name in factors) {
    temp <- paste(sep = "", paste0(name, data[, name]))
    if (is.null(data$joiner)) {
      data$joiner <- temp
    } else {
      data$joiner <- paste(sep = ":",data$joiner, temp)
    }
  }
  results <- dplyr::left_join(data, dm, by = c("joiner" = "names"))
  results$joiner <- NULL
  names(results)[ncol(results)] <- "Weighting"
  results$Weighting <- results$Weighting * nrow(results) / sum(results$Weighting)
  results
}

#' allLevels <- function(x) {
#'   unlist(lapply(unclass(x),nlevels))
#' }
#'


#' IQROutlierss
#'
#' @param x - a numeric vector 
#' @param coef - the IQR multiplier
#' @return - a logical vector of being an IQR outlier
#' @export
#'
IqrOutliers <- function(x, coef = 1.5) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - iqr * coef
  upper <- q[2] + iqr * coef
  outlier <- !is.na(x) & (x > upper | x < lower)
  outlier
}

outlierCount <- function(x, mult = 1.5, yj = FALSE) {
  if (is(x, "numeric")) {
    if (yj) {
      x <- as.data.frame(x)
      mod <- caret::preProcess(x, method = "YeoJohnson")
      x <- predict(mod, x)
      x <- x$x
    }
    stat <- boxplot.stats(x = x, coef = mult, do.out = TRUE)
    return(length(stat$out))
  } else {
    return(NA)
  }
}

DataSummary <- function(data, multiplier = 1.5, yj = FALSE) {
  nd <- ncol(data)
  numeric <- vector(mode = "logical", length = nd)
  wholeNumb <- vector(mode = "logical", length = nd)
  text <- vector(mode = "logical", length = nd)
  binary <- vector(mode = "logical", length = nd)
  factor <- vector(mode = "logical", length = nd)
  date <- vector(mode = "logical", length = nd)
  logical <- vector(mode = "logical", length = nd)
  uniqueness <- vector(mode = "integer", length = nd)
  uniqueRatio <- vector(mode = "numeric", length = nd)
  type <- vector(mode = "character", length = nd)
  missing <- vector(mode = "numeric", length = nd)
  missingRate <- vector(mode = "numeric", length = nd)
  notMissing <- vector(mode = "numeric", length = nd)
  imbalance <- rep(NA, times = nd)
  imbalanceRatio <- rep(NA, times = nd)
  nzv <- rep(FALSE, times = nd)
  outliers <-  rep(0, times = nd)
  for (col in 1:nd) {
    column <- na.omit(data[, col, drop = TRUE])
    numeric[col] <- is.numeric(column)
    wholeNumb[col] <- is.wholenumber(column)
    text[col] <- is.character(column)
    binary[col] <- is.binary(column)
    factor[col] <- is.factor(column)
    date[col] <- is(column, "Date") || is(column,"POSIXct") || is(column, "POSIXlt")
    logical[col] <- is.logical(column)
    uniqueness[col] <- sum(!is.na(unique(column)))
    uniqueRatio[col] <- uniqueness[col] / sum(!is.na(column))
    type[col] <- class(data[,col])[1]
    if (wholeNumb[col]) type[col] <- "integer"
    if (binary[col]) type[col] <- "binary"
    missing[col] <- sum(is.na(data[,col]))
    missingRate[col] <- sum(is.na(data[,col])) / nrow(data)
    notMissing[col] <- sum(!is.na(data[,col]))
    if (binary[col] | factor[col]) {
      tab <- table(data[,col], useNA = "no")
      expected <- sum(tab) / length(tab)
      p <- max(tab)/notMissing[col]
      sd <- sqrt(notMissing[col] * p * (1 - p))
      imbalance[col] <- (max(tab) - expected) / sd
      imbalanceRatio[col] <- max(tab) / expected
    }
    nzv[col] <- any(caret::nzv(column) == 1)
    outliers[col] <- outlierCount(column, multiplier, yj)
  }
  constant <- uniqueness == 1
  data.frame(numeric, wholeNumb, text, binary, factor, date, logical, uniqueness, uniqueRatio, constant, type, missing, missingRate, notMissing, imbalance, imbalanceRatio, nzv, outliers, row.names = colnames(data))
}

nzvf <- function(x, freqCut = 95/5, uniqueCut = 10) {
  t <- table(x, useNA = "no")
  if (length(t) <= 1) {
    freqRatio <- 0
  } else {
    w <- which.max(t)
    freqRatio <- (max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
  }
  lunique <- length(unique(x[!is.na(x)]))
  percentUnique <- 100 * lunique/length(x)
  zeroVar <- (lunique == 1) | all(is.na(x))
  (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar
}


missingCounts <- function(x) {
  if (!(is.matrix(x) || is.data.frame(x)))
    stop("Data should be a matrix or dataframe")
  if (ncol(x) < 2)
    stop("Data should have at least two columns")
  R <- is.na(x)
  nmis <- colSums(R)
  R <- matrix(R[, order(nmis)], dim(x))
  pat <- apply(R, 1, function(x) paste(as.numeric(x), collapse = ""))
  sortR <- matrix(R[order(pat), ], dim(x))
  if (nrow(x) == 1) {
    mpat <- is.na(x)
  }
  else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  if (all(!is.na(x))) {
    mpat <- t(as.matrix(mpat, byrow = TRUE))
    rownames(mpat) <- table(pat)
  }
  else {
    if (is.null(dim(mpat))) {
      mpat <- t(as.matrix(mpat))
    }
    rownames(mpat) <- table(pat)
  }
  r <- cbind(abs(mpat - 1), rowSums(mpat))
  r <- rbind(r, c(nmis[order(nmis)], sum(nmis)))
  rowOrder <- order(as.numeric(rownames(r)), decreasing = FALSE)
  r <- r[rowOrder,]
  
  plot.new()
  if (is.null(dim(sortR[!duplicated(sortR), ]))) {
    R <- t(as.matrix(r[1:nrow(r) - 1, 1:ncol(r) - 1]))
  }
  else {
    if (is.null(dim(R))) {
      R <- t(as.matrix(R))
    }
    R <- r[1:nrow(r) - 1, 1:ncol(r) - 1]
  }
  scale = 0.9
  par(mar = c(3,3,5,2))
  plot.window(xlim = c(-1, ncol(R) + 1), ylim = c(-1, nrow(R) + 1), asp = 1)
  M <- (cbind(c(row(R)), c(col(R))) - 1) * scale
  
  shade <- ifelse(R[nrow(R):1, ], 1, 2)
  rect(M[, 2], M[, 1], M[, 2] + 1, M[, 1] + 1, col = shade)
  for (i in 1:ncol(R)) {
    text(x = (i - 0.5)*scale, y = (nrow(R) + 0.3)*scale, labels = colnames(r)[i], adj = 0, srt = 90)
    text(x = (i - 0.5)*scale, y = -0.3*scale, labels = nmis[order(nmis)][i], adj = 1, srt = 90)
  }
  for (i in 1:nrow(R)) {
    text(x = (ncol(R) + 0.3)*scale, y = (i - 0.5)*scale, labels = r[(nrow(r) - 1):1, ncol(r)][i], adj = 0)
    text(x = -0.5*scale, y = (i - 0.5)*scale, labels = rownames(r)[(nrow(r) - 1):1][i], adj = 1)
  }
  text(x = (ncol(R) + 1)*scale, y = -1*scale, labels = r[nrow(r), ncol(r)], adj = 0, cex = 1.5)
  title(main = paste("Missing v alue counts for", nrow(x), "observarions"))
  text(x = -3 * scale, y = nrow(R)/2 * scale, labels = "Counts of missing-pattern", adj = 0.5, srt = 90)
  text(x = (ncol(R) + 3) * scale, y = nrow(R)/2 * scale, labels = "Missing-counts within pattern", adj = 0.5, srt = 90)
  text(x = ncol(R)/2, y = -3, labels = "Missing-counts within variable", adj = 0.5, srt = 0)
}

# 3 * 5 Project related

getProjects <- function() {
  gsub(pattern = "\\.rdata$", x = dir(path = "data", pattern = ".*\\.rdata$", ignore.case = TRUE), replacement = "", ignore.case = TRUE)
}


saveProject <- function(project) {
  if (project == "") return(TRUE)
  require(R.utils, quietly = TRUE)
  if (!file.exists("data")) {
    R.utils::mkdirs("data")
  }
  name <- paste0("data/", project, ".rdata")
  save(list = ls(), file = name)
}


make.affinity <- function(Sim, neighbours = 2) {
  N <- length(Sim[,1])
  if (neighbours >= N) {  # fully connected
    Aff <- Sim
  } else {
    Aff <- matrix(rep(0,N^2), ncol = N)
    for (i in 1:N) { # for each line
      # only connect to those points with large similarity
      best.similarities <- sort(Sim[i,], decreasing = TRUE)[1:neighbours]
      for (s in best.similarities) {
        j <- which(Sim[i,] == s)
        Aff[i,j] <- Sim[i,j]
        Aff[j,i] <- Sim[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  #make binary (either connected or not)
  Aff[Aff > 0] <- 1
  colnames(Aff) <- colnames(Sim)
  rownames(Aff) <- colnames(Sim)
  Aff
}


getHelp <- function() {
  help <- xlsx::read.xlsx(file = "help.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE)
}


isRoleValid <- function(target, data = NULL) {
  if (is.null(target)) return(FALSE)
  if (target == "") return(FALSE)
  if (!is.null(data) & !target %in% colnames(data)) return(FALSE)
  TRUE
}

getModelHoldOutResults <- function(mod) {
  indexes <- data.frame( Resample = c(), rowIndex = c())
  for (name in names(mod$control$indexOut)) {
    dfname <- data.frame( "Resample" = name, "rowIndex" = mod$control$indexOut[[name]], stringsAsFactors = FALSE )
    indexes <- rbind(indexes, dfname)
  }
  mod$bestTune %>% 
    inner_join(mod$pred, by = colnames(mod$bestTune)) %>% 
    inner_join(indexes, by = c("Resample", "rowIndex"))
}

proper <- function(text) {
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(text), perl=TRUE)
}


#' cor.distance - calculate "distance correlation" based upon the energy package
#'
#' @param data - a numeric matrix or all-numeric data.frame object
#'
#' @return - a non-symmetric correlation matrix
#' @export
#'
cor.distance <- function(data) {
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  if (!is.numeric(data)) {
    stop("Supply a numeric data variable")
  }
  d <- ncol(data)
  m <- matrix(NA, nrow = d, ncol = d)
  for (i in 1:d) {
    for (j in i:d) {
      if (j == i) {
        m[i,j] <- 1
      } else {
        dd <- na.omit(data[,c(i,j)])
        m[i,j] <- energy::dcor(dd[,1, drop = TRUE], dd[,2, drop = TRUE])
      }
    }
  }
  
  for (i in 2:d) {
    for (j in 1:i) {
      if (j == i) {
        m[i,j] <- 1
      } else {
        m[i,j] <- m[j,i]
      }
    }
  }
  colnames(m) <- colnames(data)
  rownames(m) <- colnames(data)
  m
}

# madSummary <- function(data, lev = NULL, model = NULL) {
#   out <- stats::mad(data$obs - data$pred, na.rm = TRUE)
#   names(out) <- "MAD"
#   out
# }

# 3 * 2 Parallel ----

#' startParallel
#'
#' @param name - a name to give this parallel process
#' @return - an object to pass to stopParallel()
#' #'
#' @export
#'
startParallel <- function(name) {
  outfile <- paste0(tempdir(), .Platform$file.sep, name,".txt")
  unlink(outfile)
  cluster <- parallel::makePSOCKcluster(parallel::detectCores(), outfile = outfile)
  parallel::clusterEvalQ(cluster, library("plyr"))
  parallel::clusterEvalQ(cluster, library("dplyr"))
  matches <- c(ls(pattern = ".*step.*", envir = as.environment("package:recipes")),
               ls(pattern = ".*step.*", envir = as.environment("package:embed")),
               ls(pattern = ".*step.*", envir = as.environment("package:textrecipes")),
               ls(pattern = ".*step.*", envir = .GlobalEnv))
  parallel::clusterExport(cl = cluster, varlist = matches, envir = environment())
  clusterOutfile <- outfile
  doParallel::registerDoParallel(cluster)
  list(cluster = cluster, clusterOutfile = clusterOutfile)
}


#' stopParallel
#' @param obj - the value returned from startParallel()
#'
#' @return - any recorded error messages during parallel execution
#' @export
#'
stopParallel <- function(obj) {
  text <- paste(readLines(con = obj[["clusterOutfile"]]), collapse = "\n")
  parallel::stopCluster(obj[["cluster"]])
  foreach::registerDoSEQ()
  unlink(obj$clusterOutfile)
  text
}

# 3 * 3 Step related ----


remove_step <- function(recipe, ids) {
  steps <- recipe$steps
  if (length(steps) == 0) return(recipe)
  
  steps2 <- list()
  pat <- paste0("^(", paste0(ids,"_", collapse = "|"), ".*)")
  for (i in 1:length(steps)) {
    id <- steps[[i]]$id
    if (is.null(id)) next
    if (!grepl(pattern = pat, x = id)) {
      steps2 <- rlist::list.append(steps2, steps[[i]])
    }
  }
  recipe$steps <- steps2
  recipe
}


get_step <- function(recipe, id) {
  steps <- recipe$steps
  if (length(steps) == 0) return(NULL)
  for (i in 1:length(steps)) {
    if (grepl(pattern = paste0("^",id,"_"), x = steps[[i]]$id)) {
      return(i)
    }
  }
  return(NULL)
}
