library(rlang)
options('na.action' = na.omit)

regChoices <- c("RMSE", "MAE", "Rsquared")
clasChoices <- c("Accuracy", "Kappa")
biClasChoices <- c("ROC", "Sens", "Spec")
minimiseMetric <- c("RMSE", "MAE")
selectionChoices <- c("best","oneSE", "tolerance")
searchChoices <- c("random","grid")
Verbose <- TRUE
maxRows <- 1000

source("RecipeSteps.R")
# consider a step for processing cyclical ordered factors e.g. Month

###########################################################################
allClass <- function(df) {
  if (length(df)==0) return(NULL)
  cname <- vector(length = ncol(df), mode = "character")
  for(col in 1:ncol(df)) {
    cname[col] <- class(df[, col])[1]
  }
  cname
}

###########################################################################
factorNames <- function(data) {
  colnames(data)[unlist(lapply(data, is.factor))]
}

###########################################################################
is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
  if(!is.numeric(x)) return(FALSE)
  x <- na.omit(x)
  all(x - round(x) < tol)
}

###########################################################################
is.binary <- function(x) {
  if(!is.numeric(x)) return(FALSE)
  x <- unique(x)
  length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)
}

###########################################################################
allLevels <- function(x) {
  unlist(lapply(unclass(x),nlevels))
}


cluster <- NULL
###########################################################################
startParallel <- function() {
  cluster <<- makePSOCKcluster(detectCores())
  registerDoParallel(cluster)
}


###########################################################################
stopParallel <- function() {
  stopCluster(cluster)
  registerDoSEQ()
  cluster <<- NULL
}


###########################################################################
madSummary <- function (data, lev = NULL, model = NULL) {
  out <- mad(data$obs - data$pred, na.rm = TRUE)
  names(out) <- "MAD"
  out
}

###########################################################################
remove_step <- function(recipe, classes) {
  steps <- recipe$steps
  if (length(steps)==0) return(recipe)
  steps2 <- list()
  for (i in 1:length(steps)) {
    if(!class(steps[[i]])[1] %in% classes) {
      steps2 <- rlist::list.append(steps2, steps[[i]])
    }
  }
  recipe$steps <- steps2
  recipe
}


###########################################################################
DataSummary <- function(data) {
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
  missingRate <- vector(mode = "numeric", length = nd)
  for (col in 1:nd) {
    column <- na.omit(data[, col, drop=TRUE])
    numeric[col] <- is.numeric(column)
    wholeNumb[col] <- is.wholenumber(column)
    text[col] <- is.character(column)
    binary[col] <- is.binary(column)
    factor[col] <- is.factor(column)
    date[col] <- is(column, "Date")
    logical[col] <- is.logical(column)
    uniqueness[col] <- length(unique(column))
    uniqueRatio[col] <- uniqueness[col] / length(column)
    type[col] <- class(data[,col])[1]
    missingRate <- sum(is.na(data[,col])) / length(data[,col])
  }
  constant <- uniqueness == 1
  data.frame(numeric, wholeNumb, text, binary, factor, date, logical, uniqueness, uniqueRatio, constant, type, missingRate, row.names = colnames(data))
}

nzvf <- function (x, freqCut = 95/5, uniqueCut = 10) {
  t <- table(x[!is.na(x)])
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


# cost <- function (data, lev = NULL, model = NULL)
# {
#   lvls <- levels(data$obs)
#   if (!all(levels(data[, "pred"]) == lvls))
#     stop("levels of observed and predicted data do not match")
#   CM <- table(data[, "pred"], data[, "obs"])
#   CMcost <- CM * costMat
#   c("Cost" = CMcost)
# }
