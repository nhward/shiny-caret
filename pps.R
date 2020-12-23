# Read https://towardsdatascience.com/rip-correlation-introducing-the-predictive-power-score-3d90808b9598

library(doParallel)
library(foreach)
library(rpart)
library(recipes)

getOper <- function(x) {
  if (x) `%dopar%` else  `%do%`
}

binary2Factor <- function(data) {
  for (c in 1:ncol(data)) {
    col <- data[,c,drop = TRUE]
    if (is(col, "numeric")) {
      if (length(unique(col)) <= 2 & all(col %in% c(0:1))) {
        #print(paste("converting", colnames(data)[c]))
        col <- ifelse(col == 0, "N", "Y")
        data[,c] <- as.factor(col)
      }
    }
  }
  data
}



# no cross validation employed so results are optimistic (but fast)
# no rpart hyperparameter (cp) optimisation
# currently not suited to unbalanced factors due to use of accuracy 
pps <- function(data, allowParallel = TRUE, limit = 5000) {
  if (!is(data, "data.frame")) {
    data <- as.data.frame(data)
  }
  
  data <- recipes::recipe(~., data = data) %>%
    recipes::step_zv(everything()) %>% #Constant columns crash rpart
    recipes::step_other(all_nominal()) %>% #Important to remove high cardinality for rpart
    recipes::prep(training = data) %>%
    recipes::juice(composition = "data.frame")
  if (ncol(data) <= 1) return(NULL)
  if (nrow(data) > limit) {
    index <- sample(x = 1:nrow(data), size = limit)
    data <- data[index,]
  }
  data <- binary2Factor(data)
  pairs <- rep(1:ncol(data), times = ncol(data))
  pairs <- matrix(data = c(pairs, sort(pairs)), byrow = TRUE, nrow = 2)
  pairs <- pairs[, pairs[1,] != pairs[2,]]

  `%op%` <- getOper(allowParallel && foreach::getDoParWorkers() > 1)
  
  pp <- foreach::foreach( col = 1:ncol(pairs), .combine = c)  %op% {
    pair <- pairs[,col, drop = TRUE]
    form <- as.formula(paste(collapse = "~", colnames(data)[pair]))
    model <- rpart::rpart(formula = form, data = data)
    pred = predict(model, type = "vector")
    obs = data[, pair[1], drop = TRUE]
    if (is(obs, "numeric")) {
      residual <- obs - pred
      mse <- mean(residual*residual, na.rm = TRUE)
      rel <- 1 - mse / var(obs)
      rel <- sqrt(max(rel,0))
    } else {
      pred <- levels(obs)[pred]
      accuracy <- sum(obs == pred, na.rm = TRUE)  / sum(!is.na(pred))  ## accuracy is not ideal
      tab <- table(obs)
      nullRate <- tab[which.max(tab)]  / sum(!is.na(pred))
      rel <- (accuracy - nullRate) / (1 - nullRate)
      rel <- max(rel,0)
    }
    rel
  }
  
  # Version for debugging  
  # pp <- vector(mode = "numeric", length = ncol(pairs))
  # for (col in 1:ncol(pairs)) { 
  #   pair <- pairs[,col, drop = TRUE]
  #   form <- as.formula(paste(collapse = "~", colnames(data)[pair]))
  #   model <- rpart::rpart(formula = form, data = data, )
  #   pp[col] <- assessModel(pred = predict(model, type = "vector"), obs = data[, pair[1], drop = TRUE])
  # }
  
  m <- matrix(data = 1, nrow = ncol(data), ncol = ncol(data), dimnames = list(colnames(data), colnames(data))) # use byrow = T if the matrix needs a transpose 
  for (col in 1:ncol(pairs)) {
    pair <- pairs[,col, drop = TRUE]
    m[pair[1], pair[2]] <- pp[col]
  }
  m
}

