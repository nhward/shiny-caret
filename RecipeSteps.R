library(recipes)
library(Boruta)

# cyclic ----

step_cyclic_new <- function(terms, role, trained, skip, id, levels, columns) {
  step(
    subclass = "cyclic",
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id,
    levels = levels,
    columns = columns
  )
}

#' A recipe step, as per the recipes package, that converts named ordinal factor
#' variables into a pair of sine & cosine variables that allows meaningful
#' distance calculations to be performed.The original variables are dropped.
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which variables will be used to create the new variables. The selected variables should have class Date or POSIXct. See selections() for more details. For the tidy method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new variable columns created by the original variables will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake.recipe()? While all operations are baked when prep.recipe() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of recipe with the new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
step_cyclic <- function(recipe, ..., role = "predictor", trained = FALSE,
                        skip = FALSE, id = rand_id("cyclic")) {
  add_step(recipe,
           step_cyclic_new(terms = recipes::ellipse_check(...),
                           role = role,
                           trained = trained,
                           skip = skip,
                           id = id,
                           levels = NULL,
                           columns = NULL
           )
  )
}

prep.step_cyclic <- function(x, training, info = NULL, ...) {
  candidates <- training[, recipes::terms_select(x$terms, info = info), drop = FALSE]
  cols <- rep(FALSE, times = ncol(candidates))
  lvls <- rep(0, times = ncol(candidates))
  i <- 0
  for (col in colnames(candidates)) {
    i <- i + 1
    var <- candidates[ , col, drop = TRUE]
    cols[i] <- is(var, "ordered") && length(levels(var)) > 2
    lvls[i] <- length(levels(var))
  }
  step_cyclic_new(terms = x$terms, role = x$role, trained = TRUE, skip = x$skip,
                  id = x$id, levels = lvls[cols], columns  = colnames(candidates)[cols])
}

#' @export
bake.step_cyclic <- function(object, new_data, ...) {
  if (length(object$columns) > 0) {
    for (col in 1:length(object$columns)) {
      colname <- object$columns[col]
      var <- as.integer(new_data[,colname, drop = TRUE]) - 1 #zero based
      levels <- object$levels[col]
      sinOrds <- sin(var * 2 * pi / levels)
      cosOrds <- cos(var * 2 * pi / levels)
      new_names <- c(colnames(new_data), paste0(colname, "_sin"), paste0(colname, "_cos"))
      new_data <- cbind(new_data, sinOrds, cosOrds)
      colnames(new_data) <- new_names
      remove <- which(colnames(new_data) == colname)
      new_data <- new_data[, -remove]
    }
  }
  as_tibble(new_data)
}

#' @export
print.step_cyclic <- function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    if (length(x$columns) > 0) {
      newNames <- c(paste0(x$columns,"_sin"), paste0(x$columns,"_cos"))
      cat("Cyclic step added variables: ")
      cat(newNames)
    } else
      cat("Cyclic step added no terms")
  } else {
    cat("Cyclic step for ")
    cat(recipes::format_selectors(x$terms, width = width))
  }
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}

tidy.step_cyclic <- function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}


# missingVar ----

step_missingVar_new <- function(terms, role, trained, ratio, removals, skip, id) {
  step(
    subclass = "missingVar",
    terms = terms,
    role = role,
    trained = trained,
    ratio = ratio,
    removals = removals,
    skip = skip,
    id = id
  )
}

#' A recipe step, as per the recipes package, that identifies variables that have excessive missing values and drops them. The default is to treat 50% (and above) NA values as excessive. This applies to any class of variable.
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which variables will be used to create the new variables. The selected variables should have class Date or POSIXct. See selections() for more details. For the tidy method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new variable columns created by the original variables will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param ratio A threshold number [0-1] for the worst level of variable missingness.
#' @param removals A character string of variables that will be used as inputs. This field is a placeholder and will be populated once prep.recipe() is used.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake.recipe()? While all operations are baked when prep.recipe() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#'
#' @return An updated version of recipe with the new step added to the sequence of existing steps (if any).
#' @export
step_missingVar <- function(recipe,
                            ...,
                            role = NA,
                            trained = FALSE,
                            ratio = 0.5,
                            removals = NULL,
                            skip = TRUE, #do not do this with unseen data
                            id = rand_id("missingVar")) {
  add_step(
    recipe,
    step_missingVar_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      ratio = ratio,
      removals = removals,
      skip = skip,
      id = id
    )
  )
}

prep.step_missingVar <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(x$terms, info = info)
  if (nrow(training) == 0) {
    filter <- rep(FALSE, length(col_names))
  } else {
    missing <- is.na(training[,col_names])
    colCnt <- apply(X = missing, MARGIN = 2, FUN = sum)
    filter <- colCnt >= nrow(missing) * x$ratio
  }
  step_missingVar_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    ratio = x$ratio,
    removals = col_names[filter],
    skip = x$skip,
    id = x$id
  )
}

bake.step_missingVar <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
}

#' @export
print.step_missingVar <- function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    if (length(x$removals) > 0) {
      cat("Heavily missing variables step (ratio > ", x$ratio, ") removed: ", sep = "")
      #cat(recipes::format_ch_vec(x$removals, width = width))
      cat(x$removals)
    } else
      cat("Heavily missing variables step (ratio > ", x$ratio, ") removed no variables", sep = "")
  } else {
    cat("Heavily missing variables step (ratio > ", x$ratio, ") for variables", sep = "")
    cat(recipes::format_selectors(x$terms, width = width))
  }
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}

#' @export
tidy.step_missingVar <- function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}



# missingObs ----

step_missingObs_new <- function(terms, role, trained, ratio, columns, skip, id) {
  step(
    subclass = "missingObs",
    terms = terms,
    ratio = ratio,
    columns = columns,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' A recipe step, as per the recipes package, that identifies observations that have excessive missing values and drops them. The default is to treat 50% (and above) NA values as excessive. This applies to any class of variable.
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which variables will be used to create the new variables. The selected variables should have class Date or POSIXct. See selections() for more details. For the tidy method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new variable columns created by the original variables will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param ratio A threshold number [0-1] for the worst level of observation missingness.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake.recipe()? While all operations are baked when prep.recipe() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of recipe with the new step added to the sequence of existing steps (if any).
#' @export
step_missingObs <- function(recipe,
                            ...,
                            role = NA,
                            trained = FALSE,
                            ratio = 0.5,
                            skip = TRUE,  #do not apply this to unseen data
                            id = rand_id("missingObs")) {
  
  add_step(recipe,
           step_missingObs_new(
             terms = ellipse_check(...),
             role = role,
             trained = trained,
             ratio = ratio,
             columns = NULL,
             skip = skip,
             id = id
           ))
}

prep.step_missingObs <- function(x, training, info = NULL, ...) {
  step_missingObs_new(
    terms = x$terms,
    ratio = x$ratio,
    columns = recipes::terms_select(x$terms, info = info),
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

bake.step_missingObs <- function(object, new_data, ...) {
  missing <- is.na(new_data[,object$columns])
  rowCnt <- apply(X = missing, MARGIN = 1, FUN = sum)
  heavy <- rowCnt >= ncol(missing) * object$ratio
  new_data <- new_data[!heavy, ]
  as_tibble(new_data)
}

print.step_missingObs <- function(x, width = max(20, options()$width - 26), ...) {
  cat("Heavily missing observations step (ratio > ", x$ratio, ") for variables: ", sep = "")
  #cat(recipes::format_ch_vec(x$columns, width = width))
  cat(recipes::format_selectors(x$terms, width = width))
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}

tidy.step_missingObs <- function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}


# featureSelection ----
# A recipe step, as per the recipes package, that performs Feature Selection 
# using the Boruta package

step_featureSelection_new <- function(terms, role, trained, outcome, removals, parallel, skip, id) {
  step(
    subclass = "featureSelection",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    removals = removals,
    parallel = parallel,
    skip = skip,
    id = id
  )
}

step_featureSelection <- function(recipe,
                            ...,
                            role = NA,
                            trained = FALSE,
                            outcome = NULL,
                            removals = NULL,
                            parallel = FALSE,
                            skip = FALSE,
                            id = rand_id("featureSelection")) {
  if (is.null(outcome)) {
    stop("`outcome` should select at least one column.", call. = FALSE)
  }
  recipes_pkg_check("Boruta")
  
  add_step(
    recipe,
    step_featureSelection_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      outcome = outcome,
      removals = removals,
      parallel = parallel,
      skip = skip,
      id = id
    )
  )
}

prep.step_featureSelection <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(x$terms, info = info)
  cpus <- 1
  if (x$parallel) cpus <- NULL
  training <- na.omit(training[, c(col_names, x$outcome)])  # to be safe
  result <- Boruta::Boruta(x = training[, col_names], y = training[, x$outcome, drop = TRUE], 
                           maxRuns = 100, doTrace = 0, holdHistory = FALSE, num.thread = cpus)
  # if (any(result$finalDecision == "Tentative")) {
  #   result = Boruta::TentativeRoughFix(result)
  # }
  filter <- result$finalDecision == "Rejected"
  step_featureSelection_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    removals = col_names[filter],
    parallel = x$parallel,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_featureSelection <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
}

print.step_featureSelection <- function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    if (length(x$removals) > 0) {
      cat("Feature Selection step removed variables: ")
      #cat(recipes::format_ch_vec(x$removals, width = width))
      cat(x$removals)
    } else
      cat("Feature selection step removed no terms")
  } else {
    cat("Feature selection step acting on ")
    cat(recipes::format_selectors(x$terms, width = width))
  }
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}

tidy.step_featureSelection <- function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}



# plsx ----
# A recipe step, as per the step_pls recipes package, that performs Partial Least Squares (PLS) and PLSDA
step_plsx <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_comp  = 2,
           outcome = NULL,
           options = NULL,
           res = NULL,
           prefix = "PLS",
           skip = FALSE,
           id = rand_id("plsx")) {
    if (is.null(outcome))
      stop("`outcome` should select at least one column.", call. = FALSE)

    recipes_pkg_check("pls")

    add_step(
      recipe,
      step_plsx_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_comp = num_comp,
        outcome = outcome,
        options = options,
        res = res,
        prefix = prefix,
        skip = skip,
        id = id
      )
    )
  }

step_plsx_new <-
  function(terms, role, trained, num_comp, outcome, options, res,
           prefix, skip, id) {
    step(
      subclass = "plsx",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      outcome = outcome,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_plsx <- function(x, training, info = NULL, ...) {
  x_names <- terms_select(x$terms, info = info)
  y_names <- terms_select(x$outcome, info = info)
  check_type(training[, x_names], quant = TRUE)

  if (length(y_names) == 1) {
    y_form <- y_names
  } else {
    y_form <- paste0(y_names, collapse = ",")
    y_form <- paste0("cbind(", y_form, ")")
  }

  if (x$num_comp > 0) {
    if (is(training[,y_names], "numeric")) {
      args <- list(formula = as.formula(paste(y_form, ".", sep = "~")),
                   data = training[, c(y_names, x_names)])
      
      x$options$ncomp <- min(x$num_comp, length(x_names))
      args <- c(args, x$options)
      mod <- do.call(pls::plsr, args)
      
      if (!any(names(mod) == "scale"))
        mod$scale <- NA
      
      res <- mod[c("projection", "Xmeans", "scale")]
    } else {
      args <- list(x = training[, x_names, drop = FALSE], y = as.factor(training[, y_names, drop = TRUE]), 
                   ncomp = min(x$num_comp, length(x_names)))
      
      args <- c(args, x$options)
        mod <- do.call(caret::plsda, args)

      if (!any(names(mod) == "scale"))
        mod$scale <- NA

      res <- mod[c("projection", "Xmeans", "scale")]
    }
  } else {
    res <- list(x_vars = x_names, y_vars = y_names)
  }

  step_plsx_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    outcome = x$outcome,
    options = x$options,
    res = res,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_plsx <- function(object, new_data, ...) {
  if (object$num_comp > 0) {
    pls_vars <- rownames(object$res$projection)
    n <- nrow(new_data)
    input_data <- as.matrix(new_data[, pls_vars])

    if (!all(is.na(object$res$scale)))
      input_data <- sweep(input_data, 2, object$res$scale, "/")

    input_data <- sweep(input_data, 2, object$res$Xmeans, "-")

    comps <- input_data %*% object$res$projection
    comps <- check_name(comps, new_data, object)
    new_data <- bind_cols(new_data, as_tibble(comps))
    new_data <- new_data[, !(colnames(new_data) %in% pls_vars), drop = FALSE]
    new_data <- as_tibble(new_data)
  }
  new_data
}


print.step_plsx <- function(x, width = max(20, options()$width - 35), ...) {
  if (x$num_comp == 0) {
    cat("No PLS components were extracted.\n")
  } else {
    cat("PLS feature extraction with ")
    printer(rownames(x$res$projection), x$terms, x$trained, width = width)
  }
  invisible(x)
}


#' @rdname step_plsx
#' @param x A `step_plsx` object
#' @export
tidy.step_plsx <- function(x, ...) {
  if (is_trained(x)) {
    if (x$num_comp > 0) {
      res <- as.data.frame(x$res$projection)
      res <- stack(res)
      res$terms <- rep(rownames(x$res$projection), ncol(x$res$projection))
      names(res)[1:2] <- c("value", "component")
      res <- res[, c("terms", "value", "component")]
      res$component <- gsub("Comp ", "PLS", res$component)
    } else {
      res <- tibble(terms = x$res$x_vars, value = na_dbl, component  = na_chr)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component = na_chr)
  }
  res$id <- x$id
  res
}


#' @rdname tunable.step
#' @export
tunable.step_plsx <- function(x, ...) {
  tibble::tibble(
    name = "num_comp",
    call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1, 4))),
    source = "recipe",
    component = "step_pls",
    component_id = x$id
  )
}

#Test code
 # library(caret) 
 # library(recipes)
 # data(GermanCredit)
 # recipe(Amount ~ ., data = GermanCredit) %>%
 #   step_rm(all_nominal()) %>%
 #   step_nzv(all_predictors()) %>%
 #   step_pls(all_numeric(), -has_role("outcome"), outcome = "Amount", num_comp = 4) %>%
 #   train(data = GermanCredit, method = "RRF")

# dbscan ----
# A recipe step, as per the recipes package, that adds factors for the natural clusters in the
# data. An extra variable is added to record all unallocated "outliers"

step_dbscan_new <- function(terms, role, trained, skip, id, eps,  MinPts, model, data) {
  step(
    subclass = "dbscan",
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id,
    eps = eps,
    MinPts = MinPts,
    model = model,
    data = data
  )
}

step_dbscan <- function(recipe, ..., role = "predictor", trained = FALSE, skip = FALSE, 
                        eps = NULL, MinPts = NULL, id = rand_id("dbscan")) {
  if (is.null(eps)) stop("eps value is not defined")
  add_step(recipe,
           step_dbscan_new(terms = recipes::ellipse_check(...),
                           role = role,
                           trained = trained,
                           skip = skip,
                           id = id,
                           eps = eps,
                           MinPts = MinPts,
                           model = NULL,
                           data = NULL
           )
  )
}

prep.step_dbscan <- function(x, training, info = NULL, ...) {
  dat <- training[, recipes::terms_select(x$terms, info = info), drop = FALSE]
  if (sum(is.na(dat)) > 0) {
    warning("Missing values were present")
    dat <- na.omit(dat)
    if (nrow(dat) == 0) stop("No rows remain in dataset after missing values rows omitted")
  }
  if (ncol(dat) == 0) stop("Clusters not created as no numeric columns were found")
  if (is.null(x$MinPts)) {
    MinPts <- ncol(dat) + 1
  } else {
    MinPts <- x$MinPts
  }
  mod <- fpc::dbscan(data = dat, eps = x$eps, MinPts = MinPts, scale = FALSE, method = "hybrid", seeds = TRUE)
  if (is.null(mod$isseed)) {
    stop(paste("No clusters can be detected using MinPts = ", MinPts, "and eps = ", x$eps))
  }
  step_dbscan_new(terms = x$terms, role = x$role, trained = TRUE, skip = x$skip,
                  id = x$id, eps = x$eps, MinPts = x$MinPts, model = mod, data = dat)
}

bake.step_dbscan <- function(object, new_data, ...) {
  clus <- predict(object$model, data = object$data, newdata = new_data)
  new_data <- cbind(new_data, cluster = as.factor(paste0("C",clus)))
  as_tibble(new_data)
}

print.step_dbscan <- function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    cat(paste0("dbscan step found ", length(unique(x$mod$cluster)) - 1," clusters using "))
    cat(recipes::format_selectors(x$terms, width = width))
    cat(" [trained]\n")
  } else {
    cat("dbscan step for ")
    cat(recipes::format_selectors(x$terms, width = width))
    cat("\n")
  }
  invisible(x)
}

tidy.step_dbscan <- function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}



# binary ----

step_binary_new <- function(terms, role, trained, limit, preserve, levels, skip, id) {
  step(
    subclass = "binary",
    terms = terms,
    role = role,
    trained = trained,
    limit = limit,
    preserve = preserve,
    levels = levels,
    skip = skip,
    id = id
  )
}

step_binary <- function(recipe, ..., role = "predictor", trained = FALSE, limit = 1000,
                        preserve = FALSE, levels = NULL, skip = FALSE, id = rand_id("binary")) {
  add_step(
    recipe,
    step_binary_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      limit = limit,
      preserve = preserve,
      levels = levels,
      skip = skip,
      id = id
    )
  )
}

passover <- function(cmd) {
  # cat("`step_binary()` was not able to select any columns. ",
  #     "No dummy variables will be created.\n")
} # figure out how to return a warning() without exiting

#' @export
prep.step_binary <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info, empty_fun = function(cmd) {})
  
  if (length(col_names) > 0) {
    fac_check <- vapply(training[, col_names], is.factor, logical(1))
    if (any(!fac_check))
      rlang::warn(
        paste0(
          "The following variables are not factor vectors and will be ignored: ",
          paste0("`", names(fac_check)[!fac_check], "`", collapse = ", ")
        )
      )
    col_names <- col_names[fac_check]
    if (length(col_names) == 0) {
      rlang::abort(
        paste0(
          "The `terms` argument in `step_binary` did not select ",
          "any factor columns."
        )
      )
    }
    
    
    ## I hate doing this but currently we are going to have to save the terms object from the original (= training) data
    levels <- vector(mode = "list", length = length(col_names))
    names(levels) <- col_names
    for (i in seq_along(col_names)) {
      form_chr <- paste0("~", col_names[i])
      form <- as.formula(form_chr)
      terms <- model.frame(form,
                           data = training,
                           xlev = x$levels[[i]],
                           na.action = na.pass)
      levels[[i]] <- attr(terms, "terms")
      
      ## About factor levels here: once dummy variables are made,
      ## the `stringsAsFactors` info saved in the recipe (under
      ## recipe$levels will remove the original record of the
      ## factor levels at the end of `prep.recipe` since it is
      ## not a factor anymore. We'll save them here and reset them
      ## in `bake.step_binary` 
      attr(levels[[i]], "values") <-
        levels(getElement(training, col_names[i]))
      attr(levels[[i]], ".Environment") <- NULL
    }
  } else {
    levels <- NULL
  }
  
  step_binary_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    limit = x$limit,
    preserve = x$preserve,
    levels = levels,
    skip = x$skip,
    id = x$id
  )
}

warn_new_levels <- function(dat, lvl) {
  ind <- which(!(dat %in% lvl))
  if (length(ind) > 0) {
    lvl2 <- unique(dat[ind])
    rlang::warn(
      paste0("There are new levels in a factor: ",
             paste0(lvl2, collapse = ", ")
      )
    )
  }
  invisible(NULL)
}

#' @export
bake.step_binary <- function(object, new_data, ...) {
  
  # If no terms were selected
  if (length(object$levels) == 0) {
    return(new_data)
  }
  
  col_names <- names(object$levels)
  
  for (i in seq_along(object$levels)) {
    # Make sure that the incoming data has levels consistent with the original (see the note above)
    orig_var <- names(object$levels)[i]
    fac_type <- attr(object$levels[[i]], "dataClasses")
    
    if (!any(names(attributes(object$levels[[i]])) == "values"))
      rlang::abort("Factor level values not recorded")
    
    warn_new_levels(
      new_data[[orig_var]],
      attr(object$levels[[i]], "values")
    )
    
    new_data[, orig_var] <-
      factor(getElement(new_data, orig_var),
             levels = attr(object$levels[[i]], "values"),
             ordered = fac_type == "ordered")
    if (nlevels(new_data[, orig_var, drop = TRUE]) < object$limit) next
    
    indicators <-
      model.frame(
        as.formula(paste0("~", orig_var)),
        data = new_data[, orig_var],
        xlev = attr(object$levels[[i]], "values"),
        na.action = na.pass
      )
    
    fact <- indicators[[orig_var]]
    indicators <- matrix( as.integer(intToBits(as.integer(fact))), ncol = 32, nrow = length(fact), byrow = TRUE  )
    indicators <- indicators[, 1:ceiling(log(nlevels(fact) + 1)/log(2))]

    colnames(indicators) <- paste(sep = "_bit", orig_var, stringr::str_pad(1:ncol(indicators), width = 2, side = "left", pad = "0"))
    new_data <- bind_cols(new_data, as_tibble(indicators))

    if (!object$preserve) {
      new_data[, col_names[i]] <- NULL
    }
  }
  as_tibble(new_data)
}

print.step_binary <- function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      if (length(x$levels) > 0) {
        cat("Binary vars from high-cardinality variables: ")
        cat(format_ch_vec(names(x$levels), width = width))
      } else {
        cat("No suitable high-cardinality variables were found")
      }
    } else {
      cat("Binary variables from ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


get_bin_columns <- function(x) {
  tibble(columns = attr(x, "values"))
}


#' @rdname step_binary
#' @param x A `step_binary` object.
#' @export
tidy.step_binary <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$levels) > 0) {
      res <- purrr::map_dfr(x$levels, get_bin_columns, .id = "terms")
    } else {
      res <- tibble(terms = rlang::na_chr, columns = rlang::na_chr)
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#Test code
# library(caret)
# library(recipes)
# library(modeldata)
# data(okc)
# recipe(~ diet + age + height, data = okc) %>%
#  step_naomit(everything()) %>%
#  step_binary(all_nominal(), preserve = F, limit = 10) %>%
#  prep(training = okc) %>%
#  bake(new_data = okc) %>%
#  View()

  

# downsamplex ----

step_downsamplex <- function(recipe, ..., under_ratio = 1, diverse = FALSE, role = NA, trained = FALSE, column = NULL, target = NA, 
                             seed = sample.int(10^5, 1), skip = TRUE, id = rand_id("downsample")) {
  
  add_step(recipe,
           step_downsamplex_new(
             terms = ellipse_check(...),
             under_ratio = under_ratio,
             diverse = diverse,
             role = role,
             trained = trained,
             column = column,
             target = target,
             skip = skip,
             seed = seed,
             id = id
           ))
}

#' A recipe step, as per the recipes package, that down samples the observations. When the "diverse" parameter is TRUE the down sampling uses
#' PAM clustering to maintain maximum diversity in the data. 
#'
#' @param terms 
#' @param under_ratio 
#' @param diverse 
#' @param role 
#' @param trained 
#' @param column 
#' @param target 
#' @param skip 
#' @param seed 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
step_downsamplex_new <- function(terms, under_ratio, diverse, role, trained, column, target, skip, seed, id) {
  step(
    subclass = "downsamplex",
    terms = terms,
    under_ratio = under_ratio,
    diverse = diverse,
    role = role,
    trained = trained,
    column = column,
    target = target,
    skip = skip,
    id = id,
    seed = seed,
    id = id
  )
}


#' @export
prep.step_downsamplex <- function(x, training, info = NULL, ...) {
  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    rlang::abort("Please select a single factor variable.")
  if (!is.factor(training[[col_name]]))
    rlang::abort(col_name, " should be a factor variable.")
  
  obs_freq <- table(training[[col_name]])
  minority <- min(obs_freq)
  
  step_downsamplex_new(
    terms = x$terms,
    under_ratio = x$under_ratio,
    diverse = x$diverse,
    role = x$role,
    trained = TRUE,
    column = col_name,
    target = floor(minority * x$under_ratio),
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}


subsamp <- function(x, num, diverse) {
  n <- nrow(x)
  if (nrow(x) == num) {
    out <- x
  } else if (diverse) {
    # downsampling is done using pam medoids
    clus <- cluster::pam(x, k = num)
    out <- x[clus$id.med, ]
  } else {
    # downsampling is done without replacement
    out <- x[sample(1:n, min(num, n)), ]
  }
  out
}

#' @export
bake.step_downsamplex <- function(object, new_data, ...) {
  if (any(is.na(new_data[[object$column]])))
    missing <- new_data[is.na(new_data[[object$column]]),]
  else
    missing <- NULL
  split_up <- split(new_data, new_data[[object$column]])
  
  # Downsample with seed for reproducibility
  withr::with_seed(
    seed = object$seed,
    code = {
      new_data <- purrr::map_dfr(split_up, subsamp, num = object$target, diverse = object$diverse)
      if (!is.null(missing)) {
        new_data <- bind_rows(new_data, subsamp(missing, num = object$target, diverse = object$diverse))
      }
    }
  )
  as_tibble(new_data)
}

print.step_downsamplex <- function(x, width = max(20, options()$width - 26), ...) {
  cat("Down-sampling based on ", sep = "")
  printer(x$column, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_divdownsample
#' @param x A `step_downsamplex` object.
#' @export
tidy.step_downsamplex <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$column)
  }
  else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res$id <- x$id
  res
}

#' @rdname tunable.step
#' @export
tunable.step_downsamplex <- function(x, ...) {
  tibble::tibble(
    name = "under_ratio",
    call_info = list(
      list(pkg = "dials", fun = "under_ratio")
    ),
    source = "recipe",
    component = "step_divdownsample",
    component_id = x$id
  )
}

#Test code
 # library(caret)
 # library(recipes)
 # library(mlbench)
 # data("BostonHousing")
 # recipe(chas ~ ., data = BostonHousing) %>%
 #   step_downsamplex(all_outcomes(), under_ratio = 1, diverse = TRUE) %>%
 #   prep(training = BostonHousing, retain = TRUE) %>%
 #   juice() -> d
 # table(d$chas)
 # 
 # BostonHousing$Type <- "not sampled"
 # d$Type <- "sampled"
 # dd <- dplyr::distinct(rbind(BostonHousing, d))
 # ggplot() +
 #   geom_point(data = dd, mapping = aes(x = age, y = nox, color = Type)) +
 #   labs(title = "Boston Data")


# shadow_missing -----
step_shadow_missing_new <- function(terms = NULL, role = NA, skip = FALSE, trained = FALSE,
                                    prefix  = NULL, columns = NULL) {
  step(
    subclass = "shadow_missing",
    terms    = terms,
    role     = role,
    skip     = skip,
    trained  = trained,
    prefix   = prefix,
    columns  = columns
  )
}

#' For every variable that contains any missing value, an additional binary column is created with a prefix 
#' ‘shadow_’ where 1 stands for missing, and 0 for non-missing.
#' Thanks to creation of such variables, when the patterns of missingness are not random, we can account for 
#' them when training our model and hence improve it’s predictive performance. When a variable is missing at 
#' random such approach will most likely not yield any additional increase in performance and actually quite 
#' an opposite - perhaps even worsen it!
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which variables will be used to create the new variables. The selected variables should have class Date or POSIXct. See selections() for more details. For the tidy method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new variable columns created by the original variables will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param ratio A threshold number [0-1] for the worst level of variable missingness.
#' @param prefix A prefix for the new variable names
#' @param columns A character string of variables that will be used as inputs. This field is a placeholder and will be populated once prep.recipe() is used.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake.recipe()? While all operations are baked when prep.recipe() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of recipe with the new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
step_shadow_missing <- function(recipe, ..., role = NA, trained = FALSE, prefix  = "shadow", 
                                columns = NULL, skip = FALSE, id = rand_id("shadow_missing")) {
    add_step(
      recipe,
      step_shadow_missing_new(
        terms   = ellipse_check(...),
        role    = role,
        skip    = skip,
        trained = trained,
        prefix  = prefix,
        columns = columns
      )
    )
  }

prep.step_shadow_missing <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  step_shadow_missing_new(
    terms   = x$terms,
    role    = x$role,
    skip    = x$skip,
    trained = TRUE,
    prefix  = x$prefix,
    columns = col_names
  )
}

bake.step_shadow_missing <- function(object, new_data, ...) {
  col_names <- object$columns
  for (i in seq_along(col_names)) {
    if(sum(is.na(new_data[[col_names[i]]])) > 0){ # check if column has missing data 
      col <- new_data[[col_names[i]]]
      new_data[, col_names[i]] <- col # the original column should remain
      new_data[, paste0(object$prefix, "_", col_names[i])] <- ifelse(is.na(col), 1, 0) # adding the shadowing column with a prefix 
    } else {
      next 
    }
  }
  as_tibble(new_data)
}

print.bake.step_shadow_missing <- function(x, width = max(20, options()$width - 30), ...) {
    cat("Creating shadow variables for ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

tidy.step_shadow_missing <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
