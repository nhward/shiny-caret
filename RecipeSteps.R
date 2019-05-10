# A recipe step, as per the recipes package, that identifies columns
# that have excessive missing values and drops them.
# The default is to treat 50% (and above) NA values as excessive.
# This applies to any class of column.

step_missing <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           ratio = 0.5,
           removals = NULL,
           skip = FALSE,
           id = rand_id("missing")) {
    add_step(
      recipe,
      step_missing_new(
        terms = recipes:::ellipse_check(...),
        role = role,
        trained = trained,
        ratio = ratio,
        removals = removals,
        skip = skip,
        id = id
      )
    )
  }

step_missing_new <-
  function(terms, role, trained, ratio, removals, skip, id) {
    step(
      subclass = "missing",
      terms = terms,
      role = role,
      trained = trained,
      ratio = ratio,
      removals = removals,
      skip = skip,
      id = id
    )
  }

msBad <- function(x, ratio) {
  sum(is.na(x)) / length(x) > ratio
}

prep.step_missing <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  if (nrow(training) == 0) {
    filter <- rep(FALSE, length(col_names))
  } else {
    filter <- vapply(training[, col_names], FUN=msBad, FUN.VALUE=TRUE, x$ratio)
  }
  step_missing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    ratio = x$ratio,
    removals = col_names[filter],
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_missing <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
}

print.step_missing <- function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    if (length(x$removals) > 0) {
      cat("Missing data filter removed ")
      cat(recipes:::format_ch_vec(x$removals, width = width))
    } else
      cat("Missing data filter removed no terms")
  } else {
    cat("Missing data filter on ", sep = "")
    cat(recipes:::format_selectors(x$terms, wdth = width))
  }
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}
