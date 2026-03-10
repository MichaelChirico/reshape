"%||%" <- function(a, b) if (!is.null(a)) a else b

all_identical <- function(xs) {
  if (length(xs) <= 1) return(TRUE)
  for (i in seq(2, length(xs))) {
    if (!identical(xs[[1]], xs[[i]])) return(FALSE)
  }
  TRUE
}

## Get the attributes if common, NULL if not.
normalize_melt_arguments <- function(data, measure.ind, factorsAsStrings) {

  measure.attributes <- lapply(measure.ind, function(i) {
    attributes(data[[i]])
  })

  ## Determine if all measure.attributes are equal
  measure.attrs.equal <- all_identical(measure.attributes)

  if (measure.attrs.equal) {
    measure.attributes <- measure.attributes[[1]]
  } else {
    warning("attributes are not identical across measure variables; ",
      "they will be dropped", call. = FALSE)
    measure.attributes <- NULL
  }

  if (!factorsAsStrings && !measure.attrs.equal) {
    warning("cannot avoid coercion of factors when measure attributes not identical",
      call. = FALSE)
    factorsAsStrings <- TRUE
  }

  ## If we are going to be coercing any factors to strings, we don't want to
  ## copy the attributes
  any.factors <- any( sapply( measure.ind, function(i) {
    is.factor( data[[i]] )
  }))

  if (factorsAsStrings && any.factors) {
    measure.attributes <- NULL
  }

  list(
    measure.attributes = measure.attributes,
    factorsAsStrings = factorsAsStrings
  )

}

is.string <- function(x) {
  is.character(x) && length(x) == 1
}

as.quoted <- function(x, env = parent.frame()) UseMethod("as.quoted")

as.quoted.character <- function(x, env = parent.frame()) {
  res <- lapply(x, function(expr) str2lang(expr))
  names(res) <- x
  attr(res, "env") <- env
  class(res) <- "quoted"
  res
}

as.quoted.formula <- function(x, env = environment(x)) {
  rhs <- x[[length(x)]]
  extract_terms <- function(expr) {
    if (is.name(expr) || (is.call(expr) && as.character(expr[[1]]) != "+")) {
      return(list(expr))
    }
    if (is.call(expr) && as.character(expr[[1]]) == "+") {
      return(c(extract_terms(expr[[2]]), extract_terms(expr[[3]])))
    }
    list(expr)
  }
  res <- extract_terms(rhs)
  names(res) <- vapply(res, function(e) deparse(e)[1], character(1))
  attr(res, "env") <- env
  class(res) <- "quoted"
  res
}

as.quoted.call <- function(x, env = parent.frame()) {
  res <- as.quoted.formula(as.formula(paste("~", deparse(x))), env)
  res
}

as.quoted.name <- function(x, env = parent.frame()) {
  res <- list(x)
  names(res) <- as.character(x)
  attr(res, "env") <- env
  class(res) <- "quoted"
  res
}

as.quoted.numeric <- function(x, env = parent.frame()) {
  res <- list(x)
  names(res) <- as.character(x)
  attr(res, "env") <- env
  class(res) <- "quoted"
  res
}

as.quoted.NULL <- function(x, env = parent.frame()) {
  res <- list()
  attr(res, "env") <- env
  class(res) <- "quoted"
  res
}

as.quoted.quoted <- function(x, env = parent.frame()) x

as.quoted.default <- function(x, env = parent.frame()) {
  stop("Unsupported type in as.quoted", call. = FALSE)
}

eval.quoted <- function(exprs, envir = NULL, enclos = NULL) {
  if (is.null(enclos)) enclos <- attr(exprs, "env")
  if (is.null(envir)) envir <- enclos
  
  lapply(exprs, eval, envir = envir, enclos = enclos)
}
