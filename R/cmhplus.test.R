
cmhplus.test <- function(X, Y, h, statistic, method)
{
  dname <- deparse(substitute(x))

  if(is.array(x)) {
    if(length(dim(x)) == 3L) {
      if(any(dim(x) < 2L)) stop("each dimension in table must be >= 2")
    }
    else
      stop("'x' must be a 3-dimensional array")
  }
  else {
    if(is.null(y)) stop("if 'x' is not an array, 'Y' must be given")

    if(is.null(h)) stop("if 'x' is not an array, 'h' must be given")

    if(any(diff(c(length(X), length(Y), length(h))) != 0L))
      stop("'x', 'Y', and 'h' must have the same length")
    dimname <- paste(dname, "and", deparse(substitute(y)), "and", deparse(substitute(h)))
   if((nlevels(X) < 2L) || (nlevels(Y) < 2L))
      stop("'X' and 'Y' must have at least 2 levels")
    else
     x <- table(X, Y, h)
  }

  ## General CMH form

  S_h <-
  var_h <-
  statistic <- sum(S_h)/sum(sqrt(var_h))
  method <- c("chi-squared test", "correlation",)

  ## Pearson's chi-squared test
  if (method == "Pearson"){
    chisq_test= #call chi-squared test
    if(!is.character(y))
      stop("'y' must be a character")
  }
  ## Correlation test
  if (method == "correlation"){
    corr_test= #call correlation test
    if(!is.numeric(y))
      stop("y' must be a numeric")
  }

}



