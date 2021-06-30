
cmhplus.test <- function(X, Y, h, method)
{
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
   if((nlevels(X) < 2L) || (nlevels(Y) < 2L))
      stop("'X' and 'Y' must have at least 2 levels")
    else
     x <- table(X, Y, h)
  }

  method <- c("chi-squared test", "correlation",...)

  ## Pearson's chi-squared test
  if (method == "Pearson"){
    if(!is.character(y))
      stop("'y' must be a character")
    STATISTIC <- #chi-squared statistic
    PARAMETER <- 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)# if (alternative == "two.sided")
  }

  names(STATISTIC) <- "Mantel-Haenszel X-squared"
  names(PARAMETER) <- "df"
  METHOD <- paste("Mantel-Haenszel chi-squared test")

  ## General CMH form
  S_h <-
  var_h <-
  CMH.ESTIMATE <- sum(S_h)/sum(sqrt(var_h))


  ## Correlation test
  if (method == "correlation"){
    corr_test= #call correlation test
    if(!is.numeric(y))
      stop("y' must be a numeric")
  }

}



