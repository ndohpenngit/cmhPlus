
####
library(boot)

cmhplus.test <- function(data, method="", statistic, null.distr){
  # top level function for the end user

  # data: data frame, which needs the following columns
  #       X: treatment factor or covariate
  #       Y: outcome
  #       Z: stratification factor
  # method: specifies what CMH test is to be performed
  #       "correlation": within each stratum a Pearson correlation is calculated
  #       NULL: the CMH test will be performed with the user-supplied statistic
  # statistic: if the method is not specified, this gives the function
  #       for the user-defined within-stratum statistic.
  #       It must return the statistic, the standard error and a point estimate
  # null.distr: the null distribution

  x<-data$x
  y<-data$y
  z<-data$z

  #method <- c("chi-squared test", "correlation", "means"...)

  ## Correlation
  if(method=="correlation") {
    if(!is.numeric(x))
      stop("'y' must be a numeric")
    if(!is.numeric(y))
      stop("'y' must be a numeric")
    statistic<-function(x,y) {
      stat<-cor(x,y)
      se<-sqrt((1-stat^2)/(length(x)-2))
      return(list(stat=stat,se=se,estimate=stat,n=length(x),
                  weight=1/length(x)))
    }
    null.distr<-"normal"
  }

  ## Means
  if(method=="means") {
    if(!is.numeric(y))
      stop("'y' must be a numeric")
    if(!is.factor(x))
      stop("'x' must be a factor")
    statistic<-function(x,y) {
      m<-lm(y~x)
      stat<-as.numeric(m$coefficients[2])
      se<-summary(m)$coeff[2,2]
      return(list(stat=stat,se=se,estimate=stat,n=length(x),
                  weight=1/length(x)))
    }
    null.distr<-"normal"
  }

  if(null.distr=="normal") {
    pnull<-function(stat) {
      2*(1-pnorm(abs(stat)))
    }
  }

  ## Chi-squared
  if(method == "chi-squared") {
    if(is.array(data)) {
      if(length(dim(data)) == 3L) {
        if(any(dim(data) < 2L)) stop("each dimension in table must be >= 2")
      }
      else
        stop("'data' must be a 3-dimensional array")
    }
    else {
      if(is.null(y)) stop("if 'data' is not an array, 'Y' must be given")

      if(is.null(z)) stop("if 'data' is not an array, 'z' must be given")

      if(any(diff(c(length(x), length(y), length(z))) != 0L))
        stop("'x', 'y', and 'z' must have the same length")
      if((nlevels(x) < 2L) || (nlevels(y) < 2L))
        stop("'x' and 'y' must have at least 2 levels")
      else
        tab <- table(x, y, z)
    }

    if(!is.factor(x))
      stop("'x' must be a factor")
    if(!is.factor(y))
      stop("'y' must be a factor")

    statistic<-function(x,y) {
      tab <- table(x, y)
      xsq<-chisq.test(tab)
      stat<-xsq$statistic
      df<-xsq$parameter
      return(list(stat=stat,df=df,estimate=stat,n=length(x),
                 weight=1/length(x)))
    }
    null.distr<-"chi"
  }

  if(null.distr=="chi") {
    pnull<-function(stat) {
      pchisq(stat, df, lower.tail = FALSE)# if (alternative == "two.sided")
    }
  }

  results<-CMH_internal(x,y,z,statistic,pnull)

  return(list(statistic=results$stat,
              p.value=results$p.value))

}


CMH_internal<-function(x,y,z,statistic,pnull) {
  strata<-unique(z)
  numerator<-0
  denominator<-0
  estimate<-0
  sum.weights<-0
  cnt<-1
  stratum.results<-list()
  for(s in strata) {
    stratum.stat<-statistic(x[z==s],y[z==s])

    stratum.results[[cnt]]<-stratum.stat
    cnt<-cnt+1

    estimate<-estimate+stratum.stat$weight*stratum.stat$estimate
    sum.weights<-sum.weights+stratum.stat$weight
    numerator<-numerator+stratum.stat$stat
    denominator<-denominator+stratum.stat$se^2
  }
  estimate<-estimate/sum.weights
  stat<-numerator/sqrt(denominator)
  p.value<-pnull(stat)

  CMH_print(stratum.results,
            estimate,stat,p.value)

  return(list(stat=stat, p.value=p.value, estimate=estimate))
}

CMH_print<-function(results,est,stat,p) { # include degree of freedom, df for X-square
#  if((method=="means") || (method=="correlation"))
    cat("stratum \t statistic \t estimate \n")
#  else
#    cat("stratum \t statistic \t df \t estimate \n")
  for(s in 1:length(results)) {
    cat(s,"\t",
        results[[s]]$stat/results[[s]]$se,"\t",
        results[[s]]$estimate,"\n")
  }
  cat("\n")
  cat(paste("Test statistic: ",stat,"\n"))
#  cat(paste("Degrees of freedom: ",df,"\n"))
  cat(paste("p-value: ",round(p,5),"\n"))
  cat(paste("Estimate: ",est,"\n"))
}


# test

set.seed(2788)
df1<-data.frame(x=rnorm(25),
               z=rep(1:5,rep(5,5)))
df1$y<-0.75*df1$x+rnorm(25,sd=0.4)+df1$z

cmhplus.test(data=df1, method="correlation")


df2<-data.frame(x=rep(c(0,1),15),
               z=rep(1:5,rep(6,5)))
df2$y<-2+0.5*df2$x+rnorm(30,sd=0.4)+df2$z
df2$x<-as.factor(df2$x)

cmhplus.test(data=df2, method="means")


df3<-data.frame(x=rep(c(0,1),30),
                y=trunc(runif(30, 1,3)),
                z=rep(1:5,rep(12,5)))
col_names <- names(df3); df3[,col_names] <- lapply(df3[,col_names], factor)

cmhplus.test(data = df3, method = "chi-squared", null.distr = "chi")




CMH(data=df,
    statistic = function(x,y){
      teststat<-function(m,ind=1:length(x)) {
        x<-m$x[ind]
        y<-m$y[ind]
        levels<-unique(x)
        stat<-median(y[x==levels[1]])-median(y[x==levels[2]])
        return(stat)
      }
      df<-data.frame(x=x,y=y)
      s<-teststat(df)
      tmp<-boot(data = df, statistic=teststat, R=1000, sim="permutation")
      return(list(stat=s,se=sd(tmp$t),estimate=s,n=length(x),
                  weight=1/length(x)))
    },
    null.distr = "normal")

