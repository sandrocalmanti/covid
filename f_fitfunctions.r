
#############################################################################################
# Gompertz
# (source: http://www.statsathome.com/2017/06/07/fitting-non-linear-groth-curves-in-r/)
#############################################################################################

#
#This is a standard fit function with generic start
#
fit.gompertz <- function(data,time,country) {
#fit.gompertz <- function(tbl) {

  d <- data.frame(y=data, t=time, c=country)
#  d <- data.frame(y=tbl$Deaths, t=tbl$day)
  c <- unique(d$c)
  #print(c)

  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  
  if ( c=='China' ) {
  #This work well with a data along a sigmoid shape
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  } else {
    #This are tested for COVID19 total-deaths data for Italy until 2020-03-14 
    starting.values <- c(a=12024, 
                         mu=7., 
                         lambda=0.05)    
  }
  #print("Starting Values for Optimization: ")
  #print(starting.values)
  formula.gompertz <- "y~a*exp(-mu*exp(-lambda*t))"
  nlscontrol = list(maxiter = 1000, tol = 1e-04, minFactor = 1/(16*1024),
              printEval = FALSE, warnOnly = TRUE)
  nls(formula.gompertz, d, starting.values,control=nlscontrol)
}

########################################################################################

#
#This is a standard fit function with generic start
#
fit.logistic <- function(data,time,country) {
  #fit.gompertz <- function(tbl) {
  
  d <- data.frame(y=data, t=time, c=country)
  #  d <- data.frame(y=tbl$Deaths, t=tbl$day)
  c <- unique(d$c)
  print(c)

  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  
  if ( c=='China' ) {
    #This work well with a data along a sigmoid shape
    starting.values <- c(a=max(d$y), 
                         mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                         lambda=mean(d$t))
  } else {
    #This are tested for COVID19 total-deaths data for Italy until 2020-03-14 
    starting.values <- c(a=max(d$y), 
                         mu=2, 
                         lambda=max(d$t))
  }
  print("Starting Values for Optimization: ")
  print(starting.values)
  
  formula.logistic <- "y~a/(1+exp((lambda-t)/mu))"  
  nls(formula.logistic, d, starting.values)
}

###################
# Gompertz function
###################

f.gompertz <- function(t,a,mu,lambda) {
  #gmp <- a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))
  gmp <- a*exp(-mu*exp(-lambda*t))
  return(gmp)
}


###################
# Logistic function
###################

f.logistic <- function(t,a,mu,lambda) {
  lgt <- a/(1+exp((lambda-t)/mu))
  return(lgt)
}




#####################
# Generic linear fit
#####################
lfit <- function(x,t,n=0) {
  #Length of forecasting set
  xlm <- x[1:(length(x)-n)]
  tlm <- t[1:(length(t)-n)]
  lf <- lm(log(xlm) ~ tlm)
  a <- lf$coefficients[1]
  b <- lf$coefficients[2]
  y <- exp( a + t*b)
  return(y)
}
