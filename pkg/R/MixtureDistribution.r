
checkFunctions <- function(functionList)
{
  if (any(sapply(functionList, is.na)))
    stop("functionList contains NA values in qMixtureDistribution")
  if (!(length(grep("x", functionList)) == length(functionList)))
    stop("one element of functionList does not contain an x")
  if (!(length(grep("^[:blank:]*p", functionList)) == length(functionList)))
    stop("one element of functionList is not a probability function")
}

qMixtureDistribution <- function(p, functionList, xMin, xMax, nPoints=1000,
                                 logScale=FALSE)
{
  stopifnot(all(p > 0) & all(p < 1))
  checkFunctions(functionList)

  if (!logScale)
  {
    x <- seq(xMin, xMax, length=nPoints)
    
  } else
  {
    x <- 10^seq(log10(xMin), log10(xMax), length=nPoints)
  }
  
  Z <- sapply(functionList, function(z) eval(parse(text=z)))
  
  Zmean <- apply(Z, 1, mean)
  
  if (Zmean[1] > min(p))
    warning("probabilities < ", Zmean[1], " will return ", xMin,
            ": min p is ", min(p))
  if (Zmean[length(Zmean)] < max(p))
    warning("probabilities > ", Zmean[length(Zmean)], " will return ", xMax,
            ": max p is ", max(p))
    
  ret <- approx(x=Zmean, y=x, xout=p, method="linear", yleft=xMin, yright=xMax, ties=mean)
  
  return(ret$y)
}

rMixtureDistribution <- function(n, functionList)
{
  stopifnot(n >= 1)
  len <- length(functionList)
  
  if (n < len)
    warning("There are not enough requested samples to sample once from each function")
  checkFunctions(functionList)

  nOrig <- n
  n <- ceiling(n / length(functionList))

  functionList2 <- gsub("^[:blank:]*p", "r", functionList)
  functionList2 <- gsub("[(][[:blank:]]*x[[:blank:]]*[,]", "(n,", functionList2)

  Z <- sapply(functionList2, function(z) eval(parse(text=z)))

  Z[1:nOrig]
}

pMixtureDistribution <- function(x, functionList)
{
  checkFunctions(functionList)

  Z <- sapply(functionList, function(z) eval(parse(text=z)))
  
  if (!is.null(dim(Z)))
  {
    Zmean <- apply(Z, 1, mean)
  } else if (length(Z) > 0)
  {
    Zmean <- mean(Z)
  }
  
  Zmean
}

dMixtureDistribution <- function(x, functionList)
{
  checkFunctions(functionList)

  functionList2 <- gsub("^[:blank:]*p", "d", functionList)

  Z <- sapply(functionList2, function(z) eval(parse(text=z)))

  if (!is.null(dim(Z)))
  {
    Zmean <- apply(Z, 1, mean)
  } else if (length(Z) > 0)
  {
    Zmean <- mean(Z)
  }

  Zmean
}

#qMixtureDistribution(c(0.5), list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"), 0, 4)
#pMixtureDistribution(2, list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#pMixtureDistribution(c(0.5,1,2,4,5,6), list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#x1 <- rMixtureDistribution(10000, list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#mean(x1)
#x1 <- rMixtureDistribution(1, list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#x1 <- rMixtureDistribution(3, list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#x1 <- rMixtureDistribution(4, list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#xd <- seq(0,4, length=1000)
#y <- dMixtureDistribution(xd, list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#plot(xd, y)
#integrate(dMixtureDistribution, 0, 4, functionList=list("pnorm(x, 1, 0.1)","pnorm(x, 2, 0.1)","pnorm(x, 3, 0.1)"))
#
#xd <- seq(0,4, length=1000)
#y <- dMixtureDistribution(xd, list("pnorm(x, 1, 0.1)","plnorm(x, .2, 1)","pnorm(x, 3, 0.1)"))
#plot(xd, y)
#integrate(dMixtureDistribution, 0, Inf, functionList=list("pnorm(x, 1, 0.1)","plnorm(x, .2, 0.1)","pnorm(x, 3, 0.1)"))



