## -----------------------------------------------------------------------------
herbivore <- c("Yes", "Yes", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "No", "No", "Yes")
phenolics <- c("No", "Yes", "No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")
#You can combine the data into a data.frame
(myData <- data.frame(herbivore, phenolics))

## -----------------------------------------------------------------------------
#write the data to a csv file
#write.csv(myData, "inducible_defense_data.csv")

## -----------------------------------------------------------------------------
(conTable <- table(myData))

(conTable2 <- matrix(c(4,1,2,5), 2, byrow = T, dimnames = list(herbivores = c("No", "Yes"), phenolics = c("No", "Yes"))))

## -----------------------------------------------------------------------------
#Hat Island data
HI <- c(purple = 141, orange = 1)
#Strawberry Hill data
SH <- c(purple = 154, orange = 54)
(obsFreqs <- matrix(c(HI, SH), 2, 
  dimnames = list(color = c("Purple", "Orange"), site = c("Hat Island", "Strawberry Hill"))))

## -----------------------------------------------------------------------------
(obsFreqsMar <- addmargins(obsFreqs))
#Order matters, do rows and then columns for this example
(expFreqs <- outer(obsFreqsMar[1:2,3], obsFreqsMar[3,1:2])/obsFreqsMar[3,3])
#Recommend double checking your answers

## -----------------------------------------------------------------------------
(chiVal <- sum(((obsFreqs-expFreqs))^2/expFreqs))

## -----------------------------------------------------------------------------
(gVal <- 2*sum(obsFreqs*log(obsFreqs/expFreqs)))
#Remember log does natural log in R

## ---- fig.align='center'------------------------------------------------------
(pChi <- 1 - pchisq(chiVal, 1))
(pG <- 1 - pchisq(gVal, 1))
curve(dchisq(x, 1), 0, 60, ylab = "Density")
abline(v = c(chiVal, gVal, qchisq(0.95, 1)), col = terrain.colors(3))
text(c(chiVal, gVal, qchisq(0.95, 1)), 0.3, c("Chi-squared value", "G value", "Critical value"), col = terrain.colors(3))

## -----------------------------------------------------------------------------
chisq.test(obsFreqs, correct = F)

## ---- echo=FALSE--------------------------------------------------------------
# Log-likelihood tests of independence & goodness of fit
# Does Williams' and Yates' correction
# does Monte Carlo simulation of p-values, via gtestsim.c
#
# G & q calculation from Sokal & Rohlf (1995) Biometry 3rd ed.
# TOI Yates' correction taken from Mike Camann's 2x2 G-test fn.
# GOF Yates' correction as described in Zar (2000)
# more stuff taken from ctest's chisq.test()
#
# V3.3 Pete Hurd Sept 29 2001. phurd@ualberta.ca

g.test <- function(x, y = NULL, correct="williams",
  p = rep(1/length(x), length(x)), simulate.p.value = FALSE, B = 2000)
#can also use correct="none" or correct="yates"
{
  DNAME <- deparse(substitute(x))
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) {
    if (min(dim(x)) == 1) 
      x <- as.vector(x)
  }
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y)) 
      stop("x and y must have the same length")
    DNAME <- paste(DNAME, "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) < 2)) 
      stop("x and y must have at least 2 levels")
    x <- table(x, y)
  }
  if (any(x < 0) || any(is.na(x))) 
    stop("all entries of x must be nonnegative and finite")
  if ((n <- sum(x)) == 0) 
    stop("at least one entry of x must be positive")
  #If x is matrix, do test of independence
  if (is.matrix(x)) {
    #Test of Independence
    nrows<-nrow(x)
    ncols<-ncol(x)
    if (correct=="yates"){ # Do Yates' correction?
      if(dim(x)[1]!=2 || dim(x)[2]!=2) # check for 2x2 matrix
        stop("Yates' correction requires a 2 x 2 matrix")
      if((x[1,1]*x[2,2])-(x[1,2]*x[2,1]) > 0)
        {
          x[1,1] <- x[1,1] - 0.5
          x[2,2] <- x[2,2] - 0.5
          x[1,2] <- x[1,2] + 0.5
          x[2,1] <- x[2,1] + 0.5
        }
      else
        {
          x[1,1] <- x[1,1] + 0.5
          x[2,2] <- x[2,2] + 0.5
          x[1,2] <- x[1,2] - 0.5
          x[2,1] <- x[2,1] - 0.5
        }
    }

    sr <- apply(x,1,sum)
    sc <- apply(x,2,sum)
    E <- outer(sr,sc, "*")/n
    # are we doing a monte-carlo?
    # no monte carlo GOF?
    if (simulate.p.value){
      METHOD <- paste("Log likelihood ratio (G-test) test of independence\n\t with simulated p-value based on", B, "replicates")
      tmp <- .C("gtestsim", as.integer(nrows), as.integer(ncols),
                as.integer(sr), as.integer(sc), as.integer(n), as.integer(B),
                as.double(E), integer(nrows * ncols), double(n+1),
                integer(ncols), results=double(B), PACKAGE= "ctest")
      g <- 0
      for (i in 1:nrows){
        for (j in 1:ncols){
          if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
        }
      }
      STATISTIC <- G <- 2 * g
      PARAMETER <- NA
      PVAL <- sum(tmp$results >= STATISTIC)/B
    }
    else {
      # no monte-carlo
      # calculate G
      g <- 0
      for (i in 1:nrows){
        for (j in 1:ncols){
          if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
        }
      }
      q <- 1
      if (correct=="williams"){ # Do Williams' correction
        row.tot <- col.tot <- 0    
        for (i in 1:nrows){ row.tot <- row.tot + 1/(sum(x[i,])) }
        for (j in 1:ncols){ col.tot <- col.tot + 1/(sum(x[,j])) }
        q <- 1+ ((n*row.tot-1)*(n*col.tot-1))/(6*n*(ncols-1)*(nrows-1))
      }
      STATISTIC <- G <- 2 * g / q
      PARAMETER <- (nrow(x)-1)*(ncol(x)-1)
      PVAL <- 1-pchisq(STATISTIC,df=PARAMETER)
      if(correct=="none")
        METHOD <- "Log likelihood ratio (G-test) test of independence without correction"
      if(correct=="williams")
        METHOD <- "Log likelihood ratio (G-test) test of independence with Williams' correction"
      if(correct=="yates")
        METHOD <- "Log likelihood ratio (G-test) test of independence with Yates' correction"
    }
  }
  else {
    # x is not a matrix, so we do Goodness of Fit
    METHOD <- "Log likelihood ratio (G-test) goodness of fit test"
    if (length(x) == 1) 
      stop("x must at least have 2 elements")
    if (length(x) != length(p)) 
      stop("x and p must have the same number of elements")
    E <- n * p
    
    if (correct=="yates"){ # Do Yates' correction
      if(length(x)!=2)
        stop("Yates' correction requires 2 data values")
      if ( (x[1]-E[1]) > 0.25) {
        x[1] <- x[1]-0.5
        x[2] <- x[2]+0.5
      }
      else if ( (E[1]-x[1]) > 0.25){
        x[1] <- x[1]+0.5
        x[2] <- x[2]-0.5
      }
    }
    names(E) <- names(x)
    g <- 0
    for (i in 1:length(x)){
      if (x[i] != 0) g <- g + x[i] * log(x[i]/E[i])
    }
    q <- 1
    if (correct=="williams"){ # Do Williams' correction
      q <- 1+(length(x)+1)/(6*n)
    }
    STATISTIC <- G <- 2*g/q
    PARAMETER <- length(x) - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
  }
  names(STATISTIC) <- "Log likelihood ratio statistic (G)"
  names(PARAMETER) <- "X-squared df"
  names(PVAL) <- "p.value"
  structure(list(statistic=STATISTIC,parameter=PARAMETER,p.value=PVAL,
            method=METHOD,data.name=DNAME, observed=x, expected=E),
            class="htest")
}

## -----------------------------------------------------------------------------
g.test(obsFreqs, correct = "none")

## ---- fig.align = 'center'----------------------------------------------------
mosaicData <- obsFreqs/matrix(c(142, 208, 142, 208), 2, byrow = T)
#Rev the matrix so site is on the x axis
mosaicData2 <- t(mosaicData)
mosaicplot(mosaicData2,
  main = "",
  col = c("darkorchid4", "orange"),
  ylab = "Color",
  xlab = "Site")

## ---- fig.align='center'------------------------------------------------------
opar <- par()
par(mar = c(4, 4, 1, 8) + 0.1)
barplot(obsFreqs/matrix(c(142, 208, 142, 208), 2, byrow = T), 
  legend = T, 
  width = c(142, 208),
  ylab = "Relative Frequency",
  col = c("darkorchid4", "darkorange"),
  args.legend = list(x = "topright", bty = "n", inset = c(-0.3, 0.4)))
#par(opar)

## ---- fig.align='center'------------------------------------------------------
barplot(obsFreqs/matrix(c(142, 208, 142, 208), 2, byrow = T), 
  beside = T,
  legend = T, 
  ylim = c(0, 1),
  ylab = "Relative Frequency",
  col = c("darkorchid4", "darkorange"),
  args.legend = list(x = "topright", bty = "n"))

## ---- eval=FALSE--------------------------------------------------------------
#  # Log-likelihood tests of independence & goodness of fit
#  # Does Williams' and Yates' correction
#  # does Monte Carlo simulation of p-values, via gtestsim.c
#  #
#  # G & q calculation from Sokal & Rohlf (1995) Biometry 3rd ed.
#  # TOI Yates' correction taken from Mike Camann's 2x2 G-test fn.
#  # GOF Yates' correction as described in Zar (2000)
#  # more stuff taken from ctest's chisq.test()
#  #
#  # V3.3 Pete Hurd Sept 29 2001. phurd@ualberta.ca
#  
#  g.test <- function(x, y = NULL, correct="williams",
#    p = rep(1/length(x), length(x)), simulate.p.value = FALSE, B = 2000)
#  #can also use correct="none" or correct="yates"
#  {
#    DNAME <- deparse(substitute(x))
#    if (is.data.frame(x)) x <- as.matrix(x)
#    if (is.matrix(x)) {
#      if (min(dim(x)) == 1)
#        x <- as.vector(x)
#    }
#    if (!is.matrix(x) && !is.null(y)) {
#      if (length(x) != length(y))
#        stop("x and y must have the same length")
#      DNAME <- paste(DNAME, "and", deparse(substitute(y)))
#      OK <- complete.cases(x, y)
#      x <- as.factor(x[OK])
#      y <- as.factor(y[OK])
#      if ((nlevels(x) < 2) || (nlevels(y) < 2))
#        stop("x and y must have at least 2 levels")
#      x <- table(x, y)
#    }
#    if (any(x < 0) || any(is.na(x)))
#      stop("all entries of x must be nonnegative and finite")
#    if ((n <- sum(x)) == 0)
#      stop("at least one entry of x must be positive")
#    #If x is matrix, do test of independence
#    if (is.matrix(x)) {
#      #Test of Independence
#      nrows<-nrow(x)
#      ncols<-ncol(x)
#      if (correct=="yates"){ # Do Yates' correction?
#        if(dim(x)[1]!=2 || dim(x)[2]!=2) # check for 2x2 matrix
#          stop("Yates' correction requires a 2 x 2 matrix")
#        if((x[1,1]*x[2,2])-(x[1,2]*x[2,1]) > 0)
#          {
#            x[1,1] <- x[1,1] - 0.5
#            x[2,2] <- x[2,2] - 0.5
#            x[1,2] <- x[1,2] + 0.5
#            x[2,1] <- x[2,1] + 0.5
#          }
#        else
#          {
#            x[1,1] <- x[1,1] + 0.5
#            x[2,2] <- x[2,2] + 0.5
#            x[1,2] <- x[1,2] - 0.5
#            x[2,1] <- x[2,1] - 0.5
#          }
#      }
#  
#      sr <- apply(x,1,sum)
#      sc <- apply(x,2,sum)
#      E <- outer(sr,sc, "*")/n
#      # are we doing a monte-carlo?
#      # no monte carlo GOF?
#      if (simulate.p.value){
#        METHOD <- paste("Log likelihood ratio (G-test) test of independence\n\t with simulated p-value based on", B, "replicates")
#        tmp <- .C("gtestsim", as.integer(nrows), as.integer(ncols),
#                  as.integer(sr), as.integer(sc), as.integer(n), as.integer(B),
#                  as.double(E), integer(nrows * ncols), double(n+1),
#                  integer(ncols), results=double(B), PACKAGE= "ctest")
#        g <- 0
#        for (i in 1:nrows){
#          for (j in 1:ncols){
#            if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
#          }
#        }
#        STATISTIC <- G <- 2 * g
#        PARAMETER <- NA
#        PVAL <- sum(tmp$results >= STATISTIC)/B
#      }
#      else {
#        # no monte-carlo
#        # calculate G
#        g <- 0
#        for (i in 1:nrows){
#          for (j in 1:ncols){
#            if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
#          }
#        }
#        q <- 1
#        if (correct=="williams"){ # Do Williams' correction
#          row.tot <- col.tot <- 0
#          for (i in 1:nrows){ row.tot <- row.tot + 1/(sum(x[i,])) }
#          for (j in 1:ncols){ col.tot <- col.tot + 1/(sum(x[,j])) }
#          q <- 1+ ((n*row.tot-1)*(n*col.tot-1))/(6*n*(ncols-1)*(nrows-1))
#        }
#        STATISTIC <- G <- 2 * g / q
#        PARAMETER <- (nrow(x)-1)*(ncol(x)-1)
#        PVAL <- 1-pchisq(STATISTIC,df=PARAMETER)
#        if(correct=="none")
#          METHOD <- "Log likelihood ratio (G-test) test of independence without correction"
#        if(correct=="williams")
#          METHOD <- "Log likelihood ratio (G-test) test of independence with Williams' correction"
#        if(correct=="yates")
#          METHOD <- "Log likelihood ratio (G-test) test of independence with Yates' correction"
#      }
#    }
#    else {
#      # x is not a matrix, so we do Goodness of Fit
#      METHOD <- "Log likelihood ratio (G-test) goodness of fit test"
#      if (length(x) == 1)
#        stop("x must at least have 2 elements")
#      if (length(x) != length(p))
#        stop("x and p must have the same number of elements")
#      E <- n * p
#  
#      if (correct=="yates"){ # Do Yates' correction
#        if(length(x)!=2)
#          stop("Yates' correction requires 2 data values")
#        if ( (x[1]-E[1]) > 0.25) {
#          x[1] <- x[1]-0.5
#          x[2] <- x[2]+0.5
#        }
#        else if ( (E[1]-x[1]) > 0.25){
#          x[1] <- x[1]+0.5
#          x[2] <- x[2]-0.5
#        }
#      }
#      names(E) <- names(x)
#      g <- 0
#      for (i in 1:length(x)){
#        if (x[i] != 0) g <- g + x[i] * log(x[i]/E[i])
#      }
#      q <- 1
#      if (correct=="williams"){ # Do Williams' correction
#        q <- 1+(length(x)+1)/(6*n)
#      }
#      STATISTIC <- G <- 2*g/q
#      PARAMETER <- length(x) - 1
#      PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
#    }
#    names(STATISTIC) <- "Log likelihood ratio statistic (G)"
#    names(PARAMETER) <- "X-squared df"
#    names(PVAL) <- "p.value"
#    structure(list(statistic=STATISTIC,parameter=PARAMETER,p.value=PVAL,
#              method=METHOD,data.name=DNAME, observed=x, expected=E),
#              class="htest")
#  }

