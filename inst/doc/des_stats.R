## -----------------------------------------------------------------------------
ben.data <- read.csv("../inst/extdata/lengthdata.csv")
mean(ben.data$Length)

## -----------------------------------------------------------------------------
geo.mean1 <- function(x){
  prod(x)^(1/length(x))
}
geo.mean2 <- function(x){
  exp(mean(log(x)))
}

geo.mean1(ben.data$Length)
geo.mean2(ben.data$Length)

## -----------------------------------------------------------------------------
har.mean <- function(x){
  length(x)/sum(1/x)
}

har.mean(ben.data$Length)

## -----------------------------------------------------------------------------
median(ben.data$Length)

## -----------------------------------------------------------------------------
hist(ben.data$Length, col = "darkorchid4") #The mode is 3 if we use this histogram

## -----------------------------------------------------------------------------
mode <- function(x){
  y <- sort(table(x),decreasing = T)
  if(y[1] == y[2]) {  #Test whether there is more than one mode
    print("There is more than 1 mode.  You should use a histogram to determine the mode")
  } else {  #If there is only 1 mode, tell the user the mode
    as.numeric(names(y)[1])
  }
}

## -----------------------------------------------------------------------------
var(ben.data$Length)

## -----------------------------------------------------------------------------
sd(ben.data$Length)

## -----------------------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))
se(ben.data$Length)

## -----------------------------------------------------------------------------
cv <- function(x) 100*(sd(x)/mean(x))
cv(ben.data$Length)

## -----------------------------------------------------------------------------
quantile(ben.data$Length)

## -----------------------------------------------------------------------------
quantile(ben.data$Length, probs = c(0.1, 0.25, 0.33, 0.66, 0.75, 0.9))

## -----------------------------------------------------------------------------
summary(ben.data)

## -----------------------------------------------------------------------------
boxplot(ben.data$Length, col="orange")

## -----------------------------------------------------------------------------
boxplot(rnorm(1000), col = "seagreen")

