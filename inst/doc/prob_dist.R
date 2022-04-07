## -----------------------------------------------------------------------------
dbinom(10, 30, 0.2)

## -----------------------------------------------------------------------------
dpois(6, 2.1)

## -----------------------------------------------------------------------------
n <- 30
x <- 0:n
y <- dbinom(x, size = n, prob = 0.2)
plot(x, y, 
  ylab = "Probability", xlab = "Number of towns", #Names for the y and x axes
  col = "darkorchid4", pch = 16 #Changes the symbol and color of the points
)

## -----------------------------------------------------------------------------
n <- 30
x <- 0:n
y <- dbinom(x, size = n, prob = 0.2)
plot(x, y, 
  ylab = "Probability", xlab = "Number of towns", #Names for the y and x axes
  col = "darkorchid4", pch = 16 #Changes the symbol and color of the points
)
y2 <- dbinom(x, size = n, prob = 0.3)
points(x, y2, col = "yellowgreen", pch = 16)

## -----------------------------------------------------------------------------
curve(dunif(x, min = 2, max = 6), from = -3, to = 11,
  ylab = "Density", xlab = "x")

## -----------------------------------------------------------------------------
x <- seq(-3, 11, by = 0.1)
y <- dunif(x, min = 2, max = 6)
plot(x, y, type = "l", #That is the letter l for line, not the number 1
  ylab = "Density", xlab = "x")

## -----------------------------------------------------------------------------
curve(dunif(x, min = 2, max = 6), from = -3, to = 11,
  ylab = "Density", xlab = "x")
curve(dunif(x, min = 1, max = 8), add = T, col = "darkred")

## -----------------------------------------------------------------------------
curve(dnorm(x, mean = 10, sd = 3), from = 0, to = 20,
  ylab = "Density", xlab = "x")

## -----------------------------------------------------------------------------
#Poisson
plot(0:10, dpois(0:10, 0.1), pch = 16, ylab = "Probability")
#Normal
curve(dnorm(x, 5, 1), 0, 10, ylab = "Density")

## -----------------------------------------------------------------------------
pbinom(3, size = 8, prob = 0.5) 
sum(dbinom(0:3, size = 8, prob = 0.5)) #Same as above

## -----------------------------------------------------------------------------
x <- 0:8
plot(x, pbinom(x, size = 8, prob = 0.5), 
  ylab = "Probability", xlab = "Number of heads",
  col = "darkorchid4", pch = 16)
text(6, 0.8, "CDF", col = "darkorchid4")
points(x, dbinom(x, size = 8, prob = 0.5), col = "darkgreen", pch = 16)
text(6, 0.3, "PDF", col = "darkgreen")

## -----------------------------------------------------------------------------
pnorm(0, 0, 1) #Gives the probability of getting 0 or less from the standard normal distribution
qnorm(0.5, 0, 1) #Gives the value associated with 0.5 proability from the standard normal distribution

## -----------------------------------------------------------------------------
#Quantile function
curve(qnorm(x, 0, 1), 0, 1, col = "darkred", main = "Quantile function")
#CDF
curve(pnorm(x, 0, 1), -2, 2, col = "darkorange", main = "CDF")

## -----------------------------------------------------------------------------
runif(51, min = 1.23, max = 5)

## -----------------------------------------------------------------------------
rnorm(156) #The default values are mean = 0 and sd = 1, so I can leave out the mean and sd for a standard normal distribution

## -----------------------------------------------------------------------------
rpois(6, 16.3)

## -----------------------------------------------------------------------------
dnorm(23, mean = 34, sd = 12)

## -----------------------------------------------------------------------------
pbinom(5, size = 12, p = 0.61)

## -----------------------------------------------------------------------------
qunif(0.5, min = 6.12, max = 9.34)

## -----------------------------------------------------------------------------
dbinom(c(0,2,5), size = 10, p = 0.2)

## -----------------------------------------------------------------------------
dbinom(0:10, size = 10, p = 0.2)

## -----------------------------------------------------------------------------
dpois(2, 1:20)

## -----------------------------------------------------------------------------
curve(dnorm(x, 3.4, 5.1), -8.4, 14.4, ylab = "Density", xlab = "x")
abline(v = 4)

## -----------------------------------------------------------------------------
pnorm(4, 3.4, 5.1)

## -----------------------------------------------------------------------------
1-pnorm(4, 3.4, 5.1)

## -----------------------------------------------------------------------------
pnorm(4, 3.4, 5.1) - pnorm(3, 3.4, 5.1)

## -----------------------------------------------------------------------------
mus <- seq(0, 50, by = 5)
#Create a color for each mu in mus
rainbow.colors <- rainbow(length(mus))

curve(dnorm(x, 0, 2), -10, 60, ylab = "Density", xlab = "x")
#Add a bunch of normal distributions to the plot, each with a different mean and color
for(i in 1:length(mus)){
  curve(dnorm(x, mus[i], 2), add = T, col = rainbow.colors[i])
}

## -----------------------------------------------------------------------------
sds <- seq(1, 101, by = 5)
#Create a color for each mu in mus
heat.colors <- heat.colors(length(mus))

curve(dnorm(x, 0, 1), -30, 30)
for(i in 1:length(mus)){
  curve(dnorm(x, 0, sds[i]), add = T, col = heat.colors[i])
}

