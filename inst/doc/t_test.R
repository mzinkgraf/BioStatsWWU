## ---- eval=F------------------------------------------------------------------
#  tData1 <- read.csv(file.choose())
#  #Remember this reads in a data.frame, so use the $ with the name of the column to extract the data

## -----------------------------------------------------------------------------
tData2 <- c(18.2, 23.3, 19.0, 45.4, 12.6, 23.8, 34.0, 29.6)  #This is a vector

## -----------------------------------------------------------------------------
rangeData <- c(72.9, 40.9, 36.7, 64.2, 104.2, 33.6, 55.1, 44.3, 40.0, 91.1, 78.8)
meanRangeData <- mean(rangeData)
seRangeData <- sd(rangeData)/sqrt(length(rangeData))
(tRangeData <- (meanRangeData - 0)/seRangeData)

## ---- fig.align='center'------------------------------------------------------
curve(dnorm(x), -10, 10, ylab = "Density")
curve(dt(x, 1), add = T, col = "palegreen4")
curve(dt(x, 5), add = T, col = "palegreen3")
curve(dt(x, 10), add = T, col = "palegreen2")

## -----------------------------------------------------------------------------
sampleSize <- 5
iterations <- 100000 #number of time we want to sample the population
estimatedMeans <- numeric(iterations) #create a variable to hold the estimated means
for(i in 1:iterations){
  estimatedMeans[i] <- mean(rnorm(5, 12.6, 2.3))
}
adjustEstimatedMeans <- estimatedMeans - 12.6 #Shift all points 12.6 units to the left.  This centers the data on zero.
#Calculate the standard deviation of these data
sdAdjustEstimatedMeans <- sd(adjustEstimatedMeans)
adjustEstimatedMeans <- adjustEstimatedMeans/(2.3/sqrt(5))
#Compare the calculated standard deviation to the standard error
sdAdjustEstimatedMeans; 2.3/sqrt(5)
#Create a histogram
hist(adjustEstimatedMeans, freq = F, col = "darkseagreen4", main = "", breaks = 100)
curve(dt(x, 4), add = T, col = "firebrick4")
curve(dnorm(x), add = T, col = "firebrick4")
sd(adjustEstimatedMeans)

## ---- fig.align='center'------------------------------------------------------
#Prob 1.4 or greater (i.e., the right tail) with 10 degrees of freedom
1-pt(1.4, 10)
#Prob for -1.4 or less (i.e., the left tail)
#See how they are the same!
pt(-1.4, 10)
#Calculate the P-value for a 2-tailed test
pt(-1.4, 10) + (1 - pt(1.4, 10))
#Or
2*pt(-1.4, 10)
curve(dt(x, 10), -10, 10, ylab = "Density")
abline(v = c(-1.4, 1.4), col = "palegreen2")
text(c(-1.4, 1.4), 0.3, c("-1.4", "1.4"), pos = c(2,4), col = "palegreen2")

## -----------------------------------------------------------------------------
#Assume our t value is 2.9 with 35 degrees of freedom, and we expect our the mean of our sample to be greater than mu
1-pt(2.9, 35)

## -----------------------------------------------------------------------------
#Assume our t value is -3.6 with 5 degrees of freedom, and you are after the right-hand tail.
1-pt(-3.6, 5)
curve(dt(x, 5), -10, 10)
abline(v = -3.6)
text(-3.6, 0.2, "t value", pos = 2)
#Don't worry about the polygon function
polygon(c(seq(-3.6, 10), seq(10, -3.6, by = -0.1)), c(rep(0, length.out = 14), dt(seq(10, -3.6, by = -0.1), 5)), col = "steelblue")

## -----------------------------------------------------------------------------
1-pt(tRangeData, length(rangeData)-1)

## -----------------------------------------------------------------------------
t.test(rangeData, alternative = "greater")

## ---- fig.align='center'------------------------------------------------------
#Barplot
mean <- mean(rangeData)
se <- sd(rangeData)/sqrt(length(rangeData))
#Create bargraph and adjust the ylim so the error bars will fit
barVals <- barplot(mean, ylim = c(0, 70), ylab = "Range shift (km)")
#Add error bars
arrows(barVals, mean - se, barVals, mean + se, angle = 90, code = 3)

#Boxplot
boxplot(rangeData, ylab = "Range shift (km)")

## -----------------------------------------------------------------------------
#First format
s1 <- c(23, 45, 34, 37, 29, 44, 40, 34)
s2 <- c(12, 20, 19, 18, 22, 14, 17, 17)
(twoSampleData <- data.frame(Sample1 = s1, Sample2 = s2))

#Second format
sample <- rep(c("s1", "s2"), each = 8)
data <- c(23, 45, 34, 37, 29, 44, 40, 34, 12, 20, 19, 18, 22, 14, 17, 17)
(twoSampleData2 <- data.frame(Sample = sample, Data = data))

## -----------------------------------------------------------------------------
#Pull data from the first format
twoSampleData$Sample1
twoSampleData$Sample2

#Pull data from the second format
twoSampleData2$Data[twoSampleData2$Sample == "s1"]
twoSampleData2$Data[twoSampleData2$Sample == "s2"]

## -----------------------------------------------------------------------------
mean1 <- mean(twoSampleData$Sample1)
mean2 <- mean(twoSampleData$Sample2)
pooledVar <- (sum((twoSampleData$Sample1-mean1)^2) + sum((twoSampleData$Sample2-mean2)^2))/(length(twoSampleData$Sample1) + length(twoSampleData$Sample2) - 2)
pooledSE <- sqrt(pooledVar/length(twoSampleData$Sample1) + pooledVar/length(twoSampleData$Sample2))
(tTwoSample <- (mean1 - mean2)/pooledSE)

## -----------------------------------------------------------------------------
2*pt(-tTwoSample, length(twoSampleData$Sample1) + length(twoSampleData$Sample2) - 2)

## -----------------------------------------------------------------------------
t.test(twoSampleData$Sample1, twoSampleData$Sample2, var.equal = T)

## -----------------------------------------------------------------------------
t.test(twoSampleData$Sample1, twoSampleData$Sample2, var.equal = F)

## -----------------------------------------------------------------------------
se1 <- sd(twoSampleData$Sample1)/sqrt(length(twoSampleData$Sample1))
se2 <- sd(twoSampleData$Sample2)/sqrt(length(twoSampleData$Sample2))
barVals2 <- barplot(c(mean1, mean2), 
  ylim = c(0, 50), 
  ylab = "Label (units)",
  names.arg = c("Sample 1", "Sample 2"))
arrows(barVals2, c(mean1+se1, mean2+se2), barVals2, c(mean1-se1, mean2-se2), angle = 90, code = 3)

## -----------------------------------------------------------------------------
#Notice that I am using the second format
boxplot(Data ~ Sample, data = twoSampleData2,
  ylim = c(0, 50), 
  ylab = "Label (units)",
  names = c("Sample 1", "Sample 2"))

## -----------------------------------------------------------------------------
dif <- twoSampleData$Sample1 - twoSampleData$Sample2
meanDif <- mean(dif)
seDif <- sd(dif)/sqrt(length(dif))
(tDif <- (meanDif - 0)/seDif)

## -----------------------------------------------------------------------------
pt(tDif, length(dif)-1)

## -----------------------------------------------------------------------------
t.test(twoSampleData$Sample1, twoSampleData$Sample2, paired = T, alternative = "less")

## -----------------------------------------------------------------------------
t.test(dif, alternative = "less")

