## -----------------------------------------------------------------------------
#the function rep repeats a vector
#below we repeat the vector c("Pop 1", "Pop 2") 25 times
pops <- rep(c("Pop 1", "Pop 2"), 25)

#converts the data to a categorical variable
pops.fac <- factor(pops) 

#view the data
pops
pops.fac #compare the output from pops

#set the random number generator value so we all generate the same "random"" numbers
set.seed(340)  
#create some fake data
#the function rnorm returns 50 random values from
#a normal distribution with a mean of 66 and 
#standard deviation of 10
height <- rnorm(50, 66, 10)
height
#create 50 values for a normal distribution with
#a mean of 150 and standard deviation of 30
weight <- rnorm(50, 150, 30)
weight

## ---- fig.align='center', fig.width=4, fig.height=4---------------------------
plot(height~weight) 

## ---- fig.align='center', fig.width=4, fig.height=4---------------------------
plot(height~pops.fac) 
boxplot(height~pops.fac) #same as the above plot

## ---- fig.align='center', fig.width=4, fig.height=4---------------------------
#Calculate the means
pop.mean.heights <- tapply(height, pops.fac, mean)
pop.mean.heights
#Plot the means
barplot(pop.mean.heights)

## ---- fig.align='center', fig.width=4, fig.height=4---------------------------
#Calculate the sd
pop.sd.heights <- tapply(height, pops.fac, sd)
pop.sd.heights

## ---- fig.align='center', fig.width=4, fig.height=4---------------------------
barplot(pop.mean.heights, names.arg = c("Population 1", "Population 2"))

## ----fig.align='center', fig.width=4, fig.height=4----------------------------
pop.mean.weights <- tapply(weight, pops.fac, mean)
pie(pop.mean.weights)

## ----fig.align='center', fig.width=4, fig.height=4----------------------------
pie(pop.mean.weights, label = c("Population 1", "Populations 2"))

## ----fig.align='center', fig.width=4, fig.height=4----------------------------
hist(weight)
hist(weight, breaks = 4)

## ----fig.align='center'-------------------------------------------------------
plot(height~weight, main = "My Sweet Graph", sub = "Created by Ben")

## ----fig.align='center'-------------------------------------------------------
plot(height~weight, 
   main = "My Sweet Graph", 
   xlab = "Weight (mg)", 
   ylab = "Height (mm)")

## ----fig.align='center'-------------------------------------------------------
plot(height~weight, 
   main = "My Sweet Graph", 
   xlab = "Weight (mg)",
   ylab = "Height (mm)",
   cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

## ----fig.align='center'-------------------------------------------------------
plot(height~weight, 
   main = "My Sweet Graph", 
   xlab = "Weight (mg)",
   ylab = "Height (mm)",
   cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
   pch = 15, col = "goldenrod3")

## ----fig.align='center'-------------------------------------------------------
plot(height~weight, 
   main = "My Sweet Graph", 
   xlab = "Weight (mg)",
   ylab = "Height (mm)",
   cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
   pch = 15, col = "goldenrod3",
   xlim = c(0,500), ylim = c(0,100))

## ---- eval=FALSE--------------------------------------------------------------
#  getwd()       #indicates where the file will be saved
#  setwd("u:/")  #sets the working directory to your u drive	

## ---- eval=FALSE--------------------------------------------------------------
#  pdf("my first graph.pdf", paper = "letter", pointsize = 12)
#  plot(height~weight,
#     main = "My Sweet Graph",
#     xlab = "Weight (mg)",
#     ylab = "Height (mm)",
#     cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
#     pch = 15, col = "goldenrod3",
#     xlim = c(0,500), ylim = c(0,100))
#  dev.off()

