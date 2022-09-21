## -----------------------------------------------------------------------------
str(iris)

## -----------------------------------------------------------------------------
(means <- tapply(iris$Petal.Length, iris$Species, mean))
#Or you can use the function with() so you don't have to write iris so often
(means <- with(iris, tapply(Petal.Width, Species, mean)))

## -----------------------------------------------------------------------------
#A data.frame with only the rows for the setosa
setosa <- iris[iris$Species == "setosa", ]  #Notice I need two arguments (first for rows and second for columns)
head(setosa)
#A vector with the petal lengths only for setosa
(setosa_lengths <- iris$Petal.Length[iris$Species == "setosa"])

## ---- eval=-(1:3), fig.align='center', fig.height=8, fig.width=4--------------
#Open a window so two graphs fit one above the other
windows(width=4, height=8) #For PCs
qartz(width = 4, height = 8) #For Macs

#Split the window to plot two graphs and adjust the figure margins
par(mfrow=c(2, 1), mar=c(4,4,0,0)+0.5) 
#PDF of the F distribution with 2, 9 degrees of freedom
curve(df(x, 2, 9), 0, 10, ylim=c(0, 1), xlab = "F", ylab="Density")
#CDF of the F distribution with 2, 9 degrees of freedom
curve(pf(x, 2, 9), 0, 10, ylim=c(0, 1), xlab = "F", ylab="Prob")

## ---- eval=-(1:2), fig.align='center',fig.height=8, fig.width=4---------------
windows(width = 4, height = 8)
quartz(width = 4, height = 8)

par(mfrow = c(2, 1), mar = c(4,4,0,0) + 0.5)

#Adjust the degrees of freedom for the within group error (i.e., denominator)
df.num <- 10
n <- 8
(df.den <- cumprod(1:n) )
(my.colors <- rainbow(n) )

curve(df(x, df.num, 1), 0, 6, ylab="Density", xlab = "F", ylim = c(0, 1))
for(i in 1:n) curve(df(x, df.num, df.den[i]), col=my.colors[i], add=TRUE)
legend(3, 1, df.den[1:n], lty = rep(1, n), col = my.colors, title = "within error df", bty = "n")

curve(pf(x, df.num, 1), 0, 6, ylab = "Probability", xlab = "F", ylim = c(0, 1))
for(i in 1:n) curve(pf(x, df.num, df.den[i]), col = my.colors[i], add = TRUE)

#Adjust the degrees of freedom for the among group error (i.e., numerator)
df.den <- 10
n <- 8
(df.num <- cumprod(1:n) )
(my.colors <- rainbow(n) )
curve(df(x, 1, df.den), 0, 6, ylab = "Density", xlab = "F", ylim = c(0, 1))
for(i in 1:n) curve(df(x, df.num[i], df.den), col = my.colors[i], add = TRUE)
legend(3, 1, df.num[1:n], lty = rep(1, n), col = my.colors, title = "among group df", bty = "n")

curve(pf(x, 1, df.den), 0, 6, ylab="Probability", xlab = "F", ylim = c(0, 1))
for(i in 1:n) curve(pf(x, df.num[i], df.den), col = my.colors[i], add = TRUE)

## -----------------------------------------------------------------------------
1 - pf(5.6, 2, 147)
#Or we can use the lower.tail argument, same as above
pf(5.6, 2, 147, lower.tail = FALSE)

## -----------------------------------------------------------------------------
qf(0.95, 2, 147)

## -----------------------------------------------------------------------------
fit_lm <- lm(Petal.Length ~ Species, iris)
anova(fit_lm)

## -----------------------------------------------------------------------------
fit_aov <- aov(Petal.Length ~ Species, iris)
summary(fit_aov)

## -----------------------------------------------------------------------------
plot(Sepal.Length ~ Species, iris)

## -----------------------------------------------------------------------------
plot(Sepal.Length ~ Species, iris)

## -----------------------------------------------------------------------------
iris_means <- with(iris, tapply(Sepal.Length, Species, mean))
iris_se <- with(iris, tapply(Sepal.Length, Species, function(x) sd(x)/sqrt(length(x))))

#You often need to adjust the y axis to include the tops of the error bars
x_vals <- barplot(iris_means,
    ylim = c(0, 8),
    ylab = "Sepal Length (units)"
)
arrows(x_vals, iris_means + iris_se, x_vals, iris_means - iris_se, angle = 90, code = 3)
#If I want to put a box around it
box()

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  ylab("Sepal length (units)")

## -----------------------------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  stat_summary(geom = "bar", fun.y = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
  ylab("Sepal length (units)")

## -----------------------------------------------------------------------------
#Requires the Hmisc package
#Plot standard deviation
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  stat_summary(geom = "bar", fun.y = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_sdl", width = 0.2) +
  ylab("Sepal length (units)")

#Plot 95% confidence intervals
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  stat_summary(geom = "bar", fun.y = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal", width = 0.2) +
  ylab("Sepal length (units)")

## -----------------------------------------------------------------------------
plot(fit_lm)

## -----------------------------------------------------------------------------
TukeyHSD(fit_aov)

