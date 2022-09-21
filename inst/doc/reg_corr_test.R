## ---- echo=F------------------------------------------------------------------
reg.data <- data.frame(
  Predictor = c(4.2, 5.6, 5.1, 4.6, 9.2, 6.9, 10, 3.4, 7, 11.2, 1.2, 8.7), 
  Response = c(6.9, 8.3, 7.3, 6.3, 11.2, 8.1, 13.6, 5.7, 8, 13.8, 3.3, 12.8))

## -----------------------------------------------------------------------------
var(reg.data$Predictor)

## -----------------------------------------------------------------------------
#var and cov both give the same answer
var(reg.data)
cov(reg.data)

## -----------------------------------------------------------------------------
fit<-lm(Response~Predictor,data=reg.data)
summary(fit)
anova(fit)

## -----------------------------------------------------------------------------
names(fit) #Recall that "fit" is the name I gave the lm model
fit$residuals #Gives the residuals
fit$coefficients #Gives the slope and intercept

## -----------------------------------------------------------------------------
cor.test(reg.data$Predictor, reg.data$Response)
#Same as above
cor.test(reg.data$Response, reg.data$Predictor)

## -----------------------------------------------------------------------------
cor.test(reg.data$Predictor, reg.data$Response, alt = "less")

## -----------------------------------------------------------------------------
cor.test(reg.data$Predictor, reg.data$Response, method = "spearman")

## ---- fig.align='center'------------------------------------------------------
#Create some more fake data
reg.data2 <- data.frame(reg.data, Predictor2 = rpois(12, 3), Response2 = sort(rnorm(12, 23)), CatPredictor = gl(3, 4, labels = c("Low", "Med", "High")))
#Plot the all the fake data
plot(reg.data2)

## ---- fig.align="center"------------------------------------------------------
plot(Response ~ Predictor, data = reg.data, pch = 16)
abline(fit, col = "grey")
#I pull the values for the best fit line from fit to display on the graph
i <- round(fit$coef[[1]], 2) #i for the intercept
s <- round(fit$coef[[2]], 2) #s for the slope
text(2, 13, paste("y=", i, "+", s, "*x"), pos = 4)

## ---- fig.align="center"------------------------------------------------------
plot(Response ~ Predictor, data = reg.data, pch = 16)
#I pull information out of cor.test to display on the graph
corInfo <- cor.test(reg.data$Predictor, reg.data$Response)
text(2, 13, paste("r = ", signif(corInfo[["estimate"]], 3), "\nP = ", signif(corInfo[["p.value"]], 3), sep = ""), pos = 4) #\n returns to next line

