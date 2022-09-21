## -----------------------------------------------------------------------------
chisq.data <- c("red", "blue", "red", "red", "orange", "blue", "red", "orange")

## -----------------------------------------------------------------------------
(chisq.data2 <- c(red = 4, blue = 2, orange = 2))
(chisq.data3 <- table(chisq.data)) #Coverts the long form to the compact form

## -----------------------------------------------------------------------------
obs.freqs <- c(purple = 154, orange = 54)

## -----------------------------------------------------------------------------
exp.props <- c(purple = 0.75, orange = 0.25)
(exp.freqs <- sum(obs.freqs)*exp.props)

## -----------------------------------------------------------------------------
(chisq.val <- sum(((obs.freqs - exp.freqs)^2)/exp.freqs))

## ---- fig.align='center'------------------------------------------------------
curve(dchisq(x, df = 1), 0, 20, ylab = "Density")
#Now change the degrees of freddom
curve(dchisq(x, df = 2), add = T, col = "steelblue")
curve(dchisq(x, df = 3), add = T, col = "steelblue1")

## ---- fig.align='center'------------------------------------------------------
curve(dchisq(x, df = 1), 0, 20, ylab = "Density")
#Add chi-squared value to graph
abline(v = chisq.val, col = "firebrick4")
text(chisq.val, 0.1, "Test stat", pos = 4, col = "firebrick4")
#Add critical value to graph
abline(v = qchisq(0.95, df = 1), col = "steelblue")
text(qchisq(0.95, df = 1), 0.8, "Critical value", pos = 4, col = "steelblue")

## -----------------------------------------------------------------------------
#Remember you want the right tail!
1- pchisq(chisq.val, df = 1)

## -----------------------------------------------------------------------------
chisq.test(obs.freqs, p = exp.props, correct = F)

## ---- fig.align='center'------------------------------------------------------
xVals <- barplot(obs.freqs, ylim = c(0, 220), ylab = "Frequency")
#xVals now have the center of each bar
xVals
#I now play around with the values to get the lines right
lines(c(xVals[1]-0.6, xVals[1]+0.6), rep(exp.freqs[1], 2), lty = 5)
lines(c(xVals[2]-0.6, xVals[2]+0.6), rep(exp.freqs[2], 2), lty = 5)

