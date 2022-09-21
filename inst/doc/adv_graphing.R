## -----------------------------------------------------------------------------
#Create some data
#If you want the data to look just like mine then you can use the function set.seed()
set.seed(1)
x <- sort(runif(100, 0, 25)); y <- x*2 + rnorm(100, sd = 4)

plot(y ~ x, 
    pch = 16, 
    col = "steelblue", 
    cex = 1.4
)

## ---- echo=-1-----------------------------------------------------------------
plot(y ~ x, 
    pch = 16, 
    col = "steelblue", 
    cex = 1.4
)
#Add text
text(5, 45, "A", cex = 2)
text(5, 45, letters[2:5], pos = 1:4, col = hcl(seq(60, 360, length.out = 4)))
mtext("Bottom", side = 1) #Side 1 is the bottom
mtext("Left", side = 2, line = 2) #Side 2 is the left
mtext("Top", side = 3, line = 1) #Side 3 is the top
mtext("Right", side = 4, adj = 1) #Side 4 is the right

## ---- echo=-(1:9)-------------------------------------------------------------
plot(y ~ x, 
    pch = 16, 
    col = "steelblue", 
    cex = 1.4
)
#Add text
text(5, 45, "A", cex = 2)
text(5, 45, letters[2:5], pos = 1:4, col = hcl(seq(60, 360, length.out = 4)))
mtext("Bottom", side = 1) #Side 1 is the bottom
mtext("Left", side = 2, line = 2) #Side 2 is the left
mtext("Top", side = 3, line = 1) #Side 3 is the top
mtext("Right", side = 4, adj = 1) #Side 4 is the right

points(x, y+10, pch = 15, col = "yellowgreen")
#Create a shadow effect
#First plot the shadow by shifting the points slightly
points(x+0.2, y+0.3, pch = 16, col = "grey", cex= 1.4)
#Replot the original points
points(x, y, pch = 16, col = "steelblue", cex = 1.4)

## ---- echo=-(1:16)------------------------------------------------------------
plot(y ~ x, 
    pch = 16, 
    col = "steelblue", 
    cex = 1.4
)
#Add text
text(5, 45, "A", cex = 2)
text(5, 45, letters[2:5], pos = 1:4, col = hcl(seq(60, 360, length.out = 4)))
mtext("Bottom", side = 1) #Side 1 is the bottom
mtext("Left", side = 2, line = 2) #Side 2 is the left
mtext("Top", side = 3, line = 1) #Side 3 is the top
mtext("Right", side = 4, adj = 1) #Side 4 is the right

points(x, y+10, pch = 15, col = "yellowgreen")
#Create a shadow effect
#First plot the shadow by shifting the points slightly
points(x+0.3, y+0.3, pch = 16, col = "grey", cex= 1.4)
#Replot the original points
points(x, y, pch = 16, col = "steelblue", cex = 1.4)

abline(a = 0, b = 2)
abline(v = seq(0, 25, by = 5), col = rgb(0.5, 0.5, 0.5, alpha = 0.5))
abline(h = seq(0, 50, by = 10), col = rgb(0.5, 0.5, 0.5, alpha = 0.5))

## -----------------------------------------------------------------------------
plot(y ~ x, 
    ylim = c(0, 70), #I increased the ylim so there was space for the legend
    pch = 16, 
    col = "steelblue"
)
points(x, rev(y), pch = 15, col = "yellowgreen")
legend(x = 10, y = 70, legend = c("Green", "Blue"), pch = 15:16, col = c("yellowgreen", "steelblue"))

## -----------------------------------------------------------------------------
xs <- c(1, 1, 2)
ys <- c(1, 2, 2)
sizes <- c(0.25, 0.5, 0.75)
myColors <- sample(colors(), 3)
symbols(xs, ys, circles = sizes, bg = myColors, ylim = c(0,4), xlim = c(0, 4), inches = F)

## -----------------------------------------------------------------------------
xs <- c(1, 1, 2)
ys <- c(1, 2, 2)
myStars <- cbind(c(0.25, 0.40, 0.75), c(0.25, 0.40, 0.75), c(0.25, 0.40, 0.75))
myStars
myColors <- sample(colors(), 3)
symbols(xs, ys, stars = myStars, bg = myColors, ylim = c(0,4), xlim = c(0, 4), inches = F)

## -----------------------------------------------------------------------------
xs <- c(1, 1, 2)
ys <- c(1, 2, 2)
myStars <- cbind(
  c(0.25, 0.80, 0.75), 
  c(0.25, 0.40, 0.75), 
  c(0.50, 0.40, 0.75), 
  c(0.25, 0.40, 0.75), 
  c(0.50, 0.40, 0.75)
)
myColors <- sample(colors(), 3)
symbols(xs, ys, stars = myStars, bg = myColors, ylim = c(0,4), xlim = c(0, 4), inches = F)

## -----------------------------------------------------------------------------
xs <- c(1, 2, 3)
ys <- c(1, 2, 2.3)
myThermos <- cbind(
  c(0.25, 1.0, 0.10), 
  c(0.25, 1.0, 0.75), 
  c(0.50, 0.7, 0.50)
)
myColors <- sample(colors(), 3)
symbols(xs, ys, thermometers = myThermos, fg = myColors, ylim = c(0,4), xlim = c(0, 4), inches = F)

## -----------------------------------------------------------------------------
xs <- c(1, 2, 3)
ys <- c(1, 2, 2.3)
myThermos <- cbind(
  c(0.25, 1.0, 0.1), 
  c(0.25, 1.0, 0.4), 
  c(0.50, 0.1, 0.3),
  c(0.90, 0.4, 0.8)
)
myColors <- sample(colors(), 3)
symbols(xs, ys, thermometers = myThermos, fg = myColors, ylim = c(0,4), xlim = c(0, 4), inches = F)

## -----------------------------------------------------------------------------
plot(rnorm(20, 10, 2), rnorm(20, 20, 6))
symbols(10, 20, squares = 5, inches = F, add = T, fg = "orange", asp = 1)

## -----------------------------------------------------------------------------
plot(1, ylab = "", xlab = "", axes = F, frame = T) #notice this plot ranges from 0.6 to 1.4 on both axes
#create the x and y values for the polygon
xs <- c(0.8, 1.0, 1.2, 1.0)
ys <- c(1.0, 0.8, 1.0, 1.2)
polygon(xs, ys, col = "khaki")

## -----------------------------------------------------------------------------
curve(dnorm(x), -5, 5, ylab = "Density")

#Left tail
critical.lt <- qnorm(0.025) #Set left critical value
xs.lt <- seq(-5,critical.lt,by=0.01) #Take small x value steps along the tail
ys.lt <- dnorm(xs.lt) #Take small y value steps along the tail
xs.lt <- c(xs.lt,critical.lt) #Add x coord of corner of polygon
ys.lt <- c(ys.lt,0) #Add y coord of corner of polygon
polygon(xs.lt, ys.lt, col = "salmon2")

#Right tail
critical.rt <- qnorm(0.975) #Set left critical value
xs.rt <- seq(critical.rt,5, by = 0.01) #Take small x value steps along the tail
ys.rt <- dnorm(xs.rt) #Take small y value steps along the tail
xs.rt <- c(xs.rt,critical.rt) #Add x coord of corner of polygon
ys.rt <- c(ys.rt, 0) #Add y coord of corner of polygon
polygon(xs.rt, ys.rt, col= "mintcream")


## -----------------------------------------------------------------------------
#We will use the data we already created
plot.new()
plot.window(xlim = range(x), ylim = range(y))

#We will now add gridlines, just like in the example above except they will be added first.
abline(v = seq(0, 25, by = 5), col = "lightgrey")
abline(h = seq(0, 50, by = 10), col = "lightgrey")

## ----echo=-(1:8)--------------------------------------------------------------
#We will use the data we already created
plot.new()
plot.window(xlim = range(x), ylim = range(y))

#We will now add gridlines, just like in the example above except they will be added first.
abline(v = seq(0, 25, by = 5), col = "lightgrey")
abline(h = seq(0, 50, by = 10), col = "lightgrey")

box()
axis(1) #add axis to bottom
axis(2, at = seq(0, 50, by = 25)) #adds axis to left
points(x, y, pch = 16, col = "darkorchid4")

## ----echo=-(1:13)-------------------------------------------------------------
#We will use the data we already created
plot.new()
plot.window(xlim = range(x), ylim = range(y))

#We will now add gridlines, just like in the example above except they will be added first.
abline(v = seq(0, 25, by = 5), col = "lightgrey")
abline(h = seq(0, 50, by = 10), col = "lightgrey")

box()
axis(1) #add axis to bottom
axis(2, at = seq(0, 50, by = 25), las = 1) #adds axis to left and makes number horizontal
points(x, y, pch = 16, col = "darkorchid4")

title(xlab = "Length (cm)", ylab = "Height (cm)", main = "Custom graph", sub = "Pretty cool eh!")
#And let's add a rug
rug(x, side = 1) #Add rug to  bottom
rug(y, side = 2) #Add rug to left

## -----------------------------------------------------------------------------
names(par())

## ---- warning=F---------------------------------------------------------------
old.par <- par() #saves current par values
par(old.par) #restores the values that you saved
#You will get a few warnings because some of the saved arguments cannot be set.

## -----------------------------------------------------------------------------
plot.new()
plot.window(xlim =  c(0, 5), ylim = c(0, 5), ann = F, asp = 1)
int <-  0:4+0.5
abline(v = int, col = "lightgrey")
abline(h = int, col = "lightgrey")
points(rep(int, 5), rep(int, each = 5), pch = 1:25)
text(rep(int, 5), rep(int, each = 5), 1:25, pos = 4)

## -----------------------------------------------------------------------------
plot.new()
plot.window(xlim = c(0, 6), ylim =  c(0, 6), ann = F, asp = 1)
int <-  0:5+0.5
abline(h = int, lty = 1:6, lwd = 1:6)
text(1, int, paste(rep("lty = ", 6), 1:6, rep(", lwd = ", 6), 1:6), pos = 3)

## -----------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(1:10)
curve(x^2, 0, 10)
barplot(rnorm(4))
hist(rnorm(20))

## -----------------------------------------------------------------------------
layout(matrix(c(1,2,3,3), 2, byrow = T))
plot(1:10)
barplot(rnorm(4))
hist(runif(50))

## -----------------------------------------------------------------------------
#Bold or italicize text
title <- expression(italic("Species name"))
xlabel <- expression(bold("X values"))
hist(rnorm(100), main = title, xlab = xlabel)

## -----------------------------------------------------------------------------
#Bold and italicize text with special characters
title <- expression(paste("Distribution of ", italic("Species name")))
xlabel <- expression(paste(Lambda, bold(" values")))
hist(rnorm(100), main = title, xlab = xlabel)

add.text <- expression(paste("Temp = ", 32*degree))
text(-2, 15, add.text)

add.text2 <- expression(union(A[i], i==1, n))
text(2.5, 15, add.text2)

## -----------------------------------------------------------------------------
plot.new()
plot.window(xlim = c(0, 5), ylim = c(0, 1))
symbols(1, 0.5, circle = 0.25, bg = rgb(1, 0.2, .4), add = T)
symbols(2, 0.5, circle = 0.25, bg = hsv(0.5, 1, 1), add = T)
symbols(3, 0.5, circle = 0.25, bg = hcl(30, 35, 85, alpha = 0.25), add = T)
symbols(4, 0.5, circle = 0.25, bg = grey(0.7), add = T)

## ---- fig.align='center', fig.height=3----------------------------------------
barplot(rep(1, 100), col = rainbow(100), border=rainbow(100), axes = F)
barplot(rep(1, 100), col = heat.colors(100), border=heat.colors(100), axes = F)
barplot(rep(1, 100), col = terrain.colors(100), border=terrain.colors(100), axes = F)
barplot(rep(1, 100), col = topo.colors(100), border=topo.colors(100), axes = F)
barplot(rep(1, 100), col = cm.colors(100), border=cm.colors(100), axes = F)

## ---- fig.align='center', fig.height=3----------------------------------------
#University of Florida colors
my.ramp1 <- colorRampPalette(c("darkblue", "darkorange"))
barplot(rep(1, 100), col = my.ramp1(100), border=my.ramp1(100), axes = F)
#You can make a gradient across more than 2 colors
my.ramp2 <- colorRampPalette(c("steelblue", "white", "darksalmon"))
barplot(rep(1, 100), col = my.ramp2(100), border=my.ramp2(100), axes = F)

## ---- fig.align='center', fig.height=8----------------------------------------
require(RColorBrewer) #Remember I have already install the package
display.brewer.all()

## ---- fig.align='center', fig.width=4, fig.height=4---------------------------
require(grid)
grid.newpage()
vp <- viewport(x=0.5,y=0.5,width=1.0, height=1.0)
#pushViewport(vp, name = "Venn")
grid.circle(0.5, 0.5, r = 0.45, gp = gpar(fill = "gray93"))
outcomes <- paste("(", rev(letters[1:8]), ")", sep="")
x.vals <- rep(c(0.6, 0.4), each = 4)
y.vals <- rep(seq(0.3, 0.7, length.out = 4), 2)
grid.text(outcomes, x.vals, y.vals, gp = gpar(cex = 2))

