## ---- eval=-1-----------------------------------------------------------------
install.packages(ggplot2)
require(ggplot2)

## ---- eval =-1----------------------------------------------------------------
install.packages(ggplot2movies)
require(ggplot2movies) #Don't forget that you have to install the package!
data(movies)
str(movies)

## ---- fig.align='center'------------------------------------------------------
qplot(x = budget, y = rating, data = movies)

## ---- fig.align='center', warning=F-------------------------------------------
qplot(year, budget, data = movies)

## ---- fig.align='center', warning=F-------------------------------------------
qplot(year, budget, data = movies, 
    ylim = c(0, 100000000), #Set upper limit at 100 million dollars
    main = "Movies are now expensive!",
    xlab = "Year movie was released",
    ylab = "Total budget (US dollars)"
)

## ---- fig.align='center', warning=F-------------------------------------------
qplot(year, data = movies)

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget)) + geom_point()

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget, colour = mpaa)) + geom_point()

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget, colour = mpaa)) + 
  geom_point() + 
  scale_colour_brewer()

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget, colour = mpaa, size = rating)) + 
  geom_point() + 
  scale_colour_brewer()

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa)

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa) +
  theme_minimal(16)

## ---- fig.align='center', warning=F-------------------------------------------
ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa) +
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = "lightgrey")
  )

## ---- fig.align='center', warning=FALSE---------------------------------------
myTheme <- theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = "lightgrey"),
    strip.background = element_rect(fill = "steelblue"),
    strip.text = element_text(color = "white"),
    axis.text = element_text(color = "black")
)

ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa) +
  myTheme

## ---- fig.align='center', eval=-1, warning=F----------------------------------
#install.packages(ggthemes)
require(ggthemes)

ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa) +
  theme_tufte()

ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa) +
  theme_few()

#This one is ugly!!
ggplot(movies, aes(x = year, y = budget, color = rating)) + 
  geom_point() + 
  facet_wrap(~ mpaa) +
  theme_excel()

## ---- fig.align='center'------------------------------------------------------
data(iris)

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = "mean")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "summary", fun.y = mean, fill = "orange") +
  geom_errorbar(stat = "summary", fun.ymin = min, fun.ymax = max)

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_crossbar(stat = "summary", fun.y = mean, fun.ymin = min, fun.ymax = max, fill = "pink") 

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_violin(fill = "seagreen") 

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_violin(fill = "seagreen") +
  geom_text(x = 1, y = 7.5, label = "Testing 1, 2, 3...")

#Use it to label each point
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, label = Species)) +
  geom_text() 

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  stat_summary(fun.data = "mean_se")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_point(colour = "lightgrey") +
  stat_summary(fun.y = "median", colour = "orange", geom = "point") +
  stat_summary(fun.y = "mean", colour = "springgreen4", geom = "point")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point() +
  stat_smooth(method="lm")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  scale_color_brewer(palette = 10)

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Petal.Length)) +
  geom_point() +
  scale_colour_gradient(low = "pink", high = "darkorchid4")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, size = Petal.Length)) +
  geom_point() +
  scale_size_area(max_size = 4)

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, shape = Species, linetype = Species)) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_bar(stat = "summary", fun.y = mean, fill = "bisque", col = "orange")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_bar(stat = "summary", fun.y = mean, fill = "bisque", col = "orange") +
  coord_flip()

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(y = Species, x = Petal.Length)) +
  geom_bar(stat = "summary", fun.y = mean, fill = "bisque", col = "orange")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(col = "orange") +
  theme_minimal() + 
  coord_fixed() 

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(col = "darkorchid4") +
  coord_trans(x = "log10", y = "log10")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_bar(stat = "summary", fun.y = mean, fill = "steelblue", alpha = 0.5) +
  coord_polar()

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species, size = Petal.Length/Petal.Width)) +
geom_point()

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species, size = Petal.Length/Petal.Width)) +
geom_point() +
guides(size = FALSE)

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species, size = Petal.Length/Petal.Width)) +
geom_point() +
theme(legend.position = "none")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_crossbar(stat = "summary", fun.data = "mean_se", fill = "orange")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  stat_summary(fun.data = mean_se, geom = "crossbar", fill = "orange")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species, size = Petal.Length/Petal.Width)) +
geom_point() +
theme(legend.position = "top")

## ---- fig.align='center'------------------------------------------------------
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species, size = Petal.Length/Petal.Width)) +
geom_point() +
theme(legend.position = c(0.2, 0.5))

