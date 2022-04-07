## ---- eval=FALSE--------------------------------------------------------------
#    #If you have admin access
#    install.packages("plyr", dependencies=T)
#    require("plyr")
#  
#    #If you don't have admin access
#    #And install the package to your u drive
#    install.packages("plyr", lib="u:/", dependencies=T)
#    require("plyr", lib.loc="u:/")

## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
require(plyr)

## -----------------------------------------------------------------------------
  data(iris)
  str(iris)

## -----------------------------------------------------------------------------
sepal.length.species <- ddply(iris, .(Species), summarise, 
  mean.Sepal.Length = mean(Sepal.Length, na.rm = T),
  sd.Sepal.Length = sd(Sepal.Length, na.rm = T)
)
sepal.length.species

## -----------------------------------------------------------------------------
  sepal.length.species2 <- ddply(iris, .(Species), summarise, 
    mean.Sepal.Length = mean(Sepal.Length, na.rm = T),
    sd.Sepal.Length = sd(Sepal.Length, na.rm = T),
    var.Sepal.Length = var(Sepal.Length, na.rm = T)
  )
sepal.length.species2

## -----------------------------------------------------------------------------
iris2 <- data.frame(iris, NewFactor = rep(c("Big", "Small"), length.out=iris))
head(iris2)

## -----------------------------------------------------------------------------
  sepal.length.species.newfac <- ddply(iris2, .(Species, NewFactor), summarise, 
    mean.Sepal.Length = mean(Sepal.Length, na.rm = T),
    sd.Sepal.Length = sd(Sepal.Length, na.rm = T)
  )
sepal.length.species.newfac

