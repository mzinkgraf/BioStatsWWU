## -----------------------------------------------------------------------------
my.data <- c(4.5, 56, 78, 34, 5, 7)
my.fac <- factor(rep(c("Low", "High"), each=3))
my.fac2 <- gl(2, 3, labels = c("Low", "High")) #Same as my.fac
(my.frame <- data.frame(my.data, my.fac))
#Enter data and create the data.frame at once by nesting functions
#rnorm() and gl() are "inside" data.frame()
(my.frame2 <- data.frame(my.data = rnorm(6), my.factor = gl(2, 3))) 

## ----eval=FALSE---------------------------------------------------------------
#  (my.frame.v2 <- edit(my.frame)) #Change the 1st value to 13

## ----eval=F-------------------------------------------------------------------
#  ben.frame.v3 <- edit(data.frame())

## ----eval=F-------------------------------------------------------------------
#  my.import <- scan()

## ----eval=FALSE---------------------------------------------------------------
#  setwd("c:/users/minerb2")  #or
#  setwd("c:\\users\\minerb2")
#  getwd()

## -----------------------------------------------------------------------------
#dir()
(ben.data <- read.csv("../inst/extdata/lengthdata.csv"))

## -----------------------------------------------------------------------------
str(ben.data)
summary(ben.data)
head(ben.data)
tail(ben.data)

## -----------------------------------------------------------------------------
#install.packages(readxl) #Only needed if the package is not installed
library(readxl)
(ben.data2 <- read_excel("../inst/extdata/lengthdata.xlsx")) #Same as ben.data
#View the names of the sheets in an excel file
excel_sheets("../inst/extdata/lengthdata.xlsx")

## -----------------------------------------------------------------------------
ben.data[, 2] #No rows were requests so R returns all rows from column 2
ben.data[, -2] #All columns except the second
ben.data[1:3, 1] #Rows 1 through 3 in column 1
ben.data["Salinity"]
ben.data[["Salinity"]]

## -----------------------------------------------------------------------------
ben.data$Salinity

## -----------------------------------------------------------------------------
#Just to remind you of the data in ben.data
ben.data

## ----eval=FALSE---------------------------------------------------------------
#  ben.data$Length[4:7]
#  ben.data$Length[ben.data$Length > 5]
#  ben.data$Salinity[ben.data$Length > 5]
#  ben.data$Length[ben.data$Salinity == "High"]
#  ben.data$Length[ben.data$Salinity == "Low"]
#  ben.data$Length[ben.data$Salinity != "Low"]

## ----echo=FALSE---------------------------------------------------------------
ben.data$Length[4:7]
ben.data$Length[ben.data$Length > 5]
ben.data$Salinity[ben.data$Length > 5]
ben.data$Length[ben.data$Salinity == "High"]
ben.data$Length[ben.data$Salinity == "Low"]
ben.data$Length[ben.data$Salinity != "Low"]

## -----------------------------------------------------------------------------
data() #Shows you all the pre-loaded datasets
mtcars #Shows the data in the mtcars dataset
carData <- mtcars #Assigns the mtcars dataset to the name carData
head(carData)

