## ---- eval = FALSE------------------------------------------------------------
#  c(1, 5, 10.3)
#  sequence(100)
#  4:12
#  -34:28
#  seq(4, 12)
#  seq(-34, 28)
#  seq(4, 12, by = 0.2)
#  seq(1, 200, length.out = 5)
#  seq(1, 100, along.with = 1:10)
#  c("Ben", "Melissa", "Gabriel", "Gavin") #a vector of characters

## -----------------------------------------------------------------------------
c(1, 2, 3.4, 6, 8)
2:6
seq(from = 4, to = 5, by = 0.1)
rep(c(4, 5), 3)
rep(c(4, 5), each = 3)

## -----------------------------------------------------------------------------
c(4, 3, "Ben", "Dexter", "orange")

## -----------------------------------------------------------------------------
#Create a vector
(treatments <- rep(c("light", "dark"), each = 4))
#Convert the vector to a factor
(treatment.fac <- factor(treatments)) #Assigns and prints vector
#create another vector
vals <- rep(LETTERS[1:2], each = 5); vals
#Convert the vector to a factor
ben.factor <- factor(vals); ben.factor #Also assigns and prints vector

## -----------------------------------------------------------------------------
#More concise method with gl()
(treatment.fac2 <- gl(n = 2, k = 4, labels = c("light", "dark"))) #Same as treatment.fac
(new.factor <- gl(3, 10, labels = c("low", "med", "high")))

## -----------------------------------------------------------------------------
#Enter in some numeric data to go with ben.factor
data <- c(1, 2, 3, 2, 3, 5, 4, 6, 7, 6) 
length(data)    #Just checking sample size of data

## -----------------------------------------------------------------------------
tapply(data, ben.factor, mean)
tapply(data, ben.factor, sd)

## -----------------------------------------------------------------------------
ben.frame <- data.frame(Treatment = ben.factor, Data = data)
ben.frame

## -----------------------------------------------------------------------------
str(ben.frame)

## -----------------------------------------------------------------------------
summary(ben.frame)

## -----------------------------------------------------------------------------
matrix(1:10, nrow = 2)
matrix(1:10, ncol = 2)
matrix(1:10, nrow = 2, byrow = T)

## -----------------------------------------------------------------------------
array(3:12, dim = c(2, 5))

#example of looping and using an array.
my.array <- array(NA, dim = c(2, 10))
my.array
for(i in 1:2){
  for(j in 1:10){
    my.array[i, j] <- i*j
  }
}
my.array

## -----------------------------------------------------------------------------
list("Ben", 1:5, letters[1:4])
#Now with names
list(Name = "Ben", Numbers = 1:5, Letters = letters[1:4]) 

