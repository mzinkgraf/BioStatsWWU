## ---- warning=FALSE, message=FALSE--------------------------------------------
require("prob")

## -----------------------------------------------------------------------------
egg.ss <- iidspace(c("Hatched", "Not Hatched"), ntrials = 3, probs = c(0.8, 0.2))
egg.ss

## -----------------------------------------------------------------------------
f <- egg.ss[c(1,3,5,7), ] #Alternatively you could use the function subset, see below.
f

t <- egg.ss[c(1,2,3,5), ] #Alternatively you could use the function subset, see below.


## -----------------------------------------------------------------------------
#Select all the cases in which the first egg hatched
#Same as the set f we created above
subset(egg.ss, X1 == "Hatched")

#Select all the cases in which two or more eggs hatched
#Same as the set t we created above
subset(egg.ss, 
            X1 == "Hatched" & X2 == "Hatched" | 
            X2 == "Hatched" & X3 == "Hatched" |
            X1 == "Hatched" & X3 == "Hatched")

#Select all the cases in which the second and third eggs did not hatch
subset(egg.ss,X2 == "Not Hatched" & X3 == "Not Hatched")

#Select all the cases in which at least one egg hached
subset(egg.ss, X1 == "Hatched" | X2 == "Hatched" | X3 == "Hatched")


## -----------------------------------------------------------------------------
#Our homemade function to create a subset
select_set <- function(set, selection_number = 2, selection_name = "Hatched", n_trials = 3) {
  in.t <- c()
  for (i in 1:dim(set)[1]) {
    in.t[i] <- sum(set[i, 1:n_trials] == selection_name) >= selection_number
  }
  return(subset(set, in.t))
}

#Select all the cases in which two or more eggs hatched
#Same as the set t we created above
select_set(egg.ss)

#Select all the cases in which 1 or more eggs did not hatched
select_set(egg.ss, 1, "Not Hatched")


## -----------------------------------------------------------------------------
u <- union(f, t)
u
i <- intersect(f, t)
i

## -----------------------------------------------------------------------------
setdiff(f, t)
setdiff(t, f)
not.t <- setdiff(egg.ss, t) #Gives t^c
not.f <- setdiff(egg.ss, f) #Gives f^c

## -----------------------------------------------------------------------------
Prob(f)
Prob(t)
Prob(u)
Prob(i)
Prob(not.t)
Prob(not.f)

## -----------------------------------------------------------------------------
Prob(i)/Prob(f) #Formula for conditional probs
Prob(t, given = f)
Prob(f, given = t)

## -----------------------------------------------------------------------------
#set your working directory
#use dir() to see files in the working directory

data <- read.csv("../inst/extdata/ladybirddata.csv")
str(data)

## -----------------------------------------------------------------------------
table(data$Species, data$Spots)

## -----------------------------------------------------------------------------
with(data, table(Species, Spots))

## -----------------------------------------------------------------------------
with(data, table(BG_Color, Spot_Color, Species))

## -----------------------------------------------------------------------------
prop.table(with(data, table(Species, Spots)))

## -----------------------------------------------------------------------------
empirical(data)

## -----------------------------------------------------------------------------
coin.ss <- tosscoin(6)
head(coin.ss)
coin.ssp <- tosscoin(6, makespace = T)
head(coin.ssp)

## -----------------------------------------------------------------------------
dice.ss <- rolldie(2)
head(dice.ss)
dice.ssp <- rolldie(2, makespace = T)
head(dice.ssp)

## -----------------------------------------------------------------------------
cardDeck <- cards(jokers = T)
head(cardDeck)
cardDeckp <- cards(jokers = T, makespace = T)
head(cardDeckp)

## -----------------------------------------------------------------------------
roulTable <- roulette()
head(roulTable)
roulTablep <- roulette(makespace = T)
head(roulTablep)

## -----------------------------------------------------------------------------
mms <- urnsamples(c("red", "orange", "yellow", "brown", "light brown", "blue", "green"), 2, replace = T, order = T)
head(mms)

## -----------------------------------------------------------------------------
mms2 <- urnsamples(c("red", "orange", "yellow", "brown", "light brown", "blue", "green"), 2, replace = T)
head(mms2)

## -----------------------------------------------------------------------------
pokerHand <- urnsamples(cards(), 2)
head(pokerHand)

## -----------------------------------------------------------------------------
red <- subset(roulTablep, color == "Red")
Prob(red) #Prob of winning
1 - Prob(red) #Prob of losing

## -----------------------------------------------------------------------------
playingRoulette <- sim(roulTablep, ntrials = 2000)
head(playingRoulette)

## -----------------------------------------------------------------------------
wagered <- 5*2000
#nrow() returns the number of rows
earnings <- 10 * nrow(subset(playingRoulette, color == "Red")) #10 because $5 for bet and $5 for winnings
(winnings <- earnings - wagered)

