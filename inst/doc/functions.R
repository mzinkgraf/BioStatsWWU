## -----------------------------------------------------------------------------
add_two <- function(x) x + 2

## -----------------------------------------------------------------------------
add_two(4)

## -----------------------------------------------------------------------------
add_two(10)
add_two(1:10)

## -----------------------------------------------------------------------------
add_sub_two <- function(x, add) {
  if(add == TRUE) x + 2 else x - 2
}

## -----------------------------------------------------------------------------
desc_stats <- function() {
  cat("Mean =")
}
#The function has no arguments right now
desc_stats()

## -----------------------------------------------------------------------------
desc_stats <- function(x) {
  cat("Mean =", mean(x))
}
#We now need to provide the function with some data
desc_stats(1:10)

## -----------------------------------------------------------------------------
desc_stats <- function(x) {
  cat("Mean =", mean(x), "\n")
  cat("Median =", median(x))
}

desc_stats(1:10)

## -----------------------------------------------------------------------------
desc_stats <- function(x, center = TRUE, spread = TRUE) {
  cat("Mean =", mean(x), "\n")
  cat("Median =", median(x), "\n")
  cat("Variance =", var(x), "\n")
  cat("Standard deviation =", sd(x))
}
desc_stats(1:10)

## ---- error = T---------------------------------------------------------------
desc_stats <- function(x, results = "both") {
     
  if(results == "both" | results == "location") {
    cat("Measures of location", "\n")
    cat("\t", "Mean =", mean(x), "\n")
    cat("\t", "Median =", median(x), "\n \n")    
  }

  if(results == "both" | results == "spread") {
    cat("Measures of spread", "\n")
    cat("\t", "Variance =", var(x), "\n")
    cat("\t", "Standard deviation =", sd(x), "\n \n")
  }  
  
  cat("n =", length(x))
}
desc_stats(1:10)
desc_stats(1:10, results = "both")
desc_stats(1:10, results = "location") 
desc_stats(1:10, results = "spread") 

## -----------------------------------------------------------------------------
desc_stats <- function(x, results = "both") {
  if(!is.numeric(x)) stop("The data in x must be numeric. How do you expect me to calculate the mean or variance of a non-numeric value?")
  
  if(!(results %in% c("location", "spread", "both"))) stop("Options for the results are: 'location', 'spread', or 'both' -- and not whatever you put.")
     
  if(results == "both" | results == "location") {
    cat("Measures of location", "\n")
    cat("\t", "Mean =", mean(x), "\n")
    cat("\t", "Median =", median(x), "\n \n")    
  }

  if(results == "both" | results == "spread") {
    cat("Measures of spread", "\n")
    cat("\t", "Variance =", var(x), "\n")
    cat("\t", "Standard deviation =", sd(x), "\n \n")
  }  
  
  cat("n =", length(x))
}

## ---- error = T---------------------------------------------------------------
desc_stats("testing") #Give x a non-numeric object
desc_stats(1:10, results = "stats") #Give an invalid value for results

