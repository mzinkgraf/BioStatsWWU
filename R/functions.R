#author Matthew Zinkgraf


#'No in
#'
#'This function does the opposite of in
#'
#' @author Matthew Zinkgraf, \email{matthew.zinkgraf@wwu.edu}
#' @export
`%ni%`=Negate(`%in%`)




#'
#' Geometric mean 1
#'
#' @usage geo.mean1(x)
#'
#' @param x vector of numerical values
#' @examples
#' #generate some data
#' data <- rnorm(10)
#'
#' #calculate geometric mean
#' geo.mean1(data)
#'
#' @export
geo.mean1 <- function(x){
  prod(x)^(1/length(x))
}

#'
#' Geometric mean 2
#'
#' @usage geo.mean2(x)
#'
#' @param x vector of numerical values
#' @examples
#' #generate some data
#' data <- rnorm(10)
#'
#' #calculate geometric mean
#' geo.mean2(data)
#'
#' @export
geo.mean2 <- function(x){
  exp(mean(log(x)))
}

#'
#' Harmonic mean
#'
#' @usage har.mean(x)
#'
#' @param x vector of numerical values
#' @examples
#' #generate some data
#' data <- rnorm(10)
#'
#' #calculate harmonic mean
#' har.mean(data)
#'
#' @export
har.mean <- function(x){
  length(x)/sum(1/x)
}


mode <- function(x){
  as.numeric(names(sort(table(x),decreasing = T))[1])
}

#'
#' Standard error of the mean
#'
#' @usage se(x)
#'
#' @param x vector of numerical values
#' @examples
#' #generate some data
#' data <- rnorm(10)
#'
#' #calculate the standard error
#' se(data)
#'
#' @export
se <- function(x) sd(x,na.rm = T)/sqrt(length(x[!is.na(x)]))
