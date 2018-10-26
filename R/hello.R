# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world this is matt!")
}

#'No in
#'
#'This function does the opposite of in
#'
#' @author Matthew Zinkgraf, \email{mzinkgraf@gmail.com}
#' @export
`%ni%`=Negate(`%in%`)
