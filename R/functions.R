#Project 1


#' Dice
#'
#' @param dietype The type of die
#' @param probvalues The probability of getting a particular number.
#'
#' @return the sum of two dice
#' @export
#'
#' @examples
#' set.seed(123)
#' roll()
#' roll(1:20)
#'
roll <- function(dietype = 1:6, probvalues = rep(1/length(dietype), length(dietype))){
  if(sum(probvalues) !=1)
    stop("'probvalues' must add to one")
dice <- sample(dietype, size = 2, replace = TRUE, prob = probvalues)
sum(dice)
}



#' Squaring Numbers
#'
#' @param x Must be a numeric object.
#'
#' @return Squares the value in the numeric object.
#' @export
#'
#' @examples
#' sq(c(1,3,5))
#' sq(1:10)
sq <- function(x){
  x^2
  if(typeof(x) !=double)
    stop("object must be numeric")
}

#' Power Function
#'
#' @param x Must be a numeric object
#' @param power A numeric object called power \code{power}
#'
#' @return Raises the values in the numeric object \code{x} to the value in \code{power}
#' @export
#'
#' @examples
#' RP(c(1, 3, 5), 3)
#' RP(x = c(1, 3, 5), power = 3)
#'
RP <- function(x, power = 1){
  x^power
}

#' Everyday I'm shuffling
#'
#' @param cards
#'
#' @return shuffles the deck to prevent same outcome each time
#' @export
#'
#' @examples
#'site <- "https://gist.githubusercontent.com/garrettgman/9629323/raw/ee5dfc039fd581cb467cc69c226ea2524913c3d8/deck.csv"
#'deck <- readr::read_csv(site)
#'write.csv(deck, file = "cards.csv", row.names = FALSE)
#'deck[1, c("face", "suit", "value")]
#'deal <- function(deck) {
#'deck[1,1]
#'}
shuffle <- function(deck) {
  random <- sample(1:52, size = 52)
  deck[random, ]
}
#
