# the pi function of a positive integer n returns the number of primes not exceeding n.
# this code defines this function.
########################################################################################
# this function generates the list of positive divisors of n
##############################################################
divisors_of_n <- function(n){
  divisors <- c()
  for (i in 1:n){
    if (n %% i == 0){
      divisors <- append(divisors, i, after=length(divisors))
    }
  }
  return(divisors)
}
###############################################################
# this function calculates the number of primes not exceeding n
###############################################################
pi <- function(n) {
  j <- 0
  for (i in 1:n) {
    if (length(divisors_of_n(i)) == 2){
      j <- j + 1
    } else {next}
  }
  return(j)
}
