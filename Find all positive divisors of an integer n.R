##################################################################################################
# Find all positive divisors of a given integer n.  This creates a vector of all divisors of n in 
# order of magnitude. Check each i from 1 to n if it divides n or not. If it divides n, add it to
# the vector "divisors", otherwise leave it out.
##################################################################################################
n <- 8
divisors <- c()
d <-c()

for (i in 1:n){
  if (n %% i == 0){
    divisors <- append(divisors, i, after=length(divisors))
  }
}
divisors
##################################################################################################
# This next part checks if the number is prime or not, or otherwise.  Note that a number is prime if it has 
# exactly two positive divisors. This is what the function checks below.  There are also checks for 
# whether it is a product of two or three distinct primes and tells what they are.
##################################################################################################
if (length(divisors) == 2){
   "n is prime"
} else {
  "n is composite or 1"}
##################################################################################################
if (length(divisors) == 3){
  "n is the square of a prime"
} else {"n is not the square of a prime"}
##################################################################################################
if (length(divisors) == 4){
  primes <-c(divisors[2],divisors[3])
  if (divisors[3] %% divisors[2] == 0){ print("n is the cube of a prime") 
                                         divisors[2]}
  else {print("n is the product of two distinct primes, they are")
         primes}
} else {print("n is not the product of two distinct primes and not the cube of a prime")}
##################################################################################################
# this part needs some work, there are more cases to consider, but this is one of them
#################################################################################################
d <-c()
if (length(divisors) == 8){
  for (i in 1:8){
    if (length(which(divisors %% divisors[i] == 0)) == 4){
      d <- append(d, divisors[i], after=length(d))
    }
  }
  print("n is the product of three distinct primes, they are")
  d
} else {"n is not the product of three distinct primes"}
######################################################################################
# below is the simple comparison to classify positive integers as prime or composite
######################################################################################
if (length(divisors) >= 2){
  if (length(divisors) == 2){
    "n is prime"
  } else {"n is composite"}
} else {"n is 1"}