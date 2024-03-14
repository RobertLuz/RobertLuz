################################################################################
# The product of any n consecutive integers is always divisible by n.
# This is a program that demonstrates this fact by picking a valid n (positive integer), and an integer
# a to start from. Then, form the product p=a*(a+1)*(a+2)*...*(a+n-1) and show that this number
# divided by n leaves a remainder of zero.
######################################################################################################
n <- 6
a <- 5
######################################################################################################
# the product is defined recursively with this for loop
######################################################################################################
p <- a
for (i in 1:(n-1)){
  p <- p*(a+i)
}
######################################################################################################
r <- p %% n
p
r
######################################################################################################
# r is always zero because it is always the case that one of the n consecutive integer factors is 
# divisible by n