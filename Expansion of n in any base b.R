########################################################################
# This procedure gives the expansion of any positive integer
# n and  in any base b, where b is a positive integer b>1.
########################################################################
# first pick positive integer n, and base b>1 to expand by.
# The vector beon (base b expansion of n) stores the coefficients
# of the expansion with highest power coefficient first, down to the constant
# term.
########################################################################
n <- 999
b <- 3
beon <- c()
########################################################################
# the process divides repeatedly starting with n, and at each stage the 
# remainder is recorded and added to the vector beon. The remainders
# are the coefficients in the base b expansion of n. So initially,
# divide n by b to get quotient q_0 and remainder a_0. Attach a_0 to
# the beginning of beon, then reset n to the quotient q_0 and repeat
# the process. The process ends when q_0 =0. The sequence necessarily
# reaches zero since this generates a sequence of decreasing
# nonnegative integers, and the process continues as long as the terms
# are positive.  There are a finite number of terms, so the process must
# terminate eventually.
########################################################################
i <- 0
powers_of_b <- c()
while (n != 0) {
  a_0 <- n %% b
  q_0 <- (n-a_0)/b
  beon <- append(beon, a_0, after=0)
  powers_of_b <- append(powers_of_b, i, after=0)
  n <- q_0
  i <- i+1
}
####################################################################################
# this presents the results nicely in a dataframe: first column is the base b expansion,
# the second column reiterates what the base is repeatedly, and the last column is
# the exponent on b. So for each row, if the entries are x, y and z, in the 
# expansion it is x*y^z (here y=b)
#####################################################################################
results <- data.frame(coeff_of_base_b_exp=beon,base_b=rep(b,length(powers_of_b)), power_of_b = powers_of_b)
results