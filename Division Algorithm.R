# Find the quotient and remainder for the division algorithm
##############################################################
# given integers n and d (n is the dividend, d>0 is the divisor),
# there are unique integers q and r such that n=q*d+r, where 0<=r<d
##############################################################
n <- 289
d <- 29
# compute the remainder and quotient: just use modular division to compute r,
# then use this result to compute q since n=q*d+r
#############################################################
r <- n %% d
q <- (n-r)/d
#############################################################
#output the result as a vector
c(dividend=n, divisor=d, quotient=q, remainder=r)