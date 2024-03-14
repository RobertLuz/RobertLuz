# Set the future value of the annuity (FV), set the periodic payment (PMT), 
# and choose the number of payments (periods) n.
###########################################################################

FV <- 160000
PMT <- 100
n <- 360

###########################################################################
# Define the function f, which is obtained from the Future value of an ordinary
# annuity formula:  FV = PMT*((1+i)^n-1)/i.  Multiply both sides by i, and move
# all terms to the left so that only zero is on the right.  The left side is the
# function f (with x in place of i).
############################################################################

f <- function(x){
  
  PMT*(1+x)^n -FV*x-PMT
  
}

############################################################################
# this is the derivative of f, which is needed for Newton's method.
############################################################################

f_prime <- function(x){
  
  PMT*n*(1+x)^{n-1}-FV
  
}
###########################################################################
# this defines the basic Newton Method approximation, as a function.
###########################################################################

newtons_method <- function(x){
  
  x - f(x)/f_prime(x)
  
}
###########################################################################
# This iterates the process, once an initial approximation is obtained (approx)
# run this function to get a better approximation by repeatedly running this.
# The initial approximation could be a guess or use the method below.
###########################################################################

approx_of_i_with_newtons_method <- function(){
  
    approx <<- newtons_method(approx)
  
    return(approx)
  
}
###########################################################################
# This gives the initial approximation by splitting up the interval from 0 to 1
# into hundredths (vector v below).  Then, select the index where the function 
# achieves it's minimum value and where f is positive. Select the approximation
# , that is, the x-value with these properties.
#############################################################################

v <- (1:100)/100

indices_where_f_is_positive <- (f(v)>0)

v <- v[indices_where_f_is_positive]

indices_where_f_achieves_min <- (f(v) == min(f(v)))

v <- v[indices_where_f_achieves_min]

approx <- v[1]

#############################################################################
# Run the function below until (or if) there is convergence.  This is the value
# of i if it does converge.
#############################################################################

approx_of_i_with_newtons_method()

