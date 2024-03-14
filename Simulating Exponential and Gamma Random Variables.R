########################################################
# Simulating an exponential random variable
########################################################
#  suppose that X is an exponential random variable, then
# its distribution is given by F(x) = 1-exp(-x). So
# F^(-1)(u) = -log(1-u), if u is uniform on (0,1)
#########################################################
#
# this generates 5 random real numbers between 0 and 1 and
# stores the values as vector u
##########################################################
u <- runif(5, min=0,max=1)
u
############################################################
# The function below defines a random variable with
# exponential distribution and mean=c
############################################################
c <- 5
inverseF <- function(x){
  -c*log(1-x)
}
########################################################
inverseF(u)
########################################################
# To simulate a gamma (n,lambda) random variable when n
#is an integer, use the fact that the sum of n independent
#exponential random variables each having rate lambda, has the
#gamma distribution.  Hence if U_1, ..., U_n are independent
#uniform random variables on (0,1), then X = -sum(1/lambda*log(U_i)) = -1/lambda*log(prod(U_i))
#the function below only uses 3 uniform random variables, but can be
#applied to any finite number of independent uniform random variables
##############################################################################
inverseG <- function(x,y,z){
  -c*log(x*y*z)
}
u_1 <- runif(1:2, min=0,max=1)
u_2 <- runif(1:2, min=0, max=1)
u_3 <- runif(1:2,min=0,max=1)
inverseG(u_1,u_2,u_3)
