
# This tests the average slope formula by definition and
# by the formula derived, you can see both values displayed
# by using the "compare" function below.
# 
#
#############################################################################################
avg_slope <- function(a,c,p,q,r,t){
     b <- (t-q)/(p-r)
     f <- function(v,y) {(r*y-p*v+t-q)/(y-v)}
     k <- function(u) {integrate(f, lower=a, upper=b, y=u)$value}
     z <- function(l) sapply(l, k)
     w <- 1/((c-b)*(b-a))*integrate(z, lower=b, upper=c)$value 
     return(w)
}
#########################################################################################################################################
avg_slope_formula <- function(a,c,p,q,r,t){
  b <- (t-q)/(p-r)
  m <- (p+r)/2 +(r-p)*(b-a)/(2*(c-b))*log(b-a) + (r-p)*(b-c)/(2*(b-a))*log(c-b)+(r-p)*(c-a)/((b-a)*(c-b))*(c+a-2*b)/2*log(c-a)
  return(m)
}
################################################################################################################################
compare <- function(a,c,p,q,r,t) {
  n <- avg_slope(a,c,p,q,r,t)
  m <- avg_slope_formula(a,c,p,q,r,t)
  return(c(avgSlope=n,avgSlopeFormula=m))
}