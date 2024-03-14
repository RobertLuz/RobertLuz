###################################################################################
# R Codes 1.2 (Eqv.R)
# Equation of Value (1.36),
# cash flows x start at 0 and end at n
#
# PV(x,i) = present value of x at effective rate i
# FV(x,i) = future value of x at effective rate i
# LP(pv,n,i) = n level payments at time 0, 1, ..., (n-1) with
#              present value equating pv at effective rate i
# EI(pv,x) = effective rate for present value of x
#            to equate pv
##################################################################################

PV <- function(x,i){
  n = (1:length(x));
  sum(x*((1+i)^(-(n-1))))
}

FV <- function(x,i){
  n = length(x) - (1:length(x));
  sum(x*((1+i)^(n)))
}

#################################################################################
#Example given the stream of cash flows c(950,800,150,400,120) find the present
#value and the future value (after 5 years) of the cash flows evaluated at i=2%
#################################################################################
x <- c(950,800,150,400,120)
PV(x, 0.02)
(1.02)*FV(x, 0.02)

##################################################################################

LP <- function(pv,n,i){
  a = (1-(1+i)^(-n))/i*(1+i);
  pv/a
}
#################################################################################
# A savings fund requires the investor to pay an equal amount of installment each
# year for 3 years, with the first installment to be paid immediately.  At the 
# end of the 3 years, a lump sum will be paid back to the investor.  If the effective
# interest rate is 5%, what is the amount of the installment so that the investor
# can get back $10000?
################################################################################

LP(10000/1.05^3,3,0.05)

################################################################################

EI <- function(pv,x){
  uniroot(function(i) PV(x,i)-pv, lower=0, upper=1)$root
}

################################################################################
# Example A student takes out a tuition loan of $15000, and is required to pay 
# back with a step-up payment $7000 in year 1 and $8500 in year 2.  What is the
# effective rate of interest she is charged?
###############################################################################

EI(15000, c(0,7000,8500))
