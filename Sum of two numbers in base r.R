# Adding two numbers a and b represented in base r, and rewritting the sum a+b in 
# base r
################################################################################################
# this function gives the base r expansion of a positive integer m represented as a vector with highest
# power first followed by lower powers down to the constant term (power zero of r)
################################################################################################
base_r_expansion_of_m <- function(m, r){
breom <- c()
while (m != 0) {
  a_0 <- m %% r
  q_0 <- (m-a_0)/r
  breom <- append(breom, a_0, after=0)
  m <- q_0
}
return(breom)
}
###############################################################################################
# set your chosen integers a and b, and chose a base r.  Define c as the sum of a and b
###############################################################################################
a <- 13835394757347453745098099089
b <- 949058830489684560945609049084
r <- 2
c <- a+b
##############################################################################################
# convert a,b and c from base 10 to base r and print the results of a, b and c
##############################################################################################

a <- base_r_expansion_of_m(a,r)
b <- base_r_expansion_of_m(b,r)
c <- base_r_expansion_of_m(c,r)

a
b
c