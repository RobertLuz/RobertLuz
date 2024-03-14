################################################################
# R Codes 1.1 (Effr.R)
# Effective rate of interest
# 
# irm(r,m) = effective rate of interest for nominal interest r
#            compounded m times per year
# idm(d,m) =  effective rate of interest for nominal discount d
#             compounded m times per year
################################################################

irm <- function(r,m){
  (1+r/m)^m -1
}

idm <- function(d,m){
  (1-d/m)^(-m)-1
}

# Examples
irm(0.115,12)
idm(0.06,4)
