# Set up your simplex tableau here.  Set vector v below to all the entries in the matrix starting at (1,1) and move down
# the columns as you enter all values. For example, the first three entries below: 1,3,-50 is the first column of the
# matrix m, next 3 entries are the second column and so on. You may also have to adjust "nrow" = number of rows and "ncol" = number of columns below depending on the size of the tableau.
###################################################################################################################

v <- c(3,2,1,-4,2,1,1,-3,5,1,2,-2,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,23,8,7,0)
m <- matrix(data=v, nrow=4,ncol=8)
m

v <- c(0.6,0.03,0.3,-20,1.2,0.04,0.2,-30,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,960,36,270,0)
m <- matrix(data=v, nrow=4,ncol=7)
m

v <- c(2,1,3,-1,2,3,2,-2,8,2,1,-3,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,600,600,400,0)
m <- matrix(data=v, nrow=4,ncol=8)
m

v <- c(-2,0,-2,1,1,-3,1,0,0,0,1,0,0,0,1,4,10,0)
m <- matrix(data=v, nrow=3,ncol=6)
m

v <- c(2,1,1,-30,1,1,2,-40,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,10,7,12,0)
m <- matrix(data=v, nrow=4,ncol=7)
m

v <- c(-2,-1,0,-2,1,1,1,-3,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,2,5,6,0)
m <- matrix(data=v, nrow=4,ncol=7)
m
##################################################################################################################
# Once you have the tableau set up just right. Run the code below repeatedly until you get "optimal solution has been found" or
# "no optimal solution exists". print out matrix m to see the solution if there is one.
####################################################################################################################

     
x<- min(m[dim(m)[1],1:(dim(m)[2]-1)])

if(x < 0) {
    ind <- (m == x & x %in% m[dim(m)[1],])
    j <- col(m)[ind]
    j <- min(j)
    v <- m[1:(dim(m)[1]-1),j]
    names(v) <- 1:(dim(m)[1]-1)
    ind2 <- (v>0)
       if (isFALSE(ind2) || !TRUE %in% ind2) 
            {print("There is no optimal solution")} 
       else {
         v <- v[ind2]
         y <- m[1:(dim(m)[1]-1), dim(m)[2]]
         names(y)<- 1:(dim(m)[1]-1)
         y <- y[ind2]
         z <- y/v
         pivot_row_index <- names(z)[z == min(z)]
         pivot_row_index <- as.integer(pivot_row_index[1])
         ind3 <- (z == min(z) ) 
         i <- names(y)[ind3]
         i <- as.integer(i[1])
         w <- m[i,j]
         m[i,] <- m[i,]/rep(w, dim(m)[2])
           for (k in 1:(dim(m)[1])) {
             if ( k != i){
                if ( m[k,j] == 0) 
                  {next}
                else 
                  { m[k,] <- -m[k,j]*m[i,] + m[k,]}
             } else 
                 {next}
           }
      }
} else {print("optimal solution has been found")}


