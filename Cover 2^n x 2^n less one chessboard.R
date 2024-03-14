#Cover a 2^n X 2^n chessboard that is missing one square using
#L-shaped pieces
##################################################################
# set the size of the board by defining positive integer n
n <- 5

# this defines the 2^n X 2^n board (matrix m) with one random piece missing (set to zero)
m <- matrix(data=rep(1,2^(2*n)), nrow=2^n,ncol=2^n)
z_i <- sample(1:2^n,1)
z_j <- sample(1:2^n,1)
m[z_i,z_j] <- 0
###########################################################################################################################################################################
# It suffices to show that this can be done by induction.  The base case is a 2 x 2 board with one piece missing, which can be covered by an
# L-shaped piece trivially. Now for the inductive step, assume that any 2^n X 2^n board with one piece missing can be covered by L-shaped pieces.  To show that this can be done
# for a 2^(n+1) x 2^(n+1) board with one piece missing, split the board into fourths (quadrants), only one of the quadrants will contain the zero piece.  The quadrant that contains
# the zero piece is a 2^n x2^n board with one piece missing which can be covered by L-shaped pieces by assumption - so that quarter of the board is taken care of. For the remaining 3
# quadrants, place an L-shaped piece at the common corner of the 3 quadrants. This covers exactly one piece from each of the 3 remaining quadrants, and thus makes each of the remaining
# quadrants a 2^n x 2^n board with one piece missing, and again by the inductive hypothesis, each one can be covered by L-shaped pieces.  Therefore, the whole board can be 
# covered by L-shaped pieces.  
################################################################################################################################################################
#This program essentially "zooms in" on the part of the board that contains the zero piece, the 2 X2 board, this can be covered trivially, then that fits into 
# a 4 x4 board within the larger board, which can be covered (by the inductive step above) by L-shaped pieces, and so on until the entire board is covered.
#  Run the entire script first, then just the code below to repeat the process.
if (z_i <= 2^(n-1)) {
  if (z_j <= 2^(n-1)) {
     m <- m[1:2^(n-1), 1:2^(n-1)]
     n <- n-1
     ind1 <- (m == 0)
     z_j <- col(m)[ind1]
     z_i <- row(m)[ind1]
  } else {  m <- m[1:2^(n-1), (2^(n-1)+1):2^n]
            n <- n-1
            ind1 <- (m == 0)
            z_j <- col(m)[ind1]
            z_i <- row(m)[ind1]
         } 
} else { if (z_j <= 2^(n-1)){
             m <- m[(2^(n-1)+1):2^n, 1:2^(n-1)]
             n <- n-1
             ind1 <- (m == 0)
             z_j <- col(m)[ind1]
             z_i <- row(m)[ind1]
       } else {  m <- m[(2^(n-1)+1):2^n, (2^(n-1)+1):2^n]
                n <- n-1
                ind1 <- (m == 0)
                z_j <- col(m)[ind1]
                z_i <- row(m)[ind1]
              }
       }
m
