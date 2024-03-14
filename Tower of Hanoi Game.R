########################################################################################
# Tower of Hanoi Puzzle
########################################################################################
# this creates the board as a martix h with 3 cols (pegs) and 8 rows, and the
# rings are numbers 1 to 8 stacked in the first column to start
#######################################################################################

v <- c(1,2,3,4,5,6,7,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
h <- matrix(data=v,nrow=8,ncol=3)

########################################################################################
# this function is the heart of the game here which was the most difficult to
# come up with and debug - now at least it works.  Each move is from one
# column to another (from vector x to vector y).  In order to make a valid move, it has to be from a col
#(x) with at least one non-zero entry to another column (y) with at least one zero entry.  This is 
# the first check.  The second check, is that the numbers (rings) must be stacked
# according to size - you can't place a larger ring on top of a smaller one. 
#########################################################################################
swap_x_to_y <- function(x,y){
  z1 <- which(x != 0)
  w1 <- min(z1)
  k1 <- x[w1]
  z2 <- which(y==0)
  w2 <- max(z2)
  k2 <- y[w2]
    if (w1 != Inf && w2 != -Inf){
      if (k1 < y[w2+1] || w2 == length(y)){
          x[w1] <- k2
          y[w2] <- k1
          return(c(x,y))
      } else{print("Invalid move")
             return(c(x,y))}
    } else{print("Invalid move")
           return(c(x,y))}
}
##################################################################################################################
# the output of the swap_x_to_y function puts both changed vectors into a single vector c(x,y) - there may
# be a better way to do this, but I don't know it.  So, in order to split the two again, I've created the two functions
# below.  first_half(swap_x_to_y(x,y)) returns the changed x, or it is left as is before the swap function. Similarly,
# second_half(swap_x_to_y(x,y)) returns the changed y, or unchanged y if it fails the two tests above in swap_x_to_y.
##################################################################################################################
first_half <- function(x){
  x <- x[1:8]
  return(x)
}

second_half <- function(x){
  x <- x[9:16]
  return(x)
}
##########################################################################################################
# these 6 functions define the 6 possible moves in the game.
##########################################################################################################
round_change1 <- function(h){
  c <- swap_x_to_y(h[,1],h[,2])
  a <- first_half(c)
  b<- second_half(c)
  h[,1]<-a
  h[,2]<-b
  return(h)
}

round_change2 <- function(h){
  a <- first_half(swap_x_to_y(h[,1],h[,3]))
  b<- second_half(swap_x_to_y(h[,1],h[,3]))
  h[,1]<-a
  h[,3]<-b
  return(h)
}
round_change3 <- function(h){
  a <- first_half(swap_x_to_y(h[,2],h[,1]))
  b<- second_half(swap_x_to_y(h[,2],h[,1]))
  h[,2]<-a
  h[,1]<-b
  return(h)
}
round_change4 <- function(h){
  a <- first_half(swap_x_to_y(h[,2],h[,3]))
  b<- second_half(swap_x_to_y(h[,2],h[,3]))
  h[,2]<-a
  h[,3]<-b
  return(h)
}
round_change5 <- function(h){
  a <- first_half(swap_x_to_y(h[,3],h[,1]))
  b<- second_half(swap_x_to_y(h[,3],h[,1]))
  h[,3]<-a
  h[,1]<-b
  return(h)
}
round_change6 <- function(h){
  a <- first_half(swap_x_to_y(h[,3],h[,2]))
  b<- second_half(swap_x_to_y(h[,3],h[,2]))
  h[,3]<-a
  h[,2]<-b
  return(h)
}
#################################################################################################################
# This function prompts the player to enter one of the six moves.  If you enter something other than 1,2,3,4,5,or 6
# you are prompted again.  Depending on the input, the corresponding function is called, and the board is changed.
# I had trouble understanding that the variables inside a function are not global variables - you have to make them
# such by using "<<-" to make it so.  This then updates the board h according to the move that was made and prints it
# out.
#######################################################################################################################
your_move <- function(){
  x <- readline("What is your move? (1-6):")
  if (x %in% c(1,2,3,4,5,6)){
    if (x==1){h<<-round_change1(h) 
    return(h)}
    if (x==2){h<<-round_change2(h) 
    return(h)}
    if (x==3){h<<-round_change3(h) 
    return(h)}
    if (x==4){h<<-round_change4(h) 
    return(h)}
    if (x==5){h<<-round_change5(h) 
    return(h)}
    if (x==6){h<<-round_change6(h) 
    return(h)}
  } else {your_move()}
}
#############################################################################################################
# This provides a loop to play the game in continuously, and defines the criteria for winning. In order to win,
# one must get all the rings (numbers 1 to 8) stacked in the second column in order 1,2,3,4,5,6,7,8.  The third
# column is the auxiliary peg, just for moving the rings.  Note that one wins only if the top value in the second
# column is a 1.  This gives a condition to loop on.
##############################################################################################################
while(h[1,2] != 1){
 your_move()
 print(h)
}
