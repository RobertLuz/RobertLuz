# The Collatz conjecture: the sequence obtained by iterating T: n, T(n), T(T(n)), T(T(T(n))), ...
# always reaches the number 1 no matter what positive integer n starts the sequence.
#####################################################################################################

#set the starting positive integer n, and set the initial term of the sequence as n
#####################################################################################################
n <- 99999
s <- c(n)

# define the function T to iterate with, it depends on whether the input is either even or odd
#####################################################################################################
T <- function(n){
  if (n %% 2 == 0){
    n/2
  } else {(3*n+1)/2}
}

#repeat the code below to generate the sequence described above, change 100 to generate more or less terms, or repeat
#####################################################################################################
for (i in 1:200){
  s <- append(s, T(s[length(s)]), after=length(s))
}
s
#########################################################################################################
# this returns the location of the first 1 that occurs in the sequence at position= length(s)
# i range may need to be adjusted, depending on when the first one occurs.  It also returns s up to that
# point.  Reset s and n above each time the code below is run, otherwise it won't work.
n <- 999
s <- c(n)

for (i in 1:700){
  if (s[i] == 1) {break}
  else {s <- append(s, T(s[length(s)]), after=length(s))}
}
s
length(s)
