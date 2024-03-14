# Ulam numbers u_n
# First two Ulam numbers u_1=1 and u_2=2. Then u_n is
# defined recursively by integer u_m is an Ulam number
# if, and only if, it can be written uniquely as the sum of
# two distinct Ulam numbers.
##############################################################
U <- c(1,2)

#Generates Ulam numbers greater than 2,
#replace 100 in the while loop to generate as many as you like
##############################################################
while(length(U)<100){
  
su <- c()
sum <- c()

# this double for loop creates a vector "sum" (initialized above)
# which consists of all possible sums of elements from U. This is
# all possible candidates for the next Ulam number.
for (i in 1:length(U)) { 
  for (j in 1:length(U)){
    if (i < j) {
    sum <- append(sum, U[i]+U[j], after= length(sum))
    }
  }
}
# This cleans out the "sum" by getting rid of Ulam numbers already generated
sum <- sum[!sum %in% U]

# This is the part that gave me the most trouble.  The problem was in
# trying to modify the "sum" within the for loop which was defined in terms of
# the vector "sum"!  It kept throwing up errors.  I finally came up with the work-around
# by defining the vector "su" because I'm not that creative with names, and this serves as
# a sort-of buffer variable that stores the necessary information. It stores the values that can
# be written in more than one way with the present Ulam numbers already
# found.
for (n in 1:length(sum)) { 
  for (m in 1:length(sum)){
    if (m<n && sum[m] == sum[n]){
    su <- append(su,c(sum[m]),after=length(su))
    }
  }
}
# This cleans out the sum again as all the values in "sum" that are
# not in "su".
sum <- sum[!sum %in% su]
# The smallest number in "sum" is the next Ulam number. It is the unique sum of
# two distinct Ulam numbers by construction. It can be shown that if it were a smaller
# number, then, by definition it would be in "sum" and it would
# be smaller than the smallest number in that set!  A contradiction.
U <- append(U, min(sum), after=length(U))
}
U

