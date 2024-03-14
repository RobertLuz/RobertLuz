#Simulation of generating a random permutation
##################################################
n <- 4
v <- c()
# Create the basic vector v=c(1,2,3,...,n)
#################################################
for (i in 1:n){
  v <- append(v, i, after=length(v))
}
#################################################
#the while loop with I <- n creates a permutation of v
#####################################################
df <- as.data.frame(v)
w <- sample(v)
for (i in 1:2400){
I <- n
  while (I>1){
     U <- runif(1,min=0,max=1)
     N <- floor(I*U)+1

     swapI <- v[I]
     swapN <- v[N]
     v[I] <- swapN
     v[N] <- swapI

     I <- I-1

  }
df <- cbind(df,as.data.frame(v))
}

write.csv(df, "Random Permutations")

x1 <- c()
for (i in 1:2400){
  x <- which(Random.Permutations[,i] ==1)
  x1 <- append(x1,x, after=length(x1)) 
}

x2 <- c()
for (i in 1:2400){
  x <- which(Random.Permutations[,i] ==2)
  x2 <- append(x2,x, after=length(x2)) 
}

x3 <- c()
for (i in 1:2400){
  x <- which(Random.Permutations[,i] ==3)
  x3 <- append(x3,x, after=length(x3)) 
}

x4 <- c()
for (i in 1:2400){
  x <- which(Random.Permutations[,i] ==4)
  x4 <- append(x4,x, after=length(x4)) 
}
x1[x1==1]
length(x1[x1==1])
length(x1[x1==2])
length(x1[x1==3])
length(x1[x1==4])

length(x2[x2==1])
length(x2[x2==2])
length(x2[x2==3])
length(x2[x2==4])

length(x3[x3==1])
length(x3[x3==2])
length(x3[x3==3])
length(x3[x3==4])

length(x4[x4==1])
length(x4[x4==2])
length(x4[x4==3])
length(x4[x4==4])

summary(x1)
summary(x2)
summary(x3)
summary(x4)

dfx <-data.frame(x1,x2,x3,x4)

lin.reg <- lm(x1 ~ x2 +x3 + x4, data=dfx)
lin.reg
summary(lin.reg)
lin.reg <- lm(x2 ~ x1 +x3 + x4, data=dfx)
lin.reg
summary(lin.reg)

dfx == 1
dfx[dfx == c(1,2,3,4)]
dfx[dfx==1]
dfx
y <- (dfx$x1==1) 
dfx[1,]
dfx
#this counts the number of each permutation of c(1,2,3,4)
#just adjust the 4 numbers after "==" below
j<- 0
for (i in 1:2441){
  if (Random.Permutations[1,i] == 4 &&
      Random.Permutations[2,i] == 3 &&
      Random.Permutations[3,i] == 2 &&
      Random.Permutations[4,i] == 1){
   j <- j+1
  }
}
j
