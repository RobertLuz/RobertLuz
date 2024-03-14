#Fibonacci numbers f_n
f <- c(1,1)

for (i in 3:20){
  f[i] <- f[i-1] + f[i-2]
}
f
