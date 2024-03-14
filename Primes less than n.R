divisors_of_n <- function(n){
  divisors <- c()
  for (i in 1:n){
    if (n %% i == 0){
      divisors <- append(divisors, i, after=length(divisors))
    }
  }
  return(divisors)
}

primes_less_than_n <- function(n) {
  primesltn <- c()
  for (i in 1:n) {
    if (length(divisors_of_n(i)) == 2){
      primesltn <- append(primesltn, i, after=length(primesltn))
    } else {next}
  }
  return(primesltn)
}



primesLtN <- function(n){
  S <- c()
  pn <- primes_less_than_n(sqrt(n))
  S_0 <- length(pn)-1+n
  S <- append(S, S_0, after=length(S))
  P_1 <- combn(pn,1)
  S_1 <- sum(floor(rep(n,length(P_1[1,]))/P_1))
  S <- append(S, S_1, after=length(S))
  a <-c()
  for (i in 0: length(pn)){
    a <- append(a, (-1)^i, after=length(a))
  }
  for (j in 2: length(pn)) {
    P_j <- combn(pn,j)
    p_j <- c()
      for (i in 1: length(P_j[1,])){
        p_j <- append(p_j, prod(P_j[,i]), after=length(p_j))
        }
    S_j <- sum(floor(rep(n, length(p_j))/p_j))
    S <- append(S, S_j, after=length(S))
 }
return(sum(a*S))
}


  
