# given an integers n and m, determine if n is divisible by m.
###################################################################################
#
# Function to prompt user to enter an integer
###################################################################################
pick_integer <- function(){
  n <- readline("Pick an integer:")
  n <- as.integer(n)
  if (is.na(n))
    {pick_integer()} 
  else{n}
}

pick_another_integer <- function(){
  m <- readline("Pick another integer:")
  m <- as.integer(m)
  if (is.na(m))
    {pick_integer()}
  else {m} 
}

test_div <- function(n,m){
  if (is.nan(n %% m)) {
      return("Can't divide by zero")
  }
  else if ( as.integer(n) %% as.integer(m) == 0){
    
         return(list(n,"is divisible by",m))
    } else {
       return(list(n,"is NOT divisible by",m))
    }  

  } 
   


test_div(n=pick_integer(),m=pick_another_integer())

test_div(n=3,m=6)
