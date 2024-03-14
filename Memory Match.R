deck <- c("AH", "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "10H", "JH", "QH", "KH", # Standard 52-card deck
          "AD", "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "10D", "JD", "QD", "KD", #
          "AS", "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "10S", "JS", "QS", "KS", #
          "AC", "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "10C", "JC", "QC", "KC") # 
deck <- sample(deck)                                                                     # shuffle the deck                                           
round_count <- 0                                                                         # count of the round number
player_1 <- c()                                                                          # player 1's cards
player_2 <- c()                                                                          # player 2's cards
v <-function(x){                                                                         # the "v" function gives the value of each card
  if (x %in% c("AH", "AD", "AS", "AC"))                                                  #
  {return(14)}                                                                         #  
  if (x %in% c("KH", "KD", "KS", "KC"))                                                  #
  {return(13)}
  if (x %in% c("QH", "QD", "QS", "QC"))
  {return(12)}
  if (x %in% c("JH", "JD", "JS", "JC"))
  {return(11)}
  if (x %in% c("10H", "10D", "10S", "10C")) 
  {return(10)}
  if (x %in% c("9H", "9D", "9S", "9C"))
  {return(9)}
  if (x %in% c("8H", "8D", "8S", "8C")) 
  {return(8)}
  if (x %in% c("7H", "7D", "7S", "7C"))
  {return(7)}
  if (x %in% c("6H", "6D", "6S", "6C"))
  {return(6)}
  if (x %in% c("5H", "5D", "5S", "5C"))
  {return(5)}
  if (x %in% c("4H", "4D", "4S", "4C"))
  {return(4)}
  if (x %in% c("3H", "3D", "3S", "3C"))
  {return(3)}
  if (x %in% c("2H", "2D", "2S", "2C"))
  {return(2)}
}

while (length(deck) !=0) {
  select <- sample(deck, 2)
    if (v(select[1]) == v(select[2])) {
       if(round_count %% 2 == 0) {
          player_1 <- append(player_1, select, after=length(player_1))
          deck <- deck[!deck %in% select]
          round_count <-round_count+1
          }
       else {
          player_2 <- append(player_2, select, after=length(player_2))
          deck <- deck[!deck %in% select]
          round_count <-round_count+1
          }
    }
 else {round_count<-round_count+1}
}
player_1
player_2
round_count