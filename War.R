#This program simulates the card game "War"
#
# Start with a Standard 52-card deck
############################################################################################
deck <- c("AH", "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "10H", "JH", "QH", "KH", 
          "AD", "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "10D", "JD", "QD", "KD", 
          "AS", "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "10S", "JS", "QS", "KS", 
          "AC", "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "10C", "JC", "QC", "KC")  
#
# shuffle the deck
############################################################################################
deck <- sample(deck) 
#
# initialize the variables
############################################################################################
round_count <- 1                                                                         
player_1 <- c()                                                                          
player_2 <- c()                                                                          
war_deck <-c()  
#
# deal out the cards
##############################################################################################
for (i in 1:52) {                                                                     
      if (i%%2 != 0){                                                                        
    player_1 <- append(player_1, deck[i], after=length(player_1))                        
  }                                                                                      
  else                                                                                   
    player_2 <- append(player_2, deck[i], after=length(player_2))                        
}                                                                                        
#
# This defines the value function used to make logical comparisons between the top card of each 
# players' deck
################################################################################################
v <-function(x){                                                                         
  if (x %in% c("AH", "AD", "AS", "AC"))                                                  
    {return(14)}                                                                           
  if (x %in% c("KH", "KD", "KS", "KC"))                                                  
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
#
# play the game
##############################################################################################

while (length(player_1) != 0 && length(player_2) != 0) {
  if (v(player_1[1]) != v(player_2[1])) {
    if (v(player_1[1]) > v(player_2[1])) {
            player_1 <- append(player_1, c(player_2[1], player_1[1]), after=length(player_1))
            player_1 <- append(player_1, war_deck, after=length(player_1))
            player_2 <- player_2[-1]
            player_1 <- player_1[-1]
            war_deck <- c()
            round_count<-round_count+1
      }
    else {
            player_2 <- append(player_2, c(player_1[1], player_2[1]), after=length(player_2))
            player_2 <- append(player_2, war_deck, after=length(player_2))
            player_2 <- player_2[-1]
            player_1 <- player_1[-1]
            war_deck <- c()
            round_count<-round_count+1
    } 
  }
    else {
       if (length(player_1) >= 5 && length(player_2) >= 5) {
           war_deck <- append(war_deck, player_1[1:4], after=length(war_deck))
           war_deck <- append(war_deck, player_2[1:4], after=length(war_deck))
           player_1 <- player_1[-1:-4]
           player_2 <- player_2[-1:-4]
        }
       else {
             break
            }
  }
}
# view the players to see who won! Note that if one of the player's doesn't end as a "character(0)"
# or empty vector, then it terminates at a war and you are viewing the last play. The player with 
# less than 5 cards loses.
player_1
player_2
round_count


    