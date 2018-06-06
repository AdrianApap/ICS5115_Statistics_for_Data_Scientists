#Clear workspace
rm(list = ls())

#Install required packages

#install.packages('expm')
#install.packages('ggplot2')

#load packages
library(expm)
library(ggplot2)

#Monopoly tile labels
monopoly_tile_labels <- c("Go", "Mediterranean Avenue", "Community Chest 1", "Baltic Avenue",
                          "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance 1",
                          "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
                          "Electric Company", "States Avenue", "Virginia Avenue", "Pennsylvania Railroad",
                          "St. James Place", "Community Chest 2", "Tennessee Avenue", "New York Avenue",
                          "Free Parking", "Kentucky Avenue", "Chance 2", "Indiana Avenue",
                          "Illinois Avenue", "B. & O. Railroad", "Atlantic Avenue", "Ventnor Avenue",
                          "Water Works", "Marvin Gardens", "Go To Jail", "Pacific Avenue",
                          "North Carolina Avenue", "Community Chest 3", "Pennsylvania Avenue", "Short Line",
                          "Chance 3", "Park Place", "Super Tax", "Boardwalk")

monopoly_tile_color <- c('A', 'B', 'A', 'B', 'A', 'A', 'C', 'A', 'C', 'C', 'A', 'D', 'A', 'D', 'D', 'A',
                         'E', 'A', 'E', 'E', 'A', 'F', 'A', 'F', 'F', 'A', 'G', 'G', 'A', 'G', 'A', 'H',
                         'H', 'A', 'H', 'A', 'A', 'I', 'A', 'I')

color_values <- c('A'='gray49', 'B'='sienna', 'C'='steelblue', 'D'='purple', 'E'='orange',
                  'F'='red', 'G'='yellow', 'H'='green', 'I'='blue')

#Helper function to update transition matrix
update_matrix <- function(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob) {
  tmp_matrix[orig_tile+(orig_state*40),new_tile+(new_state*40)] <- 
    tmp_matrix[orig_tile+(orig_state*40),new_tile+(new_state*40)] + prob
  
  return(tmp_matrix)
}

#Function to check destination tile and update matrix accordingly
check_move <- function(tmp_matrix, orig_tile, orig_state, die1, die2, prob) {
  
  #Check if doubles
  if(die1 == die2) {
    #if in jail and got doubles then exist jail and increment state
    if(orig_tile == 11) {
      new_state <- 1
    }else{
      #Increment doubles state
      new_state <- orig_state + 1
    }
  }else{
    #Reset doubles state
    new_state <- 0
  }
  
  #Get new tile
  new_tile <- (orig_tile + die1 + die2) %% 40
  
  if(new_tile == 0) {
    new_tile <- 40
  }
  
  #Go to jail if new tile is go to jail tile
  if(new_tile == 31) {
    tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 11, 0, prob)
  #Community Chest Tile
  }else if(new_tile == 3 || new_tile == 18 || new_tile == 34) {
    tmp_matrix <- community_chest(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob) 
  #Chance Tile
  }else if(new_tile == 8 || new_tile == 23 || new_tile == 37) {
    tmp_matrix <- chance(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob)
  #Normal tile
  }else{
    tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob)
  }
  
  return(tmp_matrix)
}

#Handle community chest tile
community_chest <- function(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob) {
  #16 community chest cards
  
  #Advance to Go card
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 1, new_state, prob/16)   
  
  #Go to jail card
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 11, 0, prob/16)
  
  
  #Stay (you draw a card that gives/takes money)
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob*(14/16)) 
  
  return(tmp_matrix)
}

#Handle Chance tile
chance <- function(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob) {
  #16 chance cards
  
  #Stay (you draw a card that gives/takes money)
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, new_tile, new_state, prob*(6/16))
  
  #Advance to Go Card
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 1, new_state, prob/16)
  
  #Take a trip to Reading Railroad
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 6, new_state, prob/16)
  
  #Go to jail card
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 11, 0, prob/16)
  
  #Advance to Advance to St. Charles Place
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 12, new_state, prob/16)
  
  #Advance to Illinois Ave
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 25, new_state, prob/16)
  
  #Advance to Boardwalk
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, 40, new_state, prob/16)
  
  #Advance to the nearest utility
  if(new_tile == 23) {
    utility_tile <- 29
  }else{
    utility_tile <- 13
  }
  
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, utility_tile, new_state, prob/16)
  
  #Advance to the nearest railroad (2 cards)
  if(new_tile == 8) {
    railroad_tile <- 16
  }else if(new_tile == 23){
    railroad_tile <- 26
  }else{
    railroad_tile <- 6
  }
  
  tmp_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, railroad_tile, new_state, prob/8)
  
  #Go back 3 Spaces
  spaces_tile <- (new_tile-3) %% 40
  
  if(spaces_tile == 0) {
    spaces_tile <- 40
  }
  
  # #Handle landing on community chest
  # if(spaces_tile == 34) {
  #   tmp_matrix <- community_chest(tmp_matrix, orig_tile, orig_state, spaces_tile, new_state, prob/16)
  # } else {
  tran_matrix <- update_matrix(tmp_matrix, orig_tile, orig_state, spaces_tile, new_state, prob/16)
  # }
  
  return(tran_matrix)
}

#Main loop
tran_matrix <- matrix(0, 120, 120)
#Exit jail immediately dont wait 3 turns 
pay_fine <- TRUE 

#For each tile
for(tile in 1:40) {
  #For each state
  for(state in 0:2) {
    #For each possible die outcome
    for(die1 in 1:6) {
      for(die2 in 1:6) {
        #Get out of jail if in jail
        if(tile == 11) {
          #Pay the $50 fine or get a double or third turn
          if(pay_fine || state == 2 || die1 == die2){
            tran_matrix <- check_move(tran_matrix, tile, state, die1, die2, 1/36)
          #Stay in jail and wait 3 turns  
          } else {
            tran_matrix <- update_matrix(tran_matrix, tile, state, tile, state+1, 1/36)
          }
        }else{
          #Go to jail if 3 doubles in a row (speeding rule)
          if((die1 == die2) && (state == 2)) {
            tran_matrix <- update_matrix(tran_matrix, tile, state, 11, 0, 1/36)
          #Normal Move
          } else {
            tran_matrix <- check_move(tran_matrix, tile, state, die1, die2, 1/36)
          }
        }
      }
    }
  }
}


# Check that each row in transition matrix sums to 1
# for(i in 1:length(tran_matrix[,1])) {
#   if(!isTRUE(all.equal(sum(tran_matrix[i,]), 1))){
#     print(paste('ERROR',i,sum(tran_matrix[i,])))
#   }
# }


#Compute the eigen vector of the transition matrix
ev <- eigen(t(tran_matrix))

#Get the first eigen vector (largest eigenvalue, eigen sorts vectors by eigenvalue)
vectors <- ev$vectors
#Get real commponent of eigen vector
tmp_vec <- Re(vectors[,1])
#Normalise eigen vector to remove any negative values
tmp_vec <- tmp_vec/sum(tmp_vec)

#Merge states into final steady state vector
final_vec <- tmp_vec[1:40] + tmp_vec[41:80] + tmp_vec[81:120]

#Get Tile with max probability
print(paste('Max Tile:',which(final_vec == max(final_vec)),', Probability: ',max(final_vec),'\n'))

#Create a datafram of labels and probabilities
df <- data.frame(label=monopoly_tile_labels, probability=final_vec,color=monopoly_tile_color)
#Sort dataframe in descending order
sorted_df <- df[order(-df$probability),] 

#plot probability of each tile
probability_plot <- ggplot(df,aes(x=reorder(df$label,1:40),y=probability*100,fill=color)) + 
  geom_bar(stat = "identity", show.legend=F) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Tile Label', y='% Long-Term Probability') +
  ggtitle('Plot of long-term probabilities of each tile (120 States, stay in jail)') +
  theme(plot.title=element_text(hjust = 0.5)) +
  scale_fill_manual(values = alpha(color_values,0.7))
plot(probability_plot)