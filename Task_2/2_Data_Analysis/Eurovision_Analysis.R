#Clear workspace
rm(list = ls())

#install.packages('stringi',type='win.binary')
#install.packages('ggplot2')
#install.packages('tidyr')
#install.packages('gtools')
#install.packages('rworldmap')
#install.packages('mapproj')

library(ggplot2) 
library(tidyr)
library(gtools)
library(grid)
library(rworldmap)

#Load results from CSV
data <- read.csv("eurovision-df.csv", header=TRUE, stringsAsFactors=FALSE)

#Remove index column
data <- data[colnames(data)[colnames(data) != 'X']]

#Replace the entry final by grand-final
data[data$Stage == 'final',]$Stage <- 'grand-final'
#Replace the entry semi-final by first-semi-final
data[data$Stage == 'semi-final',]$Stage <- 'first-semi-final'

#Lowercase columns
data$From <- tolower(data$From)
data$To <- tolower(data$To)
data$Event <- tolower(data$Event)
data$Stage <- tolower(data$Stage)

#Remove special symbols from country names
data$From <- gsub('[&.-]', ' ', data$From)
data$To <- gsub('[&.-]', ' ', data$To)

#Remove multiple spaces
data$From <- gsub('\\s\\s+', ' ', data$From)
data$To <- gsub('\\s\\s+', ' ', data$To)

#Extract the year from the event column by splitting on last dash
data <- data %>% separate(Event, c('Event','Year'), sep='-(?!.*-)', remove=TRUE)
data$Year <- as.numeric(data$Year)

#Keep only data past from 2004 onwards
data <- data[data$Year >= 2004,]

#Split the votes into jury and televoting
votes.jury <- data[data$Type == 'Jury',]
votes.jury <- votes.jury[colnames(votes.jury)[colnames(votes.jury) != 'Type']]

votes.tele <- data[data$Type == 'Televoting',]
votes.tele <- votes.tele[colnames(votes.tele)[colnames(votes.tele) != 'Type']]

#Start of visualisations
#Create a heatmap of the number of times each country participaed in the final
#Sample code obtained from http://egallic.fr/en/european-map-using-r/

country_part = data.frame(country=character(),stringsAsFactors = FALSE)
grand_final.votes <- votes.tele[votes.tele$Stage=='grand-final',]

#Get countries that participated in grand final
grand_final.countries <- unique(grand_final.votes$To)

for(i in seq(2004,2017)){
  tmp_votes <- grand_final.votes[grand_final.votes$Year == i,]
  countries <- data.frame(country=unique(tmp_votes$To))
  country_part <- rbind(country_part,countries)
}

#Convert to dataframe of frequencies
country_part <- as.data.frame(table(country_part))

#Get world map
worldMap <- getMap()
#Keep only countries that took part in eurovision
euro_countries <- which(tolower(worldMap$NAME) %in% grand_final.countries)
#Get coord of countries
euro_coords <- lapply(euro_countries, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
euro_coords <- do.call("rbind", euro_coords)
euro_coords$value <- country_part$Freq[match(tolower(euro_coords$region),country_part$country_part)]

# Plot the map
mapPlot <- ggplot() + geom_polygon(data=euro_coords, aes(x=long, y=lat, group=region, fill=value),
                             colour = "black", size = 0.7) +
  coord_map(xlim = c(-30, 100),  ylim = c(10, 100))

mapPlot <- mapPlot + scale_fill_gradient(name="No. of times\ncountry participated\nin grand-final", low = "#99D1FFFF", high = "#005FFFFF", na.value = "grey50")

mapPlot <- mapPlot + theme(axis.text.x = element_blank(),
                           axis.text.y = element_blank(), axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(), axis.title = element_blank(),
                           plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
                      ggtitle("Heatmap showing the number of times each country took\npart in the grand-final stage of the Eurovision")
plot(mapPlot)

#Plot of total votes from 2004
tmpplot <- ggplot(grand_final.votes, aes(x=To, y=Points)) +
  geom_col(fill = "steelblue") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) + 
  labs(x='Country',y='Total Points (in thousands)') +
  ggtitle('Plot of total points received for each country in the grand-final.') +
  scale_y_continuous(labels=function(x)x/1000)  +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2))
plot(tmpplot)

#Start of Tests
point_vector.2014 = c(1,2,3,4,5,6,7,8,10,12)

#Helper function to 
simulate <- function(tmp_data, start_year, end_year, iterations, threshold) {
  #Load border table
  border_table <- read.csv("border-data.csv", header=TRUE, stringsAsFactors=FALSE)
  
  result <- list()
  
  #Get a list of all the countries that ever participated/voted in the contest
  all_countries <- unique(c(tmp_data$To, tmp_data$From))
  #Iterate over every country
  for(country in all_countries){
    #Get Country neighbours
    neighbours <- border_table[border_table$country == country,]$neighbours
    #split neighbour list using delimiter
    neighbours <- unlist(strsplit(neighbours,';'))
    #Keep only the neighbours that took part in the eurovision
    neighbours <- intersect(all_countries,neighbours)
    
    #if country has neighbours
    if(length(neighbours) > 0) {
      print(paste('Checking',country))
      #Iterate over every neighbour
      for(neighbour in neighbours){
        
        #Check for collusion from neighbour to country
        nc <- simulate_vote(tmp_data, c(neighbour,country), iterations, start_year, end_year, threshold)
        #Check for collusion from country to neighbour
        cn <- simulate_vote(tmp_data, c(country,neighbour), iterations, start_year, end_year, threshold)
        
        #If both nc and cn are true then there is collusion
        if(nc && cn) {
          if(is.null(result[[country]])){
            result[country] <- (neighbour)
          }else{
            result[[country]] <- c(result[[country]],neighbour)
          }
        }
      }
    }
  }
  return(result)
}

#Function to simulate a vote between two countries
simulate_vote <- function(tmp_data, vote_pair, iterations, start_year, end_year, threshold) {
  #Get the grand final votes only
  original_data <- tmp_data[tmp_data$Stage=='grand-final',]
  
  #Extract countries from vector
  vote_from <- vote_pair[1]
  vote_to <- vote_pair[2]
  
  #Get voting data of the two countries
  tmp_data <- tmp_data[(tmp_data$From == vote_from & tmp_data$To == vote_to),]
  tmp_data <- tmp_data[(tmp_data$Year >= start_year & tmp_data$Year <= end_year),]
  
  #If there are more than 1 voting instances simulate
  if(nrow(tmp_data) > 1) {
    #get the actual mean points
    mean_vote <- mean(tmp_data$Points)
    
    sim_votes <- matrix(0,iterations,(end_year+1)-start_year)
    #Iterate over every year
    for(j in seq(start_year,end_year)){
      #get number of participants of a particular year
      participants <- length(unique(original_data[original_data$Year == j,]$To))-1
      #create a voting vector made up of votes from 1 to 12 and the rest 0's
      votes <- c(point_vector.2014,rep(0,participants-length(point_vector.2014)))
      #Add column of random samples from the votes vector to sim_votes matrix
      sim_votes[,(j-start_year)+1] <- sample(votes, iterations, T)
    }
    
    #Get mean of each row which would equal to the mean votes of all years per iteration
    mean_sim_votes <- rowMeans(sim_votes)
    
    #Sort the votes and get the 95% vote
    threshold_vote <- sort(mean_sim_votes)[threshold*iterations]
    
    #return true if actual vote is greater than the simulated vote
    if(mean_vote > threshold_vote) {
      return(TRUE)
    }
  }
  #Else Return false
  return(FALSE)
}

#Helper function to clean results
clean_results <- function(results) {
  #Go over each country and remove country from neighbour list
  for (country in names(results)) {
    for (neighbour in results[[country]]) {
      results[[neighbour]] <- results[[neighbour]][results[[neighbour]] != country]
    }
  }
  
  #Remove any empty lists
  for(country in names(results)) {
    if(identical(results[[country]], character(0))){
      results[country] <- NULL
    }
  }
  
  return(results)
}

# Perform simulations
iterations <- 10000
threshold <- 0.95 

#Simulate televoting votes
result.tele<- simulate(votes.tele, 2004, 2017, iterations, threshold)
#Simulate jury votes
result.jury <- simulate(votes.jury, 2016, 2017, iterations, threshold)

result.tele <-  clean_results(result.tele)
result.jury <-  clean_results(result.jury)