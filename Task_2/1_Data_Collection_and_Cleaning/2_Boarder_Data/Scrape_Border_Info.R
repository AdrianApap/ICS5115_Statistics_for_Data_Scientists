#Clear workspace
rm(list = ls())

# Install the required packages
# install.packages('httr')
# install.packages('XML')

#load libraries
library(httr)
library(XML)

#Helper function to rename countries using the names used by the eurovision website
check_country <- function(country) {
  if(country == 'netherlands'){
    return('the netherlands')
  }else if(country == 'macedonia'){
    return('f y r macedonia')
  }else if(country == 'bosnia and herzegovina'){
    return('bosnia herzegovina')
  }else if(country == 'france excluding french overseas departments collectivities and territories'){
    return('france')
  }
  
  return(country)
}

#Cleans the Includes column and extracts neighboring countries
clean_value <- function(to_clean) {
  #Check country name and rename
  to_clean$country <- unlist(lapply(to_clean$country,check_country))

  #Split neighbour string using colon
  countries <- unlist(lapply(strsplit(to_clean$neighbours, ':'), function(x) gsub('^\\s+|\\s+$', '', x)))
  countries <- unlist(lapply(countries,check_country))
  
  #Merge list of countries using a semicolon as a delimiter
  if(length(countries) > 1) {
    to_clean$neighbours <- paste(countries, collapse=';')
  }else{
    to_clean$neighbours <- countries
  }
  
  return(to_clean)
}

#Get the wikipedia page
page <- GET('https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_land_borders')

#Check the status code if success get page content
if(status_code(page) == 200) {
  #Extract page content
  html_page <- content(page, "text")
  
  #Save content
  writeBin(html_page, 'List_of_countries_and_territories_by_land_borders.html')
  
  #Read html tables from content
  html_tables <- readHTMLTable(html_page, stringsAsFactors = FALSE)
  
  #Extract the required tables
  border_table <- html_tables[[1]]
  
  #Keep the required columns
  border_table <- border_table[c('V1','V6')]
  
  #Rename the columns
  colnames(border_table) <- c('country', 'neighbours')
  
  #Omit NA values
  border_table <- na.omit(border_table)
  
  #Remove countries without neighbours
  border_table <- border_table[border_table$neighbours != '',]
  
  #Remove the word Includes
  border_table$neighbours <- gsub('Includes:\n', '', border_table$neighbours)
  #Remove the words km and mi
  border_table$neighbours <- gsub('(km|mi)\\b', '', border_table$neighbours)
  #Remove numbers and symbols
  border_table$country <- gsub('[^a-zA-Z :\']', ' ', border_table$country)
  border_table$neighbours <- gsub('[^a-zA-Z :\']', ' ', border_table$neighbours)
  #Remove multiple spaces
  border_table$country <- gsub('\\s\\s+', ' ', border_table$country)
  border_table$neighbours <- gsub('\\s\\s+', ' ', border_table$neighbours)
  #Remove leading and trailing whitespaces
  border_table$country <- gsub('^\\s+|\\s+$', '', border_table$country)
  border_table$neighbours <- gsub('^\\s+|\\s+$', '', border_table$neighbours)
  #Lowercase values
  border_table$country <- tolower(border_table$country)
  border_table$neighbours <- tolower(border_table$neighbours)
  
  #Go over each row and extract the neighbours 
  for(i in 1:nrow(border_table)){
    border_table[i,] <- clean_value(border_table[i,])
  }
  
  #Add serbia and montenegro
  tmp_row <- data.frame('serbia montenegro', paste(c(border_table[border_table$country == 'serbia',]$neighbours,
                                          border_table[border_table$country == 'montenegro',]$neighbours), collapse=';'))
  names(tmp_row) <- names(border_table)
  border_table <- rbind(border_table, tmp_row)
  
  #Save data to csv file
  write.csv(border_table, file='border-data.csv', row.names = FALSE)
}


