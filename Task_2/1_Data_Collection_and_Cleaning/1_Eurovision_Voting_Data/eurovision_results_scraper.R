#Clear workspace
rm(list = ls())

# Install the required packages

# install.packages('rvest')
# install.packages('XML')
# install.packages('xml2')
# install.packages('selectr')
# install.packages('httr')

# Load the required packages
library(rvest)
library(XML)
library(xml2)
library(selectr)
library(httr)

# Constants
BASE_URL <- 'https://eurovision.tv'
VOTE_LABELS <- c('Televoting', 'Jury')

# Given a URL it will attempt to get the contents as text
# and save the result in an html file
# returns the parsed html page
save_get_html <- function(page_url) {
  
  # Create output directory if it doesn't exist
  if(!dir.exists("sources/")){
    dir.create("sources/")
  }
  
  cat('Getting page: ',page_url)
  
  # Attempt to retrieve the page
  page <- GET(page_url)
  
  # If the page was retreived successfully save and parse it
  if(status_code(page) == 200) {
    cat(' Done\n')
    html_page <- content(page, "text")
    name <- gsub(":", "%3A", gsub("/", "%2F", gsub("\\", "%5C", page_url, fixed=TRUE)))
    writeBin(html_page, paste0('sources/', name, '.html'))

    return(read_html(html_page))
  }else{
    # Return NULL if any other status code is returned
    cat(' ---Error---', status_code(page),'\n')
    return(NULL)
  }
}

# Extracts the votes of a particular country from a single 
# html list item
extract_vote <- function(vote_item) {
  vote_points <- xml_text(html_nodes(vote_item,'span>span>span'))
  vote_country <- xml_text(html_nodes(vote_item,'ul > li > span'))
  
  # If multiple countries were given the same vote add each country to a list
  result_list <- list()
  for(i in 1:length(vote_country)){
    result_list[[length(result_list)+1]] <- c(vote_country[i],vote_points)
  }
  
  return(result_list)
}

# Extracts the votes from an html list and saves them to a dataframe
parse_votes <- function(vote_list, vote_label, country, event_name, event_stage) {
  tmp_df <- final_data
  
  # Extract the list items
  vote_list_items <- xml_children(vote_list)
  # Extract countries and points from each list items
  parsed_vote_list <- lapply(vote_list_items, extract_vote)
  # Merge the lists into a single list
  parsed_vote_list <- unlist(parsed_vote_list, recursive = FALSE)
  
  # Write each vote to the dataframe
  for(i in 1:length(parsed_vote_list)){
    tmp_vote <- unlist(parsed_vote_list[i])
    tmp_row <- c(event_name, event_stage, country, tmp_vote[1], vote_label, tmp_vote[2])
    tmp_df[nrow(tmp_df) + 1,] = tmp_row
  }
  
  # Assign the new dataframe to the global dataframe variable
  assign("final_data", tmp_df, envir = .GlobalEnv)
}

# Extract the votes of a particular country from a scoreboard html page
parse_scoreboard <- function(country, page_url) {
  if(country != 'placeholder'){
    
    # Get all the visited urls
    tmp_visited_urls <- visited_urls
    
    # If url was never visited before attempt to retrieve it
    if(is.null(tmp_visited_urls[[paste0(BASE_URL,page_url,'/',country)]])){
      #page <- GET(paste0(base_url,page_url,'/',country))
      page <- save_get_html(paste0(BASE_URL,page_url,'/',country))
      
      # If the page was successfully retrieved try to parse it
      if(!is.null(page)){
        
        tmp_split_url = unlist(strsplit(page_url,'/',fixed=T))
        event_name = tmp_split_url[3]
        event_stage = tmp_split_url[4]
        
        # Extract the voting tables (new events have two tables one for televoting and another for jury votes)
        votes <- querySelectorAll(page, '#frame-content > div:nth-child(3) > div:nth-child(3) > div > div > div > div:nth-child(2) > div:nth-child(1) > div > ul')
        for(i in 1:length(votes)){
          parse_votes(votes[i], VOTE_LABELS[i], country, event_name, event_stage)
        }
        
        # Add the current url to the list of visited urls
        tmp_visited_urls[[paste0(BASE_URL,page_url,'/',country)]] <- T
      }
    }else{
      cat('Already Visited',page_url,'skipping\n')
    }
    
    # Assign the list of visited urls to the global variable
    assign("visited_urls", tmp_visited_urls, envir = .GlobalEnv)
  }
}


# Extracts the list of countries from a scoreboard page
parse_countries <- function(page_url) {
  #page <- GET(paste0(base_url,page_url))
  page <- save_get_html(paste0(BASE_URL,page_url))
  
  if(!is.null(page)) {
    countries <- html_nodes(page, '#choose-voter > option') %>%
      xml_attr('value')
    
    # Extract the votes of each country in the drop-down menu
    lapply(countries, parse_scoreboard, page_url=page_url) 
  }
}

# Extracts the event stages from an event page
parse_page <- function(page_url) {
  #page <- read_html(paste0(base_url,page_url))
  page <- save_get_html(paste0(BASE_URL,page_url))
  
  if(!is.null(page)) {
    page_urls <- html_nodes(page, 'a') %>%
      html_attr('href')
  
    scoreboards_urls = list()
    for (url in page_urls){
      if(startsWith(url, page_url)){
        scoreboards_urls <- append(scoreboards_urls,paste0(url,'/scoreboard'))
      }
    }
  
    #Remove first element too slow too load page that doesnt exist
    scoreboards_urls <- scoreboards_urls[-1] 
    # Extract countries and votes for each stage
    lapply(scoreboards_urls, parse_countries)
    
    # Save a checkpoint of the current data and visited urls
    final_data <- unique(final_data)
    cat('Saving', nrow(final_data),'rows of data\n')
    saveRDS(final_data, file='eurovision-df.rds')
    saveRDS(visited_urls, file='visited_urls.rds')
  }
}

## START MAIN CODE ##

#Create empty dataframe for data
final_data <- data.frame(Event=character(),
                 Stage=character(), 
                 From=character(), 
                 To=character(), 
                 Type=character(), 
                 Points=integer(), 
                 stringsAsFactors=FALSE) 

# Try to load past data
if(file_test('-f', 'eurovision-df.rds')){
  final_data <- readRDS('eurovision-df.rds')
}

# Create empty list of visited urls
visited_urls = list()

# Try to load past list of visited urls
if(file_test('-f', 'visited_urls.rds')){
  visited_urls <- readRDS('visited_urls.rds')
}

#Get all past events from events page
page <- save_get_html(paste0(BASE_URL,'/events'))

# Get all the links in the events page
page_urls <- html_nodes(page, 'a') %>%
  html_attr('href')

# If the link is an event link keep it
event_links <- c()
for(page_url in page_urls){
  if(startsWith(page_url,'/event/')){
    event_links = c(event_links, page_url)
  }
}

# Extract the votes from each event link
lapply(event_links, parse_page)

# Finnally save all the data
final_data <- unique(final_data)
saveRDS(final_data, file='eurovision-df.rds')
write.csv(final_data, file='eurovision-df.csv', row.names = FALSE)