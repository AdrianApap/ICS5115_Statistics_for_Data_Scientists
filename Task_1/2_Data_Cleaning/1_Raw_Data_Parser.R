#Clear workspace
rm(list = ls())

# install.packages('jsonlite')
# install.packages('anytime')
# install.packages('tidyr')

# Load required libraries
library(jsonlite)
library(anytime)
library(tidyr)

# Constants
INPUT_DIR <- 'raw_json'
OUTPUT_FILE <- 'flight_data.csv'

# Parses a single json file and updates the final dataframe
parse_single <- function(json_path) {
  cat('Parsing',json_path,'\n')
  
  # Extract date from filename
  date = unlist(strsplit(unlist(strsplit(json_path,'_',fixed=TRUE))[[2]],'/',fixed=TRUE))[[2]]
  
  # Parse json
  parsed_json = fromJSON(txt=json_path)
  num_rows = length(parsed_json[["data"]][["id"]])
  
  # Create a new dataframe using new data from json
  new_data <- data.frame(date=rep(date,num_rows),
                         id=parsed_json[["data"]][["id"]],
                         price=parsed_json[["data"]][["price"]],
                         fly_from=parsed_json[["data"]][["flyFrom"]],
                         fly_to=parsed_json[["data"]][["flyTo"]],
                         distance=parsed_json[["data"]][["distance"]],
                         fly_duration=parsed_json[["data"]][["fly_duration"]],
                         airlines=unlist(parsed_json[["data"]][["airlines"]], recursive=FALSE),
                         departure_time=anytime(parsed_json[["data"]][["dTime"]]),
                         arrival_time=anytime(parsed_json[["data"]][["aTime"]]),
                         departure_time_unix=parsed_json[["data"]][["dTime"]],
                         arrival_time_unix=parsed_json[["data"]][["aTime"]],
                         currency=rep(parsed_json[["currency"]],num_rows),
                         currency_rate=rep(parsed_json[["currency_rate"]],num_rows),
                         passengers=rep(parsed_json[["search_params"]][["seats"]][["passengers"]],num_rows),
                         passenger_infants=rep(parsed_json[["search_params"]][["seats"]][["infants"]],num_rows),
                         passenger_adults=rep(parsed_json[["search_params"]][["seats"]][["adults"]],num_rows),
                         passenger_children=rep(parsed_json[["search_params"]][["seats"]][["children"]],num_rows))
  
  # Update dataframe using new data
  final_df <<- rbind(final_df, new_data)
}

## START OF MAIN CODE ##
json_paths <- Sys.glob(file.path(INPUT_DIR, '*.json'))

# Create an empty dataframe
final_df <- data.frame(date=character(),
                        id=character(),
                        price=double(),
                        fly_from=character(),
                        fly_to=character(),
                        distance=double(),
                        fly_duration=double(),
                        airlines=character(),
                        departure_time=character(),
                        arrival_time=character(),
                        departure_time_unix=character(),
                        arrival_time_unix=character(),
                        currency=character(),
                        currency_rate=double(),
                        passengers=integer(),
                        passenger_infants=integer(),
                        passenger_adults=integer(),
                        passenger_children=integer(),
                        stringsAsFactors=FALSE) 

# Extract data from each json file
lapply(json_paths, parse_single)

cat('Before - Rows: ', nrow(final_df), ', Columns: ', length(colnames(final_df)), '\n')

#Check Columns
final_df <- final_df[final_df$currency_rate == 1,]
final_df <- final_df[final_df$currency == 'EUR',]
final_df <- final_df[final_df$passengers == 1,]
final_df <- final_df[final_df$passenger_infants == 0,]
final_df <- final_df[final_df$passenger_children == 0,]
final_df <- final_df[final_df$passenger_adults == 1,]

#Remove Checked Columns
columns <- colnames(final_df)
columns <- columns[columns != 'departure_time_unix']
columns <- columns[columns != 'arrival_time_unix']
columns <- columns[columns != 'currency_rate']
columns <- columns[columns != 'currency']
columns <- columns[columns != 'passengers']
columns <- columns[columns != 'passenger_infants']
columns <- columns[columns != 'passenger_children']
columns <- columns[columns != 'passenger_adults']

final_df <- final_df[columns]

#Split date into 2 columns containing date and time
final_df <- final_df %>% separate(date, c('year','month','day','hour','minute'), '-', remove=TRUE) %>% 
  unite('date', c('year', 'month', 'day'), sep='-', remove=TRUE) %>% 
  unite('time', c('hour', 'minute'), sep=':', remove=TRUE)

cat('After - Rows: ', nrow(final_df), ', Columns: ', length(colnames(final_df)), '\n')

# Output the dataframe as a csv file
write.csv(final_df, file=OUTPUT_FILE, row.names=FALSE)
