# Flight-Data-Cleaner
2 R Scripts used to parse, merge and clean multiple raw JSON files containing flight data from the [kiwi skypicker API](https://docs.kiwi.com/)

raw_json contains the raw data being used to generate the csv file.

## Requirements

* 1_Raw_Data_Parser.R requires the following packages to be installed:

  * jsonlite - ```install.packages('jsonlite')```
  * anytime - ```install.packages('anytime')```
  * tidyr - ```install.packages('tidyr')``` or the whole tidyverse package ```install.packages("tidyverse")```

* 2_Data_Cleaner.R requires the following packages to be installed:

  * nortest - ```install.packages('nortest')```
  * moments - ```install.packages('moments')```
  * ggplot2 - ```install.packages('ggplot2')```
  * tidyr - ```install.packages('tidyr')``` or the whole tidyverse package ```install.packages("tidyverse")```

## Running
1_Raw_Data_Parser.R expects the raw JSON files to be in a directory called 'raw_json/' and outputs a single CSV file called flight_data.csv

2_Data_Cleaner.R expects a CSV file called Flights.csv to be in the same directory and outputs a new CSV file called new_flight_data.csv
