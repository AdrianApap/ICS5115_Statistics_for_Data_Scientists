# Eurovision Task Data Collection

Contains 2 scripts, one for collecting the Eurovison voting data from [https://eurovision.tv/](https://eurovision.tv/) and another script that obtains data on European Countries borders from the following [Wikipedia page](https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_land_borders).

## Requirements

### Eurovision Voting Data

* httr - ```install.packages('httr')```
* XML - ```install.packages('XML')```
* xml2 - ```install.packages('xml2')```
* rvest - ```install.packages('rvest')```
* selectr - ```install.packages('selectr')```

### European Countries Border Data

* httr - ```install.packages('httr')```
* XML - ```install.packages('XML')```

## Running

* Eurovision Voting Data - Scrapes the voting data from the Eurovision websites and saves the source html files to a directory called 'sources/' and the final voting data to a csv file called eurovision-df.csv

* European Countries Border Data - Scrapes the boarder data from Wikipedia, saves the source html file and the cleaned data to a csv file called border-data.csv
