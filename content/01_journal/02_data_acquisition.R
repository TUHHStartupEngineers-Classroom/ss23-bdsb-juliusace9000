

library(RSQLite)
con <- RSQLite::dbConnect(drv = SQLite(), dbname = "00_data/02_chinook/Chinook_Sqlite.sqlite") #connect to database

library(DBI) #to print the tables
print(dbListTables(con))

library(dplyr)
print(tbl(con, "Album")) #to examine table from database

album_tbl <- tbl(con, "Album") %>% collect() #pull data in local memory

dbDisconnect(con) #disconnect database



library(glue)
name <- "Fred"
glue('My name is {name}.')

library(httr)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
print(resp)
print(rawToChar(resp$content))

library(jsonlite)
print(fromJSON(rawToChar(resp$content)))


response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={Sys.getenv('TOKEN')}"))
print(response)


library(rvest)
library(stringr)

# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()
print(sp_500)

#other example
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()

rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()
title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()
year <- html %>% 
  html_nodes(".titleColumn .secondaryInfo") %>%
  html_text() %>% 
  # Extract numbers
  stringr::str_extract(pattern = "[0-9]+") %>% 
  as.numeric()
people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")
rating <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_text() %>% 
  as.numeric()
num_ratings <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_attr('title') %>% 
  # Extract the numbers and remove the comma to make it numeric values
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()
imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)
print(imdb_tbl)

bike_data_lst <- fromJSON("00_data/bike_data.json")
bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue") %>%
  print()


# Business Case

