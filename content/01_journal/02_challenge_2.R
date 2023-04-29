# Challenge 2
library(tidyverse)
library(rvest)
library(xopen)
library(jsonlite)
library(glue)
library(stringi)


url <- "https://www.radon-bikes.de/e-bike/mountainbike/"

# People that worked on "The Dark Knight"
bike_names <- url %>% 
  read_html() %>% 
  html_nodes(".bikeTitle > h4") %>%
  html_text() 

bike_prices <- url %>% 
  read_html() %>% 
  html_nodes(".info > div > div > span") %>%
  html_text()
bike_prices <- bike_prices[seq(1,length(bike_prices),2)]

print(bike_names)
print(bike_prices)
bikes_tbl <- tibble(bike_names, bike_prices)
bikes_tbl %>% data.frame %>% print()