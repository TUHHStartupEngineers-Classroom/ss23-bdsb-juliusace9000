library(data.table)
library(tidyverse) # loads ggplot2
library(lubridate)
library(dplyr)
library(tibble)
library(scales)
library(maps)

options(repr.plot.width=50, repr.plot.height=3)

# Challenge 1

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

covid_data_graph_tbl <- covid_data_tbl %>% # Read the covid data
  filter(location == "Europe" | location == "Germany" | location == "United Kingdom" | location == "France" | location == "Spain" | location == "United States") %>% # Filter only the locations specified in the task
  select(date,total_cases,location) %>% # Only date, total_cases and location are needed
  filter(!is.na(total_cases)) %>% # Remove those dates where the total_cases number is not a number
  filter(date < '2022-04-20') # Plot in task stops in may of 2022, so this data stops there as well

covid_data_dt <- as.data.table(covid_data_graph_tbl) # Convert tibble to data.frame

last_date_europe <- covid_data_dt[location == "Europe"][order(-date)][1]$date
last_date_USA <- covid_data_dt[location == "United States"][order(-date)][1]$date

addMillions <- function(x, ...) #<== function will add " %" to any number, and allows for any additional formatting through "format".
  format(paste0(x/(1e+06), " M"), ...)

covid_data_dt %>% ggplot(aes(x=date,y=total_cases),palette="Dark2") + # plot total_cases over time
  geom_line(aes(colour=location)) + # each location gets its own line
  theme(legend.position = "bottom") + # position legend at the bottom
  scale_x_date(date_breaks = "1 month", date_labels = "%B '%y") + # Change the x axis to a date axis with montly intervals and "month 'year" labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate the x-axis labels by 45 degrees so they don't collide with each other
  scale_y_continuous(breaks = seq(0, 200000000, by = 50000000), labels = addMillions) + 
  labs( title = "COVID-19 confirmed cases worldwide", # Set plot title.
        subtitle = "As of 19/04/2022", # Set plot subtitle.
        y = "Cumulative Cases", # Set plot y-axis label.
        colour="Continent / Country") + # Set location/country legend title.
  theme(axis.title.x=element_blank(), # Remove x axis label 
        text = element_text(size=10)) + # Increase text size
  
  geom_label( # Display geom_label for europe and united states last data point
    data=covid_data_dt %>% filter((location == "Europe" & date == last_date_europe) | (location == "United States" & date == last_date_USA)),
    aes(label=total_cases),hjust=1,vjust=0.4
  )

# Challenge 2

# Get Case-Fatality rate (deaths/cases)
covid_data_graph_tbl <- covid_data_tbl %>% # Read the covid data.
  filter(!is.na(total_cases) & !is.na(total_deaths) & !is.na(total_deaths_per_million)) %>% # Remove those dates where the total_cases number is not a number.
  group_by(location) %>% summarise(total_cases = sum(total_cases),total_deaths = sum(total_deaths),total_deaths_per_million = sum(total_deaths_per_million)) %>% # Group by location (country) and sum up total_cases and total_deaths over all dates.
  mutate(fatality_rate = (total_deaths/total_cases)) %>% # Add fatality_rate column to the tibble.
  # Can be exchanged for total_deaths_per_million to visualize mortality rate.
  select(fatality_rate,location) %>%  # Only maintain fatality_rate and location
  mutate(location = case_when( # Replace non matching location names
    
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
    
  )) %>%
  distinct()

total_deaths_worldwide <- covid_data_tbl %>% filter(!is.na(total_deaths)) %>% group_by(location) %>% filter(row_number()==n()) %>% summarise(total_deaths) %>%
  ungroup() %>% summarise(total_deaths = sum(total_deaths))

library(RColorBrewer)
library(maptools)

world <- map_data("world")
ggplot(covid_data_graph_tbl) + 
  geom_map(dat=world, map=world, 
           aes(map_id=region), fill="white", color="black") + 
  geom_map(map=world, 
           aes(map_id=location, fill=fatality_rate), color="black") + 
  expand_limits(x = world$long, y = world$lat) +
  labs( title = "Confirmed COVID-19 fatality rate.", # Set plot title.
        subtitle = paste0("Around ",round(total_deaths_worldwide / 1e6, 1)," Million confirmed COVID-19 deaths worldwide."), # Set plot subtitle.
        caption = paste0("Date:",format(Sys.Date(), format="%d/%m/%Y")), # Set plot caption.
        fill = "Fatality rate") + # Set plot legend caption.
  theme(axis.title.x=element_blank(), # Remove x axis label. 
        axis.title.y=element_blank(), # Remove y axis label.
        axis.ticks = element_blank(), # Remove axis ticks.
        axis.text.x = element_blank(), # Remove x axis texts.
        axis.text.y = element_blank(), # Remove y axis texts.
  )