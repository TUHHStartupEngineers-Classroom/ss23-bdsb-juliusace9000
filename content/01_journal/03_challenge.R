library(tidyverse)
library(data.table)
library(dplyr)
library(vroom)

## Data Loading ##
# Using the reduced data set

# loading patent.tsv file
col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_integer()
)
patent_tbl <- vroom(
  file       = "Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)
patent_tbl <- rename(patent_tbl,patent_id = id) # Rename id to patent_id to make merging easier

# loading assignee.tsv file
col_types_assignee <- list(
  id = col_character(),
  type = col_integer(),
  organization = col_character()
)
assignee_tbl <- vroom(
  file       = "Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)
assignee_tbl <- rename(assignee_tbl,assignee_id = id) # Rename id to assignee_id to make merging easier

# loading patent_assignee.tsv file
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)
patent_assignee_tbl <- vroom(
  file       = "Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

# loading uspc.tsv file
col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_integer()
)
uspc_tbl <- vroom(
  file       = "Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

## Patent Dominance ##
# Convert patent_assignee_tbl to data.table and group it's entries by assignee_id. Then the number of elements per group is stored in a column patents.
patent_dominance_dt <- as.data.table(patent_assignee_tbl)[,.(patents = .N),by = assignee_id]
# Combine patent_activity_dt with assignee_tbl by patent_id to combine assignee info with company-name and -type info.

patent_dominance_dt <- as.data.table(merge(as_tibble(patent_dominance_dt),assignee_tbl,by="assignee_id")) %>%
  .[order(-patents),.(organization,type,patents)] # Afterwards the list is sorted by the number of patents.

patent_dominance_US_dt <- patent_dominance_dt[type == 2] # Then the data table can be filtered by only allowing US Companies or Corporations

print("What US company / corporation has the most patents?")
print(patent_dominance_US_dt[,patents,organization][1]) # Take the first element of the ordered list.
print("List the 10 US companies with the most assigned/granted patents.")
print(patent_dominance_US_dt[,patents,organization][1:10]) # Take the first 10 elements of the ordered list.

## Recent Patent Activity ##
# Take patent_tbl and separate into year, month and day and convert to data.table.
patent_activity_dt <- patent_tbl %>% separate(col = date,into = c("year","month","day"),sep = "-",convert = T) %>%
  as.data.table() %>% .[year == 2014 & month == 8] # Then filter by august (8) 2014.
# Combine patent_activity_dt with patent_assignee_tbl by patent_id to combine patent info with assignee info.

patent_activity_dt <- as.data.table(merge(as_tibble(patent_activity_dt),patent_assignee_tbl,by="patent_id")) %>%
  .[,.(patents = .N),by = assignee_id] # Group and sum up by assignee_id.
# Combine patent_activity_dt with assignee_tbl by patent_id to combine assignee info with company-name and -type info.

patent_activity_dt <- as.data.table(merge(as_tibble(patent_activity_dt),assignee_tbl,by="assignee_id")) %>%
  .[order(-patents),.(organization,type,patents)] # Order by number of patents and discard assignee_id.

print("What US company had the most patents granted in August 2014?")
print(patent_activity_dt[type == 2][1]) # Filter by US Company or Corporation and take the first element of the ordered list.
print("List the top 10 companies with the most new granted patents for August 2014.")
print(patent_activity_dt[1:10]) # Take the first 10 elements of the ordered list.


## Innovation In Tech ##
# For most innovative tech sensor group and sum the uspc_tbl by mainclass_id.
most_innovative_tech_sectors <- as.data.table(uspc_tbl)[,.(patents = .N),by = mainclass_id] %>%
  .[order(-patents),patents,mainclass_id] # Then order the list by descending patents.
# Retrieve the assignee_ids of the 10 companies that have the most patents and only keep the assignee_ids of those.

top_USPTO_main_classes <- as.data.table(patent_assignee_tbl)[,.(patents = .N),by = assignee_id][1:10][,assignee_id]
# Search through the patent_assignee_tbl again and only keep those patents that come from one of those 10 companies.

top_USPTO_main_classes <- as.data.table(patent_assignee_tbl)[assignee_id %in% top_USPTO_main_classes]
# With the list of patents combine it by the patent_id with the uspc_tbl.

top_USPTO_main_classes <- as.data.table(merge(as_tibble(top_USPTO_main_classes),uspc_tbl,by="patent_id")) %>%
  .[,.(patents = .N),by = mainclass_id] %>% # Group and sum up by mainclass_id.
  .[order(-patents),patents,mainclass_id] # Then order the list by descending patents.

print("What is the most innovative tech sector?")
print(most_innovative_tech_sectors[1]) # Take the first element of the ordered list.
print("For the top 10 companies (worldwide) with the most patents, what are the top 5 USPTO tech main classes?")
print(top_USPTO_main_classes[1:10]) # Take the first 10 elements of the ordered list.
