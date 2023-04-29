

library(RSQLite)
con <- RSQLite::dbConnect(drv = SQLite(), dbname = "00_data/02_chinook/Chinook_Sqlite.sqlite")
library(DBI)
print(dbListTables(con))
library(dplyr)
tbl(con, "Album")
dbDisconnect(con)