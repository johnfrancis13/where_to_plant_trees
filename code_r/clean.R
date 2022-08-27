setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library("tidyverse")
library(RSQLite)
library(R.utils)
library(dbplyr)
library(inborutils)
library(data.table)   # For like function (%like%)
library(sqldf)
#untar("../data/aot_data/AoT_Chicago.complete.latest.tar",exdir="../data/aot_daata")
#untar("C:/Users/johnf/Downloads/Waggle_Others.complete.latest.tar",exdir="../data/waggle_daata")

# AoT
nodes <- read.csv("../data/aot_data/AoT_2022-03-23/nodes.csv")
head(nodes)

sensors <- read.csv("../data/aot_data/AoT_2022_03_14/sensors.csv")

offsets <- read.csv("../data/aot_data/AoT_2022_03_14/offsets.csv")

# waggle
#nodes <- read.csv("../data/waggle_data/Waggle_Others.complete.2022-03-14/nodes.csv")

# first step is figuring out how to read in crazy amounts of data

# this should read in only certain values
start_time <- Sys.time()
a <- read_csv_chunked("../data/aot_data/AoT_2022-03-23/data.csv.gz",
                      callback = DataFrameCallback$new(function(x, pos) subset(x, value_hrf > 500)),
                      col_types = 'Dcccccd',
                      progress = F, 
                      chunk_size = 1000000)
end_time <- Sys.time()
end_time - start_time

data <- read_csv("D:/aot/aot_data_3-23-22.csv",
                 n_max = 14487441,
                 col_names = TRUE,
                 col_types=cols(
                   timestamp=col_character(),
                   node_id=col_character(),
                   subsystem=col_character(),
                   sensor=col_character(),
                   parameter=col_character(),
                   value_raw=col_character(),
                   value_hrf=col_character(),
                 ))


length(count.fields("D:/aot/aot_data_3-23-22.csv", sep = ","))


read.csv.sql("D:/aot/aot_data_3-23-22.csv", sep = ",", sql = "select count(*) from file")
# in windows...
#find /c /v "A String that is extremely unlikely to occur" D:\aot\aot_data_3-23-22.csv

count_lines = function(filepath, batch) {
  con = file(filepath, "r")
  n = 0
  while ( TRUE ) {
    lines = readLines(con, n = batch)
    present = length(lines)
    n = n + present
    if ( present <  batch) {
      break
    }
  }
  close(con)
  return(n)
}

count_lines("D:/aot/aot_data_3-23-22.csv", 100000)

# try and skip x number of lines based on the offest data file
data <- data.table::fread("../data/aot_data/AoT_2022_03_14/data.csv.gz", skip = 36217179537)

# could do this based on a value (such as a date)
subSet <- fread(csv.name, skip = "2015-06-12 15:14:39", showProgress = FALSE)
sprintf("Number of lines in data set with skipped lines: %s", nrow(subSet))

fourColumns = fread(csv.name, select = c("device_info_serial", "date_time", 
                                         "latitude", "longitude"), 
                    showProgress = FALSE)
sprintf("Size of total data in memory: %s MB", utils::object.size(allData)/1000000)

################################################################################
# create  a sql database from the gz file
# First unzip the gz file
gunzip("../data/aot_data/AoT_2022-03-23/data.csv.gz",destname="D:/aot/aot_data_3-23-22.csv")

sqlite_file <- "D:/aot/aot.sqlite"
table_name <- "aot"

#f <- function(x, pos) subset(x)

my_callback <- function (con, table_name) 
{
  function(x, pos) {
    x <- as.data.frame(x)
    dbWriteTable(con, table_name, x, append = TRUE)
  }
}

convert_csv_sqlite <- function (csv_file, sqlite_file, table_name, 
          pre_process_size = 100, chunk_size = 100000, show_progress_bar = TRUE, 
          ...) 
{
  con <- dbConnect(SQLite(), dbname = sqlite_file)
  df <- read_csv(csv_file, n_max = pre_process_size, 
                 col_names=c("timestamp",
                             "node_id",
                             "subsystem",
                             "sensor",
                             "parameter",
                             "value_raw",
                             "value_hrf"),
                 col_types=cols(
                   timestamp=col_character(),
                   node_id=col_character(),
                   subsystem=col_character(),
                   sensor=col_character(),
                   parameter=col_character(),
                   value_raw=col_character(),
                   value_hrf=col_character()))
  dbWriteTable(con, table_name, df, overwrite = TRUE)
  read_csv_chunked(csv_file, callback = my_callback(con = con, 
                                                           table_name = table_name), 
                     skip = pre_process_size + 1, chunk_size = chunk_size, 
                     progress = show_progress_bar,
                   col_names=c("timestamp",
                               "node_id",
                               "subsystem",
                               "sensor",
                               "parameter",
                               "value_raw",
                               "value_hrf"),
                   col_types=cols(
                     timestamp=col_character(),
                     node_id=col_character(),
                     subsystem=col_character(),
                     sensor=col_character(),
                     parameter=col_character(),
                     value_raw=col_character(),
                     value_hrf=col_character()))
  dbDisconnect(con)
}

start_time <- Sys.time()
# csv_to_sqlite(csv_file = "../data/aot_data/AoT_2022_03_14/aot_data.csv", 
#                           sqlite_file, table_name,
#                           chunk_size = 100000, show_progress_bar = FALSE,
#                           col_names=c("timestamp",
#                                       "node_id",
#                                       "subsystem",
#                                       "sensor",
#                                       "parameter",
#                                       "value_raw",
#                                       "value_hrf"),
#                           col_types=cols(
#                             timestamp=col_character(),
#                             node_id=col_character(),
#                             subsystem=col_character(),
#                             sensor=col_character(),
#                             parameter=col_character(),
#                             value_raw=col_character(),
#                             value_hrf=col_character()),
#               callback=append_to_sqlite(sqlite_file, table_name))

convert_csv_sqlite(csv_file = "D:/aot/aot_data_3-23-22.csv", 
              sqlite_file, table_name,
              chunk_size = 80000, show_progress_bar = FALSE)


end_time <- Sys.time()
end_time - start_time



# db <- dbConnect(SQLite(), dbname = "example.sqlite")
# dbWriteTable(db, "birdtracks", allData)
# dbDisconnect(dbConnect(SQLite(), dbname = "aot.sqlite"))
################################################################################
# create an empty database.
# # can skip this step if database already exists.
# sqldf(file = "../data/aot_data/AoT_2022_03_14/aotdb")
# 
# # read into table called iris in the testingdb sqlite database
# 
# read.csv.sql("../data/aot_data/AoT_2022_03_14/aot_data.csv", sql = "create table aot as select * from file", 
#              dbname = "../data/aot_data/AoT_2022_03_14/aotdb")
# 
# # look at first three lines
# sqldf("select * from main.iris limit 3", dbname = "testingdb")

################################################################################
#https://stackoverflow.com/questions/68628271/partially-read-really-large-csv-gz-in-r-using-vroom/68693819#68693819
library(readr)
library(arrow)

#fyl <- "...path_to_big_data_file.csv.gz"
fyl <- "D:/aot/aot_data_3-23-22.csv"

#pqFolder <- "...path_to_folder_where_chunked_parquet_files_are_to_be_saved"

pqFolder <- "D:/aot/subfiles/"

# going to try this attempt but with csvs since i dont think space is my issue here
f <- function(x, pos){
  write.csv(x,
                file.path(pqFolder, paste0(pos,"_2" ,".csv")),row.names = F)
}

read_csv_chunked(
  fyl,
  skip=2144141269,
  col_names=c("timestamp",
              "node_id",
              "subsystem",
              "sensor",
              "parameter",
              "value_raw",
              "value_hrf"),
  col_types=cols(
    timestamp=col_character(),
    node_id=col_character(),
    subsystem=col_character(),
    sensor=col_character(),
    parameter=col_character(),
    value_raw=col_character(),
    value_hrf=col_character()), # all column specifications
  callback = SideEffectChunkCallback$new(f),
  chunk_size = 14487441)


# |====================================================  | 51% 153527 MB
# Error in read_tokens_chunked_(data, callback, chunk_size, tokenizer, col_specs,  : 
# ttempt to set index 14487441/14487441 in SET_STRING_ELT
                                                                                                                                                   
################################################################################
# this crashed my computer....
# iris2 <- read.csv.sql("D:/aot/aot_data_3-23-22.csv", 
#                       sql = "select * from file WHERE (`timestamp` like '2018/02/05%') ")
# 
################################################################################
# Query the database using dplyr

db <- dbConnect(SQLite(), dbname = "D:/aot/aot.sqlite")

my_query <- tbl(db, "aot")


results <- my_query %>%
  select(timestamp, node_id, parameter,value_hrf) %>%
#  filter(instr("2018/02/10", timestamp) > 0)
#  filter(sensor == "lps25h")
  dplyr::filter(timestamp %like%  "2018/02/05%") %>% show_query() 
#  filter(timestamp == "temperature") 
#head(results)

# %>% show_query() # shows the sql code
# %>% collect() # pulls data down into a table

# get the results
df <- results %>% collect()
# total # of rows
my_query %>% summarize(n())
# n = 2,147,401,000 ... should be like 18 times higher... was  176,923,180 kb
# Error in read_tokens_chunked_(data, callback, chunk_size, tokenizer, col_specs,  : 
# attempt to set index 100000/100000 in SET_STRING_ELT 
# Time difference of 2.55798 hours with 100,000
# there should be at least 36,218,602,576 lines
dbDisconnect(db)





################################################################################
df <- read_csv("D:/aot/subfiles/2028241741_2.csv")

nodes$start_timestamp <- lubridate::ymd_hms(nodes_sf$start_timestamp)


#
my_files <- list.files("D:/aot/subfiles/")
my_data_frame <- data.frame(node_id=character(),
                            timestamp=character(),
                            sensor=character(),
                            parameter=character(),
                            value=numeric(),
                            nrows=numeric())

# make a loop to read in all of the files and create a summary file
for (i in 1:length(my_files)) {
  # read in file
  temp_file <- read_csv(paste0("D:/aot/subfiles/",my_files[i]),)
  
  #### run some functions on the file ####
  # convert timestamp to hour/day for aggregation
  temp_file$timestamp <- lubridate::floor_date(lubridate::ymd_hms(temp_file$timestamp), "hour")
  # make numeric, gonna lose some data here
  temp_file$value_hrf <- as.numeric(temp_file$value_hrf) 
  
  # aggregate to get a single value per node/sensor/day/hour
  temp_file <- temp_file %>% group_by(
    node_id,timestamp,sensor,parameter) %>% summarise(
      value=mean(value_hrf,na.rm = T),
      nrows=n()) %>% ungroup()
  temp_file$timestamp <- as.character(temp_file$timestamp) 
  # append to a summary df
  my_data_frame<- bind_rows(my_data_frame,temp_file)
  print(i)
  }
  
write.csv(my_data_frame,"D:/aot/summary_file.csv",row.names=F)

################################################################################
nodes <- read_csv("../data/aot_data/AoT_2022-03-23/nodes.csv")
df <- read_csv("D:/aot/summary_file.csv")

df$timestamp <- lubridate::ymd_hms(df$timestamp)

df2 <- df %>% filter(timestamp >= "2019-04-01" & timestamp <="2019-10-01")

df3 <- df %>% filter(sensor %in% "o3") %>% filter(is.na(value)==FALSE)
unique(df3$node_id) # 86
df3 <- df %>% filter(sensor %in% "so2") %>% filter(is.na(value)==FALSE)
unique(df3$node_id) # 86
df3 <- df %>% filter(sensor %in% "tmp421") %>% filter(is.na(value)==FALSE)
unique(df3$node_id) # 118
df3 <- df %>% filter(parameter %in% "pm25_cf1") %>% filter(is.na(value)==FALSE)
unique(df3$node_id) # 23

