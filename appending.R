## Script for appending weekly changes to main funds dataframes

# Needed packages
packages <- c("tidyverse", "readtext", "lubridate")

# Check whethet package is installed and attach it, If not installed - download, install and then attacj it
if ("tidyverse" %in% installed.packages()[, 1]) {
  library(tidyverse)
} else {
  install.packages("tidyverse")
  library(tidyverse)
}

if ("readtext" %in% installed.packages()[, 1]) {
  library(readtext)
} else {
  install.packages("readtext")
  library(readtext)
}

if ("lubridate" %in% installed.packages()[, 1]) {
  library(lubridate)
} else {
  install.packages("lubridate")
  library(lubridate)
}

setwd("~/dumps")
# Unzip the latest week update
zipname <- system("ls /home/user/dumps", intern = TRUE)
zipname <- zipname[which(
    as.Date(str_sub(zipname, 1, 10), format = "%d.%m.%Y") == max(as.Date(str_sub(zipname, 1, 10), format = "%d.%m.%Y"), na.rm = TRUE)
)]
zipname <- zipname[str_detect(zipname, fixed("week.zip"))]
unzip_command <- str_c("unzip /home/user/dumps/", zipname)
system(unzip_command)

# Path to source txt files (recently unzipped)
txt_path <- system("ls /home/user/dumps", intern = TRUE)
txt_path <- txt_path[which(
    as.Date(str_sub(txt_path, 1, 10), format = "%d.%m.%Y") == max(as.Date(str_sub(txt_path, 1, 10), format = "%d.%m.%Y"), na.rm = TRUE)
)]
txt_path <- txt_path[str_detect(txt_path, fixed("week.txt"))]
txt_path <- str_c("/home/user/dumps/", txt_path)

# Path to resulting csv files
csv_path <- str_c("/home/user/Comp_app/data/", c("funds_df_56.csv", "funds_df_5.csv"))

#Loop over txt_path
for (f in txt_path) {
  #Read file as raw text
  raw <- readtext(f, encoding = "UTF-8")$text
  
  #Split raw text into separate records
  funds <- str_split(raw, "\n\n")[[1]]
  rm(raw)
  gc()
  
  #Split each record into field strings
  funds <- map(funds, function(x) {str_split(x, "\n") %>% unlist()})
  funds <- map(funds, function(x) x[-(1:2)])
  funds[length(funds)] <- list(funds[[length(funds)]][-length(funds[[length(funds)]])])
  
  #Extract id of each record (001 field)
  ids <- map(funds, function(x) x[1]) %>% unlist() %>% str_sub(., 5, -1)
  
  #Extract number of fields in each record (without 001 field)
  id_length <- (map(funds, length) %>% unlist()) - 1
  
  #Make vector of repeating ids
  ids <- rep(ids, id_length)
  rm(id_length)
  gc()
  
  #Remove ID field (001) from each record
  funds <- map(funds, function(x) x[-1])
  
  #Extract field names
  field_names <- map(funds, str_sub, 1, 3) %>% unlist()
  
  #Extract contents of the fields
  vars <- map(funds, str_sub, 5, -1) %>% unlist()
  rm(funds)
  gc()
  
  #Make funds dataframe (without splitting into subfields)
  funds_df_new <- data.frame(
    id = ids,
    field = field_names,
    var = vars
  )
  rm(ids, field_names, vars)
  gc()
  
  #Read full funds dataframe
  funds_df <- read_tsv(csv_path[which(txt_path == f)])
  
  #Remove from it changed records
  funds_df <- funds_df[!(funds_df$id %in% funds_df_new$id), ]
  
  #Add to it new and changed records from fresh dataframe
  funds_df <- rbind(funds_df, funds_df_new)
  
  #Write the updated dataframe down
  write_tsv(funds_df, csv_path[which(txt_path == f)])
  rm(funds_df, funds_df_new)
  gc()
}