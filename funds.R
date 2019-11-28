## Main funds script

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

# Working directory
setwd("~/dumps")
# Unzip the latest full update
zipname <- system("ls /home/user/dumps", intern = TRUE)
zipname <- zipname[which(
  as.Date(str_sub(zipname, 1, 10), format = "%d.%m.%Y") == max(as.Date(str_sub(zipname, 1, 10), format = "%d.%m.%Y"), na.rm = TRUE)
)]
zipname <- zipname[str_detect(zipname, fixed("full.zip"))]
unzip_command <- str_c("unzip /home/user/dumps/", zipname)
system(unzip_command)

# Path to source txt files (recently unzipped)
txt_path <- system("ls /home/user/dumps", intern = TRUE)
txt_path <- txt_path[which(
  as.Date(str_sub(txt_path, 1, 10), format = "%d.%m.%Y") == max(as.Date(str_sub(txt_path, 1, 10), format = "%d.%m.%Y"), na.rm = TRUE)
)]
txt_path <- txt_path[str_detect(txt_path, fixed("full.txt"))]
txt_path <- str_c("/home/user/dumps/", txt_path)

# Path to resulting csv files
csv_path <- str_c("/home/user/Comp_app/data/", c("funds_df_56.csv", "funds_df_5.csv"))

# Loop over txt_path
for (f in txt_path) {
  # Read txt file as character vector of length 1
  raw <- readtext(f, encoding = "UTF-8")$text
  
  # Split raw by records
  funds <- str_split(raw, "\n\n")[[1]]
  rm(raw)
  gc()
  
  # Split each record into lines (result: list element contents record, which contents character vector of fields)
  funds <- map(funds, function(x) {str_split(x, "\n") %>% unlist()})
  
  # Remove technical information about record
  funds <- map(funds, function(x) x[-(1:2)])
  
  # Remove last element of the last record (empty)
  funds[length(funds)] <- list(funds[[length(funds)]][-length(funds[[length(funds)]])])
  
  # Make character vector of ids (001 field)
  ids <- map(funds, function(x) x[1]) %>% unlist() %>% str_sub(., 5, -1)
  
  # Count how much id for each record must be repeated to make it key vector for the resulting dataframe
  id_length <- (map(funds, length) %>% unlist()) - 1
  
  # Repeat ids
  ids <- rep(ids, id_length)
  rm(id_length)
  gc()
  
  # Remove ids from each record
  funds <- map(funds, function(x) x[-1])
  
  # Extract field names (first three characters) and field content
  field_names <- map(funds, str_sub, 1, 3) %>% unlist()
  vars <- map(funds, str_sub, 5, -1) %>% unlist()
  rm(funds)
  gc()
  
  # Make dataframe (first column - id of record, second column - name of the field in corresponding record, third column - content of corresponding field)
  funds_df <- data.frame(
    id = ids,
    field = field_names,
    var = vars
  )
  rm(ids, field_names, vars)
  gc()
  
  # Save dataframe
  write_tsv(funds_df, csv_path[which(txt_path == f)])
}