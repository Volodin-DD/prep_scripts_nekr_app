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
txt_path <- system("ls /home/user/dumps", intern = TRUE)
txt_path <- txt_path[which(
  as.Date(str_sub(txt_path, 1, 10), format = "%d.%m.%Y") == max(as.Date(str_sub(txt_path, 1, 10), format = "%d.%m.%Y"), na.rm = TRUE)
)]
txt_path <- txt_path[str_detect(txt_path, fixed("week.txt"))][3]
txt_path <- str_c("/home/user/dumps/", txt_path)

# Read raw string
raw <- readtext(txt_path, encoding = "UTF-8")$text

# Split records
circulation <- str_split(raw, "\n\n")[[1]]
rm(raw)
gc()

# Split fields
circulation <- lapply(circulation, function(x) {str_split(x, "\n") %>% unlist()})
circulation <- lapply(circulation, function(x) x[-(1:2)])

# ID vector
ids <- lapply(circulation, function(x) x[1]) %>% unlist() %>% str_sub(., 5, -1)
circulation <- lapply(circulation, function(x) x[-1])
id_length <- lapply(circulation, length) %>% unlist()

# Repeat ids
ids <- rep(ids, id_length)

# Extract fields
fields <- lapply(circulation, function(x) {str_sub(x, 1, 3)}) %>% unlist()
var <- lapply(circulation, function(x) {str_sub(x, 5, -1)}) %>% unlist()

circulation_df <- data.frame(
  id = ids,
  field = fields,
  var = var
)
rm(circulation, ids, id_length, fields, var)
gc()

subfield_extractor <- function(var, sf) {
  regexp <- str_c("\\$", sf, sep = "")
  replacement <- str_c("no $", sf, sep = "")
  x <- ifelse(
    is.na(str_locate(var, regexp)[, 1]),
    replacement,
    str_sub(var, str_locate(var, regexp)[, 1] + 2, -1)
  )
  x <- ifelse(
    x == replacement,
    replacement,
    str_sub(
      x,
      1,
      ifelse(
        is.na(str_locate(x, "\\$")[, 1]),
        -1,
        str_locate(x, "\\$")[, 1] - 1
      )
    )
  )
  return(x)
}

id_doc <- circulation_df %>% filter(field == "070") %>% select(id, INVBAR = var)

id_doc$INVBAR <- subfield_extractor(id_doc$INVBAR, "a")

id_operation <- circulation_df %>% filter(field == "983") %>% mutate(date = var) %>% 
  select(id, operation = var, date)

id_operation$operation <- subfield_extractor(id_operation$operation, "a")

id_operation$date <- subfield_extractor(id_operation$date, "c")

id_operation$date <- as.Date(str_sub(id_operation$date, 1, 8), format = "%Y%m%d")

circulation_trimmed <- left_join(
  id_doc, id_operation, by = "id"
) %>% select(-id) %>% distinct() %>% filter(operation %in% c(
  "ВЫДАЧА КНИГИ В ЗАЛ", "ПЕРЕДАТЬ ПО АБОНЕМЕНТУ", "ПЕРЕДАТЬ В ЧИТАЛЬНЫЙ ЗАЛ", "ПЕРЕДАТЬ ПЕРИОДИКУ"
))

rm(circulation_df, id_doc, id_operation)
gc()

circulation_full <- read_tsv("~/Comp_app/data/circulation.csv")

circulation <- rbind(
    circulation_full,
    circulation_trimmed
) %>% distinct()

write_tsv(circulation, "~/Comp_app/data/circulation.csv")