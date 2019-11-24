## Selector of for levels of document types and themes

# Attach or download and attach tidyverse
if ("tidyverse" %in% installed.packages()[, 1]) {
  library(tidyverse)
} else {
  install.packages("tidyverse")
  library(tidyverse)
}

# Load dataframes
main <- read_tsv("/home/user/Comp_app/data/funds_df_5.csv")
periodicals <- read_tsv("/home/user/Comp_app/data/funds_df_56.csv")

# Make column in each dataframe, indicating in what catalogue documents are
main <- main %>% mutate(fund = "main")
periodicals <- periodicals %>% mutate(fund = "periodicals")

# Merge dataframes
funds_df <- rbind(main, periodicals)
rm(main, periodicals)
gc()

# Choose only needed fields
funds_df <- funds_df %>% filter(field %in% c("101", "686", "899", "100"))

# Make separate dataframes for each fieldname
funds_899 <- funds_df %>% filter(field == "899")
funds_101 <- funds_df %>% filter(field == "101")
funds_686 <- funds_df %>% filter(field == "686")
funds_100 <- funds_df %>% filter(field == "100")
rm(funds_df)
gc()

# Main function for extracting contents of subfields
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

# Extract contents
funds_899$subfund <- subfield_extractor(funds_899$var, "b")
funds_899$bar <- subfield_extractor(funds_899$var, "p")
funds_899$inv <- subfield_extractor(funds_899$var, "x")
funds_899$shelf <- subfield_extractor(funds_899$var, "h")

# Select only needed columns
funds <- funds_899 %>% select(id, fund, subfund, bar, inv, shelf)

# Extract the language of document
funds_101$language <- ifelse(
  str_detect(funds_101$var, "\\$arus"),
  "russian",
  "foreign"
)

# First join (899 + 101)
funds <- left_join(funds,
                   funds_101 %>% select(id, language),
                   by = "id")

# Extract index and catalogue name
funds_686$catalogue <- subfield_extractor(funds_686$var, "2")
funds_686$bbk <- subfield_extractor(funds_686$var, "a")

# Select only bbk catalogue
funds_686 <- funds_686[str_detect(funds_686$catalogue, ".*rubbk.*"), ]

# Second join (899+101+686)
funds <- left_join(funds,
                   funds_686 %>% select(id, bbk),
                   by = "id")

# Extract date of documents
funds_100$var <- str_sub(funds_100$var, 5, 12)

# Make the variable date type
funds_100 <- funds_100 %>% select(id, date = var)
funds_100$date <- as.Date(funds_100$date, "%Y%m%d")

# Third join (899+100+686+100)
funds <- left_join(funds,
                   funds_100,
                   by = "id")

# Dataframe for naming the first level of funds (main, digital, microforms, regional, rare, missing)
### Here should be loaded dataframe from authority file for 899$b
fund_level_1 <- data.frame(
  subfund = c("читальный зал", "абонемент", "а", "онл", "онл-чз", "ФОД", "ох-чз", "Читальный зал",
              "ОНЛ", "Онл", "журналы ТГ", "ЖУРНАЛЫ ТГ", "эр", "аудио", "видео", "кр", "рф",
              "рФ", "Рф", "микроформа"),
  level_1 = c(rep("main", 12),
              rep("digital", 3),
              rep("regional", 1),
              rep("rare", 3),
              "microform")
)

# Join first level
funds <- left_join(
  funds,
  fund_level_1,
  by = "subfund"
)

funds <- funds %>% mutate(level_1 = if_else(is.na(level_1), "missing", level_1))

# Define levels 2 and 3
funds <- funds %>% mutate(level_2 = if_else(fund == "main", "books and digital", "periodicals"))
funds <- funds %>% mutate(level_3 = language)

# Subset bbk index strings (shelf indexes)
funds <- funds %>% mutate(shelf_1 = if_else(
  str_sub(shelf, 1, 5) == "26.89",
  "26.89",
  if_else(str_sub(shelf, 1, 1) %in% c("6", "7", "8"),
          str_sub(shelf, 1, 2), str_sub(shelf, 1, 1))
)) %>% 
  mutate(
    bbk_1 = if_else(
      str_sub(bbk, 1, 5) == "26.89",
      "26.89",
      if_else(str_sub(bbk, 1, 1) %in% c("6", "7", "8"),
              str_sub(bbk, 1, 2), str_sub(bbk, 1, 1)))) %>% mutate(theme = if_else(
                shelf_1 == "n", bbk_1, shelf_1
              ))

### Here should be loaded dataframe from authority file for 899$h and 686$a
# Dataframe of bbk indexes names
fund_level_4 <- data.frame(
  theme = c(
    "1", "2", "26.89", "3", "4", "5", "60", "63", "65", "66", "67", "68", "80", "81", "82", "83", "84",
    "85", "86", "87", "88", "71", "72", "74", "75", "76", "77", "78", "79", "9", "И", "Р"
  ),
  level_4 = c("general", "natural science", "regional geography", "technology", "farming", "medicine",
              "sociology", "history", "economics", "politics", "law", "warfare", "philology",
              "linguistics", "folklore", "literary studies", "fiction", "arts", "religion",
              "philosophy", "psychology", "culture", "science of science", "education", "sports",
              "media", "recreation", "libraries", "museums", "universal", "fiction", "fiction")
)

# Join bbk indexes
funds_leveled <- left_join(
  funds,
  fund_level_4,
  by = "theme"
) %>% select(id, bar, inv, date, level_1, level_2, level_3, level_4)

# Search for missing shelf indexes
funds_leveled <- funds_leveled %>% mutate(level_4 = if_else(is.na(level_4), "no data", level_4))

# Join bbk names by bbk indexes for missing shelf indexes by 686$a
funds_leveled <- funds_leveled %>% filter(level_4 == "no data") %>% 
  left_join(.,
            funds %>% select(id, bbk_1),
            by = "id") %>% distinct() %>% 
  select(id, theme = bbk_1) %>% distinct() %>% 
  left_join(., fund_level_4) %>% select(-theme) %>% select(id, level_4_1 = level_4) %>% 
  left_join(funds_leveled, ., by = "id") %>% 
  mutate(level_4_2 = if_else(level_4 == "no data", level_4_1, level_4)) %>% 
  select(id, bar, inv, date, level_1, level_2, level_3, level_4 = level_4_2) %>% 
  mutate(level_4 = if_else(is.na(level_4), "no data", level_4))

# Remove repeating rows
funds_leveled <- funds_leveled %>% distinct()

# Write down resulting dataframe
write_tsv(funds_leveled, "~/Comp_app/data/funds_leveled.csv")
