library(tidyverse)

funds <- read_tsv("~/Comp_app/data/funds_df_5.csv")

fields <- c("010", "200", "210", "700", "899")

funds <- funds %>% filter(field %in% fields)

funds_010 <- funds %>% filter(field == "010")

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

var <- subfield_extractor(funds_010$var, "a")
funds_010 <- data.frame(
  id = funds_010$id,
  isbn = var
)

funds_200 <- funds %>% filter(field == "200")
var <- subfield_extractor(funds_200$var, "a")
funds_200_a <- data.frame(
  id = funds_200$id,
  Title = var
)

var <- subfield_extractor(funds_200$var, "e")
funds_200_e <- data.frame(
  id = funds_200$id,
  Subtitle = var
)

var <- subfield_extractor(funds_200$var, "f")
funds_200_f <- data.frame(
  id = funds_200$id,
  Author = var
)

funds_210 <- funds %>% filter(field == "210")
var <- subfield_extractor(funds_210$var, "c")
funds_210_c <- data.frame(
  id = funds_210$id,
  Publisher = var
)

var <- subfield_extractor(funds_210$var, "d")
funds_210_d <- data.frame(
  id = funds_210$id,
  Year_pub = var
)

funds_700 <- funds %>% filter(field == "700")
var <- subfield_extractor(funds_700$var, "a")
funds_700_a <- data.frame(
  id = funds_700$id,
  Surname = var
)

var <- subfield_extractor(funds_700$var, "b")
funds_700_b <- data.frame(
  id = funds_700$id,
  Initials = var
)

funds_899 <- funds %>% filter(field == "899")
var <- subfield_extractor(funds_899$var, "p")
funds_899_p <- data.frame(
  id = funds_899$id,
  Barcode = var
)

var <- subfield_extractor(funds_899$var, "x")
funds_899_x <- data.frame(
  id = funds_899$id,
  Inv = var
)

df <- full_join(funds_010, funds_200_a) %>% distinct() %>% 
  full_join(., funds_200_e) %>% distinct() %>% full_join(., funds_200_f) %>% distinct() %>% 
  full_join(., funds_210_c) %>% distinct() %>% full_join(., funds_210_d) %>% distinct() %>% 
  full_join(., funds_700_a) %>% distinct() %>% full_join(., funds_700_b) %>% distinct() %>% 
  full_join(., funds_899_p) %>% distinct() %>% full_join(., funds_899_x) %>% distinct()

write_tsv(df, "~/Comp_app/data/price.csv")
