# Need a function that does proportion of approaches that were alone
# Rather than just number of approaches alone


library(tidyverse)

DOB <- read_csv("2019-12-01_MonkeyDOB-testday.csv") %>%
  rename(name = Monkey)

# UNIT IS HALF A SECOND

fam3_pilot_h <- read_csv("2019-11-27_Family3_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)

data <- fam3_pilot_h
column <- fam3_pilot_h$Field
monkey_name <- 'Field'
split_into <- 6
level <- 2

# takes halfsec timeline data
# level means 1 = shelf, 2 = radius, 3 = touch

approach_alone <- function(data, column, monkey_name, split_into, level) {
  
  name <- as.character(monkey_name)
  column <- head(column, -1)
  
  indexed <- data %>% 
    filter(ts_adj != 3600) %>% # get rid of end second
    mutate(index = # creates a new column for grouping
             rep(1:as.numeric(split_into), each = 3600 / as.numeric(split_into)))
  
  alone_shelf <- indexed %>%
    select(-scorer, -button, -ses, -family, -test, - object, -button) %>%
    filter(column > lag(column) & column == as.numeric(level)) %>%
    filter_at(vars(-ts_adj, -name, -index), all_vars(. == 0)) %>%
    select(index, ts_adj)
  
  return(alone_shelf)
}

approach_total <- function(data, column, monkey_name, split_into, level) {
  
  name <- as.character(monkey_name)
  column <- head(column, -1)
  
  indexed <- data %>% 
    filter(ts_adj != 3600) %>% # get rid of end second
    mutate(index = # creates a new column for grouping
             rep(1:as.numeric(split_into), each = 3600 / as.numeric(split_into))) %>%
    select(-scorer, -button, -ses, -family, -test, - object, -button)
  
  keep_index <- data.frame("index" = rep(1:as.numeric(split_into)))
  keep_index$index <- as.character(keep_index$index)
  
  total_app <- indexed %>%
    filter(column > lag(column) & column == as.numeric(level))
  
  total_app_short <- total_app %>%
    select(index, ts_adj)
  total_app_short$index <- as.factor(total_app_short$index)
  
  total_tally <- total_app_short %>%
    group_by(index, .drop = F) %>%
    tally(name = "total") %>%
    arrange(index)#%>%
   # drop_na(index)
  
  alone_shelf <- total_app %>%
    filter_at(vars(-ts_adj, -name, -index), all_vars(. == 0)) %>%
    select(index, ts_adj)
  alone_shelf$index <- as.factor(alone_shelf$index)
  
  alone_tally <- alone_shelf %>%
    group_by(index, .drop = F) %>%
    tally(name = "alone") %>%
    arrange(index) #%>%
    #drop_na(index)
  
  full_tally <- full_join(total_tally, alone_tally, by = "index")
  full_tally$index <- as.character(full_tally$index)
  full_tally_index <- full_join(full_tally, keep_index, by = "index")
  
  return(full_tally_index)
}

mountain <- approach_total(fam3_pilot_h, 
                           fam3_pilot_h$Mountain,
                           "Mountain",
                           6,
                           1)

rafeky <- approach_total(fam3_pilot_h, 
                           fam3_pilot_h$Rafeky,
                           "Rafeky",
                           6,
                           1) %>%
  replace_na(list(total = 0, alone = 0))

field <- approach_total(fam3_pilot_h, 
                         fam3_pilot_h$Field,
                         "Field",
                         split_into = 6,
                         level = 2)

#joined <- full_join(total_shelf, alone_shelf, by = c("index", "ts_adj"))

# alone_tally <- alone_shelf %>%
#   group_by(index, .drop = F) %>%
#   tally(name = "alone") %>%
#   arrange(index) %>%
#   drop_na(index) 
# 
# total_tally <- total_shelf %>%
#   group_by(index, .drop = F) %>%
#   tally(name = "total") %>%
#   arrange(index) %>%
#   drop_na(index)
# 
# full_tally <- full_join(alone_tally, total_tally, by = "index")
# 
# test <- data.frame("index" = rep(1:8))
# test1 <- full_join(full_tally, test, by = c("index"))
# 
# 
# alone_tally1 <- alone_shelf %>%
#   group_by(index, .drop = F) %>%
#   tally() %>%
#   arrange(index)

mountain <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Mountain,
    monkey_name = "Mountain",
    split_into = 6,
    level = 1
  )