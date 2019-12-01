# going to try to get timeline data collapsed to bouts
# then can see where monkeys were at each approach
# and how many approaches alone

library(tidyverse)

# UNIT IS HALF A SECOND

fam3_pilot_h <- read_csv("2019-11-27_Family3_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)

# below gives on and off times for each transition

mountain <- fam3_pilot_h %>%
  mutate(transition =
           ifelse(
             test = (Mountain == lag(Mountain) &
                       Mountain == lead(Mountain)),
             yes =  0,
             no = 1
           )) %>%
  filter(transition == 1)

# want to get approaches alone. first to shelf
 
# m <- as.character(m)
# other <- fam3_pilot_h %>%
#   select(-scorer, -button, -ses, -family, -test, - object, -button, -ts_adj, -m)
# n <- names(other)

# others <- fam3_pilot_h %>%
#   select(-scorer, -button, -ses, -family, -test, - object, -button) 
# 
# test <-
#   others %>% 
#   filter(Mountain != lag(Mountain) & Mountain == 1) %>%
#   filter_at(vars(-ts_adj, -Mountain), .vars_predicate = all_vars(. == 0))

# now need to add time bins

m <- "Mountain" 
split_into <- 18
 
mountain_indexed <- fam3_pilot_h %>% 
   filter(ts_adj != 3600) %>% # get rid of end second
   mutate(index = # creates a new column for grouping
            rep(1:as.numeric(split_into), each = 3600 / as.numeric(split_into)))
 
mountain_alone_shelf <- mountain_indexed %>%
  select(-scorer, -button, -ses, -family, -test, - object, -button) %>%
  filter(Mountain != lag(Mountain) & Mountain == 1) %>%
  filter_at(vars(-ts_adj, -m, -index), all_vars(. == 0))

mountain_count <- mountain_alone_shelf %>% group_by(index) %>% tally()

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
    filter(column != lag(column) & column == as.numeric(level)) %>%
    filter_at(vars(-ts_adj, -name, -index), all_vars(. == 0))
  
  return(alone_shelf)
}


mountain_alone <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Mountain,
    monkey_name = "Mountain",
    split_into = 8,
    level = 1
  )
malachite_alone <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Malachite,
    monkey_name = "Malachite",
    split_into = 10,
    level = 1
  )
