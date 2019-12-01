# going to try to get timeline data collapsed to bouts
# then can see where monkeys were at each approach
# and how many approaches alone

library(tidyverse)

# UNIT IS HALF A SECOND

fam3_pilot <- read_csv("2019-11-27_Family3_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)

mountain <- fam3_pilot %>%
  mutate(transition =
           ifelse(
             test = (Mountain == lag(Mountain) &
                       Mountain == lead(Mountain)),
             yes =  0,
             no = 1
           )) %>%
  filter(transition == 1)
