# testing elements of function to get full time series of behaviour transitions
# from already processed .dat data

library(tidyverse)

dat <- read_csv("2019-11-17_Testdata.csv") %>% select(-X1)

dat$ts <- round(dat$ts/100, digits = 0) # converting to secs/10
dat$ts_adj <- round(dat$ts_adj/100, digits = 0)

time <- as.data.frame(rep(1:18000)) %>%
  rename(ts_adj = 1)

sp1 <- dat %>% 
  filter(key != "EOF") %>%
  spread(monkey, value = key) %>% 
  arrange(ts_adj)

joined <- full_join(time, sp1, by = "ts_adj") %>% 
  select(-ts) %>% 
  fill(-ts_adj, .direction = "down") %>%
  fill(-ts_adj, .direction = "up")

latencies <- joined[!duplicated(joined$Ackbar),]
