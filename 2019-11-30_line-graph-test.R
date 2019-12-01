# New graphs with time blocks (timeline split, total approach score)


library(tidyverse)

fam3 <- read_csv("2019-11-26_Family3_timeline_1.csv") %>%
  select(-X1)

# going to try splitting into 10 sections and see how it looks

fam3_10 <- fam3 %>%
  filter(ts_adj != 1800) %>% # removing second at end to get divisible number
  mutate(index =
           rep(1:18, each = 1800 / 18))

fam_3_sum <- fam3_10 %>%
  group_by(index) %>%
  summarise_at(.vars = c("Rafeky", "Mountain", "Field", "Chalk", "Coal", "Malachite"),
               sum) 

fam3_long <- gather(fam_3_sum, key = "Monkey", value = "Approach", -index)

# fam_3_sum1 <- fam3_10 %>%
#   group_by(index) %>%
#   summarise_at(.vars = c("Rafeky", "Mountain", "Field", "Chalk", "Coal", "Malachite"),
#                sum) %>%
#   gather(key = "Monkey", value = "Approach", -index)
# 

ggplot(data = fam3_long, aes(x = index, y = Approach, color = Monkey)) +
  geom_point() +
  geom_line(size = 1)

# function to create line graphs with summarised blocks
# takes timeline data, in units of seconds
# set monkey names before you start
# split_into must be a multiple of 1800

monkeys <- c("Rafeky", "Mountain", "Field", "Chalk", "Coal", "Malachite")

line_graph <- function(data, split_into, monkey_names) {
  
  data_indexed <- data %>% 
    filter(ts_adj != 1800) %>% # get rid of end second
    mutate(index = # creates a new column for grouping
             rep(1:as.numeric(split_into), each = 1800 / as.numeric(split_into)))
  
  data_sum_long <- data_indexed %>%
    group_by(index) %>% # group by index column
    summarise_at(.vars = monkey_names, sum) %>% # sum of approach score
    gather(key = "Monkey", value = "Approach", -index)
  
  p <- ggplot(data = data_sum_long, aes(x = index, y = Approach, color = Monkey)) +
    geom_point() +
    geom_line(size = 1)
  
  return(p)
}

line_graph(data = fam3, split_into = 18, monkey_names = monkeys)

f3pilot <- read_csv("2019-11-22_Family3_timeline.csv") %>%
  select(-X1)

monkeyspilot <- c("Rafeky", "Shiba", "Mountain", "Field", "Chalk", "Coal", "Malachite")

line_graph(data = f3pilot, split_into = 18, monkey_names = monkeyspilot)

