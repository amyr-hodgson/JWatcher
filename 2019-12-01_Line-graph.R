# New line graph
# Will probably be better to have one function for preparing data
# Another for the graph

library(tidyverse)

DOB <- read_csv("2019-12-01_MonkeyDOB.csv")

st_age <- function(df) {
  
  age <- df %>%
    mutate(age = difftime(as.Date(test_day), as.Date(DOB), units = "weeks") /
             52.25)
  
  dev <- age %>%
    mutate(Stage = case_when(
      (age * 12) > 20 ~ "Adult",
      (age * 12) > 14 &
        (age * 12) < 20 ~ "Late Adolescent",
      (age * 12) < 14 & (age * 12) > 8 ~ "Early Adolescent",
      (age * 12) > 4 & (age * 12) < 8 ~ "Older Infant",
      (age * 12) < 4 ~ "Young Infant"
    ))
  
  return(dev)
}

time_graph <- function(data) {
p <- ggplot(data = data, aes(
  x = index,
  y = Approach,
  color = Stage,
  group = Monkey
)) + 
  geom_point(colour = 'black', size = .8) +
  geom_point(size = .4) +
  geom_line(color = 'black', size = 1.5) +
  geom_line(size = 1.1) +
  colScale +
#  theme_bw()+
  labs(x = "Time Interval", y = "Approach Score")+
  coord_cartesian(xlim = c(1, as.numeric(max(data$index)))) +
  scale_x_continuous(breaks = seq(0, as.numeric(max(data$index)), 1))

return(p)
}

line_prep <- function(data, split_into, monkey_names, DOB, testday) {
  
  data_indexed <- data %>% 
    filter(ts_adj != 1800) %>% # get rid of end second
    mutate(index = # creates a new column for grouping
             rep(1:as.numeric(split_into), each = 1800 / as.numeric(split_into)))
  
  data_sum_long <- data_indexed %>%
    group_by(index) %>% # group by index column
    summarise_at(.vars = monkey_names, sum) %>% # sum of approach score
    gather(key = "Monkey", value = "Approach", -index)
  
  data_sum_DOB <- inner_join(data_sum_long, DOB, by = "Monkey") %>%
    mutate(test_day = as.Date(testday)) %>%
    st_age()
  
  data_sum_DOB$Stage <- factor(
    data_sum_DOB$Stage,
    levels = c("Adult", "Late Adolescent", "Early Adolescent", "Older Infant", "Young Infant"),
    labels = c(
      "Adult",
      "Late Adol.",
      "Early Adol.",
      "Older Infant",
      "Young Infant"
    )
  )
  
  return(data_sum_DOB)
  
}


mycol <- c("#e6194B", "#f58231", "#ffe119", "#3cb44b", "#4363d8")

fam3 <- read_csv("2019-11-26_Family3_timeline_1.csv") %>%
  select(-X1)
fam3_pilot <- read_csv("2019-11-22_Family3_timeline.csv") %>%
  select(-X1)
fam4 <- read_csv("2019-11-28_Family4_timeline_sec.csv") %>%
  select(-X1)
fam5 <- read_csv("2019-11-22_Family5_timeline.csv") %>%
  select(-X1)
fam6 <- read_csv("2019-11-22_Family6_timeline.csv") %>%
  select(-X1)

monkeys3 <- c("Rafeky", "Mountain", "Field", "Chalk", "Coal", "Malachite")

f3 <- line_prep(fam3, 
                split_into = 18, 
                monkey_names = monkeys3, 
                DOB = DOB,
                testday = "2019-11-19")

names(mycol) <- levels(f3$Stage)
colScale <- scale_color_manual(name = "Stage", values = mycol)

time_graph(f3)

monkeys3p <- c("Rafeky", "Shiba", "Mountain", "Field", "Chalk", "Coal", "Malachite")

f3p <- line_prep(fam3_pilot, 
                split_into = 18, 
                monkey_names = monkeys3p, 
                DOB = DOB,
                testday = "2019-09-12")

names(mycol) <- levels(f3p$Stage)

time_graph(f3p)


monkeys4 <- c("Chunk", "Slim", "Saluki", "Samoyed", "Obsidian", "Onyx", "Rock", "Mineral")

f4 <- line_prep(fam4, 
                 split_into = 18, 
                 monkey_names = monkeys4, 
                 DOB = DOB,
                 testday = "2019-09-19")

names(mycol) <- levels(f4$Stage)

time_graph(f4)

monkeys5 <- c("Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz")

f5 <- line_prep(fam5, 
                split_into = 18, 
                monkey_names = monkeys5, 
                DOB = DOB,
                testday = "2019-09-26")

names(mycol) <- levels(f5$Stage)

time_graph(f5)

monkeys6 <- c("Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja")

f6 <- line_prep(fam6, 
                split_into = 18, 
                monkey_names = monkeys6, 
                DOB = DOB,
                testday = "2019-10-24")

names(mycol) <- levels(f6$Stage)

time_graph(f6)
