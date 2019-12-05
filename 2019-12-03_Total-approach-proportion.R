#
# Want to calculate total approach score for each animal divided by total group
# activity divided by number of monkeys in the group (to account for the fact
# that larger groups will have a higher total activity score)
#
# Then going to correlate this with age in months
# 
# Formula:
#
# score = approach score / (total group approach score / number of monkeys in the group)

# Required packages

library(tidyverse)

# Read in data, add test day which should have been there from the start...

fam3 <- read_csv("2019-11-26_Family3_timeline_1.csv") %>%
  select(-X1) %>%
  mutate(testday = "2019-11-19")

fam3_pilot <- read_csv("2019-11-22_Family3_timeline.csv") %>%
  select(-X1) %>%
  mutate(testday = "2019-09-12")

fam4 <- read_csv("2019-11-28_Family4_timeline_sec.csv") %>%
  select(-X1)%>%
  mutate(testday = "2019-09-19")

fam5 <- read_csv("2019-11-22_Family5_timeline.csv") %>%
  select(-X1) %>%
  mutate(testday = "2019-09-26")

fam6 <- read_csv("2019-11-22_Family6_timeline.csv") %>%
  select(-X1) %>%
  mutate(testday = "2019-10-24")

fam9 <- read_csv("2019-12-02_Family9_timeline_sec.csv") %>%
  select(-X1) %>%
  mutate(testday = "2019-11-21")


DOB <- read_csv("2019-12-01_MonkeyDOB-testday.csv") %>%
  rename(test_day = headphones_testday)

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

monkeys3 <- c("Rafeky", "Shiba", "Mountain", "Field", "Chalk", "Coal", "Malachite")
monkeys4 <- c("Chunk", "Slim", "Saluki", "Samoyed", "Obsidian", "Onyx", "Rock", "Mineral")
monkeys5 <- c("Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz")
monkeys6 <- c("Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja")
monkeys9 <- c("Puerto", "Napa", "Arsenic", "Asbestos", "Fusion", "Flux")

# Want to have: 
#
# - Total activity for each animal
# - Total group activity
# - Number of animals in the group

# tot_approach <- fam3_pilot %>% 
#   summarise_at(.vars = monkey_names, sum) %>% # sum of approach score
#   gather(key = "Monkey", value = "Approach") %>%
#   mutate(
#     total_group = sum(Approach),
#     group_size = n(),
#     adjusted_total = total_group/group_size,
#     adjusted_approach = Approach/adjusted_total)

total_approach <- function(data, monkey_names) {
  output <- data %>% 
    summarise_at(.vars = monkey_names, sum) %>% # sum of approach score
    gather(key = "Monkey", value = "Approach") %>%
    mutate(
      total_group = sum(Approach),
      group_size = n(),
      adjusted_total = total_group/group_size,
      adjusted_approach = Approach/adjusted_total)
  
  return(output)
}

group3 <- total_approach(fam3_pilot, monkeys3)
group4 <- total_approach(fam4, monkeys4)
group5 <- total_approach(fam5, monkeys5)
group6 <- total_approach(fam6, monkeys6)
group9 <- total_approach(fam9, monkeys9)

all_groups <- bind_rows("3" = group3, "4" = group4, "5" = group5, "6" = group6, "9" = group9)

all_testday <- inner_join(all_groups, DOB, by = "Monkey") %>% 
  st_age() 

all_testday$Family <- as.factor(all_testday$Family)

###############
# All animals #
###############

ggplot(data = all_testday, 
       aes(x = as.numeric(age), y = adjusted_approach)) +
  geom_point(aes()) +#color = Sex
  geom_smooth(method = "lm", se = F)+
  scale_x_continuous(breaks = seq(0, 9, .5))

ggplot(data = all_testday, 
       aes(x = as.numeric(age), y = adjusted_approach)) +
  geom_point(aes()) +#color = Sex
  geom_smooth(method = "loess", se = F)+
  scale_x_continuous(breaks = seq(0, 9, .5))

R# cor(as.numeric(all_testday_no3$age), all_testday_no3$adjusted_approach)
# summary(lm(all_testday_no3$adjusted_approach ~ as.numeric(all_testday_no3$age)))

ggplot(data = all_testday, 
       aes(x = as.numeric(age), y = adjusted_approach,color = Sex)) +
  geom_point(aes()) +
  geom_smooth(method = "lm", se = F)+
  scale_x_continuous(breaks = seq(0, 9, .5))

############################
# Rafeky's group taken out #
############################

all_testday_no3 <- all_testday %>% filter(Family != 3)

all_testday_no3$Family <- as.factor(all_testday_no3$Family)

ggplot(data = all_testday_no3, 
       aes(x = as.numeric(age), y = adjusted_approach)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  scale_x_continuous(breaks = seq(0, 9, .5))

ggplot(data = all_testday_no3, 
       aes(x = as.numeric(age), y = adjusted_approach, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(breaks = seq(0, 9, .5))
