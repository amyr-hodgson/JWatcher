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


DOB <- read_csv("2019-12-11_MonkeyDOB-testday.csv") %>%
  rename(test_day = headphones_testday)

# st_age <- function(df) {
#   
#   age <- df %>%
#     mutate(age = difftime(as.Date(test_day), as.Date(DOB), units = "weeks") /
#              52.25)
#   
#   dev <- age %>%
#     mutate(Stage = case_when(
#       (age * 12) > 20 ~ "Adult",
#       (age * 12) > 14 &
#         (age * 12) < 20 ~ "Late Adolescent",
#       (age * 12) < 14 & (age * 12) > 8 ~ "Early Adolescent",
#       (age * 12) > 4 & (age * 12) < 8 ~ "Older Infant",
#       (age * 12) < 4 ~ "Young Infant"
#     ))
#   
#   return(dev)
# }

st_age <- function(df) {
  
  age <- df %>%
    mutate(age = difftime(as.Date(test_day), as.Date(DOB), units = "weeks") /
             52.15,
           agemonths = difftime(as.Date(test_day), as.Date(DOB), units = "weeks")/ 4.345)
  
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

# could add over time

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

# ggplot(data = all_testday %>% filter(), 
#        aes(x = as.numeric(agemonths), y = adjusted_approach)) +
#   geom_point(aes(), size = 1.5) +#,color = Sex
#   geom_smooth(method = "lm", se = F, size = 1.2)+
#   geom_smooth(method = "loess", se = F, linetype = "dashed", color = 'darkred', size = 1.2) +
#   scale_x_continuous(breaks = seq(0, 115, 4), expand = c(0,0))+ 
#   scale_y_continuous(breaks = seq(0,4.5, .5)) +
#   geom_rect(aes(xmin = 0, xmax = 4, ymin = -Inf, ymax = Inf),
#             fill = "#4363d8", alpha = 0.01)+ 
#   geom_rect(aes(xmin = 4, xmax = 8, ymin = -Inf, ymax = Inf),
#             fill = "#3cb44b", alpha = 0.009)+ 
#   geom_rect(aes(xmin = 8, xmax = 14, ymin = -Inf, ymax = Inf),
#             fill = "#ffe119", alpha = 0.01)+ 
#   geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf),
#             fill = "red", alpha = 0.0095)+ #e6194B
#   geom_rect(aes(xmin = 14, xmax = 20, ymin = -Inf, ymax = Inf),
#             fill = "darkorange", alpha = 0.013) +
#   coord_cartesian(ylim = c(0, 4.2),xlim = c(0, 110)) +
#   labs(y = "Adjusted Approach Score", x = "Age (months)") +
#   theme_bw()+
#   theme(axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 11))

#mycol <- c("#e6194B", "#f58231", "#ffe119", "#3cb44b", "#4363d8")

ggplot(data = all_testday %>% filter(), 
       aes(x = as.numeric(agemonths), y = adjusted_approach)) +
  geom_point(aes(), size = 1.5) +#,color = Sex
  
  scale_x_continuous(breaks = seq(0, 115, 4), expand = c(0,0))+ 
  scale_y_continuous(breaks = seq(0,4.5, .5)) +
  annotate(geom = "rect", xmin=0, xmax=4, ymin=-Inf, ymax=Inf, alpha = 0.2,
            fill = "#4363d8")+ 
  annotate(geom = "rect", xmin=4, xmax=8, ymin=-Inf, ymax=Inf, alpha = 0.2,
            fill = "#3cb44b")+ 
  annotate(geom = "rect", xmin=8, xmax=14, ymin=-Inf, ymax=Inf, alpha = 0.2,
           fill = "#ffe119")+ 
  annotate(geom = "rect", xmin=14, xmax=20, ymin=-Inf, ymax=Inf, alpha = 0.2,
           fill = "#f58231")+
  annotate(geom = "rect", xmin=20, xmax=Inf, ymin=-Inf, ymax=Inf, alpha = 0.2,
           fill = "#e6194B")+
  geom_smooth(method = "lm", se = F, size = 1.2)+
  geom_smooth(method = "loess", se = F, linetype = "dashed", color = 'darkred', size = 1.2) +
  coord_cartesian(ylim = c(0, 4.2),xlim = c(0, 110)) +
  labs(y = "Adjusted Approach Score", x = "Age (months)") +
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11))
#library(cairoDevice)

#ggsave(filename = "approach.png", width = 9, height = 6, dpi = 300, device = Cairo_png)
ggsave(filename = "approach1.png", width = 9, height = 6, dpi = 300, type = "cairo")


ggplot(data = all_testday %>% filter(Family !="3"), 
       aes(x = as.numeric(agemonths), y = adjusted_approach)) +
  geom_point(aes(), size = 1.5) +#,color = Sex
  geom_smooth(method = "lm", se = F, size = 1.2)+
  geom_smooth(method = "loess", se = F, linetype = "dashed", color = 'darkred', size = 1.2) +
  scale_x_continuous(breaks = seq(0, 115, 4), expand = c(0,0))+ 
  scale_y_continuous(breaks = seq(0,4.5, .5)) +
  geom_rect(aes(xmin = 0, xmax = 4, ymin = -Inf, ymax = Inf),
            fill = "#4363d8", alpha = 0.008)+ 
  geom_rect(aes(xmin = 4, xmax = 8, ymin = -Inf, ymax = Inf),
            fill = "#3cb44b", alpha = 0.009)+ 
  geom_rect(aes(xmin = 8, xmax = 14, ymin = -Inf, ymax = Inf),
            fill = "#ffe119", alpha = 0.009)+ 
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "#e6194B", alpha = 0.009)+ 
  geom_rect(aes(xmin = 14, xmax = 20, ymin = -Inf, ymax = Inf),
            fill = "#f58231", alpha = 0.01) +
  coord_cartesian(ylim = c(0, 3),xlim = c(0, 68)) +
  theme_bw()

ggplot(data = all_testday %>% filter(adjusted_approach<5), 
       aes(x = as.numeric(agemonths), y = adjusted_approach)) +
  geom_point(aes()) +#color = Sex
  geom_smooth(method = "loess", se = F)+
  scale_x_continuous(breaks = seq(0, 110, 4))

# cor(as.numeric(all_testday_no3$age), all_testday_no3$adjusted_approach)
# summary(lm(all_testday_no3$adjusted_approach ~ as.numeric(all_testday_no3$age)))
#

# myColorss <- c("#e6194B", "#4363d8")
# names(myColorss) <- levels(all_testday$Sex)
# colScales <- scale_colour_manual(name = "Sex",values = myColorss)

# 
mycol <- c("#e6194B", "#f58231", "#ffe119", "#3cb44b", "#4363d8")

famcol <- c("#800000", "#808000", "#469990", "#000075", "#911eb4")
famcol2 <- c("#fabebe", "#ffd8b1", "#fffac8", "#aaffc3", "#e6beff")

ggplot(data = all_testday %>% filter(age<10), 
       aes(x = as.numeric(agemonths), y = adjusted_approach, fill = Sex, colour = Sex)) +
  geom_point(aes(shape = Family),  size = 2.5,color = 'black', alpha = .6) +#
  geom_smooth(method = "lm", se = F, size = 1.2, alpha = .3,aes())+
  scale_x_continuous(breaks = seq(0, 110, 4), expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,4.5, .5)) +
  coord_cartesian(ylim = c(0, 4.2),xlim = c(0, 110)) +
  labs(y = "Adjusted Approach Score", x = "Age (months)") +
  scale_shape_manual(values=c(21,22,23,24,25))+#15,16,17,18,19
 # scale_fill_manual(values = mycol) +
  theme_grey()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)) #+ colScales

ggsave(filename = "sexdiffshape.png", width = 10, height = 6, dpi = 300, type = "cairo")

ggplot(data = all_testday %>% filter(age<10), 
       aes(x = as.numeric(age), y = adjusted_approach,color = Family)) +
  geom_point(aes()) +
  geom_smooth(method = "lm", se = F)+
  scale_x_continuous(breaks = seq(0, 9, .5))

############################
# Rafeky's group taken out #
############################

all_testday_no3 <- all_testday %>% filter(Family != 3)

all_testday_no3$Family <- as.factor(all_testday_no3$Family)

ggplot(data = all_testday_no3, 
       aes(x = as.numeric(agemonths), y = adjusted_approach)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  scale_x_continuous(breaks = seq(0, 110, 2))

ggplot(data = all_testday_no3%>% filter(age<10), 
       aes(x = as.numeric(agemonths), y = adjusted_approach, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(breaks = seq(0, 110, 2))

ggplot(data = all_testday_no3%>% filter(age<10), 
       aes(x = as.numeric(agemonths), y = adjusted_approach, color = Family)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(breaks = seq(0, 110, 2))
