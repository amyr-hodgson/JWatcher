# Function that does proportion of approaches that were alone
# Rather than just number of approaches alone

library(tidyverse)

DOB <- read_csv("2019-12-11_MonkeyDOB-testday.csv") %>%
  rename(name = Monkey, test_day = headphones_testday)

prop_alone <- function(dat, replace = NA, replace_prop = NA) {
  proportion <- dat %>%
    mutate(prop = alone/total) %>%
    replace_na(list(total = replace, 
                    alone = replace, 
                    prop = replace_prop, 
                    adjusted_prop = replace_prop,
                    app = replace_prop))
  return(proportion)
}

# UNIT IS HALF A SECOND

fam3_pilot_h <- read_csv("2019-11-27_Family3_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)
fam4 <- read_csv("2019-12-01_Family4_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)
fam5 <- read_csv("2019-12-01_Family5_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)
fam6 <- read_csv("2019-12-01_Family6_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)
fam9 <- read_csv("2019-12-02_Family9_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)

# data <- fam3_pilot_h
# column <- fam3_pilot_h$Malachite
# monkey_name <- 'Malachite'
# split_into <- 6
# level <- 1

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
    #drop_na(index)
  
  alone_shelf <- total_app %>%
    filter_at(vars(-ts_adj, -name, -index), all_vars(. == 0)) %>%
    select(index, ts_adj)
  alone_shelf$index <- as.factor(alone_shelf$index)
  
  alone_tally <- alone_shelf %>%
    group_by(index, .drop = F) %>%
    tally(name = "alone") %>%
    arrange(index) #%>%
    #drop_na(index)
  
  total_tally$index <- as.character(total_tally$index)
  alone_tally$index <- as.character(alone_tally$index)
  full_tally <- full_join(total_tally, alone_tally, by = "index")
  full_tally_index <- full_join(full_tally, keep_index, by = "index")
  
  return(full_tally_index)
}

############
# Family 3 #
############

# mountain <-
#   approach_total(
#     fam3_pilot_h,
#     column = fam3_pilot_h$Mountain,
#     monkey_name = "Mountain",
#     split_into = 6,
#     level = 1
#   )

mega_fam3 <- function(dat, split_into, level) {
  mountain <-
    approach_total(
      dat,
      column = dat$Mountain,
      monkey_name = "Mountain",
      split_into = split_into,
      level = level
    )
  malachite <-
    approach_total(
      dat,
      column = dat$Malachite,
      monkey_name = "Malachite",
      split_into = split_into,
      level = level
    )
  chalk <-
    approach_total(
      dat,
      column = dat$Chalk,
      monkey_name = "Chalk",
      split_into = split_into,
      level = level
    )
  coal <-
    approach_total(
      dat,
      column = dat$Coal,
      monkey_name = "Coal",
      split_into = split_into,
      level = level
    )
  field <-
    approach_total(
      dat,
      column = dat$Field,
      monkey_name = "Field",
      split_into = split_into,
      level = level
    )
  rafeky <-
    approach_total(
      dat,
      column = dat$Rafeky,
      monkey_name = "Rafeky",
      split_into = split_into,
      level = level
    )
  shiba <-
    approach_total(
      dat,
      column = dat$Shiba,
      monkey_name = "Shiba",
      split_into = split_into,
      level = level
    )
  
  all <-
    bind_rows(
      "Malachite" = malachite,
      "Mountain" = mountain,
      "Field" = field,
      "Chalk" = chalk,
      "Coal" = coal,
      "Shiba" = shiba,
      "Rafeky" = rafeky,
      .id = "name"
    )
  return(all)
  
}

fam3_split6_shelf <- mega_fam3(fam3_pilot_h, split_into = 6, level = 1)
fam3_nosplit_shelf <- mega_fam3(fam3_pilot_h, split_into = 1, level = 1)
fam3_nosplit_radius <- mega_fam3(fam3_pilot_h, split_into = 1, level = 2)
fam3_nosplit_touch <- mega_fam3(fam3_pilot_h, split_into = 1, level = 3)


# all_prop <- all %>%
#   mutate(prop = alone/total) %>%
#   replace_na(list(total = 0, alone = 0, prop = 0))

############
# Family 4 #
############

mega_fam4 <- function(dat, split_into, level){
  
  slim <-
    approach_total(
      dat,
      column = dat$Slim,
      monkey_name = "Slim",
      split_into = split_into,
      level = level
    )
  chunk <-
    approach_total(
      dat,
      column = dat$Chunk,
      monkey_name = "Chunk",
      split_into = split_into,
      level = level
    )
  saluki <-
    approach_total(
      dat,
      column = dat$Saluki,
      monkey_name = "Saluki",
      split_into = split_into,
      level = level
    )
  samoyed <-
    approach_total(
      dat,
      column = dat$Samoyed,
      monkey_name = "Samoyed",
      split_into = split_into,
      level = level
    )
  onyx <-
    approach_total(
      dat,
      column = dat$Onyx,
      monkey_name = "Onyx",
      split_into = split_into,
      level = level
    )
  obsidian <-
    approach_total(
      dat,
      column = dat$Obsidian,
      monkey_name = "Obsidian",
      split_into = split_into,
      level = level
    )
  rock <-
    approach_total(
      dat,
      column = dat$Rock,
      monkey_name = "Rock",
      split_into = split_into,
      level = level
    )
  mineral <-
    approach_total(
      dat,
      column = dat$Mineral,
      monkey_name = "Mineral",
      split_into = split_into,
      level = level
    )
  all <-
    bind_rows(
      "Slim" = slim,
      "Chunk" = chunk,
      "Saluki" = saluki,
      "Samoyed" = samoyed,
      "Onyx" = onyx,
      "Obsidian" = obsidian,
      "Rock" = rock,
      "Mineral" = mineral,
      .id = "name"
    )
  return(all)
}

fam4_split6_shelf <- mega_fam4(fam4, split_into = 6, level = 1)
fam4_nosplit_shelf <- mega_fam4(dat = fam4, split_into = 1, level = 1)
fam4_nosplit_radius <- mega_fam4(fam4, split_into = 1, level = 2)
fam4_nosplit_touch <- mega_fam4(fam4, split_into = 1, level = 3)

############
# Family 5 #
############

mega_fam5 <- function(dat, split_into, level){
  
alderaan <-
  approach_total(
    dat,
    column = dat$Alderaan,
    monkey_name = "Alderaan",
    split_into = split_into,
    level = level
  )
scout <-
  approach_total(
    dat,
    column = dat$Scout,
    monkey_name = "Scout",
    split_into = split_into,
    level = level
  )
quantum <-
  approach_total(
    dat,
    column = dat$Quantum,
    monkey_name = "Quantum",
    split_into = split_into,
    level = level
  )
quartz <-
  approach_total(
    dat,
    column = dat$Quartz,
    monkey_name = "Quartz",
    split_into = split_into,
    level = level
  )
zinc <-
  approach_total(
    dat,
    column = dat$Zinc,
    monkey_name = "Zinc",
    split_into = split_into,
    level = level
  )
zircon <-
  approach_total(
    dat,
    column = dat$Zircon,
    monkey_name = "Zircon",
    split_into = split_into,
    level = level
  )

all <-
  bind_rows(
    "Alderaan" = alderaan,
    "Scout" = scout,
    "Quantum" = quantum,
    "Quartz" = quartz,
    "Zinc" = zinc,
    "Zircon" = zircon,
    .id = "name"
  )
return(all)
}

fam5_split6_shelf <- mega_fam5(fam5, split_into = 6, level = 1)
fam5_nosplit_shelf <- mega_fam5(fam5, split_into = 1, level = 1)
fam5_nosplit_radius <- mega_fam5(fam5, split_into = 1, level = 2)
fam5_nosplit_touch <- mega_fam5(fam5, split_into = 1, level = 3)

############
# Family 6 #
############

mega_fam6 <- function(dat, split_into, level) {
  
  ackbar <-
    approach_total(
      dat,
      column = dat$Ackbar,
      monkey_name = "Ackbar",
      split_into = split_into,
      level = level
    )
  bouncer <-
    approach_total(
      dat,
      column = dat$Bouncer,
      monkey_name = "Bouncer",
      split_into = split_into,
      level = level
    )
  spaniel <-
    approach_total(
      dat,
      column = dat$Spaniel,
      monkey_name = "Spaniel",
      split_into = split_into,
      level = level
    )
  papillon <-
    approach_total(
      dat,
      column = dat$Papillon,
      monkey_name = "Papillon",
      split_into = split_into,
      level = level
    )
  poodle <-
    approach_total(
      dat,
      column = dat$Poodle,
      monkey_name = "Poodle",
      split_into = split_into,
      level = level
    )
  nugget <-
    approach_total(
      dat,
      column = dat$Nugget,
      monkey_name = "Nugget",
      split_into = split_into,
      level = level
    )
  ninja <-
    approach_total(
      dat,
      column = dat$Ninja,
      monkey_name = "Ninja",
      split_into = split_into,
      level = level
    )
  
  all <-
    bind_rows(
      "Ackbar" = ackbar,
      "Bouncer" = bouncer,
      "Spaniel" = spaniel,
      "Papillon" = papillon,
      "Poodle" = poodle,
      "Nugget" = nugget,
      "Ninja" = ninja,
      .id = "name"
    )
  return(all)
}

fam6_split6_shelf <- mega_fam6(fam6, split_into = 6, level = 1)
fam6_nosplit_shelf <- mega_fam6(fam6, split_into = 1, level = 1)
fam6_nosplit_radius <- mega_fam6(fam6, split_into = 1, level = 2)
fam6_nosplit_touch <- mega_fam6(fam6, split_into = 1, level = 3)

############
# Family 9 #
############

mega_fam9 <- function(dat, split_into, level) {
  
  puerto <-
    approach_total(
      dat,
      column = dat$Puerto,
      monkey_name = "Puerto",
      split_into = split_into,
      level = level
    )
  napa <-
    approach_total(
      dat,
      column = dat$Napa,
      monkey_name = "Napa",
      split_into = split_into,
      level = level
    )
  arsenic <-
    approach_total(
      dat,
      column = dat$Arsenic,
      monkey_name = "Arsenic",
      split_into = split_into,
      level = level
    )
  asbestos <-
    approach_total(
      dat,
      column = dat$Asbestos,
      monkey_name = "Asbestos",
      split_into = split_into,
      level = level
    )
  fusion <-
    approach_total(
      dat,
      column = dat$Fusion,
      monkey_name = "Fusion",
      split_into = split_into,
      level = level
    )
  flux <-
    approach_total(
      dat,
      column = dat$Flux,
      monkey_name = "Flux",
      split_into = split_into,
      level = level
    )
  
  all <-
    bind_rows(
      "Puerto" = puerto,
      "Napa" = napa,
      "Arsenic" = arsenic,
      "Asbestos" = asbestos,
      "Fusion" = fusion,
      "Flux" = flux,
      .id = "name"
    )
  return(all)
}

fam9_split6_shelf <- mega_fam9(fam9, split_into = 6, level = 1)
fam9_nosplit_shelf <- mega_fam9(fam9, split_into = 1, level = 1)
fam9_nosplit_radius <- mega_fam9(fam9, split_into = 1, level = 2)
fam9_nosplit_touch <- mega_fam9(fam9, split_into = 1, level = 3)

###############
# Pictures :) #
###############
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

DOB$Family <- as.character(DOB$Family)

nosplit_shelf <-
  bind_rows(
    "3" = fam3_nosplit_shelf,
    "4" = fam4_nosplit_shelf,
    "5" = fam5_nosplit_shelf,
    "6" = fam6_nosplit_shelf,
    "9" = fam9_nosplit_shelf,
    .id = "Family"
  ) %>%
  mutate(prop = alone/total) %>%
 # prop_alone(replace = 0) %>%
  inner_join(DOB, by = c("name", "Family")) %>%
  st_age()


nosplit_shelf$name <- as.factor(nosplit_shelf$name)
nosplit_shelf$Family <- as.factor(nosplit_shelf$Family)

ggplot(data = nosplit_shelf, aes(x = Family, y = prop, fill = name)) +
 # geom_line(position = position_dodge(0), size = 1.2, lineend = 'round') +
 # geom_density(aes(y = total)) +
  geom_col(position = "dodge") +
 # geom_point(position = position_dodge(.2), aes(y = total)) +
  # geom_smooth(method = 'lm', se = F) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = nosplit_shelf, aes(x = as.numeric(age), y = prop)) +
  geom_point(aes(color = Family.x)) +
  geom_smooth(method = 'lm', se = F)+
  scale_x_continuous(breaks = seq(0, 9, 1))

young <- nosplit_shelf %>% filter(age < 6)

ggplot(data = young, aes(x = as.numeric(age), y = prop)) +
  geom_point(aes(color = Family.x)) +
  geom_smooth(method = 'lm', se = F)+
  scale_x_continuous(breaks = seq(0, 9, 1))

ggplot(data = nosplit_shelf, aes(x = name, y = total, group = name, fill = name)) +
  # geom_line(position = position_dodge(0), size = 1.2, lineend = 'round') +
  geom_col(position = position_dodge(.2))

# weight by opportunity somehow? 


time_unoccupied <- function(dat, split_into) {
  
indexed <- dat %>% 
  filter(ts_adj != 3600) %>% # get rid of end second
  mutate(index = # creates a new column for grouping
           rep(1:as.numeric(split_into), each = 3600 / as.numeric(split_into))) %>%
  select(-scorer, -button, -ses, -family, -test, - object, -button)

unoccupied <- indexed %>%
  filter_at(vars(-ts_adj, -index), all_vars(. == 0)) %>%
  select(index, ts_adj)

counted <- unoccupied %>%
  group_by(index) %>%
  tally(name = "nobody")

return(counted)
}

unoccupied3 <- time_unoccupied(fam3_pilot_h, split_into = 1)
unoccupied4 <- time_unoccupied(fam4, split_into = 1)
unoccupied5 <- time_unoccupied(fam5, split_into = 1)
unoccupied6 <- time_unoccupied(fam6, split_into = 1)
unoccupied9 <- time_unoccupied(fam9, split_into = 1)

unoccupied_all <- bind_rows("3" = unoccupied3, 
                            "4" = unoccupied4, 
                            "5" = unoccupied5, 
                            "6" = unoccupied6, 
                            "9" = unoccupied9, .id = "Family")

nosplit_unoccupied <- inner_join(nosplit_shelf, unoccupied_all, by = "Family") %>%
  mutate(adjusted_prop = prop/nobody *1000) %>%
  prop_alone(replace = 0, replace_prop = 0)

# could also adjust for fanily number of approaches?

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# it's giving NA when 0 (alone) is divided by a number.
# does total number of approaches predict prop score?

nosplit_unoccupied <- nosplit_unoccupied %>%
  filter(total != 0)

ggplot(data = nosplit_unoccupied %>% filter(age<11), 
       aes(x = as.numeric(agemonths), y = adjusted_prop)) +
  geom_point(aes(colour = Family)) + #
  geom_smooth(method = 'lm', se = F, aes())+
  scale_x_continuous(breaks = seq(0, 110, 2))+
  coord_cartesian(xlim = c(0, 68))

ggplot(data = nosplit_unoccupied %>% filter(age<11), 
       aes(x = as.numeric(agemonths), y = adjusted_prop)) +
  geom_point(aes(color = Sex)) +
  geom_smooth(method = 'lm', se = F, aes(color = Sex))+
  scale_x_continuous(breaks = seq(0, 110, 2))+
  coord_cartesian(xlim = c(0, 70))

ggplot(data = nosplit_unoccupied %>% filter(age<11), 
       aes(x = as.numeric(agemonths), y = adjusted_prop)) +
  geom_point(aes(color = Family)) +
  geom_smooth(method = 'lm', se = F, aes(color = Family))+
  scale_x_continuous(breaks = seq(0, 110, 2))+
  coord_cartesian(xlim = c(0, 70))





##########
# Radius #
##########

nosplit_radius <-
  bind_rows(
    "3" = fam3_nosplit_radius,
    "4" = fam4_nosplit_radius,
    "5" = fam5_nosplit_radius,
    "6" = fam6_nosplit_radius,
    "9" = fam9_nosplit_radius,
    .id = "Family"
  ) %>%
 # prop_alone(replace = 0) %>%
  inner_join(DOB, by = c("name", "Family")) %>%
  st_age()

nosplit_unoccupied_rad <- inner_join(nosplit_radius, unoccupied_all, by = "Family") %>%
  mutate(prop = alone/total) %>%
  mutate(adjusted_prop = prop/nobody *1000) %>%
  prop_alone(replace = 0, replace_prop = 0)

nosplit_unoccupied_rad <- nosplit_unoccupied_rad %>%
  filter(total != 0)

ggplot(data = nosplit_unoccupied_rad, # %>% filter(Family != "3")
       aes(x = as.numeric(agemonths), y = adjusted_prop)) +
  geom_point() +#aes(color = Family)
  geom_smooth(method = 'lm', se = F, aes())+
  scale_x_continuous(breaks = seq(0, 60, 2))

ggplot(data = nosplit_unoccupied_rad, # %>% filter(Family != "3")
       aes(x = as.numeric(agemonths), y = adjusted_prop, color = Sex)) +
  geom_point(aes(color = Sex)) +
  geom_smooth(method = 'lm', se = F)+
  scale_x_continuous(breaks = seq(0, 60, 2))



#########
# Touch #
#########

nosplit_touch <-
  bind_rows(
    "3" = fam3_nosplit_touch,
    "4" = fam4_nosplit_touch,
    "5" = fam5_nosplit_touch,
    "6" = fam6_nosplit_touch,
    "9" = fam9_nosplit_touch,
    .id = "Family"
  ) %>%
#  prop_alone(replace = 0) %>%
  inner_join(DOB, by = c("name", "Family")) %>%
  st_age()

nosplit_unoccupied_touch <- inner_join(nosplit_touch, unoccupied_all, by = "Family") %>%
  mutate(prop = alone/total) %>%
  mutate(adjusted_prop = prop/nobody *1000,
         app = prop / (nobody/3600)) %>%
  prop_alone(replace = 0, replace_prop = 0)

nosplit_unoccupied_touch <- nosplit_unoccupied_touch %>%
  filter(total != 0)

ggplot(data = nosplit_unoccupied_touch %>% filter(Family != "1"), #, #
       aes(x = as.numeric(agemonths), y = adjusted_prop)) +
  geom_point(aes()) +
 # geom_smooth(method = 'lm', se = F)+ #color = Sex
  scale_x_continuous(breaks = seq(0, 110, 2), expand = c(0,0))+
  coord_cartesian(xlim = c(0, 68))+
  labs(y = "Adjusted Independent Touches", x = "Age (months)") +
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
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11))
# doing alone time/total time makes no difference

ggsave(filename = "touch.png", width = 9, height = 6, dpi = 300, type = "cairo")

# n too small for sex diffs
# problem- many do not touch

ggplot(data = nosplit_unoccupied_touch %>% filter(Family != "1"), #, #
       aes(x = as.numeric(agemonths), y = adjusted_prop)) +
  geom_point(aes(color = Sex)) +
  geom_smooth(method = 'lm', se = F, aes(color = Sex))+
  scale_x_continuous(breaks = seq(0, 110, 2))+
  coord_cartesian(xlim = c(0, 65))

# because it's number of touches, could be more than one within touch bout
