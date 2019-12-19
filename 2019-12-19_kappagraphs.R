library(tidyverse)
library(GGally)
library(psych)
library(reshape2)

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
fam9 <- read_csv("2019-12-02_Family9_timeline_sec.csv") %>%
  select(-X1)

binary <- function(data, maxcol) {
  dat <- data %>% 
    select(9:as.numeric(maxcol)) %>%
    mutate_all(
      funs(
        case_when(
          . == 2 ~ 1,
          . == 3 ~ 1,
          . == 1 ~ 1,
          . == 0 ~ 0
        )
      )
    )
  return(dat)
}
# maxcol is final column with animal data, depends on number in group

kappa_and_prep <- function(data, rounding = 2) {
  
  kap <- cohen.kappa(data)
  kapmatrix <- kap [[1]]
  kapmelt <- melt(kapmatrix, na.rm = T)
  kapmelt$value <- round(kapmelt$value, rounding)
  return(kapmelt)
  
}

#fam3 = 15, fam 4 = 16, fam5 = 14, fam6 = 15, fam9 = 14

f3b <- binary(fam3_pilot, 15)
f3k <- kappa_and_prep(f3b)
f4b <- binary(fam4, 16)
f4k <- kappa_and_prep(f4b)
f5b <- binary(fam5, 14)
f5k <- kappa_and_prep(f5b)
f6b <- binary(fam6, 15)
f6k <- kappa_and_prep(f6b)
f9b <- binary(fam9, 14)
f9k <- kappa_and_prep(f9b)

#Factoring
#####

f3k$Var1 <- factor(ck3m$Var1, levels = c(
  "Rafeky", "Shiba", "Mountain", "Field","Chalk", "Coal", "Malachite"
))
f3k$Var2 <- factor(ck3m$Var2, levels = c(
  "Rafeky", "Shiba", "Mountain", "Field","Chalk", "Coal", "Malachite"
))

f4k$Var1 <- factor(ck4m$Var1, levels = c(
  "Slim", "Chunk", "Saluki", "Samoyed", "Obsidian", "Onyx", "Rock", "Mineral"
))
f4k$Var2 <- factor(ck4m$Var2, levels = c(
  "Slim", "Chunk", "Saluki", "Samoyed", "Obsidian", "Onyx", "Rock", "Mineral"
))

f5k$Var1 <- factor(ck5m$Var1, levels = c(
  "Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz"
  ))
f5k$Var2 <- factor(ck5m$Var2, levels = c(
  "Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz"
  ))

f6k$Var1 <- factor(ck6m$Var1, levels = c(
  "Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja"))
f6k$Var2 <- factor(ck6m$Var2, levels = c(
  "Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja"))

f9k$Var1<- factor(ck9m$Var1, levels = c(
  "Puerto", "Napa", "Arsenic", "Asbestos", "Fusion", "Flux"
))
f9k$Var2<- factor(ck9m$Var2, levels = c(
  "Puerto", "Napa", "Arsenic", "Asbestos", "Fusion", "Flux"
))



#twins
#####
f4k <- f4k %>% mutate(twins = case_when(
  Var1 == "Saluki" & Var2 == "Samoyed" ~ 1,
  Var1 == "Samoyed" & Var2 == "Saluki" ~ 1,
  Var1 == "Obsidian" & Var2 == "Onyx" ~ 1,
  Var1 == "Onyx" & Var2 == "Obsidian" ~ 1, 
  Var1 == "Rock" & Var2 == "Mineral" ~ 1,
  Var1 == "Mineral" & Var2 == "Rock" ~ 1
))

f3k <- f3k %>% mutate(twins = case_when(
  Var1 == "Chalk" & Var2 == "Coal" ~ 1,
  Var1 == "Coal" & Var2 == "Chalk" ~ 1,
  Var1 == "Mountain" & Var2 == "Field" ~ 1,
  Var1 == "Field" & Var2 == "Mountain" ~ 1, 
))

f5k <- f5k %>% mutate(twins = case_when(
  Var1 == "Zinc" & Var2 == "Zircon" ~ 1,
  Var1 == "Zircon" & Var2 == "Zinc" ~ 1,
  Var1 == "Quartz" & Var2 == "Quantum" ~ 1,
  Var1 == "Quantum" & Var2 == "Quartz" ~ 1, 
))

f6k <- f6k %>% mutate(twins = case_when(
  Var1 == "Papillon" & Var2 == "Poodle" ~ 1,
  Var1 == "Poodle" & Var2 == "Papillon" ~ 1,
  Var1 == "Nugget" & Var2 == "Ninja" ~ 1,
  Var1 == "Ninja" & Var2 == "Nugget" ~ 1, 
))

f9k <- f9k %>% mutate(twins = case_when(
  Var1 == "Arsenic" & Var2 == "Asbestos" ~ 1,
  Var1 == "Asbestos" & Var2 == "Arsenic" ~ 1,
  Var1 == "Fusion" & Var2 == "Flux" ~ 1,
  Var1 == "Flux" & Var2 == "Fusion" ~ 1, 
))


myCol <- 'white'
f3k$twins <- as.factor(f3k$twins)
names(myCol) <- levels(f3k$twins)
f4k$twins <- as.factor(f4k$twins)
names(myCol) <- levels(f4k$twins)
f5k$twins <- as.factor(f5k$twins)
names(myCol) <- levels(f5k$twins)
f6k$twins <- as.factor(f6k$twins)
names(myCol) <- levels(f6k$twins)
f9k$twins <- as.factor(f9k$twins)
names(myCol) <- levels(f9k$twins)
colScal <- scale_colour_manual(name = "twins",values = myCol)

matrix_graph <- function(data) {
  data <- data %>% rename(Kappa = value)
  p <- ggplot(data = data, aes(x=Var1, y=Var2, fill=Kappa)) + #
  geom_tile()+
  geom_tile(aes(colour = twins, width = .8, height = .8), 
            data = subset(data, twins == 1), size = 1.5) +
  guides(color = F) +
  # scale_fill_viridis(option = "viridis", direction = 1) +
  geom_text(
    data = subset(data, Kappa != 1), aes(Var2, Var1, label = Kappa), 
    color = "white", size = 4.5, fontface = 2)+
  theme_bw() + 
    theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11)) + colScal
  return(p)
}

matrix_graph(f3k)
matrix_graph(f4k)
matrix_graph(f5k)
matrix_graph(f6k)
matrix_graph(f9k)
