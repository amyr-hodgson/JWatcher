library(tidyverse)
library(GGally)

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

fam4m <- fam4 %>% select(9:16)

f4cor <- round(cor(fam4m), 2)

fam4binary <- fam4 %>% select(9:16) %>%
  mutate_all(
    funs(case_when(
      . == 2 ~ 1,
      . == 3 ~ 1,
      . == 1 ~ 1,
      . == 0 ~ 0
    )))

f4corb <- round(cor(fam4binary), 2)

ggpairs(as.data.frame(f4corb))
pairs(as.data.frame(f4corb))

fam3m <- fam3_pilot %>% select(9:15)

f3cor <- round(cor(fam3m), 2)

fam3binary <- fam3_pilot %>% select(9:15) %>%
  mutate_all(
    funs(case_when(
      . == 2 ~ 1,
      . == 3 ~ 1,
      . == 1 ~ 1,
      . == 0 ~ 0
    )))

f3corb <- round(cor(fam3binary), 2)

ggpairs(as.data.frame(f3corb))
pairs(as.data.frame(f3corb))

fam5m <- fam5 %>% select(9:14)

f5cor <- round(cor(fam5m), 2)

fam5binary <- fam5 %>% select(9:14) %>%
  mutate_all(
    funs(case_when(
      . == 2 ~ 1,
      . == 3 ~ 1,
      . == 1 ~ 1,
      . == 0 ~ 0
    )))

f5corb <- round(cor(fam5binary), 2)

ggpairs(as.data.frame(f5corb))
pairs(as.data.frame(f5corb))

fam6m <- fam6 %>% select(9:15)

f6cor <- round(cor(fam6m), 2)

fam6binary <- fam6 %>% select(9:15) %>%
  mutate_all(
    funs(case_when(
      . == 2 ~ 1,
      . == 3 ~ 1,
      . == 1 ~ 1,
      . == 0 ~ 0
    )))

f6corb <- round(cor(fam6binary), 2)

ggpairs(fam6binary)
pairs(as.data.frame(f6corb))

fam9m <- fam9 %>% select(9:14)

f9cor <- round(cor(fam9m), 2)

fam9binary <- fam9 %>% select(9:14) %>%
  mutate_all(
    funs(case_when(
      . == 2 ~ 1,
      . == 3 ~ 1,
      . == 1 ~ 1,
      . == 0 ~ 0
    )))

f9corb <- round(cor(fam9binary, method = 'spearman'), 2)

ggpairs(fam9binary, method = 'spearman')
pairs(as.data.frame(f9corb))

library(psych)
library(reshape2)

ck9 <- cohen.kappa(fam9binary)
ck9 <- ck9[[1]]
ggcorr(ck9) #what is it visualising?

ck9m <- melt(ck9)
ck9m$value <- round(ck9m$value, 2)

ggplot(data = ck9m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "green",
                       midpoint = .5, limit = c(-.1,1))

cohen.kappa(fam9binary)

ck3 <- cohen.kappa(fam3binary)
ck3 <- ck3[[1]]
ck3m <- melt(ck3)
ck3m$value <- round(ck3m$value, 2)

ggplot(data = ck3m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "green",
                       midpoint = .5, limit = c(-.2, 1))

ck4 <- cohen.kappa(fam4binary)
ck4 <- ck4[[1]]

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_lower_tri(ck4)
ck4m <- melt(ck4, na.rm = T)
ck4m$value <- round(ck4m$value, 2)
ggplot(data = ck4m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "green",
                       midpoint = .5, limit = c(-.2, 1))

ck5 <- cohen.kappa(fam5binary)
ck5 <- ck5[[1]]
ck5m <- melt(ck5, na.rm = T)
ck5m$value <- round(ck5m$value, 2)

ggplot(data = ck5m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "green",
                       midpoint = .5, limit = c(-.1, 1))

library(viridis)

ck6 <- cohen.kappa(fam6binary)
ck6 <- ck6[[1]]
ck6m <- melt(ck6, na.rm = T)
ck6m$value <- round(ck6m$value, 2)
ggplot(data = ck6m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "green",
                       midpoint = .5, limit = c(-.1, 1))+ 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

ck6m$Var1 <- factor(ck6m$Var1, levels = c(
  "Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja"))
ck6m$Var2 <- factor(ck6m$Var2, levels = c(
  "Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja"))

ck3m$Var1 <- factor(ck3m$Var1, levels = c(
  "Rafeky",
  "Shiba",
  "Mountain",
  "Field",
  "Chalk",
  "Coal",
  "Malachite"
))
ck3m$Var2 <- factor(ck3m$Var2, levels = c(
  "Rafeky",
  "Shiba",
  "Mountain",
  "Field",
  "Chalk",
  "Coal",
  "Malachite"
))

ck4m$Var1 <- factor(ck4m$Var1, levels = c(
  "Slim",
  "Chunk",
  "Saluki",
  "Samoyed",
  "Obsidian",
  "Onyx",
  "Rock",
  "Mineral"
))
ck4m$Var2 <- factor(ck4m$Var2, levels = c(
  "Slim",
  "Chunk",
  "Saluki",
  "Samoyed",
  "Obsidian",
  "Onyx",
  "Rock",
  "Mineral"
))

ck5m$Var1 <- factor(ck5m$Var1, levels = c(
  "Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz"))
ck5m$Var2 <- factor(ck5m$Var2, levels = c(
  "Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz"))

ck9m$Var1<- factor(ck9m$Var1, levels = c(
  "Puerto", "Napa", "Arsenic", "Asbestos", "Fusion", "Flux"
))
ck9m$Var2<- factor(ck9m$Var2, levels = c(
  "Puerto", "Napa", "Arsenic", "Asbestos", "Fusion", "Flux"
))

ggplot(data = ck6m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(option = "viridis") +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

ggplot(data = ck5m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(option = "viridis") +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

ggplot(data = ck9m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(option = "viridis") +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

ggplot(data = ck3m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(option = "viridis") +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

ck4m <- ck4m %>% mutate(twins = case_when(
  Var1 == "Saluki" & Var2 == "Samoyed" ~ 1,
  Var1 == "Samoyed" & Var2 == "Saluki" ~ 1,
  Var1 == "Obsidian" & Var2 == "Onyx" ~ 1,
  Var1 == "Onyx" & Var2 == "Obsidian" ~ 1, 
  Var1 == "Rock" & Var2 == "Mineral" ~ 1,
  Var1 == "Mineral" & Var2 == "Rock" ~ 1
))

ggplot(data = ck4m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(aes(color = as.factor(twins), width = .9, height = .9), size = 1)+
  scale_fill_viridis(option = "viridis") +
  geom_text(data = subset(ck4m, value != 1), aes(Var2, Var1, label = value), color = "white", size = 4)+
  theme_bw()

ck4m$twins <- as.factor(ck4m$twins)
myCol <- 'white'
names(myCol) <- levels(ck4m$twins)
colScal <- scale_colour_manual(name = "twins",values = myCol)

ggplot(data = ck4m, aes(x=Var1, y=Var2, fill=value)) + #
  geom_tile()+
  geom_tile(aes(colour = twins, width = .9, height = .9), 
            data = subset(ck4m, twins == 1), size = 2) +
 # scale_fill_viridis(option = "viridis", direction = 1) +
  geom_text(data = subset(ck4m, value != 1), aes(Var2, Var1, label = value), color = "white", size = 4)+
  theme_bw() + colScal

library(irr)

kappa2(fam9binary[,1:2])
