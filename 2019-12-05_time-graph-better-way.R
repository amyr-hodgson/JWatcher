
fam3_10sec <- read_csv("2019-12-05_Family3_timeline_10thsec.csv")

fam3_transitions <- fam3_10sec %>%
  gather(key = "Monkey", value = "loc", Rafeky, Shiba, Mountain, Field, Chalk, Coal, Malachite) %>%
  mutate(transition =
           ifelse(
             test = (loc == lag(loc) &
                       loc == lead(loc)),
             yes =  0,
             no = 1
           )) %>%
  fill(transition, .direction = "up") %>%
  filter(transition == 1 | ts_adj == 0 | ts_adj == 18000)

ggplot(data = fam3_transitions, aes(
  x = ts_adj,
  y = loc,
  #colour = Stage
)) +
  geom_point(size = .02) +
  geom_line(size = .5) +
#  colScale +
  facet_grid(Monkey ~.)+
  coord_cartesian(ylim = c(0, 3.1)) +
#  scale_y_continuous(breaks = seq(0, 3.1, 1), labels = abbrev_y) +
  theme(panel.spacing = unit(.5, "lines")) +
  labs(x = "Time (seconds)", y = "Location")

