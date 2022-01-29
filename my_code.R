library(tidyverse)
library(magrittr)
library(circular)
library(lubridate)
df <- read_tsv("area_groupsize_tbs.txt", col_names = TRUE)
df %<>% mutate(day = if_else(periodo == "p1", 7, 21))
df %<>% mutate(newdate = make_date(year=year, month=month, day=day))
df %<>% mutate(angle = 30 * (month+ day/28))
df %<>% mutate(x = mean * )

d <- as.circular(df$angle, type = "angles",
                 units="degrees", modulo="asis", zero = 0,
                 rotation = "clock")
plot(d, stack = TRUE)
ggplot(df, aes(x = angle, y = area_m2_ha, color = factor(year))) +
  geom_point() +
  coord_polar() +
  scale_x_continuous(limits = c(0,360))

ggplot(df, aes(x = newdate, y = area_m2_ha, color = factor(year))) +
  geom_bar(stat = "identity")

ggplot(df, aes(x = newdate, y = variance, color = factor(year))) +
  geom_bar(stat = "identity")

