print("Hello World")

library(tidyverse)
theme_set(theme_bw())

mtcars %>% 
  ggplot(aes(mpg, wt)) +
  geom_point()
