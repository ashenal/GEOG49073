library(tidyverse)
library(ggplot2)

mydf <- read_csv("./data/oh_counties_DP2020.csv")

mydf %>% dplyr::filter(., poptotal > 50000 & medianage < 40)

plot(mydf$poptotal, mydf$Hhtotal)
plot(mydf$medianage, mydf$Hhtotal)
hist(mydf$Hhtotal)
hist(mydf$Hhtotal, breaks = 20)




mydf %>% dplyr::filter(., name != "Ohio") %>% 
  ggplot(mydf, aes(x = poptotal, y = medianage)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'glm', color = 'red')
  theme_bw() +
  labs(x = "Total Population", y = "Median Age",
       title = "My First ggplot")

