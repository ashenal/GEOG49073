
library(tidyverse)

rainfall <- c(0.0, 2.1, 2.5, .1, 0.0, 0.0, 6.8, 3.1, 2.2)

rainfall[1]

rainfall[1] >= 3

if(rainfall[1] >= 3){
  print("big storm")
} else{
  print('little storm')
}


f.storm.test <- function(rainfall){
  if(rainfall[1] >= 3){
    print("big storm")
  } else{
    print('little storm')
  }
}

for(i in rainfall){
  f.storm.test(i)
}


rainfall %>% purrr::map(., f.storm.test)

rainfall >= 3

max(rainfall)

which(rainfall == max(rainfall))


# -------------------------------------------------------------------------


mydf <- read_csv('./ne_counties.csv')

max(mydf$MedValHous)
which(mydf$MedValHous == max(mydf$MedValHous)) %>% mydf[.,]

mydf %>% dplyr::slice_max(MedValHous)

newdf <- mydf %>% mutate(deviation = MedValHous - max(MedValHous))

newdf %>%  dplyr::slice_min(deviation)

newdf %>% ggplot(., aes(x=deviation))+
  geom_histogram(fill = 'dark green')+
  theme_classic() +
  labs(title = 'Deviations from maximum NE housing value',
       subtitle = 'County scale',
       x = "Deviation",
       y = 'Count')



newdf %>% ggplot(., aes(x = deviation, y = after_stat(density))) +
  geom_histogram(fill = "dark green") +
  geom_vline(xintercept = mean(newdf$deviation), color = "red", linewidth = 2) +
  geom_density(color = "black", linewidth = 1) +
  theme_classic() +
  labs(title = "Deviations from maximum NE housing value",
       subtitle = "County scale",
       x = "Deviation",
       y = "Density")



