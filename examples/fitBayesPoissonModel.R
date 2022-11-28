data(cavalryDeaths)

cavalryDeaths %>% 
  group_by(Corps) %>% 
  summarise(Deaths=sum(Deaths), Years=sum(Year)) %>% 
  fitBayesPoissonModel(Deaths, Years)