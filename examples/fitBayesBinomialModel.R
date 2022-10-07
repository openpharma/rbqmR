results <- berrySummary %>% 
             fitBayesBinomialModel(
               n=Subjects,
               r=Events
             )