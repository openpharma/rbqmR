berrySummary <- tibble::tibble(
                  Site=1:9,
                  Subjects=c(20, 10, 16, 19, 14, 46, 10, 9, 6),
                  Events=  c(20,  4, 11, 10,  5, 36,  9, 7, 4)
                ) %>% 
                dplyr::mutate(ObservedResponse=Events/Subjects)

usethis::use_data(berrySummary, overwrite = TRUE)
