berrySubject <- tibble::tibble(
                  Site=1:9,
                  Subjects=c(20, 10, 16, 19, 14, 46, 10, 9, 6),
                  Events=  c(20,  4, 11, 10,  5, 36,  9, 7, 4)
                ) %>% 
                dplyr::group_by(Site) %>% 
                dplyr::mutate(SubjectID=list(1:Subjects)) %>% 
                tidyr::unnest(cols = c(SubjectID)) %>% 
                dplyr::mutate(
                  Event=SubjectID <= Events, 
                  SubjectID=SubjectID + 100*Site
                ) %>% 
                dplyr::select(Site, SubjectID, Event)

usethis::use_data(berrySubject, overwrite = TRUE)
