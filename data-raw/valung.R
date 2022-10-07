vaLung <- tibble::as_tibble(read.csv("./data-raw/valung.csv"))
usethis::use_data(vaLung, overwrite = TRUE)
