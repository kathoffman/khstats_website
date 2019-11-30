id <- c(1,1,1,1,2,2,2)
date <- as.Date(c("2017-06-22", "2017-07-13", "2017-08-29",
                           "2017-04-01", "2017-05-02", "2017-11-14", "2018-01-14"))
dat <- data.frame(id, date)

dat %>% group_by(id) %>%
  mutate(visit = row_number())
