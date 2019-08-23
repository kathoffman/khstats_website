event <- dems2 %>% select(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) 

dems2 %>%
  select(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) %>%
  replace(is.na(.), Inf) %>% 
  rowwise() %>%
  mutate(out.time = pmin(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT,na.rm = T),
         out.status = which.min(c(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT)),
         out.status = ifelse(out.status == 1 && is.infinite(out.time), 0, out.status),
         out.time = ifelse(is.infinite(out.time), Days.to.lastFU, out.time)) %>%
  select(out.time,out.status)

apply(events,2,function(x){
  which.min(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT)
})
??which.min


events3 <- events2 %>% filter(out.status == 1)
head(dems)



dems2 %>%
  select(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) %>%
  replace(is.na(.), Inf) %>%  # which.min cannot handle NA, so we'll make NA's infinity for now
  rowwise() %>% # to allow which.min to search along rows (dplyr naturally looks down columns)
  mutate(FUorFEtime = pmin(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT, na.rm = T),
         FUorFEstatus = which.min(c(Days.to.lastFU, Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT)),
         # numbers correspond to order of the Days* columns
         Event_yn = ifelse(FUorFEstatus == 1, 0, 1)) %>%# condensed variable for survival, 1 if any event
  select(contains("fe"), Event_yn) %>%
  rownames_to_column() %>% #allow for joining with no duplicate cols
  full_join(dems2 %>% rownames_to_column()) %>%
  select(-rowname) -> dems2

write.csv(file="dems2_dplyr.csv", dems2, row.names = F)



