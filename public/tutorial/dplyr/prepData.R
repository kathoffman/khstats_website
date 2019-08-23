library(tidyverse)
library(here)
load("dat_clean.Rdata")

names(dems)

dems <- dems %>% 
  # need a censoring day??
  mutate(id = sample(PatientID),
         bmi = sample(BMI),
         age = sample(Age.at.implant),
         race = sample(Race),
         sex = sample(Sex),
         days_death = sample(Days.to.death),
         days_stroke = sample(Days.to.stroke),
         days_FU = ifelse(is.na(days_death), sample(1:600), days_death)
         ) %>%
  select(id, bmi, race, age, sex, days_death, days_stroke, days_FU)

# meds %>% select(PatientID, Days_ImplantToVisit,
#                 ends_with("..y.n."),
#                 ends_with("type"), ends_with("dosage")
#                 )

meds <- meds %>% 
         rename_at(vars(ends_with("..y.n.")), funs(gsub("..y.n.","_yn", .))) %>%
         rename_at(vars(ends_with(".dosage")), funs(gsub(".dosage","_dose", .))) %>%
         rename_at(vars(ends_with(".type")), funs(gsub(".type","_type", .))) %>%
         rename_at(vars(starts_with("BB")), funs(gsub("BB", "trtA", .))) %>%
         rename_at(vars(starts_with("PDE")), funs(gsub("PDE5i", "trtB", .))) %>%
         rename(id = PatientID, days_dx = Days_ImplantToVisit) %>%
        mutate(trtA_yn = as.numeric(as.character(trtA_yn)),
               trtB_yn = as.numeric(as.character(trtB_yn))) %>%
  select(id, days_dx, starts_with("trtA"), starts_with("trtB"))
  
  getwd()
save(dems, meds, file=here("prac_dat.Rdata"))

meds
dems
