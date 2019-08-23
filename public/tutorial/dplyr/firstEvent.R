#################
# JSM Analysis
# Columbia Surg, LVAD pts
# K Hoffman
#################

##################
### DATA CLEANING
##################

rm(list=ls())

library(tidyverse)
library(survival)
library(summarytools)
library(survminer)
library(ipw)
library(MatchIt)
library(here)

#Load raw data from Amelia
meds1 <- read.csv("medslist_10-03-2018.csv", na.strings = c("?"))
dems1 <- read.csv("demographics_10-03-2018.csv", na.strings = c("","#VALUE!","na"))

#Delete patient 149 (email from Amelia 10/4/18) from both data sets
dems1 <- dems1[-which(dems1$PatientID == 149), ]
meds1 <- meds1[-which(meds1$PatientID == 149), ]

#####################
# Create vars for survival analysis
#####################
dems2 <- dems1 %>% mutate(Death = ifelse(Outcome == 2, 1, 0),
                          Days.to.death = ifelse(Outcome == 2, Time.to.outcome, NA)) %>%
  rename(Days.to.lastFU = Time.to.outcome)

write.csv(file= "dems_dplyr.csv", dems2, row.names = F)

#isolate events of interest
events <- dems2 %>% select(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT)

dems2 %>% tbl_df() %>%
  select(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) %>%
  #R will throw an error if there are NAs in which.min, so make Inf for now
  replace(is.na(.), Inf) %>% 
  # this will return a 1 for first column if event never occurred,
  # but we want to multiply by NA to convert negated (!) rowSums of logical matrix (!is.na(events))
  mutate(First.event = apply(., 1, which.min) * NA^!rowSums(!is.na(events))) %>%
  #get column name that corresponds to first event
  mutate(First.event = colnames(.)[First.event],
         First.event = factor(First.event,
                              levels = c("Days.to.death", "Days.to.stroke", "Days.to.GIB", "Days.to.PT"),
                              labels = c("Death","Stroke","GIB","PT"))) %>%
  select(First.event) %>%
  rownames_to_column() %>%
  full_join(dems2 %>% rownames_to_column()) %>%
  mutate(Days.to.first.event = pmin(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT, na.rm=T)) %>%
  select(-rowname) -> dems3

dems3 %>% select(First.event, Days.to.first.event, Days.to.stroke, Days.to.GIB, Days.to.PT, Days.to.death)

dems2 %>% tbl_df() %>%
  mutate(Days.to.first.event = pmin(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT, na.rm=T)) %>%
  select(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) %>%
  #R will throw an error if there are NAs in which.min, so make Inf for now
  # replace(is.na(.), Inf) %>% 
  # this will return a 1 for first column if event never occurred,
  # but we want to multiply by NA to convert negated (!) rowSums of logical matrix (!is.na(events))
  # mutate(First.event = apply(., 1, function(x) factor(names(x)[which.min(x)]))) %>%  pull(First.event) 
  #mutate(y = names(.)[which.min(.*-1)+1L])
  #get column name that corresponds to first event
  mutate(First.event = colnames(.)[First.event],
         First.event = factor(First.event,
                              levels = c("Days.to.death", "Days.to.stroke", "Days.to.GIB", "Days.to.PT"),
                              labels = c("Death","Stroke","GIB","PT"))) %>%
  # change all the infinities back to NA
  select(First.event) %>%
  rownames_to_column() %>%
  full_join(dems2 %>% rownames_to_column()) %>%
  mutate(Days.to.first.event = pmin(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT, na.rm=T))





#mutate( First.event = pmin(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) * NA^!rowSums(!is.na(.))) %>% #for some reason this doesn't run

# mutate_all(.funs = funs(replace(., is.infinite(.), NA))) %>%
  # add back to full data set
  #full_join(dems2,
   #         by=c("Days.to.death", "Days.to.stroke", "Days.to.GIB","Days.to.PT"))

#isolate events of interest
events <- dems2 %>% select(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT)

#R will throw an error if there are NAs in which.min, so make Inf for now
dems2 %>% mutate_at(.vars = vars(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT),
                    .funs=funs(replace(., is.na(.), Inf))) %>%
  #this will return a 1 for first column if event never occurred,
  #but we want to multiply by NA to convert negated (!) rowSums of logical matrix (!is.na(events))
  mutate( First.event = pmin(Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT) * NA^!rowSums(!is.na(.))) %>%
  mutate( First.event2 = colnames(select(., Days.to.death, Days.to.stroke, Days.to.GIB, Days.to.PT))[First.event])

  #get column name that corresponds to first event
  mutate(First.event = colnames(.)[First.event], First.event = as.factor(First.event)) %>% select(First.event)


#Rename First.event vector with more intuitive levels
First.event <- recode_factor(as.vector(First.event[,1]), Days.to.death = "Death", Days.to.GIB = "GIB", Days.to.stroke = "Stroke", Days.to.PT = "PT", .default = NA_character_)
#add in the First.event vector back to large df
dems3 <- data.frame(cbind(dems2, First.event))


dems4 <- dems3 %>% filter(!is.na(PatientID)) %>%
  #Composite event survival var: whether any event occurred or censored
  mutate(AnyEvent = ifelse(!is.na(First.event), 1, 0),
         Days.FE.or.censor =  ifelse(AnyEvent == 1, Days.to.first.event, Time.to.outcome)) #time 0 is day of implantation
  
dems4$FirstEv <- fct_explicit_na(dems4$First.event, "Censored")

### BELOW WAS MY PREVIOUS ANALYSIS, TIME 0 WAS FIRST OUTPATIENT VISIT
# dems4 <- filter(dems3, !is.na(PatientID))
# dems4$AnyEvent <- ifelse(!is.na(dems4$First.event), 1, 0) #Composite event survival var: whether any event occurred or censored
# dems4$Days.FE.or.censor <- ifelse(dems4$AnyEvent == 1, dems4$Days.to.first.event, dems4$Time.to.outcome)
#dems5 <- filter(dems4, Days.FE.or.censor > Days.to.first.outpt.visit)
#dems5$Days.FE.or.censor <-  dems5$Days.FE.or.censor - dems5$Days.to.first.outpt.visit  #start Time0 at first outpatient visit
  # Make new columns for survival analysis of secondary outcomes
  # mutate(Days.to.stroke.op = ifelse(is.na(Days.to.stroke), Time.to.outcome - Days.to.first.outpt.visit, Days.to.stroke - Days.to.first.outpt.visit),
  #        Days.to.death.op = ifelse(is.na(Days.to.death), Time.to.outcome - Days.to.first.outpt.visit, Days.to.death - Days.to.first.outpt.visit),
  #        Days.to.GIB.op = ifelse(is.na(Days.to.GIB), Time.to.outcome - Days.to.first.outpt.visit, Days.to.GIB - Days.to.first.outpt.visit),
  #        Days.to.PT.op = ifelse(is.na(Days.to.PT), Time.to.outcome - Days.to.first.outpt.visit, Days.to.PT - Days.to.first.outpt.visit))

######################
# Correct y/n drug coding
######################
#Replace "." with 0 and "" with NA (Amelia's 10/4/18 email)
yes_no <- meds1 %>% select(ends_with("..y.n.")) %>% apply(., 2, function(x) as.factor(ifelse(x == ".", 0, ifelse(x=="", NA, x)))) %>% data.frame()
#recombine recoded yes/no vars
meds2 <- meds1 %>% select(-ends_with("..y.n.")) %>% cbind(., yes_no) %>% data.frame()
meds2[which(meds2$PDE5i..y.n. == 25), "PDE5i..y.n."] <- rep(0) # 10/8/2018 Email from Alberto - this was a copy/paste error
meds2$PDE5i..y.n. <- factor(meds2$PDE5i..y.n.) #get rid of extra "25" level
summary(meds2$PDE5i..y.n.)

#Save in derived data folder
meds <- meds2
dems <- dems4
#save(dems, meds, file="Data/Derived/clean_JSM.Rdata")

str(meds)
write.csv(file="meds_dplyr.csv", meds, row.names=F )

##########################
# MSM ANALYSIS
##########################

meds$Aspirin = ifelse(is.na(as.numeric(as.character(meds$Aspirin))) == T |
                        as.numeric(as.character(meds$Aspirin)) == 0, 0, 1)
#load(here("Data/Derived/clean_JSM.Rdata"))

dat <- full_join(meds, dems, by="PatientID") %>%
  mutate(Months_ImplantToVisit = round(Days_ImplantToVisit/30, digits=0), #switch to months
         Months_FEorCens = round(Days.FE.or.censor / 30, digits=0)) %>%
  mutate(ACEARB..y.n. = ifelse(as.numeric(as.character(ACE..y.n.)) > 0 | as.numeric(as.character(ARB..y.n.)) > 0, 1, 0),
         PDE5i..y.n. = ifelse(as.numeric(as.character(PDE5i..y.n.)) > 0, 1, 0)) %>%
  mutate_at(vars(ends_with("..y.n.")), funs(as.character)) %>%
  mutate_at(vars(ends_with("..y.n.")), funs(as.numeric)) %>%
  filter(!is.na(ACEARB..y.n.), !is.na(AldoB..y.n.), !is.na(Digoxin..y.n.),
         !is.na(PDE5i..y.n.), !is.na(BB..y.n.)) %>%
  ungroup() %>%
  group_by(PatientID) %>%
  select(-Visit.n, -Days_ImplantToVisit) %>%
  #complete(Months_ImplantToVisit = full_seq(1:max(Months_ImplantToVisit), 1)) %>%
  complete(Months_ImplantToVisit = full_seq(1:max(Months_FEorCens), 1)) %>%
  fill(ends_with("..y.n."), Aspirin, BMI, Age.at.implant, Sex, LoopD..y.n., Months_FEorCens, AnyEvent,
       IMCS, Creat_dx, #GIBorSTROKE_IA,
       DT, LDH_Dx, Caucasian, Speed, INR_dx, FirstEv,
       Stroke_IA, GIB_IA, Stroke_Hx, Pulmonary_Hx, HTN_Hx, Ischemic, DM, Smoking_Hx, .direction="down") %>%
  #complete(Months_ImplantToVisit = full_seq(1:min(Months_ImplantToVisit), 1)) %>%
  fill(ends_with("..y.n."), Aspirin, BMI, Age.at.implant, Sex, LoopD..y.n., Months_FEorCens,
       IMCS, Creat_dx, #GIBorSTROKE_IA,
       DT, LDH_Dx, Caucasian, Speed, INR_dx, FirstEv,
       Stroke_IA, GIB_IA, Stroke_Hx, Pulmonary_Hx, HTN_Hx, Speed, Ischemic, DM, Smoking_Hx, AnyEvent,
       .direction = "up") %>%
  mutate(Dig.prev = lag(Digoxin..y.n., order_by = Months_ImplantToVisit),
         BB.prev = lag(BB..y.n., order_by = Months_ImplantToVisit),
         ACEARB.prev = lag(ACEARB..y.n., order_by = Months_ImplantToVisit),
         AldoB.prev = lag(AldoB..y.n., order_by = Months_ImplantToVisit),
         PDE5i.prev = lag(PDE5i..y.n., order_by = Months_ImplantToVisit),
         aspirin.prev = lag(Aspirin, order_by = Months_ImplantToVisit),
         LoopD.prev = lag(LoopD..y.n., order_by = Months_ImplantToVisit)) %>%
  fill(Dig.prev, BB.prev, ACEARB.prev, AldoB.prev, PDE5i.prev, aspirin.prev, LoopD.prev, .direction="up") %>% #assuming patients on same drugs previously
  ungroup() %>%
  mutate(EventDuringTime=0, 
         censored = ifelse(AnyEvent == 1, 0, 1),
         censoredEnd=0) %>%
  group_by(PatientID) %>%
  mutate(censoredEnd.prev = lag(censoredEnd, order_by=Months_ImplantToVisit)) %>%
  arrange(PatientID, Months_ImplantToVisit) %>%
  mutate(EventDuringTime = c(EventDuringTime[-n()], AnyEvent[n()]),
         censoredEnd = c(censoredEnd[-n()], censored[n()]),
         BMI_cat = ifelse(BMI < 25, 0, 1),
         Speed_cat = ifelse(Speed < 9000, 0, 1)) %>%
  ungroup() %>%
  select(PatientID, Months_ImplantToVisit, 
         censored, censoredEnd, 
        EventDuringTime, AnyEvent, FirstEv, Months_FEorCens, 
                      Age.at.implant, Sex, BMI,
                      Dig.prev, Digoxin..y.n.,
                      BB.prev, BB..y.n., aspirin.prev,
                      ACEARB.prev, ACEARB..y.n., 
                      AldoB.prev, AldoB..y.n.,
                      PDE5i.prev, PDE5i..y.n.,
                      LoopD.prev, Stroke_IA, GIB_IA,
                      Stroke_Hx, Pulmonary_Hx, HTN_Hx,
                      Ischemic, DM, Smoking_Hx, Speed_cat,
                      IMCS, Creat_dx, #GIBorSTROKE_IA,
                      DT, LDH_Dx, Caucasian, Speed, INR_dx, BMI_cat,
                      Aspirin, aspirin.prev) %>%
  na.omit() %>% #%>% #patients with only one time point are removed
  arrange(PatientID, Months_ImplantToVisit)  %>%
  as.data.frame()
#filter(EventDuringTime == 1 | Months_ImplantToVisit %% 3 == 0) #to reduce granularity to 3 months

#View(dat)
view(dfSummary(dat))

#dat$NumberDrugs <- dat$Digoxin..y.n. + dat$ACEARB..y.n. + dat$AldoB..y.n. + dat$PDE5i..y.n. + dat$BB..y.n.


View(dat)

length(unique(dat$PatientID))

###################
# DIGOXIN
###################

newids <- dat %>% select(PatientID, Months_ImplantToVisit) %>%
  arrange(-Months_ImplantToVisit) %>%
  group_by(PatientID) %>% 
  filter(row_number() == 1) %>%
  select(-Months_ImplantToVisit)
newids$newid <- 1:nrow(newids)

merge(newids, dat, by="PatientID") %>%
  ggplot(aes(
         Months_ImplantToVisit,
         newid,
         group = newid,
         col = factor(Digoxin..y.n.))) +
  geom_line(size = .2) + geom_point(shape = 15, size = .9) + theme_classic() + scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(shape = factor(FirstEv), alpha=factor(EventDuringTime)), col="black") +
  scale_alpha_manual(values=c(0,1),guide=F) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Months since LVAD Implantation", y = "Patient ID",
                                                    col = "Dig. Status",
                                                    title = "Digoxin Treatment Timeline") + scale_color_discrete(labels = c("No", "Yes"))


dig.temp <- ipwtm(exposure = Digoxin..y.n., family="binomial", link="logit",
                  numerator = ~ Age.at.implant + Sex + BMI_cat + Caucasian + Speed_cat +
                    Stroke_IA + Speed_cat + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                    Ischemic + DM + Smoking_Hx + Dig.prev,
                  denominator = ~
                    Age.at.implant + Sex + + BMI_cat + Caucasian + 
                    Stroke_IA + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                    Ischemic + DM + Smoking_Hx + Speed_cat +
                    ACEARB.prev + AldoB.prev + PDE5i.prev + BB.prev + Dig.prev +
                    aspirin.prev + LoopD.prev,
                  id=PatientID, timevar = Months_ImplantToVisit,
                  type="all", data = dat, trunc = .01)

#To  correct  for  censoring,  we  estimateswi†(k)inamanner  analogous  to  the  estimation  ofswi(k)  exceptwithA(k)  replaced  byC(k)  as  the  outcome  variable,withA(k21) added as an additional regressor, and notconditioning onA#(k21)50 but rather onC#(k21)50
dig.temp2 <- ipwtm(exposure = censoredEnd, family="binomial", link="logit",
                  numerator = ~ Age.at.implant + Sex + BMI_cat + Caucasian +
                    Stroke_IA + Speed_cat + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                    Ischemic + DM + Smoking_Hx + Dig.prev,
                  denominator = ~
                    Age.at.implant + Sex + BMI + Speed_cat +
                    Stroke_IA + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                    Ischemic + DM + Smoking_Hx +
                    ACEARB.prev + AldoB.prev + PDE5i.prev + BB.prev + Dig.prev +
                    aspirin.prev + LoopD.prev,
                  id=PatientID, timevar = Months_ImplantToVisit,
                  type="all", data = dat, trunc = .01)


# Drug weights
summary(dig.temp$ipw.weights)
summary(dig.temp$weights.trunc)
ipwplot(dig.temp$ipw.weights, timevar = dat$Months_ImplantToVisit,
        binwidth=1)
ipwplot(dig.temp$weights.trunc, timevar = dat$Months_ImplantToVisit,
        binwidth=1)

# Censoring weights
summary(dig.temp2$ipw.weights)
summary(dig.temp2$weights.trunc)
ipwplot(dig.temp2$ipw.weights, timevar = dat$Months_ImplantToVisit,
        binwidth=1)
ipwplot(dig.temp2$weights.trunc, timevar = dat$Months_ImplantToVisit,
        binwidth=1)

dat$tstart <- tstartfun(id=PatientID, timevar=Months_ImplantToVisit, data=dat)


#including weights
summary(coxph(Surv(tstart, Months_ImplantToVisit, EventDuringTime ) ~ Digoxin..y.n. + 
                cluster(PatientID), data=dat,
              weights = dig.temp$weights.trunc * dig.temp2$weights.trunc))


summary(coxph(Surv(tstart, Months_ImplantToVisit, EventDuringTime ) ~ Digoxin..y.n. + 
                cluster(PatientID), data=dat))

summary(coxph(Surv(tstart, Months_ImplantToVisit, EventDuringTime ) ~ Digoxin..y.n. , data=dat))


dat_uni <- dat %>% group_by(PatientID) %>% arrange(PatientID, Months_ImplantToVisit) %>% filter(row_number() == 1)
coxph(formula = Surv(Months_FEorCens, AnyEvent) ~ Digoxin..y.n., data=dat_uni)



############################
# DIGOXIN
############################

#HR is 13.1
dig.f.ps <- matchit(Digoxin..y.n. ~ Age.at.implant + #Avg_MAP +
                      IMCS + Creat_dx + DT +  #GIBorSTROKE_IA + Days.to.IA.discharge
                      LDH_Dx + Caucasian + DM + Ischemic + BMI_cat + Speed_cat + Stroke_Hx + INR_dx + 
                      BB..y.n. + ACEARB..y.n. + PDE5i..y.n. + AldoB..y.n.,
                      data=dat_uni, method="nearest", distance="logit", m.order="random", caliper=.3)

dig <- as.data.frame(dig.f.ps$match.matrix)
colnames(dig) <- c("matched.unit")
dig$matched.unit.num <- as.numeric(gsub('PSID','', dig$matched.unit))
dig$treated.unit <- as.numeric(gsub('NSW','',rownames(dig)))
#delete NA
dig.ps <- dig[!is.na(dig$matched.unit), ]
dig.ps$pair.num <- 1:dim(dig.ps)[1]

dig.treat.matched <- dat[dig.ps$treated.unit,]
dig.treat.matched$pair.num <- dig.ps$pair.num

dig.control.matched <- dat[dig.ps$matched.unit.num,]
dig.control.matched$pair.num <- dig.ps$pair.num

dig.treat.control.matched <- rbind(dig.treat.matched, dig.control.matched)
dig.treat.control.matched$id <- dig.treat.control.matched$pair.num

#Digoxin vs. no digoxin
dig.f.mod.ps <- coxph(formula = Surv(Months_FEorCens, AnyEvent) ~ Digoxin..y.n., data=dig.treat.control.matched)

summary(dig.f.mod.ps)

#USING TWANG
set.seed(7)
dig.ps <- ps(Digoxin..y.n. ~ Age.at.implant + #Avg_MAP +
     IMCS + Creat_dx + DT +  #GIBorSTROKE_IA + Days.to.IA.discharge
     LDH_Dx + Caucasian + DM + Ischemic + BMI_cat + Speed_cat + Stroke_Hx + INR_dx + 
     BB..y.n. + ACEARB..y.n. + PDE5i..y.n. + AldoB..y.n.,
   data=dat_uni)

colSums(is.na(dat_uni))


###################
# BB
###################


bb.temp <- ipwtm(exposure = BB..y.n., family="binomial", link="logit",
                  numerator = ~ Age.at.implant + Sex + BMI +
                    Stroke_IA + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                    Ischemic + DM + Smoking_Hx + BB.prev,
                  denominator = ~ Age.at.implant + Sex + BMI + 
                    Stroke_IA + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                    Ischemic + DM + Smoking_Hx + ACEARB.prev +
                    AldoB.prev + PDE5i.prev + BB.prev + Dig.prev, + aspirin.prev,
                  id=PatientID, timevar = Months_ImplantToVisit,
                  type="all", data = dat, trunc = .01)

#To  correct  for  censoring,  we  estimateswi†(k)inamanner  analogous  to  the  estimation  ofswi(k)  exceptwithA(k)  replaced  byC(k)  as  the  outcome  variable,withA(k21) added as an additional regressor, and notconditioning onA#(k21)50 but rather onC#(k21)50
bb.temp2 <- ipwtm(exposure = censoredEnd, family="binomial", link="logit",
                   numerator = ~ Dig.prev + Age.at.implant + Sex + BMI + 
                     Stroke_IA + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                     Ischemic + DM + Smoking_Hx + Dig.prev,
                   denominator = ~ Age.at.implant + Sex + BMI + 
                     Stroke_IA + GIB_IA + Stroke_Hx + Pulmonary_Hx + HTN_Hx +
                     Ischemic + DM + Smoking_Hx + ACEARB.prev +
                     AldoB.prev + PDE5i.prev + BB.prev + Dig.prev, + aspirin.prev,
                   id=PatientID, timevar = Months_ImplantToVisit,
                   type="all", data = dat, trunc = .01)


# Drug weights
summary(bb.temp$ipw.weights)
summary(bb.temp$weights.trunc)
ipwplot(bb.temp$ipw.weights, timevar = dat$Months_ImplantToVisit,
        binwidth=1)
ipwplot(bb.temp$weights.trunc, timevar = dat$Months_ImplantToVisit,
        binwidth=1)

# Censoring weights
summary(bb.temp2$ipw.weights)
summary(bb.temp2$weights.trunc)
ipwplot(bb.temp2$ipw.weights, timevar = dat$Months_ImplantToVisit,
        binwidth=1)
ipwplot(bb.temp2$weights.trunc, timevar = dat$Months_ImplantToVisit,
        binwidth=1)

dat$tstart <- tstartfun(id=PatientID, timevar=Months_ImplantToVisit, data=dat)


#including weights
summary(coxph(Surv(tstart, Months_ImplantToVisit, EventDuringTime ) ~ BB..y.n. + 
                cluster(PatientID), data=dat,
              weights = bb.temp$ipw.weights * bb.temp2$ipw.weights))


summary(coxph(Surv(tstart, Months_ImplantToVisit, EventDuringTime ) ~ Digoxin..y.n. + 
                cluster(PatientID), data=dat))

summary(coxph(Surv(tstart, Months_ImplantToVisit, EventDuringTime ) ~ Digoxin..y.n. , data=dat))


dat_uni <- dat %>% group_by(PatientID) %>% arrange(PatientID, Months_ImplantToVisit) %>% filter(row_number() == 1)
coxph(formula = Surv(Months_FEorCens, AnyEvent) ~ Digoxin..y.n., data=dat_uni)

