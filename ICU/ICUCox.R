#Input: ICU Case data with sepsis scores of each patient on a daily bases
#Output: COX models

library(ggplot2)
library(dplyr)
library(readxl)
library(xlsx)
library(tidyverse)
library(gtsummary)
library(cusum)
library(qcc)
library(ggsurvfit)
library("survminer",warn.conflicts = FALSE)
library("Rcpp")
library('gt')
library(survival)

#has trimmed data with sepsis info by patient.
cases <- read.csv("trimmeddata.csv")

#data with cases with daily metrics
dailycases <- read_excel("ICUcases_w_sepsis.xlsx")

cases$Age <- as.integer(cases$Age)
cases$status <- cases$Outcome
cases$status[cases$status == "death"] <- 1
cases$status[cases$status == "downgraded"] <- 0
cases$status[cases$status == "transferred to outside hospital"] <- 0
cases$status[cases$status == "unspecified"] <- 0
cases$status <- as.numeric(cases$status)
cases$time <- as.numeric(cases$Length.of.stay)
cases$WBC <- as.numeric(cases$WBC)
cases$Hgb <- as.numeric(cases$Hgb)
cases <- cases %>%
  mutate(across(everything(), ~if_else(. == "PEND", NA,.)))


dailycases <- dailycases %>% 
  fill(Patient, .direction = "down") %>%
  fill(Gender, .direction = "down") %>%
  fill(Age, .direction = "down") %>%
  mutate(Patient = as.numeric(Patient)) %>%
  mutate(Age = as.numeric(Age)) %>%
  mutate(Vasopressors = if_else(is.na(Vasopressors), "N", Vasopressors)) %>%
  mutate(Vasopressors = factor(Vasopressors, levels = c("N", "Y")))


dailycases_tv <- dailycases %>%
  arrange(Patient, Day) %>%
  group_by(Patient) %>%
  mutate(
    # Pull the Day 0 outcome down to all rows for that patient
    Outcome = first(Outcome[Day == 0]),
    
    # event = 1 only on the last clinical day, if patient died
    event = ifelse(
      row_number() == n() & Outcome == "death", 1, 0
    ),
    
    t.start = Day,
    t.stop  = Day + 1 #because day 0 is the first day
    
  ) %>%
  ungroup() 

write.csv(dailycases_tv,"ICUCOXdata.csv", row.names = FALSE)
# Check a patient who died
# dailycases_tv %>%
#   filter(Patient == 80) %>%          # swap in a known death
#   select(Patient, Day, t.start, t.stop, event, Outcome)

# Confirm each patient has exactly 0 or 1 events
# dailycases_tv %>%
#   group_by(Patient) %>%
#   summarise(total_events = sum(event), outcome = first(Outcome)) %>%
#   count(total_events, outcome)
# Deaths should all have total_events == 1
# Survivors should all have total_events == 0

#can't do vasopressors because missingness

cox_MEWS <- coxph(
  Surv(t.start, t.stop, event) ~ 
    MEWSgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)

cox_qsofa <- coxph(
  Surv(t.start, t.stop, event) ~ 
    qSOFAgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)

cox_UVA <- coxph(
  Surv(t.start, t.stop, event) ~ 
    UVAgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)

cox_SIRS <- coxph(
  Surv(t.start, t.stop, event) ~ 
    SIRSsuspected + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)

cox_NEWS <- coxph(
  Surv(t.start, t.stop, event) ~ 
    NEWSgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)

sapply(list(MEWS = cox_MEWS, qSOFA = cox_qsofa, SIRS = cox_SIRS, UVA = cox_UVA),
       function(m) summary(m)$concordance)

distancecox <- coxph(Surv(time, status)~ distance, data = trimmedcases)

agecox <- coxph(Surv(time, status)~ Age, data = trimmedcases)

qsofacox <- coxph(Surv(time, status)~ qSOFA, data = trimmedcases)

nrow(dailycases_tv)

# Check missingness for each model variable across ALL rows
dailycases_tv %>%
  summarise(
    total_rows = n(),
    MEWS_na    = sum(is.na(MEWS)),
    Age_na     = sum(is.na(Age)),
    event_na   = sum(is.na(event)),
    tstart_na  = sum(is.na(t.start)),
    tstop_na   = sum(is.na(t.stop))
  )
