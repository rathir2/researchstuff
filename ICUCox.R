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



UVAd0 <- coxph(Surv(time, status)~ UVAScore_day0, data = cases)

SIRSd0 <- coxph(Surv(time, status)~ SIRS_day0, data = cases)

UVAd0m <- coxph(Surv(time, status) ~ UVAScore_day0 + Age + Creatinine..mg.dL.
                + data = cases)

distancecox <- coxph(Surv(time, status)~ distance, data = trimmedcases)

agecox <- coxph(Surv(time, status)~ Age, data = trimmedcases)

qsofacox <- coxph(Surv(time, status)~ qSOFA, data = trimmedcases)

hcccox <- coxph(Surv(time, status)~ Total_IR_Burden + Age + ALT + MELD_Score
                + Tumor_size + Tumor_Number, data = outputtable)




