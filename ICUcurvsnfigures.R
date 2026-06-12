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
#adds sepsis score to the trimmed cases
#trimmedcases$SepsisT <- sepsis_patients$sepsis_suspected

cases$Age <- as.integer(cases$Age)
#fix with Day 0

#adding survival status
#this version changed trimmedcases to ICUcases.
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

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. < 15 ] <- "Short"

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. > 15 | 
                        trimmedcases$Distance.to.Butare..mi. < 31 ] <- "Medium"

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. > 31 | 
                        trimmedcases$Distance.to.Butare..mi. < 50 ] <- "Medium-long"

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. > 50 ] <- "Very-long"


cases %>%
  select(Age, Gender,`District.of.Origin`, sepsis, 'High.Risk.Sepsis', UVAScore, Outcome) %>% 
  tbl_summary(label = list(
    Age = "Age, median (range), years",
    Gender = "Sex, No. (%)",
    `District of Origin` = "District of Residence"
  )
  ) |>
  modify_header(label~"**Donor Characteristics**")

#Kaplan Meier
survfit2(Surv(time, status) ~ UVAScore_day0, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red", "blue", "green", "yellow", "black"),
    labels = c("UVA score 0", "UVA Score 1", "UVA score 2", "UVA score 3", "UVA score 4")
  ) +
  scale_color_discrete(
    labels = c("UVA score 0", "UVA Score 1", "UVA score 2", "UVA score 3", "UVA score 4")
  )

#figure out a way to make this work without having all the NAs
survfit2(Surv(time, status) ~ UVAScore_day3, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red", "blue", "green", "yellow", "black"),
    labels = c("UVA score 0", "UVA Score 1", "UVA score 2", "UVA score 3", "UVA score 4")
  ) +
  scale_color_discrete(
    labels = c("UVA score 0", "UVA Score 1", "UVA score 2", "UVA score 3", "UVA score 4")
  )

survfit2(Surv(time, status) ~ SIRS_day0, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red", "blue", "green", "yellow", "black"),
    labels = c("Sirs score 0", "Sirs Score 1", "Sirs score 2", "Sirs score 3", "Sirs score 4")
  ) +
  scale_color_discrete(
    labels = c("Sirs score 0", "Sirs Score 1", "Sirs score 2", "Sirs score 3", "Sirs score 4")
  )



#what the actual fuck was i trying to do here.
UVAcomp <- list((suppressWarnings(as.numeric(trimmedcases$SBP) <= 90) %in% TRUE), 
  (suppressWarnings(as.numeric(trimmedcases$RR) >= 30) %in% TRUE),
  (suppressWarnings(as.numeric(trimmedcases$GCS) < 15) %in% TRUE), 
  (suppressWarnings(as.numeric(trimmedcases$Temp) < 36) %in% TRUE),
  (suppressWarnings(as.numeric(trimmedcases$pO2) < 92) %in% TRUE))
for(i in 1:5){
  coxph(Surv(time, status) ~ UVAcomp[i])
}
  
