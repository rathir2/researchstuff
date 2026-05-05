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

ICUcases <- read_csv("trimmedcases.csv")

trimmedcases <- read.xlsx("ICUcases_w_sepsis.xlsx", 1)

#adds sepsis score to the trimmed cases
trimmedcases$SepsisT <- sepsis_patients$sepsis_suspected
trimmedcases$Age <- as.integer(trimmedcases$Age)
trimmedcases %>%
  select(Age, Gender,`District.of.Origin`, sepsis, 'High.Risk.Sepsis', UVAScore, Outcome) %>% 
  tbl_summary(label = list(
    Age = "Age, median (range), years",
    Gender = "Sex, No. (%)",
    `District of Origin` = "District of Residence"
  )
  ) |>
  modify_header(label~"**Donor Characteristics**")
#adding survival status
trimmedcases$status <- trimmedcases$Outcome
trimmedcases$status[trimmedcases$status == "death"] <- 1
trimmedcases$status[trimmedcases$status == "downgraded"] <- 0
trimmedcases$status[trimmedcases$status == "transferred to outside hospital"] <- 0
trimmedcases$status[trimmedcases$status == "unspecified"] <- 0
trimmedcases$status <- as.numeric(trimmedcases$status)
trimmedcases$time <- trimmedcases$Length.of.stay

#Kaplan Meier
survfit2(Surv(time, status) ~ UVAScore, data = trimmedcases, start.time = 0) |>
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

survfit2(Surv(time, status) ~ SIRS, data = trimmedcases, start.time = 0) |>
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

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. < 15 ] <- "Short"

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. > 15 | 
                        trimmedcases$Distance.to.Butare..mi. < 31 ] <- "Medium"

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. > 31 | 
                        trimmedcases$Distance.to.Butare..mi. < 50 ] <- "Medium-long"

trimmedcases$distance[trimmedcases$Distance.to.Butare..mi. > 50 ] <- "Very-long"

UVAcox <- coxph(Surv(time, status)~ UVAScore, data = trimmedcases)

SIRScox <- coxph(Surv(time, status)~ SIRS, data = trimmedcases)

distancecox <- coxph(Surv(time, status)~ distance, data = trimmedcases)

agecox <- coxph(Surv(time, status)~ Age, data = trimmedcases)

qsofacox <- coxph(Surv(time, status)~ qSOFA, data = trimmedcases)

hcccox <- coxph(Surv(time, status)~ Total_IR_Burden + Age + ALT + MELD_Score
                + Tumor_size + Tumor_Number, data = outputtable)
