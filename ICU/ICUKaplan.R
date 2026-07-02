library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(cusum)
library(qcc)
library(ggsurvfit)
library("survminer",warn.conflicts = FALSE)
library("Rcpp")
library('gt')
library(survival)
library(broom)


#has trimmed data with sepsis info by patient.
cases <- read.xlsx("trimmeddata.xlsx")


#data with cases with daily metrics
dailycases <- read.xlsx("ICUcases_w_sepsis.xlsx")
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
  mutate(across(everything(), ~if_else(. == "PEND", NA,.))) %>%
  mutate(across(where(is.character), ~ gsub("\\?", "", .x)))

cases$distance[cases$'Distance.to.Butare.(mi)' < 15 ] <- "Short"

cases$distance[cases$'Distance.to.Butare.(mi)' > 15 | 
                        cases$'Distance.to.Butare.(mi)' < 31 ] <- "Medium"

cases$distance[cases$'Distance.to.Butare.(mi)' > 31 | 
                        cases$'Distance.to.Butare.(mi)' < 50 ] <- "Medium-long"

cases$distance[cases$'Distance.to.Butare.(mi)' > 50 ] <- "Very-long"


cases %>%
  select(Age, Gender,`District.of.Origin`, SIRSsuspected,  
         UVAgroup, UVAgroup2, qSOFAgroup, MEWSgroup, NEWSgroup, Outcome) %>% 
  tbl_summary(label = list(
    Age = "Age, median (range), years",
    Gender = "Sex, No. (%)",
    `District of Origin` = "District of Residence"
  )
  ) |>
  modify_header(label~"**Donor Characteristics**")

#Kaplan Meier
survfit2(Surv(time, status) ~ UVAgroup, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival",
    title = "Survival by UVA Score"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red", "blue", "green"),
    labels = c("Low Risk (<2)", "Medium Risk (2-4)", "High Risk (>4)")
  ) +
  scale_color_discrete(
    labels = c("Low Risk (<2)", "Medium Risk (2-4)", "High Risk (>4)")
  ) +
  add_risktable()

survfit2(Surv(time, status) ~ UVAgroup2, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival",
    title = "Survival by UVA Score (adjusted)"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red", "blue", "green"),
    labels = c("Low Risk", "Medium Risk", "High Risk")
  ) +
  scale_color_discrete(
    labels = c("Low Risk", "Medium Risk", "High Risk")
  ) +
  add_risktable()


survfit2(Surv(time, status) ~ qSOFAgroup, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival",
    title = "Survival by qSOFA Score"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red",  "green"),
    labels = c("Low Risk (<2)", "High Risk (≥2)")
  ) +
  scale_color_discrete(
    labels = c("Low Risk (<2)", "High Risk (≥2)")
  ) +
  add_risktable()


survfit2(Surv(time, status) ~ MEWSgroup, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival",
    title = "Survival by MEWS Score"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red",  "green", "blue", "black"),
    labels = c("Very Low Risk (0)", "Low Risk (1-2)", "Medium Risk (3-4)", "High Risk (>4)")
  ) +
  scale_color_manual(
    values = c("red", "green", "blue", "black"),
    labels = c("Very Low Risk (0)", "Low Risk (1-2)", "Medium Risk (3-4)", "High Risk (>4)")
  ) +
  add_risktable()

survfit2(Surv(time, status) ~ NEWSgroup, data = cases, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Days)",
    y = "Overall survival",
    title = "Survival by NEWS Score"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red",  "green", "blue"),
    labels = c("Low Risk (<4)", "Medium Risk (4-6)", "High Risk (>6)")
  ) +
  scale_color_manual(
    values = c("red", "green", "blue"),
    labels = c("Low Risk", "Medium Risk", "High Risk")
  ) +
  add_risktable()



# This doesn't work and it looks liek shit. Thanks gemini!
# # 1. Save your fit object
# fit <- survfit2(Surv(time, status) ~ MEWSgroup, data = cases, start.time = 0)
# 
# # 2. Extract data coordinates for your shapes
# plot_data <- tidy_survfit(fit)
# 
# # 3. Optional: Thin the shapes out if they look crowded on the lines
# # Change 10 to whatever day interval looks best for your timeline
# thinned_data <- subset(plot_data, round(time) %% 1 == 0)
# 
# # 4. Generate your final plot
# fit |> 
#   ggsurvfit(linewidth = 1) + 
#   
#   # Overlay the symbols on the lines
#   geom_point(
#     data = thinned_data, # Use 'plot_data' here instead if you want symbols on every step
#     aes(x = time, y = estimate, color = strata, shape = strata), 
#     size = 2
#   ) +
#   
#   ylim(0, 1) + 
#   labs(x = "Time (Days)", y = "Overall survival") + 
#   theme(legend.position = 'top') + 
#   
#   # Map colors (Must match name and labels exactly to merge with shape legend)
#   scale_color_manual(
#     name = "MEWSgroup",
#     values = c("red", "green", "blue", "black"), 
#     labels = c("No Risk", "Low Risk", "Medium Risk", "High Risk") 
#   ) + 
#   scale_fill_manual(
#     name = "MEWSgroup",
#     values = c("red", "green", "blue", "black"), 
#     labels = c("No Risk", "Low Risk", "Medium Risk", "High Risk") 
#   ) +
#   
#   # Assign unique symbols to each line (Must match name and labels exactly)
#   scale_shape_manual(
#     name = "MEWSgroup",
#     values = c(16, 17, 15, 18), # Circle, Triangle, Square, Diamond
#     labels = c("No Risk", "Low Risk", "Medium Risk", "High Risk")
#   )










#_____________________________________Day 0  vs Day 3 stuff______________________
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


# 
# #what the actual fuck was i trying to do here.
# UVAcomp <- list((suppressWarnings(as.numeric(trimmedcases$SBP) <= 90) %in% TRUE), 
#   (suppressWarnings(as.numeric(trimmedcases$RR) >= 30) %in% TRUE),
#   (suppressWarnings(as.numeric(trimmedcases$GCS) < 15) %in% TRUE), 
#   (suppressWarnings(as.numeric(trimmedcases$Temp) < 36) %in% TRUE),
#   (suppressWarnings(as.numeric(trimmedcases$pO2) < 92) %in% TRUE))
# for(i in 1:5){
#   coxph(Surv(time, status) ~ UVAcomp[i])
# }
#   
