#Input: ICU Case data with sepsis scores of each patient on a daily bases
#Output: COX models

library(ggplot2)
library(dplyr)
library(readxl)
library(xlsx)
library(tidyverse)
library(gtsummary)
library(ggsurvfit)
library("survminer",warn.conflicts = FALSE)
library("Rcpp")
library('gt')
library(survival)
library(flextable)


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
  ungroup() %>%
  mutate(
    status = case_when(
      Outcome == "death" ~ 1,
      Outcome == "downgraded" ~ 0,
      Outcome == "transferred to outside hospital" ~ 0,
      Outcome == "unspecified" ~ 0
    )
  )

patient_data <- dailycases_tv %>%
  group_by(Patient) %>%
  summarise(
    MEWS     = max(MEWS,      na.rm = TRUE),
    qSOFA    = max(qSOFA,     na.rm = TRUE),
    SIRS     = max(SIRS,      na.rm = TRUE),
    UVAScore = max(UVAScore,  na.rm = TRUE),
    NEWS     = max(NEWS,      na.rm = TRUE),
    died     = max(event,     na.rm = TRUE),
    Age = first(Age[Day == 0]),
    Gender = first(Gender[Day == 0]),
    .groups = "drop"
  )


write.csv(dailycases_tv,"ICUCOXdata.csv", row.names = FALSE)
write.xlsx(patient_data, "trimmedLOGdata.xlsx", rowNames = FALSE)
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
MEWS_ph_test <- cox.zph(cox_MEWS)

log_MEWS <- glm(status ~ MEWSgroup + Age + Gender, 
                family = binomial(link = 'logit'), 
                data = dailycases_tv)
log_MEWS %>%
  tbl_regression(exponentiate = TRUE)

# Pseudo R-squared
pR2(log_MEWS)

# Hosmer-Lemeshow goodness of fit
hoslem.test(dailycases_tv$status, fitted(log_MEWS))

roc_obj <- roc(dailycases_tv$status, fitted(log_MEWS))
auc(roc_obj)
plot(roc_obj,
     print.auc = TRUE,          # adds AUC to the plot
     auc.polygon = TRUE,        # shades the area under the curve
     grid = TRUE,
     col = "blue",
     main = "ROC Curve - Logistic Regression")

tidy(cox_MEWS, exponentiate = TRUE, conf.int = TRUE) |>
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(x = "Hazard Ratio", y = NULL) +
  theme_minimal()

cox_qsofa <- coxph(
  Surv(t.start, t.stop, event) ~ 
    qSOFAgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)
qsofa_ph_test <- cox.zph(cox_qsofa)

cox_UVA <- coxph(
  Surv(t.start, t.stop, event) ~ 
    UVAgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)
UVA_ph_test <- cox.zph(cox_UVA)

cox_UVA2 <- coxph(
  Surv(t.start, t.stop, event) ~ 
    UVAgroup2 + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)
UVA_ph_test <- cox.zph(cox_UVA2)

cox_SIRS <- coxph(
  Surv(t.start, t.stop, event) ~ 
    SIRSsuspected + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)
SIRS_ph_test <- cox.zph(cox_SIRS)

cox_NEWS <- coxph(
  Surv(t.start, t.stop, event) ~ 
    NEWSgroup + Age + Gender,
  data    = dailycases_tv,
  id      = Patient,       # tells R which rows belong to same patient
  cluster = Patient        # robust SE to account for within-patient correlation
)
NEWS_ph_test <- cox.zph(cox_NEWS)

comparemodels <- sapply(list(MEWS = cox_MEWS, qSOFA = cox_qsofa, SIRS = cox_SIRS, UVA = cox_UVA, NEWS = cox_NEWS),
       function(m) summary(m)$concordance)

