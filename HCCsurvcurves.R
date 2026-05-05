#thank you to dan nedialkov, who spent half an afternoon rewriting my code and telling me
#how much basic coding I do not know
#someone employ this man ASAP
# Author: Rohan Rathi
# Date: 5/3/2026
# Project: HCC
# Uses HCCcleaned.xlsx to build survival curves for the project. NB: If JVIR gets mad about 
# us not checking collinear use lines 106 onwards
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(readxl)
library(tidyverse)
library(gtsummary)
library(xlsx)
library("survminer",warn.conflicts = FALSE)
library("Rcpp")
library('gt')
library(knitr)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(survival)
library(tidycmprsk)
library(devtools)
library(patchwork)

#thank you to dan nedialkov, who spent half an afternoon rewriting my code and telling me
#how much basic coding I do not know
#someone get this man a job ASAP


#CANNOT HAVE NULL AS REPLACEMENT



outputtable <- read_xlsx("NewData.xlsx")
#remove all commas in the service of making everything that needs to be numeric numeric
outputtable <- outputtable %>%
  mutate(across(where(is.character), ~ str_replace_all(., ",", "")))
outputtable <- outputtable %>%
  mutate(across(where(is.character), ~ str_replace_all(., ",", "")))
#outputtable <- lapply(outputtable, function(x) if(is.character(x)) gsub(",", "", x) else x)

#reformatting
outputtable[outputtable == "#N/A"] <- NA

#resigned myself to just hand coding
outputtable$Age <- as.numeric(outputtable$Age)
outputtable$MELD_Score <- as.numeric(outputtable$MELD_Score)
outputtable$WBC <- as.numeric(outputtable$WBC)
outputtable$AFP_ng_mL <- as.numeric(outputtable$AFP_ng_mL)
outputtable$Creatinine <- as.numeric(outputtable$Creatinine)
outputtable$Total_Bilirubin <- as.numeric(outputtable$Total_Bilirubin)
outputtable$ALT <- as.numeric(outputtable$ALT)
outputtable$RFA_Count <- as.numeric(outputtable$RFA_Count)
outputtable$Microvascular_Invasion <- as.numeric(outputtable$Microvascular_Invasion)
outputtable$Macrovascular_Invasion <- as.numeric(outputtable$Macrovascular_Invasion)
outputtable$Tumor_Number <- as.numeric(outputtable$Tumor_Number)
outputtable$Sodium <- as.numeric(outputtable$Sodium)
outputtable$INR <- as.numeric(outputtable$INR)
outputtable$Operative_Time_min <- as.numeric(outputtable$Operative_Time_min)
outputtable$pRBC_Transfused_mL <- as.numeric(outputtable$pRBC_Transfused_mL)
outputtable$LOS_days <- as.numeric(outputtable$LOS_days)
outputtable$'Tumor_Size_cm' <- as.numeric(outputtable$Tumor_Size_cm)
outputtable$Tumor_Number <- as.numeric(outputtable$Tumor_Number)
outputtable$Explant_Necrosis_Percent <- as.numeric(outputtable$Explant_Necrosis_Percent)
outputtable$Microvascular_Invasion <- as.numeric(outputtable$Microvascular_Invasion)
outputtable$Macrovascular_Invasion <- as.numeric(outputtable$Microvascular_Invasion)
outputtable$Recurrence <- as.numeric(outputtable$Recurrence)



#removing the cases  that got all 3
outputtable$Total_IR_Burden[outputtable$Total_IR_Burden == '3'] <- '2' 
outputtable$Total_IR_Burden[outputtable$Total_IR_Burden == '4'] <- '2'

#removing all cases with no tumor resected.
outputtable <- outputtable[is.na(outputtable$Tumor_size)==FALSE,]

#Start of Table
outputtable$Date_of_Death <- as.Date(outputtable$Date_of_Death, format = "%m/%d/%Y")
outputtable$Last_FU <- as.Date(outputtable$Last_FU, format = "%m/%d/%Y")
outputtable$Tumor_size <- as.numeric(outputtable$Tumor_size)

#Cox test 
last_fusurvcurve <-as.integer(as.Date(outputtable$Last_FU, format = "%m/%d/%Y") - as.Date("1/1/2010", format = "%m/%d/%Y"))
time <- as.integer(as.Date(outputtable$Date_of_Death, format = "%m/%d/%Y") - as.Date("1/1/2010", format = "%m/%d/%Y"))
time[is.na(time)] <- last_fusurvcurve[is.na(time)]


outputtable$Survival_Status[outputtable$Survival_Status == "Yes"] <- "0"
outputtable$Survival_Status[outputtable$Survival_Status == "No"] <- "1"
status <- as.integer(outputtable$Survival_Status)
outputtable$Total_IR_Burden <- factor(outputtable$Total_IR_Burden)

hcccox <- coxph(Surv(time, status)~ Total_IR_Burden + Age + ALT + MELD_Score
                + Tumor_size + Tumor_Number, data = outputtable)
summary(hcccox)

library(broom)
library(car)
write.csv(tidy(hcccox, exponentiate = TRUE, conf.int = TRUE), "HCCcox_results.csv", row.names = FALSE)
cox.zph(hcccox)

#To use if they get mad about us not checking correlation enough!!!!
vif(hcccox)
library(corrplot)
num_vars <- outputtable[, c("Age", "ALT", "MELD_Score", "Tumor_size", "Tumor_Number")]
correlationmatrix <- corrplot(cor(num_vars, use = "complete.obs"), method = "number")
gtsummary(correlationmatrix)
library(apaTables)
apa.cor.table(outputtable[, c("Age", "ALT", "MELD_Score", 
                              "Tumor_size", "Tumor_Number")],
              filename = "correlation_table.doc",
              table.number = 1)


Surv(time, status)
survcurve <- survfit(Surv(time, status)~factor(outputtable$Total_IR_Burden), data = outputtable)

time <- time/30.44
#Kaplan meier curve
survfit2(Surv(time, status) ~ outputtable$Total_IR_Burden, data = outputtable, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Time (Months)",
    y = "Overall survival"
  ) + 
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c("red", "blue", "green"),
    labels = c("No burden", "Single burden", "Multiple burden")
  ) +
  scale_color_discrete(
    labels = c("No burden", "Single burden", "Multiple burden")
  )


#individual survival curves using patchwork library

plot0 <- survfit2(Surv(time[outputtable$Total_IR_Burden==0], 
                       status[outputtable$Total_IR_Burden==0]) ~ 1, 
                  data = outputtable[outputtable$Total_IR_Burden == 0, ], start.time=0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    title = "No pre-op burden",
    x = "Time (Months)",
    y = "Overall survival",
    tag = "A"
  ) +
  theme(legend.position = 'top') 

plot1 <- survfit2(Surv(time[outputtable$Total_IR_Burden==1], 
                       status[outputtable$Total_IR_Burden==1]) ~ 1, 
                  data = outputtable[outputtable$Total_IR_Burden == 1, ], start.time=0) |>
  ggsurvfit() +
    ylim(0,1) +
    coord_cartesian(xlim = c(0, 125)) +
    labs(
      title = "Single pre-op procedure",
      x = "Time (Months)",
      y = "Overall survival",
      tag = "B"
    ) +
    theme(legend.position = 'top')

#there is some sapply function that would do my job here without making such a mess of code for a survfit
#oh well!
plot2 <- survfit2(Surv(time[outputtable$Total_IR_Burden==2], 
                       status[outputtable$Total_IR_Burden==2]) ~ 1, 
                  data = outputtable[outputtable$Total_IR_Burden == 2, ], start.time=0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    title = "Multiple pre-op procedures",
    x = "Time (Months)",
    y = "Overall survival",
    tag = "C"
  ) +
  theme(legend.position = 'top') 
wrap_plots(A = plot0, B = plot1, C = plot2, design = "AABB\n#CC#")


plotTACE <- survfit2(Surv(time[outputtable$TACE_Count==1], 
                       status[outputtable$TACE_Count==1]) ~ 1, 
                  data = outputtable[outputtable$TACE_Count==1, ], start.time=0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    title = "TACE",
    x = "Time (Months)",
    y = "Overall survival",
    tag = "C"
  ) +
  theme(legend.position = 'top') 

plotRFA <- survfit2(Surv(time[outputtable$RFA_Count==1], 
                          status[outputtable$RFA_Count==1]) ~ 1, 
                     data = outputtable[outputtable$RFA_Count==1, ], start.time=0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    title = "RFA",
    x = "Time (Months)",
    y = "Overall survival",
    tag = "C"
  ) +
  theme(legend.position = 'top')

plotY90 <- survfit2(Surv(time[outputtable$Y90_Count==1], 
                         status[outputtable$Y90_Count==1]) ~ 1, 
                    data = outputtable[outputtable$Y90_Count==1, ], start.time=0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    title = "Y90",
    x = "Time (Months)",
    y = "Overall survival",
    tag = "C"
  ) +
  theme(legend.position = 'top')
wrap_plots(A = plotTACE, B = plotRFA, C = plotY90, design = "AABB\n#CC#")
