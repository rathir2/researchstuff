library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(gtsummary)
library(xlsx)
library("survival")
library("survminer")
library("Rcpp")
library('gt')
library(knitr)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(tidycmprsk)
library(devtools)


ICUdata <- pd.read_csv("Master ICU Data(Sheet1).csv", dtype = str)

outputtable %>%
  select('Surgery_Type', 'Operative_Time_min', 
         'pRBC_Transfused_mL', 'LOS_days', 'Survival_Status','Total_IR_Burden') %>%
  tbl_summary(by = 'Total_IR_Burden', 
              label = list(
                Surgery_Type = "Surgery Type, no (%)",
                Operative_Time_min = "Operative time, median (range), min",
                pRBC_Transfused_mL = "Transfused pRBC, median (range), mL",
                LOS_days = "Length of Stay, median (range), days",
                Total_Bilirubin = "T. bil, median (range), mg/dL",
                INR = "INR, median (range)",
                Sodium = "Sodium, median (range), mg/dL",
                Survival_Status = "% patients alive, no. (%)"
              )
  ) |> add_p() |>
  modify_header(label~"**Intra-operative Characteristics**",
                stat_1 ~ "**No previous IR surgery**  \nN = 174 ",
                stat_2 ~ "**Single previous IR surgery**  \nN = 90", 
                stat_3 ~ '**Multiple previous IR surgeries**  \nN = 27'
  ) 