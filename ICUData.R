# Author: Rohan Rathi
# Date: 3/24/2026
# Project: Rwanda ICU
# Master code for trimming and analysis of the ICU data from Rwanda.
library(ggplot2)
library(dplyr)
library(readxl)
library(xlsx)
library(writexl)
library(tidyverse)
library(gtsummary)
library(cusum)
library(qcc)


ICUcases <- read_excel("ICUcases_w_sepsis.xlsx")
#trim down ICUcases-found a better way to do this.
#trimmedcases <- ICUcases %>%
 # filter(!is.na(Patient) & Patient != ""& Patient != " ")
#trimmedcases <- trimmedcases[trimmedcases$Patient != trimmedcases$Patient[61],]

#trimmedcases$Age <- as.numeric(trimmedcases$Age)
#trimmedcases$Gender[trimmedcases$Gender == "M?"] <- "M"
#write.xlsx(trimmedcases,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/rwanda/trimmedcases.xlsx", 
 #          sheetName = "Sheet1", row.names = TRUE, append = FALSE) 

#Calculates SIRS, qSOFA, UVA, suspected sepsis, and MEWS
ICUcases <- ICUcases %>%
  mutate(SIRS = 
           # Temp criterion
           (suppressWarnings(as.numeric(Temp) > 38 | as.numeric(Temp) < 36) %in% TRUE) +
           # HR criterion
           (suppressWarnings(as.numeric(HR) > 90) %in% TRUE) +
           # RR or pCO2 criterion
           (suppressWarnings(as.numeric(RR) > 20 | as.numeric(pCO2) < 32) %in% TRUE) +
           # WBC criterion
           (suppressWarnings(as.numeric(WBC) > 12 | as.numeric(WBC) < 4) %in% TRUE),
         sepsis = ifelse(SIRS >= 2, "Suspected", "No"),
         qSOFA = 
           # GCS criterion
           (suppressWarnings(as.numeric(GCS) < 15) %in% TRUE) +
           # SBP criterion
           (suppressWarnings(as.numeric(SBP) <= 100) %in% TRUE) +
           # RR criterion
           (suppressWarnings(as.numeric(RR) >= 22) %in% TRUE),
         'High Risk Sepsis' = ifelse(qSOFA >= 2, "Yes", "No"),
         UVAScore = 
           # SBP criterion
           (suppressWarnings(as.numeric(SBP) <= 90) %in% TRUE) + 
           # RR criterion
           (suppressWarnings(as.numeric(RR) >= 30) %in% TRUE) +
           # GCS criterion
           (suppressWarnings(as.numeric(GCS) < 15) %in% TRUE)*4 + 
           #Temp
           (suppressWarnings(as.numeric(Temp) < 36) %in% TRUE)*2 +
           # pO2 criterion
           (suppressWarnings(as.numeric(pO2) < 92) %in% TRUE)*2,
         MEWS =
           # SBP criterion
           case_when(
             suppressWarnings(as.numeric(SBP) <= 70)                                        ~ 3,
             suppressWarnings(as.numeric(SBP) >= 71)  & as.numeric(SBP) <= 80              ~ 2,
             suppressWarnings(as.numeric(SBP) >= 81)  & as.numeric(SBP) <= 100             ~ 1,
             suppressWarnings(as.numeric(SBP) >= 101) & as.numeric(SBP) <= 199             ~ 0,
             suppressWarnings(as.numeric(SBP) >= 200)                                       ~ 2,
             TRUE ~ 0
           ) +
           # HR criterion
           case_when(
             suppressWarnings(as.numeric(HR) < 40)                                          ~ 2,
             suppressWarnings(as.numeric(HR) >= 40)  & as.numeric(HR) <= 50                ~ 1,
             suppressWarnings(as.numeric(HR) >= 51)  & as.numeric(HR) <= 100               ~ 0,
             suppressWarnings(as.numeric(HR) >= 101) & as.numeric(HR) <= 110               ~ 1,
             suppressWarnings(as.numeric(HR) >= 111) & as.numeric(HR) <= 129               ~ 2,
             suppressWarnings(as.numeric(HR) >= 130)                                        ~ 3,
             TRUE ~ 0
           ) +
           # RR criterion
           case_when(
             suppressWarnings(as.numeric(RR) < 9)                                           ~ 2,
             suppressWarnings(as.numeric(RR) >= 9)  & as.numeric(RR) <= 14                 ~ 0,
             suppressWarnings(as.numeric(RR) >= 15) & as.numeric(RR) <= 20                 ~ 1,
             suppressWarnings(as.numeric(RR) >= 21) & as.numeric(RR) <= 29                 ~ 2,
             suppressWarnings(as.numeric(RR) >= 30)                                         ~ 3,
             TRUE ~ 0
           ) +
           # Temp criterion
           case_when(
             suppressWarnings(as.numeric(Temp) < 35)                                        ~ 2,
             suppressWarnings(as.numeric(Temp) >= 35)   & as.numeric(Temp) <= 38.4         ~ 0,
             suppressWarnings(as.numeric(Temp) >= 38.5)                                     ~ 2,
             TRUE ~ 0
           ) +
           # Neurological criterion (mapped from GCS)
           case_when(
             suppressWarnings(as.numeric(GCS) == 15)                                        ~ 0,  # Alert
             suppressWarnings(as.numeric(GCS) >= 12) & as.numeric(GCS) <= 14               ~ 1,  # Voice
             suppressWarnings(as.numeric(GCS) >= 9)  & as.numeric(GCS) <= 11               ~ 2,  # Pain
             suppressWarnings(as.numeric(GCS) <= 8)                                         ~ 3,  # Unresponsive
             TRUE ~ 0
           ) 
         )

#Removing empty columns and rows
ICUcases <- ICUcases %>%
  select(-matches("^\\.{3}\\d{3}$"))
ICUcases <- ICUcases %>% 
  filter(!is.na(Day) & Day != "")

#because i'm working on two computers, having to switch back and forth here. not great
write.xlsx(ICUcases,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/rwanda/ICUcases_w_sepsis.xlsx", 
          sheetName = "Sheet1", row.names = TRUE, append = FALSE) 
#write_xlsx(ICUcases,"/home/rrathi02/ICU/ICUcases_w_sepsis.xlsx")
trimmedcases <- read.xlsx("ICUcases_w_sepsis.xlsx", 1)

#summarizes the ICU cases by whether a patient had sepsis suspected during their time.
sepsis_patients <- ICUcases %>%
  group_by(Patient) %>%
  summarise(
    sepsis_suspected = ifelse(any(sepsis == "Suspected", na.rm = TRUE), "Yes", "No")
  ) %>%
  arrange(as.numeric(Patient))
sepsis_patients <- sepsis_patients[-c(104,105),]

#adds sepsis score to the trimmed cases
trimmedcases$SepsisT <- sepsis_patients$sepsis_suspected
trimmedcases$Age <- as.integer(trimmedcases$Age)
trimmedcases %>%
  select(Age, Gender,`District.of.Origin`, sepsis, 'High.Risk.Sepsis') %>% 
  tbl_summary(label = list(
    Age = "Age, median (range), years",
    Gender = "Sex, No. (%)",
    `District of Origin` = "District of Residence"
  )
  ) |>
  modify_header(label~"**Donor Characteristics**")
