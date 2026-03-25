# Author: Rohan Rathi
# Date: 3/24/2026
# Project: Rwanda ICU
# Master code for trimming and analysis of the ICU data from Rwanda.
library(ggplot2)
library(dplyr)
library(readxl)
library(xlsx)
library(tidyverse)
library(gtsummary)
library(cusum)
library(qcc)

#some nonsense this is wrong
ICUcases <- read_csv("trimmedcases.csv")
#trim down ICUcases
trimmedcases <- ICUcases %>%
  filter(!is.na(Patient) & Patient != ""& Patient != " ")
trimmedcases <- trimmedcases[trimmedcases$Patient != trimmedcases$Patient[61],]

trimmedcases$Age <- as.numeric(trimmedcases$Age)
trimmedcases$Gender[trimmedcases$Gender == "M?"] <- "M"
write.xlsx(trimmedcases,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/rwanda/trimmedcases.xlsx", 
           sheetName = "Sheet1", row.names = TRUE, append = FALSE) 

#Calculates SIRS score and whether sepsis is suspected
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
         'High Risk Sepsis' = ifelse(qSOFA >= 2, "Yes", "No"))

#Removing empty columns and rows
ICUcases <- ICUcases %>%
  select(-matches("^\\.{3}\\d{3}$"))
ICUcases <- ICUcases %>% 
  filter(!is.na(Day) & Day != "")


write.xlsx(ICUcases,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/rwanda/ICUcases_w_sepsis.xlsx", 
           sheetName = "Sheet1", row.names = TRUE, append = FALSE) 
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

trimmedcases %>%
  select(Age, Gender,`District.of.Origin`, sepsis, 'High.Risk.Sepsis') %>% 
  tbl_summary(label = list(
    Age = "Age, median (range), years",
    Gender = "Sex, No. (%)",
    `District of Origin` = "District of Residence"
  )
  ) |>
  modify_header(label~"**Donor Characteristics**")
