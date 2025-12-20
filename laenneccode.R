library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(gtsummary)

#load data
cases <- read_excel("robot living donor liver.xlsx")

cases<- subset(cases, cases$`open conversion` != "planned") 
cases$`Operation Name` <- "Laennec Approach Robotic donor hepatectomy (Rt)"
#previous operations need to be segregated into upper abdominal surgeries and not upper abdominal surgeries.
cases$'upper abdominal?' <- "no"
cases$`upper abdominal?`[cases$`Previous operation` == "prev. cholecystectomy"]<- "yes"
cases$`upper abdominal?`[cases$`Previous operation` == "cholecystectomy"]<- "yes"
cases$`upper abdominal?`[cases$`Previous operation` == "C-sec x 3"]<- "yes"
cases$`upper abdominal?`[cases$`Previous operation` == "hysterectomy, kidney donation (Lt)"]<- "yes"
cases$`upper abdominal?`[cases$`Previous operation` == "C-section"]<- "yes"


#initial donor characteristics. Modeled heavily off of Sambommatsu et al. 2025-i think Kush did those stats
cases %>%
  select(Age, Sex, BMI,`Operation Name`, `upper abdominal?`) %>% 
  tbl_summary(by = `Operation Name`,
              statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
              label = list(
                Age = "Age, median (range), years",
                Sex = "Sex, No. (%)",
                BMI = "BMI, median (range)",
                `previous operation` = "Previous upper abdominal surgery, No. (%)",
                `Operation time, min` = "Operation time (min)",
                `ABO Blood Type` = "Blood Type",
                'upper abdominal?' = "Previous Upper Abdominal Surgery"
              )
              ) |>
  modify_header(label~"**Donor Characteristics**")

##these are all for anatomic characteristics
#cleaning up hepatic vein
cases$`Hepatic vein`[cases$`Hepatic vein`== "SINGLE"] <- "single"
cases$`Hepatic vein`[cases$`Hepatic vein`== "single"] <- "1"
cases$`Hepatic vein`[cases$`Hepatic vein`== "one"] <- "1"
cases$`Hepatic vein`[cases$`Hepatic vein`== "two"] <- "2"
cases$`Hepatic vein`[cases$`Hepatic vein`== "Two"] <- "2"
cases$`Hepatic vein`[cases$`Hepatic vein`== "four"] <- "4"
cases$`Hepatic vein`[cases$`Hepatic vein`== "three"] <- "3"

#adding artery origins
cases$'artery origin' <- "Celiac Artery"
cases$'artery origin'[cases$artery == "single (from SMA)"] <- "Superior Mesenteric Artery"
cases$'artery origin'[cases$artery == "single (above CBD)"] <- "Above Common Bile Duct"
cases$'artery origin'[cases$artery == "single (from CA)"] <- "CA"


#cleaning up artery stuff
cases$'artery'[cases$artery == "single"] <- "1"
cases$'artery'[cases$artery == "two"] <- "2"
cases$'artery'[cases$artery == "single (from SMA)"] <- "1"
cases$'artery'[cases$artery == "single (above CBD)"] <- "1"
cases$'artery'[cases$artery == "single (from CA)"] <- "1"
cases$'artery'[cases$artery == "SINGLE"] <- "1"

#cleaning up portal stuff
cases$'portal'[cases$portal == "single"] <- "1"
cases$'portal'[cases$portal == "two"] <- "2"
cases$'portal'[cases$portal == "SINGLE"] <- "1"



#anatomic characteristics table

cases %>%
  select(`Operation Name`, 
         `Liver volumetry`,
         `Actual graft`, `artery`, `portal`,
         `bile duct`, `Hepatic vein`, 'artery origin') %>% 
  tbl_summary(by = `Operation Name`,
              label = list(
                `Liver volumetry` = "Total liver volume, median (range), mL",
                `Actual graft` = "Actual graft, median (range), g",
                `artery` = "Number of hepatic arteries, No.",
                `portal` = "Number of Portal Veins, No.",
                `bile duct` = "Number of Bile Ducts, No.",
                `Hepatic vein` = "Number of Hepatic Veins, No.",
                'artery origin' = "Origin of Hepatic Arteries"
              )
  ) |>
  modify_header(label~"**Anatomic Characteristics**")

#intraoperative characteristics table
cases %>%
  select(`Operation Name`, `EBL`, `parenchymal time`, `operation time`, `WIT`
         ) %>% 
  tbl_summary(by = `Operation Name`,
              statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
              label = list(
                `EBL` = "EBL, median (range), mL",
                `Cell Saver amount, mL (or none)` = "Intraoperative cell salvage use, No. (%)",
                `operation time` = "Operation time, median (range), min",
                `parenchymal time` = "Parenchymal transection time, median (range), min",
                `WIT` = "WIT, median (range), min"
              )
              ) |>
  modify_header(label~"**Operative Characteristics**")

cases$'complications yes or no'<- "yes"
cases$'complications yes or no'[cases$complications == "no"] <- "no"
cases %>%
  select(`Operation Name`, `Peak T.bil` , 
         `complications yes or no`, `Peak ALT`, 
         `hospital stay`) %>% 
  tbl_summary(by = `Operation Name`,
              type = list('hospital stay' ~ "continuous2"),
              statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
              label = list(
                `Ileus (y/n)` = "Postoperative Ileus",
                `hospital stay` = "Hospital Stay (days)",
                `complications yes or no` = "Total postoperative complications, no.",
                `Bile leak (y/n)` = "Bile leak, No. (%)"
                )
              ) |>
  modify_header(label~"**Post Operative Outcomes**")



