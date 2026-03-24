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
library(survival)
library(tidycmprsk)
library(devtools)

ezfun::set_ccf_palette("contrast")

#load data
hccdata <- read_csv("HCC Database.csv")
HCCdata <- read_csv("HCC Database.csv")
#make first row header (not possible in readcsv?)
names(hccdata) <- hccdata[1,]
hccdata <- hccdata[-1,]


#Fixing data (remember to delete first row when done!)
#sex
hccdata$Sex[hccdata$Sex == "1"] <- "Male"
hccdata$Sex[hccdata$Sex == "2"] <- "Female"
#race
sublist <- list("White", "Black", "Hispanic", "Asian", "Others")
for (i in 1:8){
  hccdata$'Race'[hccdata$'Race'== i] <- sublist[i]
}
#cirrhosis etiology
sublist <- list("HBV", "HCV", "NASH", "Alcoholic", "PBC/PSC", "Autoimmune", "Other etiology", "HCV + HBV")
for (i in 1:8){
  hccdata$'diagnosis for cirrhosis'[hccdata$'diagnosis for cirrhosis'== i] <- sublist[i]
}
#there's a way to do this in dplyr that's significantly easier but i couldn't figure out how 
#and when faced with the thought of doing something timeconsuming i say no
#tace
hccdata$TACE[hccdata$TACE == "1"] <- "Yes"
hccdata$TACE[hccdata$TACE == "2"] <- "No"
hccdata$TACE[is.na(hccdata$TACE)] <- "Unknown"
#RFA
hccdata$RFA[hccdata$RFA == "1"] <- "Yes"
hccdata$RFA[hccdata$RFA == "2"] <- "No"
hccdata$RFA[is.na(hccdata$RFA)] <- "Unknown"
#Y90
colnames(hccdata)[36] <- "y90"
hccdata$y90[hccdata$'Other pre-treatment' == "Y-90"] <- "Yes"
hccdata$y90[hccdata$y90 == '2'] <- "No"
hccdata$y90[is.na(hccdata$y90)] <- "Unknown"
#Total IR burden (this is painful)
hccdata$TACE[hccdata$TACE == "Yes"] <- 1
hccdata$TACE[hccdata$TACE == "No"] <- 0
hccdata$TACE[hccdata$TACE == "Unknown"] <- 0
hccdata$RFA[hccdata$RFA == "Yes"] <- 1
hccdata$RFA[hccdata$RFA == "No"] <- 0
hccdata$TACE[hccdata$TACE == "Unknown"] <- 0
hccdata$y90[hccdata$y90 == "Yes"] <- 1
hccdata$y90[hccdata$y90 == "Unknown"] <- 0
hccdata$y90[hccdata$y90 == "No"] <- 0
hccdata$totalpreopburden <- as.numeric(hccdata$TACE) +
  as.numeric(hccdata$RFA) + as.numeric(hccdata$y90)

#recurrence site
hccdata$'Intra- vs Extra-hepatic'[hccdata$'Intra- vs Extra-hepatic' == "1"] <- "Intrahepatic"
hccdata$'Intra- vs Extra-hepatic'[hccdata$'Intra- vs Extra-hepatic' == "2"] <- "Extrahepatic"
hccdata$'Intra- vs Extra-hepatic'[hccdata$'Intra- vs Extra-hepatic' == "3"] <- "Both"

#this broke the race column and I don't know why so here's a hotfix
#hccdata$Race <- HCCdata$Race
#it didn't work. i think im gonna kill myself
#this did
hccdata$Race <- as.character(hccdata$Race)
colnames(hccdata) [6] <- "Etiology"
colnames(hccdata) [49] <- "Total size"
hccdata <- hccdata[,-5]


#surgery type
hccdata$'surgery type'[hccdata$'surgery type' == "1"] <- "Open"
hccdata$'surgery type'[hccdata$'surgery type' == "2"] <- "Laparoscopic"

#survival
hccdata$Survival[hccdata$Survival == '1'] <- "Yes"
hccdata$Survival[hccdata$Survival == '2'] <- "No"

colnames(hccdata)[55]<- "Total tumor size"
#output table
colnames(hccdata)[52] <- "size, largest"
outputdata <-hccdata[,c('Sex','Age','Race', 'diagnosis for cirrhosis','AFP', 'MELD Score'
                        , 'WBC (10^9/L)', 'ALT', 'Cr', 'T. bili (mg/dL)', 'INR', 'Na'
                        , 'TACE','RFA', 'y90', 'totalpreopburden', 'surgery type', 'Operative time', 
                        'Transfused pRBC (mL)', 'LOS (days post-op to discharge)', 
                        'Total tumor size', 'tumor number', 'tumor necrosis', 'Microvessel invasion',
                        'Macrovessel invasion', 'Recurrence', 'Intra- vs Extra-hepatic', 'Survival',
                        'Date of last follow up', 'AJCC staging', 'Date of death', 'size, largest'
                        )
                     ]
outputnames <- read_excel("template names.xlsx")
colnames(outputdata) <- colnames(outputnames)[-1]

outputdata$`Race`<- as.character(outputdata$`Race`)
outputdata$'Cirrhosis_Etiology'<- as.character(outputdata$`Cirrhosis_Etiology`)


write.xlsx(outputdata,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/HCC/HCCcleaned.xlsx", 
           sheetName = "Sheet1", row.names = TRUE, append = FALSE)

outputtable <- read_xlsx("HCCcleaned.xlsx")
outputtable <- outputtable[-1,]
#remove all commas in the service of making everything that needs to be numeric numeric
outputtable <- outputtable %>%
  mutate(across(where(is.character), ~ str_replace_all(., ",", "")))
#outputtable <- lapply(outputtable, function(x) if(is.character(x)) gsub(",", "", x) else x)
#series of functions to make everything numeric so it'll work (it didn't work)
#is_all_numeric <- function(x) {
#  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
#}
#outputtable <- outputtable %>% 
#  mutate_if(is_all_numeric,as.numeric) %>%
#  str()
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
#Explant necrosis fix
outputtable$Explant_Necrosis_Percent[outputtable$Explant_Necrosis_Percent == "1"] <- "Yes"
outputtable$Explant_Necrosis_Percent[outputtable$Explant_Necrosis_Percent == "2"] <- "No"

#Microvascular invasion fix
outputtable$Microvascular_Invasion[outputtable$Microvascular_Invasion == "1"] <- 'Yes'
outputtable$Microvascular_Invasion[outputtable$Microvascular_Invasion == "2"] <- 'No'
outputtable$Microvascular_Invasion[outputtable$Microvascular_Invasion == "0"] <- NULL

#Macrovascular invasion fix
outputtable$Macrovascular_Invasion[outputtable$Macrovascular_Invasion == "1"] <- 'Yes'
outputtable$Macrovascular_Invasion[outputtable$Macrovascular_Invasion == "2"] <- 'No'
outputtable$Macrovascular_Invasion[outputtable$Macrovascular_Invasion == "0"] <- NULL

#Recurrence fix
outputtable$Recurrence[outputtable$Recurrence == "1"] <- "Yes"
outputtable$Recurrence[outputtable$Recurrence == "2"] <- "No"

#removing the cases  that got all 3
outputtable$Total_IR_Burden[outputtable$Total_IR_Burden == '3'] <- '2' 
outputtable$Total_IR_Burden[outputtable$Total_IR_Burden == '4'] <- '2'

#removing all cases with no tumor resected.
outputtable <- outputtable[is.na(outputtable$Tumor_size)==FALSE,]

#the tables
#[1] "...1"                     "Sex"                      "Age"                     
#[4] "Race"                     "Cirrhosis_Etiology"       "AFP_ng_mL"               
#[7] "MELD_Score"               "WBC"                      "ALT"                     
#[10] "Creatinine"               "Total_Bilirubin"          "INR"                     
#[13] "Sodium"                   "TACE_Count"               "RFA_Count"               
#[16] "Y90_Count"                "Total_IR_Burden"          "Operation_name"          
#[19] "Surgery_Type"             "Operative_Time_min"       "pRBC_Transfused_mL"      
#[22] "LOS_days"                 "Tumor_Size_cm"            "Tumor_Number"            
#[25] "Explant_Necrosis_Percent" "Microvascular_Invasion"   "Macrovascular_Invasion"  
#[28] "Recurrence"               "Recurrence_Site"          "Survival_Status"         
#[31] "Date_of_Death"            "Last_FU"  
outputtable %>%
  select('Sex','Age','Race', 'Cirrhosis_Etiology', 'MELD_Score', 'Total_IR_Burden') %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
              type = list(`Age` ~ "continuous2"),
              label = list(
                Age = "Age, median (range), years",
                Sex = "Sex, No. (%)",
                Race = "Race, No. (%)",
                Total_IR_Burden = "Pre-operative IR Burden (TACE, RFA, 
                and/or Radiation)",
                Cirrhosis_Etiology = "Cirrhosis Etiology",
                MELD_Score = "MELD Score"
                )
              ) |>
  modify_header(label~"**Patient Demographics**")

#Table 2
outputtable %>%
  select('AFP_ng_mL','WBC', 'ALT', 'Creatinine', 'Total_Bilirubin', 'INR', 'Sodium', 'Total_IR_Burden') %>% 
  tbl_summary(by= 'Total_IR_Burden',
              statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
              type = list(`AFP_ng_mL` ~ "continuous2",
                          'WBC' ~ "continuous2",
                          'ALT' ~ "continuous2",
                          'Creatinine' ~ "continuous2",
                          'Total_Bilirubin' ~ "continuous2",
                          'Sodium' ~ 'continuous2'
                          ),
              label = list(
                AFP_ng_mL = "AFP, median (range), ng/mL",
                WBC = "WBC, median (range), 10^9/L",
                ALT = "ALT, median (range), U/L",
                Creatinine = "Creatinine, median (range), mg/dL",
                Total_Bilirubin = "T. bil, median (range), mg/dL",
                INR = "INR, median (range)",
                Sodium = "Sodium, median (range), mg/dL"
              )
  ) |> add_p()|>
  modify_header(label~"**Pre-operative Characteristics**",
                stat_1 ~ "**No previous IR surgery**  \nN = 174 ",
                  stat_2 ~ "**Single previous IR surgery**  \nN = 90", 
                stat_3 ~ '**Multiple previous IR surgeries**  \nN = 27'
                )

#Table 3
outputtable %>%
  select('AFP_ng_mL','WBC', 'ALT', 'Creatinine', 'Total_Bilirubin', 'INR', 'Sodium','Total_IR_Burden') %>% 
  tbl_summary(by= 'Total_IR_Burden'
    ,statistic = list(all_continuous() ~ "{median} ({min}, {max})"),
              type = list(`AFP_ng_mL` ~ "continuous2",
                          'WBC' ~ "continuous2",
                          'ALT' ~ "continuous2",
                          'Creatinine' ~ "continuous2",
                          'Total_Bilirubin' ~ "continuous2",
                          'Sodium' ~ 'continuous2'
              ),
              label = list(
                AFP_ng_mL = "AFP, median (range), ng/mL",
                WBC = "WBC, median (range), 10^9/L",
                ALT = "ALT, median (range), U/L",
                Creatinine = "Creatinine, median (range), mg/dL",
                Total_Bilirubin = "T. bil, median (range), mg/dL",
                INR = "INR, median (range)",
                Sodium = "Sodium, median (range), mg/dL"
              )
  ) |> add_p() |>
  modify_header(label~"**Pre-operative Characteristics**",
                stat_1 ~ "**No previous IR surgery**  \nN = 174 ",
                stat_2 ~ "**Single previous IR surgery**  \nN = 90", 
                stat_3 ~ '**Multiple previous IR surgeries**  \nN = 27'
  ) 
#Table 4
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

outputtable$Date_of_Death <- as.Date(outputtable$Date_of_Death)
outputtable$Last_FU <- as.Date(outputtable$Last_FU, format = "%m/%d/%y")
outputtable$Tumor_size <- as.numeric(outputtable$Tumor_size)
#Table 5
outputtable %>%
  select("Tumor_size", "Tumor_Number", "Explant_Necrosis_Percent", "Microvascular_Invasion", 
         "Macrovascular_Invasion", "Recurrence", "Recurrence_Site", 'Total_IR_Burden', 'AJCC_Staging') %>%
  tbl_summary(by = 'Total_IR_Burden',
              label = list(
                Tumor_size = "Tumor size, Median (range), cm",
                Tumor_Number = "Tumor number, no. (%)",
                Explant_Necrosis_Percent = "Necrosis present in tumor, no, (%)",
                Microvascular_Invasion = "Microvascular Invasion, no., (%)",
                Macrovascular_Invasion = "Macrovascular Invasion, no., (%)",
                Recurrence = "Recurrence, No. (%)",
                Recurrence_Site = "Recurrence site, no, %"
                )
              )|> add_p() |>
  modify_header(label~"**Tumor Characteristics**",
                stat_1 ~ "**No previous IR surgery**  \nN = 174 ",
                stat_2 ~ "**Single previous IR surgery**  \nN = 90", 
                stat_3 ~ '**Multiple previous IR surgeries**  \nN = 27'
  ) 

#Table 5
outputtable %>%
  select("Date_of_Death","Last_FU") %>%
  tbl_summary(label = list(
    Date_of_Death = "Median date of death, mm/dd/yyyy",
    Last_FU = "Median last followup, mm/dd/yyyy"
  )
  )
last_fusurvcurve <-as.integer(as.Date(outputtable$Last_FU, format = "%m/%d/%Y") - as.Date("1/1/2010", format = "%m/%d/%Y"))
time <- as.integer(as.Date(outputtable$Date_of_Death, format = "%m/%d/%Y") - as.Date("1/1/2010", format = "%m/%d/%Y"))
time[is.na(time)] <- last_fusurvcurve[is.na(time)]
outputtable$Survival_Status[outputtable$Survival_Status == "Yes"] <- "0"
outputtable$Survival_Status[outputtable$Survival_Status == "No"] <- "1"
status <- as.integer(outputtable$Survival_Status)

#Logrank test
hccsurv <- survdiff(Surv(time, status)~Total_IR_Burden, data = outputtable)
chi_sq <- hccsurv$chisq
p_value <- hccsurv$pvalue
resultsdf <- data.frame(Test = "Log-Rank Test", 'Chi Squared' = chi_sq, DF = length(hccsurv$n) - 1, 'P Value' = p_value)
survtable <- gt(resultsdf)
gtsave(survtable, 'log_rank_results.html')

#Cox test
last_fusurvcurve <-as.integer(as.Date(outputdata$`Date of last follow up`, format = "%m/%d/%Y") - as.Date("1/1/2010", format = "%m/%d/%Y"))
time <- na.omit(as.integer(as.Date(outputdata$`Date_of_Death`, format = "%m/%d/%Y") - as.Date("1/1/2010", format = "%m/%d/%Y")))
time[is.na(time)] <- last_fusurvcurve[is.na(time)]
outputdata$Survival[outputdata$Survival == "Yes"] <- "0"
outputdata$Survival[1] <- NA
outputdata$Survival[outputdata$Survival == "No"] <- "1"
status <- na.omit(as.integer(outputdata$Survival_Status))

hcccox <- coxph(Surv(time, status)~ factor(totalpreopburden), data = outputdata)
summary(hcccox)

Surv(time, status)
survcurve <- survfit(Surv(time, status)~factor(Total_IR_Burden), data = outputdata)
#Kaplan meier curve
burden <- na.omit(outputdata$Total_IR_Burden)
survfit2(Surv(time, status) ~ burden, data = outputdata, start.time = 0) |>
  ggsurvfit() +
  ylim(0,1) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() + 
  add_risktable()


#setting up barplot for LRT burden effect on necrosis
outputdata$`tumor necrosis`[1] = ""
outputdata$`tumor necrosis`[outputdata$`tumor necrosis` == "Yes"] = "1"
outputdata$`tumor necrosis`[outputdata$`tumor necrosis` == ""] = "2"
noburdennec <- as.numeric(na.omit(outputdata$`tumor necrosis`[outputdata$`totalpreopburden` == 0]))
someburdennec <- as.numeric(na.omit(outputdata$`tumor necrosis`[outputdata$`totalpreopburden` == 1]))
multburdennec <- as.numeric(na.omit(append(outputdata$`tumor necrosis`[outputdata$`totalpreopburden` == 2], 
                        outputdata$`tumor necrosis`[outputdata$`totalpreopburden` == 3])))

barplotdata <- data.frame(x = c("No prior LRT", "Single prior LRT", "Multiple prior LRT"),
                          y = c(mean(noburdennec == 1)*100, mean(someburdennec == 1)*100, mean(multburdennec == 1)*100))
barplotdata <- arrange(barplotdata)
  
#anova
aovnecro <- aov(y~x, data = barplotdata)
#tukey- this doesnt work and im not competent enough to know why
fx <- factor(outputdata$totalpreopburden)
tukeynecro <- TukeyHSD(aov(outputdata$'tumor necrosis'~fx, data = outputdata))



#bar plot
textlabel <- c("a", 'b', 'b')
barplotdata %>%
  mutate(x = fct_relevel(x, "No prior LRT", "Single prior LRT", "Multiple prior LRT")) %>%
  ggplot(aes(x = x,y = y)) + 
  geom_bar(stat = "identity", width = .5) +
  geom_text(aes(label=textlabel), vjust=0) +
  labs(x = "Previous IR burden", y = "Tumors with Necrosis (%)",
       title ="Central Tumor Necrosis on Histology by LRT Burden") +
  theme_minimal()
