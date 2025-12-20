library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(gtsummary)
library(cusum)
library(qcc)

#load data
cases <- read_excel("Robotic Donor Hepatectomies Data.xlsx")
cases$`Operation Name` <- cases$`Operation Name (robotic, lapraoscopic, or open only)`

#changing names around for operation names to ensure we get clean output
cases$`Operation Name`[cases$`Operation Name`== "Open"] <- "Open right hepatectomy"
cases$`Operation Name`[cases$`Operation Name`== "Robotic donor hepatectomy (Rt), open conversion"] <- "Open right hepatectomy"
cases$`Operation Name`[cases$`Operation Name`== "Open right hepatectomy with MHV and cholecystectomy"] <- "Open right hepatectomy"

#previous operations need to be segregated into upper abdominal surgeries and not upper abdominal surgeries.
#i had to go in and manually do this in excel
names(cases)[names(cases) == "Previous (abdominal/suprapubic) operation (name)"] <- "previous operation"
cases$`previous operation`[cases$`previous operation` == "none"]<- "no"
cases$`previous operation`[cases$`previous operation` == "No"]<- "no"

#these variables were input as characters-this fucks up the graph so switched them to numeric.
cases$WIT <- as.numeric(cases$WIT)
cases$`Cell Saver amount, mL (or none)`<- as.numeric(cases$`Cell Saver amount, mL (or none)`)
cases$`Total Liver volumetry, g (weight of the liver before backtable)`<- as.numeric(cases$`Total Liver volumetry, g (weight of the liver before backtable)`)


#initial donor characteristics. Modeled heavily off of Sambommatsu et al. 2025-i think Kush did those stats
cases %>%
  select(Age, Sex, BMI,`Operation Name`, `previous operation`, `ABO Blood Type`) %>% 
  tbl_summary(by = `Operation Name`,
              label = list(
                Age = "Age, median (range), years",
                Sex = "Sex, No. (%)",
                BMI = "BMI, median (range)",
                `previous operation` = "Previous upper abdominal surgery, No. (%)",
                `Operation time, min` = "Operation time (min)",
                `ABO Blood Type` = "Blood Type"
              )
              ) |>
  add_p() |>
  modify_header(label~"**Donor Characteristics**")

#cleaning up anatomy characteristics
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "SINGLE"] <- "single"
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "single"] <- "1"
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "one"] <- "1"
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "two"] <- "2"
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "Two"] <- "2"
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "four"] <- "4"
cases$`Hepatic vein (number/description)`[cases$`Hepatic vein (number/description)`== "three"] <- "3"

cases$`parenchymal time (minutes)` <- as.numeric(cases$`parenchymal time (minutes)`)

cases <- read_csv("RDHLCcleaneddata.csv")

#operative characteristics: blood loss, liver ischemic time, etc.

cases %>%
  select(`Operation Name`, 
         `Total Liver volumetry, g (weight of the liver before backtable)`,
         `Actual graft (g)`, `artery (number/description)`, `portal vein (number/description)`,
         `bile duct (number/description)`, `Hepatic vein (number/description)`) %>% 
  tbl_summary(by = `Operation Name`,
              label = list(
                `Total Liver volumetry, g (weight of the liver before backtable)` = "Total liver volume, median (range), mL",
                `Actual graft (g)` = "Actual graft, median (range), g",
                `artery (number/description)` = "Number of hepatic arteries, No.",
                `portal vein (number/description)` = "Number of Portal Veins, No.",
                `bile duct (number/description)` = "Number of Bile Ducts, No.",
                `Hepatic vein (number/description)` = "Number of Hepatic Veins, No."
              )
  ) |>
  add_p() |>
  modify_header(label~"**Anatomic Characteristics**")


cases %>%
  select(`Operation Name`, `EBL, mL`, `WIT`,
         `Operation time, min`, `Cell Saver amount, mL (or none)`, `parenchymal time (minutes)` 
         ) %>% 
  tbl_summary(by = `Operation Name`,
              type = list(`Cell Saver amount, mL (or none)` ~ "continuous2", 
                          `parenchymal time (minutes)` ~ "continuous2"),
              label = list(
                `EBL, mL` = "EBL, median (range), mL",
                `WIT` = "WIT, median (range), min",
                `Cell Saver amount, mL (or none)` = "Intraoperative cell salvage use, No. (%)",
                `Operation time, min` = "Operation time, median (range), min",
                `parenchymal time (minutes)` = "Parenchymal transection time, median (range), min"
              )
              ) |>
  add_p() |>
  modify_header(label~"**Operative Characteristics**")

#simplifying the post-op ileus column so data isn't a mangled mess.
cases$`Ileus (y/n)`[cases$`Ileus (y/n)`=="y"] <- "yes"
cases$`Ileus (y/n)`[cases$`Ileus (y/n)`=="none"] <- "no"
cases$`Ileus (y/n)`[cases$`Ileus (y/n)`=="n"] <- "no"
cases$`Ileus (y/n)`[cases$`Ileus (y/n)`=="Yes"] <- "yes"
cases$`POD3 INR` <- as.numeric(cases$`POD3 INR`)
cases$`POD5 ALT` <- as.numeric(cases$`POD5 ALT`)
cases$`POD5 AST` <- as.numeric(cases$`POD5 AST`)
cases$`POD5 INR` <- as.numeric(cases$`POD5 INR`)
cases$`POD3 T.bil` <- as.numeric(cases$`POD3 T.bil`)
cases$`POD5 T.bil` <- as.numeric(cases$`POD5 T.bil`)
cases$`Bile leak (y/n)`[cases$`Bile leak (y/n)`== "none"] <- "no"

cases %>%
  select(`Operation Name`, `Peak T.bil` , `Ileus (y/n)`, `Bile leak (y/n)`, 
         `Other complications (list other notable complications)`, `Peak ALT`, `Peak AST`,
         `hospital stay, days`, `POD3 T.bil`, `POD3 ALT`, `POD3 AST`, `POD3 INR`,
         `POD5 ALT`, `POD5 AST`, `POD5 INR`, `POD5 T.bil`) %>% 
  tbl_summary(by = `Operation Name`,
              type = list(`POD3 INR` ~ "continuous2",
              `POD5 ALT` ~ "continuous2",
              `POD5 INR` ~ "continuous2",
              `POD5 T.bil` ~ "continuous2"),
              label = list(
                `Ileus (y/n)` = "Postoperative Ileus",
                `hospital stay, days` = "Hospital Stay (days)",
                `Other complications (list other notable complications)` = "Total postoperative complications, no.",
                `Bile leak (y/n)` = "Bile leak, No. (%)"
                )
              ) |>
  add_p() |>
  modify_header(label~"**Post Operative Outcomes**")


write.csv(cases,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/RDHLCcleaneddata.csv", row.names = FALSE)


rdhlccases <- subset(cases, cases$`Operation name (robotic, lapraoscopic, or open only)` == "Robotic donor hepatectomy (Rt)")
rdhlccases$number <- c(rep("1",13), rep("2", 14))
write.csv(rdhlccases,"C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/RDHLConly.csv", row.names = FALSE)
rdhlccases <- read_csv("C:/Users/rrath/Desktop/med school stuff/OrganCraftCrew/RDHLConly.csv")

rdhlccases %>%
  select(Age, Sex, BMI,`Operation Name`, `previous operation`, `ABO Blood Type`, 
         `EBL, mL`, `WIT`, `Operation time, min` ,`Peak T.bil` , `Ileus (y/n)`, `Peak ALT`, `Peak AST`,
         `Other complications (list other notable complications)`,
         `hospital stay, days`, `number`, `open conversion (y/n; if y, planned or not)`, 
         `POD3 T.bil`, `POD3 ALT`, `POD3 AST`, `POD3 INR`,
         `POD5 ALT`, `POD5 AST`, `POD5 INR`, `POD5 T.bil`) %>% 
  tbl_summary(by = `number`,
              type = list(`hospital stay, days` ~ "continuous2", 
                          `POD3 INR` ~ "continuous2",
                          `POD5 ALT` ~ "continuous2",
                          `POD5 INR` ~ "continuous2",
                          `POD5 T.bil` ~ "continuous2"),
              label = list(
                Age = "Age, median (range), years",
                Sex = "Sex, No. (%)",
                BMI = "BMI, median (range)",
                `previous operation` = "Previous upper abdominal surgery, No. (%)",
                `Operation time, min` = "Operation time (min)",
                `ABO Blood Type` = "Blood Type",
                `EBL, mL` = "EBL (mL)",
                `WIT` = "WIT (min)",
                `Operation time, min` = "Operation time (min)",
                `Ileus (y/n)` = "Postoperative Ileus",
                `hospital stay, days` = "Hospital Stay (days)",
                `Other complications (list other notable complications)` = "Total postoperative complications, no."
              )
  ) |>
  add_p() |>
  modify_header(label~"**Robotic Donor Hepatectomy Outcomes over Time**")

rdhlccases <- rdhlccases[rdhlccases$`Operation name (robotic, lapraoscopic, or open only)` == 
                           "Robotic donor hepatectomy (Rt)",]

#Non-risk adjusted CUSUM for operation time
target <- mean(rdhlccases$`Operation time, min`)
deviations <- rdhlccases$`Operation time, min` - target
cusum <- cumsum(deviations)
cusum <- data.frame(cusum)
ggplot(cusum, mapping = aes(x = 1:27, y = cusum)) + 
  geom_line() + geom_point() + geom_vline(xintercept = c(9, 17, 25), linetype = 2) + 
  geom_hline(aes(yintercept = 0), linewidth = 1) +
  labs(x = "Case Number", y = "CUSUM",
       title ="Total Operative Time (Minutes)") +
  scale_x_continuous(breaks = round(seq(1, 27, by = 2),1)) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "gray"), 
        plot.title = element_text(hjust = 0.45), aspect.ratio = .5,
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))


#Non-risk adjusted CUSUM for parenchymal time
target <- 175.6666
deviations <- rdhlccases$`parenchymal time (minutes)` - target
cusum <- cumsum(deviations)
cusum <- data.frame(cusum)
ggplot(cusum, mapping = aes(x = 1:27, y = cusum)) + 
  geom_line() + geom_point() + geom_vline(xintercept = c(3, 17, 25), linetype = 2) + 
  geom_hline(aes(yintercept = 0), linewidth = 1) +
  labs(x = "Case Number", y = "CUSUM",
       title ="Parenchymal Time (Minutes)") +
  scale_x_continuous(breaks = round(seq(1, 27, by = 2),1)) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "gray"), 
        plot.title = element_text(hjust = 0.45), aspect.ratio = .5,
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

target <- 175.6666
deviations <- rdhlccases$`parenchymal time (minutes)` - target
cusum <- cumsum(deviations)
cusum <- data.frame(cusum)
ggplot(cusum, mapping = aes(x = 1:27, y = cusum)) + 
  geom_line() + geom_point() + geom_vline(xintercept = c(3, 17, 25), linetype = 2) + 
  geom_hline(aes(yintercept = 0), linewidth = 1) +
  labs(x = "Case Number", y = "CUSUM",
       title ="Parenchymal Time (Minutes)") +
  scale_x_continuous(breaks = round(seq(1, 27, by = 2),1)) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "gray"), 
        plot.title = element_text(hjust = 0.45), aspect.ratio = .5,
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))



DI = 3
shift = 1
testcusum <- cusum(rdhlccases$`Operation time, min`, center = mean(rdhlccases$`Operation time, min`),
                   std.dev = sd(rdhlccases$`Operation time, min`), decision.interval = DI, se.shift = shift, 
                   plot = TRUE)
plot(ylab = "CUSUM (standard deviations)")

testcusum <- cusum(rdhlccases$`parenchymal time (minutes)`, center = mean(rdhlccases$`parenchymal time (minutes)`),
                   std.dev = sd(rdhlccases$`parenchymal time (minutes)`), decision.interval = DI, se.shift = shift, 
                   plot = TRUE)


