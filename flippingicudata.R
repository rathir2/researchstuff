library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(gtsummary)
library(xlsx)
library(janitor)



#get all sheet names
wb <- createWorkbook()
#get sheet length
numsheets <- length(excel_sheets("ICU Data.xlsx"))

#run through every single worksheet, do a bunch of gymnastics and cleanup, then transpose it
#and add it to a dataframe
for (x in 3:52){
  patient <- paste("Patient", as.character(x-2))
  tempsheet <- read_excel("ICU Data.xlsx", sheet = x)
  tempsheet <- data.frame(tempsheet)
  transposedtempsheet <- t(tempsheet)
  colnames(transposedtempsheet) <- transposedtempsheet[1,]
  transposedtempsheet <- transposedtempsheet[-1,]
  transposedtempsheet <- cbind(rownames(transposedtempsheet), transposedtempsheet)
  transposedtempsheet <- cbind(paste("Patient", as.character(x-2)), transposedtempsheet)
  sheet = createSheet(wb, paste("Patient", as.character(x-2)))
  addDataFrame(transposedtempsheet, sheet = sheet, row.names = FALSE)
}
#save it to the excel file
saveWorkbook(wb, "Master ICU Data.xlsx")

#concatenate every single sheet into a single file. 
sheet <- excel_sheets("Master ICU Data.xlsx")
data_frame = lapply(setNames(sheet, sheet), 
                    function(x) read_excel("Master ICU Data.xlsx", sheet=x))
data_frame = bind_rows(data_frame, .id="Patient")

#get rid of rows where the day is NA or Column1/2/3/4/5 etc. 
data_frame$keep <- grepl("Day", data_frame$V2)
filtered_data_frame <- data_frame[data_frame$keep == TRUE,]

demographics <- read_excel("ICU Data.xlsx", sheet = 1)
demographics <- demographics[c("Demographics", "Age", "Gender", "Height", "Weight",
                               "District of Origin", "Distance to Butare (mi)", 
                               "Date of Initial illness", "Date of Hospital Admit",
                               "Date of ICU Admit", "Hospital Admit Diagnosis", 
                               "ICU Admit Diagnosis", "Length of stay", "Outcome", 
                               "Cause of Death")]
#merge by patient
demographics <- demographics %>% rename(Patient = Demographics)
filtered_data_frame <- merge(demographics, filtered_data_frame, by = "Patient")
#merging by patient messed a bunch of things up so I redid 
filtered_data_frame$V1 <- substring(filtered_data_frame$V1, 8)
filtered_data_frame <- filtered_data_frame[order(as.numeric(filtered_data_frame$V1)),]

write.csv(filtered_data_frame, "Final ICU Data.csv")


wkbook <- loadWorkbook(file = "Master ICU Data.xlsx")


#Patient 50 (that's when x = 53) broke my computer. 
for (x in 54:numsheets){
  tempsheet <- read_excel("ICU Data.xlsx", sheet = x)
  tempsheet <- data.frame(tempsheet)
  transposedtempsheet <- t(tempsheet)
  colnames(transposedtempsheet) <- NULL
  sheet = createSheet(wb, paste("Patient", as.character(x-1)))
  addDataFrame(transposedtempsheet, sheet = sheet, row.names = FALSE)
  
}


sheets_to_read <- readxl::excel_sheets("ICU Data.xlsx")

#read all sheets, add tabname, and then bind rows length(sheets_to_read)
df <- bind_rows(lapply(1:20,
                       function(i)readxl::read_excel("ICU Data.xlsx", sheet = sheets_to_read[i]) %>% 
                         mutate(tabname = sheets_to_read[i])))


library(readxl)    
read_excel_allsheets <- function("ICU Data.xlsx", tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


#select the columns you want
df %>% select(1)


icudata <- read_excel("ICU Data.xlsx")

test <- bind_rows(lapply(1:10,readxl::read_excel("ICU Data.xlsx", sheet = sheets_to_read[10])
