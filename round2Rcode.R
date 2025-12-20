library(ggplot2)
library(dplyr)
library(readxl)

# Sample data
cases <- read_excel("rawlengthsdata.xlsx")


Surg_df <- subset(cases, select = c(1, 2, 3, 4))
names(Surg_df)[1] <- 'case_num'
Surg_df$case_num <- gsub("Case ", "", Surg_df$case_num)
Surg_df$case_num <- as.numeric(Surg_df$case_num)
# Convert times to POSIXct and adjust for wraparound cases
Surg_df <- Surg_df %>%
  mutate(
    Surg_df$start <- as.POSIXct(Surg_df$start, format = "%H:%M", tz = "UTC"),
    Surg_df$finish <- as.POSIXct(Surg_df$finish, format = "%H:%M", tz = "UTC"),
    # Handle cases where the end time is before the start time (crossing midnight)
    Surg_df$finish <- ifelse(Surg_df$finish < Surg_df$start, Surg_df$finish
                             + 86400, Surg_df$finish)
  )

# Create a new data frame to handle cases that wrap around midnight
cases_split <- cases %>%
  mutate(
    wraparound = end_time > as.POSIXct("24:00", format = "%H:%M", tz = "UTC")
  ) %>%
  rowwise() %>%
  do({
    if (.$wraparound) {
      data.frame(
        case_number = .$case_number,
        segment = c("pre-midnight", "post-midnight"),
        start_time = c(.$start_time, as.POSIXct("00:00", format = "%H:%M", tz = "UTC")),
        end_time = c(as.POSIXct("24:00", format = "%H:%M", tz = "UTC"), .$end_time - 86400)
      )
    } else {
      data.frame(
        case_number = .$case_number,
        segment = "single",
        start_time = .$start_time,
        end_time = .$end_time
      )
    }
  })

# Plot the data
ggplot(cases_split, aes(y = factor(case_number), xmin = as.numeric(start_time), xmax = as.numeric(end_time))) +
  geom_rect(aes(xmin = start_time, xmax = end_time, ymin = as.numeric(case_number) - 0.4, ymax = as.numeric(case_number) + 0.4), fill = "steelblue") +
  scale_x_continuous(
    breaks = seq(as.POSIXct("00:00", format = "%H:%M", tz = "UTC"), 
                 as.POSIXct("24:00", format = "%H:%M", tz = "UTC"), 
                 by = "2 hours"),
    labels = format(seq(as.POSIXct("00:00", format = "%H:%M", tz = "UTC"),
                        as.POSIXct("24:00", format = "%H:%M", tz = "UTC"),
                        by = "2 hours"), "%H:%M")
  ) +
  labs(x = "Time", y = "Case Number", title = "Surgery Start and End Times") +
  theme_minimal()
