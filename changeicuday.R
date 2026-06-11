#Input: ICU Case data with sepsis scores of each patient on a daily bases
#Output: Case data where each patient has a single row and Day 0 and 3 sepsis data is added.

library(readxl)
library(dplyr)

# ── Load data ──────────────────────────────────────────────────────────────────
df <- read_excel("ICUcases_w_sepsis.xlsx")

# ── Columns to move from Day 3 → Day 0 ────────────────────────────────────────
cols <- c("SIRS", "sepsis", "qSOFA", "High Risk Sepsis", "UVAScore", "MEWS")

# ── Assign a patient group ID ──────────────────────────────────────────────────
# Patient ID appears only on Day 0 rows; all subsequent rows have NA.
# Fill down to tag every row with its patient group.
df <- df %>%
  mutate(patient_group = cumsum(!is.na(Patient)))

# ── Pull Day 0 values per patient ─────────────────────────────────────────────
day0_vals <- df %>%
  filter(Day == 0) %>%
  select(patient_group, all_of(cols)) %>%
  rename_with(~ paste0(.x, "_day0"), all_of(cols))
# ── Pull Day 3 values per patient ─────────────────────────────────────────────
day3_vals <- df %>%
  filter(Day == 3) %>%
  select(patient_group, all_of(cols)) %>%
  rename_with(~ paste0(.x, "_day3"), all_of(cols))

# ── Join Day 0,3 values onto the Day 0 rows ─────────────────────────────────────
df_out <- df %>%
  left_join(day0_vals, by = "patient_group") %>%
  left_join(day3_vals, by = "patient_group") %>%
  select(-patient_group, -cols)           # drop the helper column

# ── Result ─────────────────────────────────────────────────────────────────────
# df_out now has 10 new columns populated on every row for a patient,
# but they are only meaningful on the Day 0 row. Filter to Day 0

trimmeddata <- df_out %>% filter(Day == 0)

# ── Save ───────────────────────────────────────────────────────────────────────
# Full dataset with new columns added
# write.csv(df_out,      "ICUcases_with_day3_scores.csv",      row.names = FALSE)

# One-row-per-patient (Day 0 only)
write.csv(trimmeddata,   "trimmeddata.csv", row.names = FALSE)

