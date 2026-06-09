library(readxl)
library(dplyr)

# ── Load data ──────────────────────────────────────────────────────────────────
df <- read_excel("ICUcases_w_sepsis.xlsx")

# ── Columns to move from Day 3 → Day 0 ────────────────────────────────────────
day0_cols <- c("SIRS", "sepsis", "qSOFA", "High Risk Sepsis", "UVAScore")

# ── Assign a patient group ID ──────────────────────────────────────────────────
# Patient ID appears only on Day 0 rows; all subsequent rows have NA.
# Fill down to tag every row with its patient group.
df <- df %>%
  mutate(patient_group = cumsum(!is.na(Patient)))

# ── Pull Day 3 values per patient ─────────────────────────────────────────────
day0_vals <- df %>%
  filter(Day == 0) %>%
  select(patient_group, all_of(day0_cols)) %>%
  rename_with(~ paste0(.x, "_day0"), all_of(day0_cols))

# ── Join Day 3 values onto the Day 0 rows ─────────────────────────────────────
df_out <- df %>%
  left_join(day0_vals, by = "patient_group") %>%
  select(-patient_group)           # drop the helper column

# ── Result ─────────────────────────────────────────────────────────────────────
# df_out now has 5 new columns (SIRS_day3, sepsis_day3, qSOFA_day3,
# High Risk Sepsis_day3, UVAScore_day3) populated on every row for a patient,
# but they are only meaningful on the Day 0 row. Filter to Day 0 if you want
# a one-row-per-patient summary:

day0_only <- df_out %>% filter(Day == 0)

# ── Save ───────────────────────────────────────────────────────────────────────
# Full dataset with new columns added
#write.csv(df_out,      "ICUcases_with_day3_scores.csv",      row.names = FALSE)

# One-row-per-patient (Day 0 only)
write.csv(day0_only,   "ICUcases_day0_scores.csv", row.names = FALSE)

