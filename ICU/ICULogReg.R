#Input: ICU Case data with sepsis scores of each patient on a daily bases
#Output: Logistic regression models

library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(pscl)
library(ResourceSelection)
library(pROC)
library(geepack)

patient_data <- read.xlsx("trimmedLOGdata.xlsx")
dailycases_tv <- read.csv("ICUCOXdata.csv")

#unadjusted
# glm_MEWS  <- glm(died ~ MEWS,     data = patient_data, family = binomial)
# glm_qSOFA <- glm(died ~ qSOFA,    data = patient_data, family = binomial)
# glm_SIRS  <- glm(died ~ SIRS,     data = patient_data, family = binomial)
# glm_UVA   <- glm(died ~ UVAScore, data = patient_data, family = binomial)
# 

glm_MEWS  <- glm(died ~ MEWS + Age + Gender,     data = patient_data, family = binomial)
glm_qSOFA <- glm(died ~ qSOFA + Age + Gender,    data = patient_data, family = binomial)
glm_SIRS  <- glm(died ~ SIRS + Age + Gender,     data = patient_data, family = binomial)
glm_UVA   <- glm(died ~ UVAScore + Age + Gender, data = patient_data, family = binomial)
glm_NEWS   <- glm(died ~ NEWS + Age + Gender, data = patient_data, family = binomial)

# ROC curves
roc_MEWS  <- roc(patient_data$died, fitted(glm_MEWS),  quiet = TRUE)
roc_qSOFA <- roc(patient_data$died, fitted(glm_qSOFA), quiet = TRUE)
roc_SIRS  <- roc(patient_data$died, fitted(glm_SIRS),  quiet = TRUE)
roc_UVA   <- roc(patient_data$died, fitted(glm_UVA),   quiet = TRUE)
roc_NEWS   <- roc(patient_data$died, fitted(glm_NEWS),   quiet = TRUE)


roc_list <- list(MEWS = roc_MEWS, qSOFA = roc_qSOFA, SIRS = roc_SIRS, UVA = roc_UVA, NEWS = roc_NEWS)

auroc_table <- map_dfr(roc_list, function(r) {
  ci <- ci.auc(r, method = "delong")
  tibble(
    AUROC    = round(as.numeric(r$auc), 3),
    CI_lower = round(ci[1], 3),
    CI_upper = round(ci[3], 3)
  )
}, .id = "Score") %>%
  arrange(desc(AUROC))

print(auroc_table)

# Pairwise DeLong tests
roc_comparisons <- combn(names(roc_list), 2, simplify = FALSE)

comparison_table <- map_dfr(roc_comparisons, function(pair) {
  test <- roc.test(roc_list[[pair[1]]], roc_list[[pair[2]]], method = "delong")
  tibble(
    Comparison = paste(pair[1], "vs", pair[2]),
    Z          = round(test$statistic, 3),
    p_value    = round(test$p.value, 4)
  )
})

print(comparison_table)

# Apply Bonferroni correction
comparison_table %>%
  mutate(p_bonferroni = p.adjust(p_value, method = "bonferroni"),
         p_fdr        = p.adjust(p_value, method = "BH")) %>%  # or FDR
  arrange(p_value)

# Plot
ggroc(roc_list, legacy.axes = TRUE, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c(MEWS = "#E41A1C", qSOFA = "#377EB8", SIRS = "#4DAF4A", UVA = "#984EA3"),
    labels = c(
      MEWS  = paste0("MEWS  (AUC = ", round(roc_MEWS$auc,  3), ")"),
      qSOFA = paste0("qSOFA (AUC = ", round(roc_qSOFA$auc, 3), ")"),
      SIRS  = paste0("SIRS  (AUC = ", round(roc_SIRS$auc,  3), ")"),
      UVA   = paste0("UVA   (AUC = ", round(roc_UVA$auc,   3), ")")
    )
  ) +
  labs(
    title = "ROC Curves for ICU Mortality Prediction",
    x     = "1 - Specificity (False Positive Rate)",
    y     = "Sensitivity (True Positive Rate)",
    color = "Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = c(0.72, 0.25))
