# ── APA 7 Descriptive Statistics Table → Excel ────────────────────────────────
# Packages: openxlsx, dplyr, tidyr
# Output:   output/descriptive_stats_apa.xlsx
# ──────────────────────────────────────────────────────────────────────────────

library(openxlsx)
library(dplyr)
library(tidyr)
library(here)

if (!dir.exists("output")) dir.create("output")

# 1. Compute Statistics (The only object left in your Environment)
desc_stats <- analysis_data |>
  group_by(group_name, type) |>
  summarise(
    n        = sum(!is.na(mean_score)),
    Mean     = mean(mean_score,   na.rm = TRUE),
    Median   = median(mean_score, na.rm = TRUE),
    SD       = sd(mean_score,     na.rm = TRUE),
    Variance = var(mean_score,    na.rm = TRUE),
    IQR      = IQR(mean_score,    na.rm = TRUE),
    Min      = min(mean_score,    na.rm = TRUE),
    Max      = max(mean_score,    na.rm = TRUE),
    Skewness = {
      x  <- mean_score[!is.na(mean_score)]
      m3 <- mean((x - mean(x))^3); m2 <- mean((x - mean(x))^2)
      m3 / m2^1.5
    },
    Kurtosis = {
      x  <- mean_score[!is.na(mean_score)]
      m4 <- mean((x - mean(x))^4); m2 <- mean((x - mean(x))^2)
      (m4 / m2^2) - 3
    },
    SE       = SD / sqrt(n),
    CI_lower = Mean - qt(0.975, df = n - 1) * SE,
    CI_upper = Mean + qt(0.975, df = n - 1) * SE,
    .groups  = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  rename(Group = group_name, Type = type, `SE (Mean)` = SE, 
         `95% CI Lower` = CI_lower, `95% CI Upper` = CI_upper)

# 2. The "Clean Room" Function (Executes and disappears)
save_apa_excel <- function(df, filename = "descriptive_stats_apa.xlsx") {
  wb <- createWorkbook()
  addWorksheet(wb, "Descriptive Statistics")
  
  # Configuration
  FONT <- "Times New Roman"; ws <- "Descriptive Statistics"
  n_cols <- ncol(df); DATA_END <- 4 + nrow(df) - 1
  
  # Styles
  s_title  <- createStyle(fontName = FONT, fontSize = 12, textDecoration = "bold")
  s_note   <- createStyle(fontName = FONT, fontSize = 10, textDecoration = "italic")
  s_header <- createStyle(fontName = FONT, border = "TopBottom", textDecoration = "bold", halign = "center")
  s_body   <- createStyle(fontName = FONT, halign = "center")
  s_left   <- createStyle(fontName = FONT, halign = "left")
  s_bottom <- createStyle(fontName = FONT, border = "bottom")
  
  # Writing
  writeData(wb, ws, "Table 1", startRow = 1)
  addStyle(wb, ws, s_title, rows = 1, cols = 1)
  writeData(wb, ws, "Descriptive Statistics for Mean Scores", startRow = 2)
  addStyle(wb, ws, s_note, rows = 2, cols = 1)
  
  writeData(wb, ws, df, startRow = 3, colNames = TRUE)
  addStyle(wb, ws, s_header, rows = 3, cols = 1:n_cols)
  addStyle(wb, ws, s_body,   rows = 4:DATA_END, cols = 3:n_cols, gridExpand = TRUE)
  addStyle(wb, ws, s_left,   rows = 4:DATA_END, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, ws, s_bottom, rows = DATA_END, cols = 1:n_cols, stack = TRUE)
  
  # Footer Note
  writeData(wb, ws, "Note. n = sample size; SD = std. deviation; Skew/Kurt = excess.", startRow = DATA_END + 2)
  addStyle(wb, ws, s_note, rows = DATA_END + 2, cols = 1)
  
  # Formatting
  setColWidths(wb, ws, cols = 1:2, widths = 15)
  setColWidths(wb, ws, cols = 3:n_cols, widths = 10)
  showGridLines(wb, ws, showGridLines = FALSE)
  
  saveWorkbook(wb, here("output", filename), overwrite = TRUE)
}

# 3. Execute
save_apa_excel(desc_stats)