# ── APA 7 Descriptive Statistics Table → Excel ────────────────────────────────
# Packages: openxlsx, dplyr, tidyr
# Output:   output/descriptive_stats_apa.xlsx
# ──────────────────────────────────────────────────────────────────────────────

library(openxlsx)
library(dplyr)
library(tidyr)
library(here)

if (!dir.exists("output")) dir.create("output")

# ── 1. Compute Statistics ──────────────────────────────────────────────────────
# Replace `data_analysis` and `mean_score` / `group_name` / `type`
# with your own data frame and column names.

desc_stats <- data_analysis |>
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
      m3 <- mean((x - mean(x))^3)
      m2 <- mean((x - mean(x))^2)
      m3 / m2^1.5
    },
    Kurtosis = {
      x  <- mean_score[!is.na(mean_score)]
      m4 <- mean((x - mean(x))^4)
      m2 <- mean((x - mean(x))^2)
      (m4 / m2^2) - 3      # excess kurtosis (0 = normal)
    },
    SE       = SD / sqrt(n),
    CI_lower = Mean - qt(0.975, df = n - 1) * SE,
    CI_upper = Mean + qt(0.975, df = n - 1) * SE,
    .groups  = "drop"
  ) |>
  rename(
    Group = group_name,
    Type  = type,
    `SE (Mean)`   = SE,
    `95% CI Lower` = CI_lower,
    `95% CI Upper` = CI_upper
  )

# Round all numeric columns to 2 decimal places (APA standard)
desc_stats <- desc_stats |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

# ── 2. Build Workbook ─────────────────────────────────────────────────────────

wb <- createWorkbook()

# ── 3. APA Style Definitions ──────────────────────────────────────────────────
# Defined after createWorkbook() to ensure openxlsx is fully initialised.

FONT_FAMILY <- "Times New Roman"   # APA requires serif font

style_title <- createStyle(
  fontName       = FONT_FAMILY,
  fontSize       = 12,
  textDecoration = "bold",
  halign         = "left"
)

style_note <- createStyle(
  fontName       = FONT_FAMILY,
  fontSize       = 10,
  textDecoration = "italic",
  halign         = "left"
)

style_header <- createStyle(
  fontName       = FONT_FAMILY,
  fontSize       = 11,
  textDecoration = "bold",
  halign         = "center",
  border         = "TopBottom",     # APA: top and bottom border on header row
  borderStyle    = "thin"
)

style_body <- createStyle(
  fontName  = FONT_FAMILY,
  fontSize  = 11,
  halign    = "center"
)

style_body_left <- createStyle(     # left-align text columns (Group, Type)
  fontName  = FONT_FAMILY,
  fontSize  = 11,
  halign    = "left"
)

style_bottom_border <- createStyle( # APA: closing border at bottom of table
  fontName    = FONT_FAMILY,
  fontSize    = 11,
  border      = "bottom",
  borderStyle = "thin"
)

# ── 4. Sheet Setup ─────────────────────────────────────────────────────────────
addWorksheet(wb, "Descriptive Statistics")
ws <- "Descriptive Statistics"

# Row layout
TITLE_ROW  <- 1
HEADER_ROW <- 3
DATA_START  <- 4
DATA_END    <- DATA_START + nrow(desc_stats) - 1
NOTE_ROW   <- DATA_END + 2

n_cols <- ncol(desc_stats)

# ── 3a. Title (APA: "Table N" bold, then italicised description below) ─────────
writeData(wb, ws, "Table 1", startRow = TITLE_ROW, startCol = 1)
addStyle(wb, ws, style_title, rows = TITLE_ROW, cols = 1)

writeData(wb, ws,
          "Descriptive Statistics for Mean Scores by Group and Question Type",
          startRow = TITLE_ROW + 1, startCol = 1)
addStyle(wb, ws, style_note, rows = TITLE_ROW + 1, cols = 1)

# ── 3b. Header row ─────────────────────────────────────────────────────────────
writeData(wb, ws, desc_stats, startRow = HEADER_ROW, startCol = 1,
          colNames = TRUE, rowNames = FALSE)

addStyle(wb, ws, style_header,
         rows = HEADER_ROW, cols = 1:n_cols, gridExpand = TRUE)

# ── 3c. Data rows ──────────────────────────────────────────────────────────────
addStyle(wb, ws, style_body,
         rows = DATA_START:DATA_END, cols = 3:n_cols,   # numeric cols
         gridExpand = TRUE)

addStyle(wb, ws, style_body_left,
         rows = DATA_START:DATA_END, cols = 1:2,         # text cols
         gridExpand = TRUE)

# Bottom border on last data row (APA table closing line)
addStyle(wb, ws, style_bottom_border,
         rows = DATA_END, cols = 1:n_cols, gridExpand = TRUE,
         stack = TRUE)   # stack = TRUE preserves existing cell styles

# ── 3d. APA note ───────────────────────────────────────────────────────────────
writeData(wb, ws,
          paste0("Note. n = sample size; SD = standard deviation; IQR = interquartile range; ",
                 "SE = standard error of the mean; CI = confidence interval. ",
                 "Skewness and Kurtosis are reported as excess kurtosis (normal distribution = 0). ",
                 "95% CIs are based on the t-distribution."),
          startRow = NOTE_ROW, startCol = 1)
addStyle(wb, ws, style_note, rows = NOTE_ROW, cols = 1)

# ── 3e. Column widths ──────────────────────────────────────────────────────────
setColWidths(wb, ws, cols = 1,        widths = 16)   # Group
setColWidths(wb, ws, cols = 2,        widths = 14)   # Type
setColWidths(wb, ws, cols = 3,        widths = 7)    # n
setColWidths(wb, ws, cols = 4:n_cols, widths = 12)   # numeric stats

# Merge title cells across all columns for a clean header block
mergeCells(wb, ws, cols = 1:n_cols, rows = TITLE_ROW)
mergeCells(wb, ws, cols = 1:n_cols, rows = TITLE_ROW + 1)
mergeCells(wb, ws, cols = 1:n_cols, rows = NOTE_ROW)

# Freeze panes below the header so columns stay visible when scrolling
freezePane(wb, ws, firstActiveRow = DATA_START)

# Remove gridlines for a cleaner APA look
showGridLines(wb, ws, showGridLines = FALSE)

# ── 4. Save ────────────────────────────────────────────────────────────────────
out_path <- here("output", "descriptive_stats_apa.xlsx")
saveWorkbook(wb, out_path, overwrite = TRUE)
message("Saved: ", out_path)