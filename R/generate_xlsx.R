# ── APA 7 Descriptive Statistics Table → Excel ────────────────────────────────
{
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(here)
  
  if (!dir.exists("output")) dir.create("output")
}


# 1. Compute Statistics (The only object left in your Environment)
xlsx_output <- data_analysis |>
  group_by(group_name, type) |>
  summarise(
    n           = sum(!is.na(mean_score)),
    Mittelwert  = mean(mean_score,   na.rm = TRUE),
    Median      = median(mean_score, na.rm = TRUE),
    SD          = sd(mean_score,     na.rm = TRUE),
    Varianz     = var(mean_score,    na.rm = TRUE),
    IQR         = IQR(mean_score,    na.rm = TRUE),
    Min         = min(mean_score,    na.rm = TRUE),
    Max         = max(mean_score,    na.rm = TRUE),
    Schiefe = {
      x  <- mean_score[!is.na(mean_score)]
      m3 <- mean((x - mean(x))^3); m2 <- mean((x - mean(x))^2)
      m3 / m2^1.5
    },
    Exzess = {
      x  <- mean_score[!is.na(mean_score)]
      m4 <- mean((x - mean(x))^4); m2 <- mean((x - mean(x))^2)
      (m4 / m2^2) - 3
    },
    SE       = SD / sqrt(n),
    unteres_KI = Mittelwert - qt(0.975, df = n - 1) * SE,
    oberes_KI = Mittelwert + qt(0.975, df = n - 1) * SE,
    .groups  = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  rename(Gruppe = group_name, Aufgabentyp = type, `SEM` = SE, 
         `95% unteres_KI` = unteres_KI, `95% oberes_KI` = oberes_KI)

# 2. The "Clean Room" Function (Executes and disappears)
save_apa_excel <- function(df, filename = "Deskriptivstatistik.xlsx") {
  wb <- createWorkbook()
  addWorksheet(wb, "Deskriptivstatistik")
  
  # Configuration
  FONT <- "Times New Roman"; ws <- "Deskriptivstatistik"
  n_cols <- ncol(df); DATA_END <- 4 + nrow(df) - 1
  
  # Styles
  s_title  <- createStyle(fontName = FONT, fontSize = 12, textDecoration = "bold")
  s_note   <- createStyle(fontName = FONT, fontSize = 10, textDecoration = "italic")
  s_header <- createStyle(fontName = FONT, border = "TopBottom", textDecoration = "bold", halign = "center")
  s_body   <- createStyle(fontName = FONT, halign = "center")
  s_left   <- createStyle(fontName = FONT, halign = "left")
  s_bottom <- createStyle(fontName = FONT, border = "bottom")
  
  # Writing
  
  writeData(wb, ws, df, startRow = 3, colNames = TRUE)
  addStyle(wb, ws, s_header, rows = 3, cols = 1:n_cols)
  addStyle(wb, ws, s_body,   rows = 4:DATA_END, cols = 3:n_cols, gridExpand = TRUE)
  addStyle(wb, ws, s_left,   rows = 4:DATA_END, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, ws, s_bottom, rows = DATA_END, cols = 1:n_cols, stack = TRUE)
  
  # Footer Note
  writeData(wb, ws, "Anmerkung: n = Stichprobengrösse; SD = Standardabweichung; IQR = Interquartilbereich; SEM = Standardfehler des Mittelwerts", startRow = DATA_END + 2)
  addStyle(wb, ws, s_note, rows = DATA_END + 2, cols = 1)
  
  # Formatting
  setColWidths(wb, ws, cols = 1:2, widths = 15)
  setColWidths(wb, ws, cols = 3:n_cols, widths = 25)
  showGridLines(wb, ws, showGridLines = FALSE)
  
  saveWorkbook(wb, here("output", filename), overwrite = TRUE)
}

# 3. Execute
save_apa_excel(xlsx_output)