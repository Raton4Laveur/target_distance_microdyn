{
library("here")
library("dplyr")
library("tidyr")
library("stringr")
library("forcats")
library("tibble")

if (!dir.exists("output")) dir.create("output")
}

## 1. Import Data ----
data_raw <- readRDS(here("data", "expra_eg_b.rds"))
## Uncomment next line for mock data
#data_raw <- read.csv(here("data", "mock_data.csv"))

## 2. Cleaning Pipeline ----
data_clean <- data_raw |>
  rename(group_name = group) |>
  filter(
    as.character(finished) %in% c("T", "TRUE"), 
    str_detect(self_declaration, "Yes")
  ) |>
    group_by(group_name) |>
    filter(
    # Keep subjects within Mean +/- 2*SD
    md_total_duration_s <= (mean(md_total_duration_s, na.rm = TRUE) + 2 * sd(md_total_duration_s, na.rm = TRUE)),
    md_total_duration_s >= (mean(md_total_duration_s, na.rm = TRUE) - 2 * sd(md_total_duration_s, na.rm = TRUE))
  ) |>
  ungroup()

## 3. Pivot to Long Format ----
data_analysis <- data_clean |>
  select(pid, group_name, starts_with("kap_par_")) |>
  pivot_longer(
    cols = starts_with("kap_par_"),
    names_to = "question",
    values_to = "score"
  ) |>
  drop_na(score) |>
  mutate(type = if_else(question %in% c("kap_par_01", "kap_par_02", "kap_par_08"), 
                        "Anchor", "Manipulated")) |>
  # Initial aggregation by type
  group_by(pid, group_name, type) |> 
  summarize(mean_score = mean(score), .groups = "drop")

## 4. Unified Analysis Data (Replacing workaround) ----
# Calculate the overall mean per subject and join it back to the data_analysis
data_analysis <- data_analysis |>
  group_by(pid) |>
  mutate(subject_overall_mean = mean(mean_score, na.rm = TRUE)) |>
  ungroup()

## 5. APA Theme ----
theme_apa <- function(base_size = 11, base_family = "serif") {
  
  apa_colors <- c("#000000", "#999999")
  options(apa_plot_colors = apa_colors)
  
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text               = element_text(family = "serif", size = base_size),
      
      plot.title         = element_blank(),
      plot.subtitle      = element_blank(),
      
      axis.line          = element_line(colour = "black", linewidth = 0.5),
      axis.ticks         = element_line(colour = "black", linewidth = 0.5),
      axis.title         = element_text(face = "plain"), # HIER KORRIGIERT: "plain" statt "regular"
      axis.text          = element_text(family = "sans", colour = "black", size = base_size - 1),
      
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = rel(1)),
      
      legend.position    = "bottom",
      legend.justification = "left",
      legend.title       = element_text(face = "bold", size = base_size - 1),
      legend.text        = element_text(size = base_size - 1),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}
# Helper functions for colors
scale_fill_apa <- function(...) {
  cols <- getOption("apa_plot_colors")
  scale_fill_manual(values = cols, ...)
}
scale_color_apa <- function(...) {
  cols <- getOption("apa_plot_colors")
  scale_color_manual(values = cols, ...)
}

## 6. Save Clean Data ----
saveRDS(data_analysis, here("output", "data_analysis_cleaned.rds"))