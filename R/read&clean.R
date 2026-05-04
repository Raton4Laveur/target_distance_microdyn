library("here")
library("dplyr")
library("tidyr")
library("stringr")
library("forcats")
library("tibble")

if (!dir.exists("output")) dir.create("output")

## 1. Import Data ----
# Using here() ensures it works on any computer
raw_data <- readRDS(here("data", "expra_eg_b.rds"))
## Uncomment next line for mock data
#raw_data <- read.csv(here("data", "mock_data.csv"))

## 2. Cleaning Pipeline ----
cleaned_data <- raw_data |>
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
analysis_data <- cleaned_data |>
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
analysis_data <- analysis_data |>
  group_by(pid) |>
  mutate(subject_overall_mean = mean(mean_score, na.rm = TRUE)) |>
  ungroup()

## 5. Save Clean Data ----
# You now only need one file for all subsequent analysis
saveRDS(analysis_data, here("output", "data_analysis_cleaned.rds"))