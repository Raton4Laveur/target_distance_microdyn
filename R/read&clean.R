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
# This replaces all those manual vector splits (KG_par, TG_par)
data_analysis <- cleaned_data |>
  select(pid, group_name, starts_with("kap_par_")) |>
  #Pivot to long to easily categorize questions
  pivot_longer(
    col = starts_with("kap_par_"),
    names_to = "question",
    values_to = "score"
  ) |>
  drop_na(score) |>
  mutate(type = if_else(question %in% c("kap_par_01", "kap_par_02", "kap_par_08"), 
                        "Anchor", "Manipulated")) |>
  
  group_by(pid, group_name, type) |> 
  summarize(mean_score = mean(score), .groups = "drop")

## 4. workaround ----
subject_data <- data_analysis |>
  group_by(pid, group_name) |> # Ensure subject_id is in your dataset
  summarise(mean_score = mean(mean_score, na.rm = TRUE), .groups = "drop")

## 5. Save Clean Data ----
# Save the progress so you don't have to re-clean every time
saveRDS(data_analysis, here("output", "data_analysis_cleaned.rds"))
saveRDS(subject_data, here("output", "subject_data_cleaned.rds"))
