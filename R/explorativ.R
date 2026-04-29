# ks-test showed: KG == ns ----
## Levene's Test
data_analysis |>
  filter(group_name == "EG-B") |>
  levene_test(mean_score ~ type) |>
  add_significance()

# Welch T-test comparing Anchor vs Manipulated within groups
data_analysis |>
  filter(group_name == "EG-B") |>
  t_test(mean_score ~ type, var.equal = FALSE, paired = TRUE) |>
  add_significance()

# Wilcoxon-Test comparing Anchor vs Manipulated within groups
data_analysis |>
  group_by(group_name) |>
  rstatix::wilcox_test(mean_score ~ type, paired = TRUE) |>
  add_significance()

#Anchor between KG & TG ----
data_analysis |>
  filter(type == "Anchor") |>
  rstatix::wilcox_test(mean_score ~ group_name, alternative = "two.sided") |> 
  add_significance()
  

