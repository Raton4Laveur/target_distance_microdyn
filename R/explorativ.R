# Paired T-test comparing Anchor vs Manipulated within EG-B
analysis_data |>
  filter(group_name == "EG-B") |>
  t_test(mean_score ~ type, paired = TRUE, alternative = "two.sided") |>
  add_significance()

analysis_data |>
  filter(group_name == "EG-B") |>
  cohens_d(mean_score ~ type, paired = TRUE)

# Wilcoxon-Test comparing Anchor vs Manipulated within KG
analysis_data |>
  filter(group_name == "KG") |>
  rstatix::wilcox_test(mean_score ~ type, paired = TRUE, alternative = "two.sided") |>
  add_significance()

analysis_data |>
  filter(group_name == "KG") |>
  rstatix::wilcox_effsize(mean_score ~ type, paired = TRUE, alternative = "two.sided")

#Anchor between KG & TG ----
analysis_data |>
  filter(type == "Anchor") |>
  rstatix::wilcox_test(mean_score ~ group_name, alternative = "two.sided") |> 
  add_significance()

analysis_data |>
  filter(type == "Anchor") |>
  rstatix::wilcox_effsize(mean_score ~ group_name)