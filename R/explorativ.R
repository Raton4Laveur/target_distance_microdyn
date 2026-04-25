# Welch T-test comparing Anchor vs Manipulated within groups
data_analysis |>
  group_by(group_name) |>
  t_test(mean_score ~ type, var.equal = FALSE) |>
  add_significance()

# Wilcoxon-Test comparing Anchor vs Manipulated within groups
data_analysis |>
  group_by(group_name) |>
  rstatix::wilcox_test(mean_score ~ type, paired = TRUE) |>
  add_significance()


# Levene's Test (typeless)
subject_data |>
  levene_test(mean_score ~ group_name) |>
  add_significance()

# Levene's Test
data_analysis |>
  filter(type == "Manipulated") |>
  levene_test(mean_score ~ group_name) |>
  add_significance()



# overall wilcoxon test (typeless)
subject_data |>
  rstatix::wilcox_test(mean_score ~ group_name) |>
  add_significance()


#Anchor between KG & TG
data_analysis |>
  filter(type == "Anchor") |>
  rstatix::wilcox_test(mean_score ~ group_name, alternative = "two.sided") |> 
  add_significance()
  

# sanity check
data_analysis |> group_by(group_name, type) |> count()
data_analysis |> 
  pull(pid) |> 
  n_distinct()

subject_data |> group_by(group_name) |> count()
subject_data |> 
  pull(pid) |> 
  n_distinct()
