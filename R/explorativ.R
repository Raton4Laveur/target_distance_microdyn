{
  library("rstatix")
  library("coin")

  if (!dir.exists("output")) dir.create("output")
}
{
results_explorative <- list(
  
  # Test for Age disparity significance
  "KS-test_age" =
    data_demo |>
      group_by(group_name) |>
      summarise(
        # The "Ties" Issue: By using jitter(), you tell R: 
        # "Pretend these identical scores are actually 0.000001 apart." 
        # This satisfies the mathematical requirement of the KS test.
        ks_stat = ks.test(jitter(sd03_age), "pnorm", 
                          mean = mean(sd03_age, na.rm = TRUE), 
                          sd = sd(sd03_age, na.rm = TRUE))$statistic,
        p_value = ks.test(jitter(sd03_age), "pnorm", 
                          mean = mean(sd03_age, na.rm = TRUE), 
                          sd = sd(sd03_age, na.rm = TRUE))$p.value,
        .groups = "drop"
      ) |>
      add_significance("p_value")
    ,
    
  "Wilcoxon-test_age" =
    data_demo |>
      rstatix::wilcox_test(sd03_age ~ group_name, alternative = "greater") |>
      add_significance()
    ,
  
  "Wilcoxon-effectsize_age" =
    data_demo |>
      rstatix::wilcox_effsize(sd03_age ~ group_name)
    ,
  
  "paired-T-test_EG-B" =
    # Paired T-test comparing Anchor vs Manipulated within EG-B
    data_analysis |>
      filter(group_name == "TG") |>
      t_test(mean_score ~ type, paired = TRUE, alternative = "two.sided") |>
      add_significance()
    ,
  "Cohens-d_EG-B" =
    data_analysis |>
      filter(group_name == "TG") |>
      cohens_d(mean_score ~ type, paired = TRUE)
    ,
  "Signed-wilcoxon-test_KG" =
    # Wilcoxon-Test comparing Anchor vs Manipulated within KG
    data_analysis |>
      filter(group_name == "KG") |>
      rstatix::wilcox_test(mean_score ~ type, paired = TRUE, alternative = "two.sided") |>
      add_significance()
    ,
  "Wilcoxon-effectsize_KG" =
    data_analysis |>
      filter(group_name == "KG") |>
      rstatix::wilcox_effsize(mean_score ~ type, paired = TRUE, alternative = "two.sided")
    ,
  "Wilcoxon-test_Anchor" =
    #Anchor between KG & TG ----
    data_analysis |>
      filter(type == "Anker") |>
      rstatix::wilcox_test(mean_score ~ group_name, alternative = "two.sided") |> 
      add_significance()
    ,
  "Wilcoxon-effectsize_Anchor" =
    data_analysis |>
      filter(type == "Anker") |>
      rstatix::wilcox_effsize(mean_score ~ group_name)
)
print(results_explorative)
openxlsx::write.xlsx(results_explorative, 
                     file = here("output", "Explorativresultate.xlsx"), 
                     overwrite = TRUE)
}