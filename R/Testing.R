library("rstatix")
library("coin")

## 1. Shapiro-Wilk test for every sub-group at once ----
shapiro_test <- data_analysis |>
  group_by(group_name, type) |>
# Only attempt the test if n >= 3 (mock data workaround)
  filter(n() >= 3) |>
  shapiro_test(mean_score) |>
  add_significance()

print(shapiro_test)

## 2. KS-Test for Normality (Large Samples) ----
ks_results <- data_analysis |>
  group_by(group_name, type) |>
  summarise(
    # The "Ties" Issue: By using jitter(), you tell R: 
    # "Pretend these identical scores are actually 0.000001 apart." 
    # This satisfies the mathematical requirement of the KS test.
    ks_stat = ks.test(jitter(mean_score), "pnorm", 
                      mean = mean(mean_score, na.rm = TRUE), 
                      sd = sd(mean_score, na.rm = TRUE))$statistic,
    p_value = ks.test(jitter(mean_score), "pnorm", 
                      mean = mean(mean_score, na.rm = TRUE), 
                      sd = sd(mean_score, na.rm = TRUE))$p.value,
    .groups = "drop"
  ) |>
  add_significance("p_value")

print(ks_results)
# The ks-test was not preregistered! --> footnote

## 3. Wilcoxon test between KG and TG (excluding Anchor)----
stat_test <- data_analysis |>
  filter(type == "Manipulated") |>
  rstatix::wilcox_test(mean_score ~ group_name, alternative = "greater") |> # Explicitly use rstatix
  ## uncomment next line for two-sided alternative (for github).
  #rstatix::wilcox_test(mean_score ~ group_name, alternative = "two.sided") |> 
  add_significance()

print(stat_test)

## 4. Wilcoxon effect size ----
wilcox_effsize <- data_analysis |>
  filter(type == "Manipulated") |>
  rstatix::wilcox_effsize(mean_score ~ group_name)

print(wilcox_effsize)