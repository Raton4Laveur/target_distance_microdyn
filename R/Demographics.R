# 1. Setup ----

library("cobalt")
library("ggplot2")
library("rstatix")

if (!dir.exists("plots")) dir.create("plots")

GROUP_COLOURS <- c("#2E86AB", "#E84855")   # swap to your palette if needed

theme_apa <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title         = element_text(face = "bold", size = base_size + 2, margin = margin(b = 6)),
      plot.subtitle      = element_text(face = "italic", size = base_size, colour = "grey40", margin = margin(b = 14)),
      axis.line          = element_line(colour = "grey30"),
      axis.ticks         = element_line(colour = "grey30"),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold"),
      legend.position    = "bottom",
      legend.title       = element_text(face = "bold"),
      panel.grid.major.x = element_line(colour = "grey92"),
      panel.grid.major.y = element_blank()
    )
}

# 2. Data Cleaning ----

# Collapse granular education categories into 6 meaningful buckets
recode_education <- function(x) {
  case_when(
    x %in% c("No schooling completed", "Nursery school", "Kindergarten",
             "Grade 1 through 11", "12th Grade - NO DIPLOMA",
             "Regular high school diploma", "GED or alternative credential")
    ~ "High School or Less",
    x %in% c("Some college credit, but less than 1 year of college credit",
             "1 or more years of college credit, no degree")
    ~ "Some College",
    x == "Associate's degree (AA, AS)"
    ~ "Associate's",
    x == "Bachelor's degree (BA, BS)"
    ~ "Bachelor's",
    x %in% c("Master's degree (MA, MS, MEng, MEd, MSW, MBA)",
             "Professional degree (MD, DDS, DVM, LLB, JD)",
             "Doctorate degree (PhD, EdD)")
    ~ "Graduate",
    x == "Prefer not to say"
    ~ "Prefer not to say",
    TRUE ~ NA_character_
  )
}

edu_levels_collapsed <- c(
  "High School or Less", "Some College", "Associate's", "Bachelor's", "Graduate"
)

demo_data <- cleaned_data |>
  select(pid, group_name, sd02_gender, sd03_age, sd04_ethnicity, sd05_education) |>
  mutate(
    # sd03_age is a 1-100 index; participants are all 18+, so index maps to age as 17 + sd03_age
    sd03_age       = 17 + sd03_age,
    group_name     = as.factor(group_name),
    sd05_education = factor(recode_education(as.character(sd05_education)),
                            levels = edu_levels_collapsed, ordered = TRUE)
  ) |>
  # Exclude "Prefer not to say" from gender, ethnicity, and education globally
  filter(
    !is.na(sd02_gender),  !is.na(sd04_ethnicity), !is.na(sd03_age),
    sd02_gender    != "Prefer not to say",
    sd04_ethnicity != "Prefer not to say",
    !is.na(sd05_education)   # NAs are the recoded "Prefer not to say" rows
  )


# ── Figure 1: Age by Group — Violin + Boxplot + Mean ──────────────────────────
# Clean, journal-friendly alternative to the raincloud.
# Two violins make the group age difference immediately visible.

p_age <- demo_data |>
  ggplot(aes(x = group_name, y = sd03_age, fill = group_name, colour = group_name)) +
  
  geom_violin(alpha = 0.25, trim = FALSE, linewidth = 0.5) +
  geom_boxplot(width = 0.12, alpha = 0.6, outlier.shape = NA, colour = "grey20") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, colour = "grey10") +
  
  scale_fill_manual(values   = GROUP_COLOURS, guide = "none") +
  scale_colour_manual(values = GROUP_COLOURS, guide = "none") +
  coord_flip() +
  
  theme_apa() +
  labs(
    title    = "Age Distribution by Group",
    subtitle = "Violin = full distribution; box = IQR; diamond = mean",
    x = NULL, y = "Age (years)"
  )

print(p_age)
ggsave(here("plots", "01_age_violin.pdf"), p_age,
       width = 18, height = 10, units = "cm", dpi = 300)


# ── Figure 2: Faceted Grouped Bar — Gender, Ethnicity, Education by Group ────────
# One panel per variable; dodged bars show each group's proportion side-by-side.
# Standard in journal methods sections — easy to read across all three variables.

p_grouped_bar <- demo_data |>
  # 1. Prepare and Pivot
  mutate(across(c(sd02_gender, sd04_ethnicity, sd05_education), as.character)) |>
  pivot_longer(
    cols = c(sd02_gender, sd04_ethnicity, sd05_education),
    names_to = "variable", 
    values_to = "category"
  ) |>
  # 2. Filter and Clean
  filter(!is.na(category), category != "Prefer not to say") |>
  # 3. Calculate Proportions
  count(group_name, variable, category) |> 
  group_by(group_name, variable) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  # 4. Refactor Labels and Ordering
  mutate(
    variable = recode_values(variable,
                             "sd02_gender"    ~ "Gender",
                             "sd04_ethnicity" ~ "Ethnicity",
                             "sd05_education" ~ "Education"
    ),
    category = fct_reorder(category, prop, .desc = FALSE)
  ) |> 
  # 5. Direct to Plotting
  ggplot(aes(x = prop, y = category, fill = group_name)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = GROUP_COLOURS, name = "Group") +
  theme_apa() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Categorical Demographic Breakdown by Group",
    subtitle = "Proportions within each group; bars are directly comparable between groups",
    x = "Proportion (%)", 
    y = NULL
  )


print(p_grouped_bar)
ggsave(here("plots", "02_grouped_bar.pdf"), p_grouped_bar,
       width = 18, height = 28, units = "cm", dpi = 300)


# ── Figure 3: Education by Group — Stacked Proportional Bar ────────────────────
# Two bars (one per group) showing the proportional education makeup.
# Simple, uncluttered, and directly answers "do the groups differ in education?".

(p_edu_group <- demo_data |>
    # 1. Data Processing
    filter(!is.na(sd05_education)) |>
    count(group_name, sd05_education) |>
    group_by(group_name) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    
    # 2. Plotting
    ggplot(aes(x = group_name, y = prop, fill = sd05_education)) +
    geom_col(position = "fill", width = 0.5, alpha = 0.9) +
    geom_text(
      aes(label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")),
      position = position_fill(vjust = 0.5),
      size = 3.2, colour = "white", fontface = "bold"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_viridis_d(
      option = "plasma", 
      name = "Education Level",
      guide = guide_legend(reverse = TRUE)
    ) +
    coord_flip() +
    theme_apa() +
    theme(legend.position = "right") +
    labs(
      title    = "Educational Makeup by Group",
      subtitle = "Proportional breakdown; labels shown for segments ≥ 5%",
      x = NULL, y = "Proportion (%)"
    ))

# 3. Save
ggsave(
  here("plots", "03_edu_group.pdf"), 
  p_edu_group,
  width = 22, height = 10, units = "cm", dpi = 300
)


# ── Figure 4: Standardised Mean Differences — Love Plot ─────────────────────────
# Summarises group balance across ALL demographic variables in a single panel.
# SMD < 0.1 (dashed reference line) is the conventional threshold for adequate
# balance; points beyond it flag variables that may need covariate adjustment.

# cobalt::bal.tab() expects a binary treatment indicator (0/1)
## Figure 4: Standardised Mean Differences (Love Plot) - FIXED ----
(p_love <- demo_data |>
   # 1. Explicitly create binary flags for each ethnicity level
   mutate(
     treat         = as.integer(as.factor(group_name)) - 1L,
     gender_female = as.integer(sd02_gender == "Female"),
     edu_numeric   = as.integer(as.factor(sd05_education)),
     # Create explicit binary columns for ethnicity
     eth_white    = as.integer(sd04_ethnicity == "White"),
     eth_black    = as.integer(sd04_ethnicity == "Black"),
     eth_hispanic = as.integer(sd04_ethnicity == "Hispanic or Latino"),
     eth_asian    = as.integer(sd04_ethnicity == "Asian"),
     eth_other    = as.integer(sd04_ethnicity == "Other")
   ) |>
   # 2. Generate SMDs using cobalt
   {\(d) cobalt::bal.tab(
     treat ~ sd03_age + gender_female + edu_numeric + 
       eth_white + eth_black + eth_hispanic + eth_asian + eth_other,
     data = d, 
     binary = "std", 
     continuous = "std"
   )$Balance}() |> 
   
   # 3. Clean and Relabel
   rownames_to_column("variable") |>
   filter(variable != "distance") |>
   transmute(
     smd = Diff.Un,
     variable = case_match(variable,
                           "sd03_age"      ~ "Age",
                           "gender_female" ~ "Gender (Female)",
                           "edu_numeric"   ~ "Education (ordinal)",
                           "eth_white"     ~ "Ethnicity: White",
                           "eth_black"     ~ "Ethnicity: Black",
                           "eth_hispanic"  ~ "Ethnicity: Hispanic / Latino",
                           "eth_asian"     ~ "Ethnicity: Asian",
                           "eth_other"     ~ "Ethnicity: Other",
                           .default = variable
     ),
     balanced = abs(smd) < 0.1,
     variable = fct_reorder(variable, abs(smd))
   ) |>
   
   # 4. Plotting
   ggplot(aes(x = smd, y = variable, colour = balanced)) +
   geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.5) +
   geom_vline(xintercept = c(-0.1, 0.1), colour = "grey60", linewidth = 0.4, linetype = "dashed") +
   geom_segment(aes(x = 0, xend = smd, yend = variable), linewidth = 1.5, alpha = 0.6) +
   geom_point(size = 3.5) +
   scale_colour_manual(
     values = c("TRUE" = "#2E86AB", "FALSE" = "#E84855"),
     labels = c("TRUE" = "Balanced (|SMD| < 0.1)", "FALSE" = "Imbalanced (|SMD| ≥ 0.1)"),
     name   = NULL,
     drop   = FALSE # Ensures the legend shows both even if all are balanced
   ) +
   # Centering the plot around 0
   coord_cartesian(xlim = c(-0.15, 0.15)) + 
   theme_apa() +
   theme(
     legend.position   = "bottom",
     panel.grid.major.y = element_line(colour = "grey92"),
     panel.grid.major.x = element_blank()
   ) +
   labs(
     title    = "Group Balance on Demographic Covariates",
     subtitle = "Standardised mean differences (SMD); dashed lines mark the |0.1| balance threshold",
     x = "Standardised Mean Difference", y = NULL
   ))

# 5. Save
ggsave(here("plots", "04_love_plot.pdf"), p_love, 
       width = 22, height = 16, units = "cm", dpi = 300)


## Quick Test for Age disparity significance ----

demo_data |>
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


demo_data |>
  rstatix::wilcox_test(sd03_age ~ group_name, alternative = "greater") |> # Explicitly use rstatix
  ## uncomment next line for two-sided alternative (for github).
  #rstatix::wilcox_test(mean_score ~ group_name, alternative = "two.sided") |> 
  add_significance()

demo_data |>
  rstatix::wilcox_effsize(sd03_age ~ group_name)