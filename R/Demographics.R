# 1. Setup ----

library("cobalt")
library("ggplot2")

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
    title    = "Figure 1. Age Distribution by Group",
    subtitle = "Violin = full distribution; box = IQR; diamond = mean",
    x = NULL, y = "Age (years)"
  )

print(p_age)
ggsave(here("plots", "01_age_violin.pdf"), p_age,
       width = 18, height = 10, units = "cm", dpi = 300)


# ── Figure 2: Faceted Grouped Bar — Gender, Ethnicity, Education by Group ────────
# One panel per variable; dodged bars show each group's proportion side-by-side.
# Standard in journal methods sections — easy to read across all three variables.

cat_props <- demo_data |>
  mutate(across(c(sd02_gender, sd04_ethnicity, sd05_education), as.character)) |>
  pivot_longer(cols = c(sd02_gender, sd04_ethnicity, sd05_education),
               names_to = "variable", values_to = "category") |>
  filter(!is.na(category), category != "Prefer not to say") |>
  group_by(group_name, variable, category) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(group_name, variable) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  mutate(
    variable = recode(variable,
                      sd02_gender    = "Gender",
                      sd04_ethnicity = "Ethnicity",
                      sd05_education = "Education"
    ),
    # Order categories by overall proportion so bars go largest → smallest
    category = fct_reorder(category, prop, .desc = FALSE)
  )

p_grouped_bar <- cat_props |>
  ggplot(aes(x = prop, y = category, fill = group_name)) +
  
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = GROUP_COLOURS, name = "Group") +
  
  theme_apa() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Figure 2. Categorical Demographic Breakdown by Group",
    subtitle = "Proportions within each group; bars are directly comparable between groups",
    x = "Proportion (%)", y = NULL
  )

print(p_grouped_bar)
ggsave(here("plots", "02_grouped_bar.pdf"), p_grouped_bar,
       width = 18, height = 28, units = "cm", dpi = 300)


# ── Figure 3: Education by Group — Stacked Proportional Bar ────────────────────
# Two bars (one per group) showing the proportional education makeup.
# Simple, uncluttered, and directly answers "do the groups differ in education?".

edu_group_data <- demo_data |>
  filter(!is.na(sd05_education)) |>
  group_by(group_name, sd05_education) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(group_name) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

p_edu_group <- edu_group_data |>
  ggplot(aes(x = group_name, y = prop, fill = sd05_education)) +
  
  geom_col(position = "fill", width = 0.5, alpha = 0.9) +
  geom_text(
    aes(label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")),
    position = position_fill(vjust = 0.5),
    size = 3.2, colour = "white", fontface = "bold"
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "plasma", name = "Education Level",
                       guide = guide_legend(reverse = TRUE)) +
  coord_flip() +
  
  theme_apa() +
  theme(legend.position = "right") +
  labs(
    title    = "Figure 3. Educational Makeup by Group",
    subtitle = "Proportional breakdown; labels shown for segments ≥ 5%",
    x = NULL, y = "Proportion (%)"
  )

print(p_edu_group)
ggsave(here("plots", "03_edu_group.pdf"), p_edu_group,
       width = 22, height = 10, units = "cm", dpi = 300)


# ── Figure 4: Standardised Mean Differences — Love Plot ─────────────────────────
# Summarises group balance across ALL demographic variables in a single panel.
# SMD < 0.1 (dashed reference line) is the conventional threshold for adequate
# balance; points beyond it flag variables that may need covariate adjustment.

# cobalt::bal.tab() expects a binary treatment indicator (0/1)
demo_bal <- demo_data |>
  mutate(
    treat         = as.integer(group_name) - 1L,   # first factor level → 0, second → 1
    edu_numeric   = as.integer(sd05_education),    # ordered factor → ordinal integer
    gender_female = as.integer(sd02_gender == "Female"),
    # One-hot encode ethnicity (drop one level to avoid redundancy)
    eth_white     = as.integer(sd04_ethnicity == "White"),
    eth_black     = as.integer(sd04_ethnicity == "Black or African American"),
    eth_hispanic  = as.integer(sd04_ethnicity == "Hispanic or Latino"),
    eth_asian     = as.integer(sd04_ethnicity == "Asian"),
    eth_other     = as.integer(sd04_ethnicity == "Other")
  )

bal <- bal.tab(
  treat ~ sd03_age + gender_female + edu_numeric +
    eth_white + eth_black + eth_hispanic + eth_asian + eth_other,
  data      = demo_bal,
  binary    = "std",   # standardise binary vars the same way as continuous
  continuous = "std"
)

# Extract SMD table and give variables readable labels
smd_df <- bal$Balance |>
  rownames_to_column("variable") |>
  filter(variable != "distance") |>
  transmute(
    variable = recode(variable,
                      sd03_age       = "Age",
                      gender_female  = "Gender (Female)",
                      edu_numeric    = "Education (ordinal)",
                      eth_white      = "Ethnicity: White",
                      eth_black      = "Ethnicity: Black / African American",
                      eth_hispanic   = "Ethnicity: Hispanic / Latino",
                      eth_asian      = "Ethnicity: Asian",
                      eth_other      = "Ethnicity: Other"
    ),
    smd = Diff.Un   # unadjusted SMD
  ) |>
  mutate(
    balanced  = abs(smd) < 0.1,
    variable  = fct_reorder(variable, abs(smd))
  )

p_love <- smd_df |>
  ggplot(aes(x = smd, y = variable, colour = balanced)) +
  
  # Reference lines
  geom_vline(xintercept = 0,    colour = "grey50", linewidth = 0.5) +
  geom_vline(xintercept =  0.1, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = -0.1, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
  
  # Horizontal whisker from 0 to the point
  geom_segment(aes(x = 0, xend = smd, yend = variable), linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 3.5) +
  
  scale_colour_manual(
    values = c("TRUE" = "#2E86AB", "FALSE" = "#E84855"),
    labels = c("TRUE" = "Balanced (|SMD| < 0.1)", "FALSE" = "Imbalanced (|SMD| ≥ 0.1)"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  
  theme_apa() +
  theme(
    legend.position  = "bottom",
    panel.grid.major.y = element_line(colour = "grey92"),  # horizontal guides aid reading
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title    = "Figure 4. Group Balance on Demographic Covariates",
    subtitle = "Standardised mean differences (SMD); dashed lines mark the |0.1| balance threshold",
    x = "Standardised Mean Difference", y = NULL
  )

print(p_love)
ggsave(here("plots", "04_love_plot.pdf"), p_love,
       width = 22, height = 16, units = "cm", dpi = 300)