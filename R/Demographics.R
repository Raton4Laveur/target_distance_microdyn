# 1. Setup ----
{
library("cobalt")
library("ggplot2")
library("rstatix")

if (!dir.exists("plots")) dir.create("plots")
if (!dir.exists("output")) dir.create("output")

theme_set(theme_apa())
}

# 2. Data Cleaning ----

# Collapse granular education categories into 6 meaningful buckets
recode_education <- function(x) {
  case_when(
    x %in% c("No schooling completed", "Nursery school", "Kindergarten",
             "Grade 1 through 11", "12th Grade - NO DIPLOMA",
             "Regular high school diploma", "GED or alternative credential")
    ~ "Abitur / Obligatorische Schulzeit",
    x %in% c("Some college credit, but less than 1 year of college credit",
             "1 or more years of college credit, no degree")
    ~ "Studium ohne Abschluss",
    x == "Associate's degree (AA, AS)"
    ~ "Berufsakademie / FH",
    x == "Bachelor's degree (BA, BS)"
    ~ "Bachelor",
    x %in% c("Master's degree (MA, MS, MEng, MEd, MSW, MBA)",
             "Professional degree (MD, DDS, DVM, LLB, JD)",
             "Doctorate degree (PhD, EdD)")
    ~ "Master / Promotion",
    x == "Prefer not to say"
    ~ "Keine Angabe",
    TRUE ~ NA_character_
  )
}


data_demo <- data_clean |>
  select(pid, group_name, sd02_gender, sd03_age, sd04_ethnicity, sd05_education) |>
  mutate(
    # sd03_age is a 1-100 index; participants are all 18+, so index maps to age as 17 + sd03_age
    sd03_age       = sd03_age,
    group_name     = as.factor(group_name),
    sd05_education = factor(recode_education(as.character(sd05_education)),
                            levels = c(
                              "Abitur / Obligatorische Schulzeit", "Studium ohne Abschluss", "Berufsakademie / FH", "Bachelor", "Master / Promotion"
                            ), ordered = TRUE)
  ) |>
  mutate(group_name = case_when(
    group_name == "EG-B" ~ "TG",
    TRUE ~ as.character(group_name)
  )) |>
  # Exclude "Prefer not to say" from gender, ethnicity, and education globally
  filter(
    !is.na(sd02_gender),  !is.na(sd04_ethnicity), !is.na(sd03_age),
    sd02_gender    != "Prefer not to say",
    sd04_ethnicity != "Prefer not to say",
    !is.na(sd05_education)   # NAs are the recoded "Prefer not to say" rows
  )



# ── Figure 1: Age by Group — Violin + Boxplot + Mean ──────────────────────────

(p_age <- data_demo |>
  ggplot(aes(x = group_name, y = sd03_age, fill = group_name, colour = group_name)) +
  
  geom_violin(alpha = 0.25, trim = FALSE, linewidth = 0.5) +
  geom_boxplot(width = 0.12, alpha = 0.6, outlier.shape = NA, colour = "grey20") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, colour = "grey10") +
  
    scale_fill_apa() +
    scale_color_apa() +
    coord_flip() +
  
  labs(
    title    = "Altersverteilung nach Gruppen",
    subtitle = "Violine = Gesamtverteilung; Box = IQR; Raute = Mittelwert",
    x = NULL, y = "Alter (Jahre)"
  ) +
    theme(legend.position = "none")
)
  

# ── Figure 2: Faceted Grouped Bar — Gender, Ethnicity, Education by Group ────────
# One panel per variable; dodged bars show each group's proportion side-by-side.

(p_grouped_bar <- data_demo |>
  # 1. Prepare and Pivot
  mutate(across(c(sd02_gender, sd04_ethnicity, sd05_education), as.character)) |>

  # Übersetzung der ethnischen Kategorien im Datensatz für den Plot
  mutate(sd04_ethnicity = case_when(
    sd04_ethnicity == "White" ~ "Weiss",
    sd04_ethnicity == "Black" ~ "Schwarz / Afroamerikanisch",
    sd04_ethnicity == "Hispanic or Latino" ~ "Hispanic / Latino",
    sd04_ethnicity == "Native American or Alaskan Native" ~ "Indigene Amerikaner oder Ureinwohner Alaskas",
    sd04_ethnicity == "Asian" ~ "Asiatisch",
    sd04_ethnicity == "Other" ~ "Andere",
    TRUE ~ sd04_ethnicity
  )) |>
  mutate(sd02_gender = case_when(
    sd02_gender == "Female" ~ "Weiblich",
    sd02_gender == "Male" ~ "Männlich",
    TRUE ~ sd02_gender
  )) |>

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
                             "sd02_gender"    ~ "Geschlecht",
                             "sd04_ethnicity" ~ "Ethnie",
                             "sd05_education" ~ "Bildung"
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
    scale_fill_apa() +
    theme(legend.position = "bottom") +
  labs(
    title    = "Kategoriale demografische Zusammensetzung nach Gruppen",
    subtitle = "Anteile innerhalb jeder Gruppe; Balken sind direkt miteinander vergleichbar",
    x = "Anteil (%)", 
    y = NULL,
    fill = "Gruppen"
  )
)




# ── Figure 3: Education by Group — Stacked Proportional Bar ────────────────────
# Two bars (one per group) showing the proportional education makeup.
# Simple, uncluttered, and directly answers "do the groups differ in education?".

(p_edu_group <- data_demo |>
    # 1. Data Processing
    filter(!is.na(sd05_education)) |>
    count(group_name, sd05_education) |>
    group_by(group_name) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    
    # 2. Plotting
    ggplot(aes(x = group_name, y = prop, fill = sd05_education)) +
    geom_col(position = "fill", width = 0.5, alpha = 0.9, color = "#99999999") +
    geom_text(
      aes(label = ifelse(prop >= 0.05, scales::percent(prop, accuracy = 1), "")),
      position = position_fill(vjust = 0.5),
      size = 4.5, family = "sans", colour = "black", fontface = "bold"
    ) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
    scale_fill_brewer(
      palette = "Greys", 
      name = "Bildungsniveau",
      guide = guide_legend(reverse = TRUE)
    ) +
    coord_flip() +
    theme_apa() +
    theme(legend.position = "right") +
    labs(
      title    = "Bildungsstruktur nach Gruppe",
      subtitle = "Prozentuale Anteile innerhalb jeder Gruppe",
      x = NULL, y = "Anteil (%)"
    ))




# ── Figure 4: Standardised Mean Differences — Love Plot ─────────────────────────
# Summarises group balance across ALL demographic variables in a single panel.
# SMD < 0.1 (dashed reference line) is the conventional threshold for adequate

(p_love <- data_demo |>
   # 1. Explicitly create binary flags for each ethnicity level
   mutate(
     treat         = as.integer(as.factor(group_name)) - 1L,
     gender_female = as.integer(sd02_gender == "Female"),
     edu_numeric   = as.integer(as.factor(sd05_education)),
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
     continuous = "std",
     s.d.denom = "pooled"
   )$Balance}() |> 
   
   # 3. Clean and Relabel
   rownames_to_column("variable") |>
   filter(variable != "distance") |>
   transmute(
     smd = Diff.Un,
     variable = recode_values(variable,
                           "sd03_age"      ~ "Alter",
                           "gender_female" ~ "Geschlecht (Weiblich)",
                           "edu_numeric"   ~ "Bildung (ordinal)",
                           "eth_white"     ~ "Ethnie: Weiss",
                           "eth_black"     ~ "Ethnie: Schwarz",
                           "eth_hispanic"  ~ "Ethnie: Hispanic / Latino",
                           "eth_asian"     ~ "Ethnie: Asiatisch",
                           "eth_other"     ~ "Ethnie: Andere",
     ),
     variable = fct_reorder(variable, abs(smd))
   ) |>
   
   # 4. Plotting
   ggplot(aes(x = smd, y = variable)) +
   geom_vline(xintercept = 0, colour = "black", linewidth = 0.5) +
   geom_vline(xintercept = c(-0.1, 0.1), colour = "grey70", linewidth = 0.4, linetype = "dashed") +
   geom_segment(aes(x = 0, xend = smd, yend = variable), linewidth = 2, alpha = 0.5) +
   geom_point(size = 3) +
   # Centering the plot around 0
   scale_color_apa() +
   coord_cartesian(xlim = c(-0.125, 0.125)) + 
   theme_apa() +
   theme(
   ) +
   labs(
     title    = "Gruppenbalance der demographischen Kovarianten",
     subtitle = "Standardisierte Mittelwertsdifferenzen (SMD); Gestrichelte Linien markieren die Toleranzgrenze von |0,1|",
     x = "(SMD)", y = NULL
   )
 )

{
## Saving Data ----
  saveRDS(data_demo, here("output", "data_demographics.rds"))

## Saving Plots ----

  # Plot 1
  ggsave(here("plots", "03_alter_violinen.pdf"), p_age,
         width = 18, height = 10, units = "cm", dpi = 300)
  # Plot 2
  ggsave(here("plots", "04_bildung_gruppe.pdf"), p_edu_group,
         width = 22, height = 10, units = "cm", dpi = 300)
  # Plot 3
  ggsave(here("plots", "05_kategorial_balken.pdf"), p_grouped_bar,
         width = 18, height = 28, units = "cm", dpi = 300)
  # Plot 4
  ggsave(here("plots", "06_love_plot.pdf"), p_love, 
         width = 22, height = 16, units = "cm", dpi = 300)
}