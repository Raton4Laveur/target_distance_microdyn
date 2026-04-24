## 1. Save Distribution Plot (Histogram) ----
p1 <- ggplot(subject_data, aes(x = mean_score, fill = group_name)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.5, bins = 15, position = "identity") +
  geom_density(alpha = 0.2) +
  facet_wrap(~group_name) + 
  theme_minimal() +
  labs(title = "Distribution of Mean Scores by Group",
       subtitle = "Histogram & Density Plot",
       x = "Mean Score per Participant")

print(p1)

# APA Recommendation: Use PDF for scalability in documents
ggsave(here("plots", "distribution_plot.pdf"), 
       plot = p1, 
       width = 8, 
       height = 6)

p2 <- ggplot(data_analysis, aes(x = mean_score, fill = group_name)) +
  # We keep the fill by group, but facet by both group and type
  geom_histogram(aes(y = after_stat(density)), alpha = 0.6, bins = 15) +
  geom_density(alpha = 0.2) +
  # facet_grid(rows ~ columns)
  facet_grid(type ~ group_name) + 
  theme_minimal() +
  labs(title = "Distribution of Mean Scores by Group and Type",
       subtitle = "Faceted by Anchor vs Manipulated",
       x = "Mean Score per Participant")

print(p2)

# APA Recommendation: Use PDF for scalability in documents
ggsave(here("plots", "seperate_distribution_plot.pdf"), 
       plot = p2, 
       width = 8, 
       height = 6)


## 2. Save QQ-Plot (Diagnostic) ----
p3 <- ggplot(data_analysis, aes(sample = mean_score, color = group_name)) +
  # Adding 'geom_jitter' logic or reducing alpha helps see the density of ties
  stat_qq(alpha = 0.4) + 
  stat_qq_line() +
  facet_grid(group_name ~ type) + 
  theme_light() +
  labs(title = "QQ-Plots: Normality Check per Condition")

print(p3)

ggsave(here("plots", "qq_normality_check.png"), 
       plot = p3, 
       width = 8, 
       height = 6, 
       dpi = 300)

## 3. Save Final Comparison Plot (APA Standard) ----
p4 <- ggplot(data_analysis, aes(x = type, y = mean_score, fill = group_name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6, position = position_dodge(0.7)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
             alpha = 0.3, size = 1, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "white", 
               position = position_dodge(0.7)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Anchor vs. Manipulated Scores",
       subtitle = "White diamonds represent group means",
       x = "Question Category", y = "Mean Score", fill = "Group")

print(p4)

# APA Recommendation: High-res TIFF for journal submission
ggsave(
  filename = here("plots", "final_comparison_apa.tif"), 
  plot = p4, 
  device = "tiff", 
  dpi = 600,       # High resolution for print
  width = 7, 
  height = 5, 
  units = "in"
)
