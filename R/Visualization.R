{
library("ggplot2")
library("scales")

if (!dir.exists("plots")) dir.create("plots")

theme_set(theme_apa())
}

## 1. Save Distribution Plot (Histogram) ----
(p1 <- ggplot(data_analysis, aes(x = subject_overall_mean, fill = group_name)) +
   geom_histogram(aes(y = after_stat(density)), alpha = 0.5, bins = 15, position = "identity") +
   geom_density(alpha = 0.2) +
   facet_wrap(~group_name) + 
   scale_fill_apa() +
   labs(title = "Distribution of Mean Scores by Group",
        subtitle = "Histogram & Density Plot",
        x = "Mean Score per Participant")
)

## 2. Distribution Plot by group & question type ----
(p2 <- ggplot(data_analysis, aes(x = mean_score, fill = group_name)) +
   geom_histogram(aes(y = after_stat(density)), alpha = 0.6, bins = 15) +
   geom_density(alpha = 0.2) +
   facet_grid(type ~ group_name) + 
   scale_fill_apa() +
   labs(title = "Distribution of Mean Scores by Group and Type",
       subtitle = "Faceted by Anchor vs Manipulated",
       x = "Mean Score per Participant")
)


## 3. QQ-Plot (Diagnostic) ----
(p3 <- ggplot(data_analysis, aes(sample = mean_score, color = group_name)) +
   # Adding 'geom_jitter' logic or reducing alpha helps see the density of ties
   stat_qq(alpha = 0.4) + 
   stat_qq_line(color = "black") +
   facet_grid(group_name ~ type) + 
   scale_color_apa() +
   theme(
      strip.background = element_rect(fill = "grey95", colour = "grey30"),
      legend.position = "none"
    ) +
   labs(title = "QQ-Plots: Normality Check per Condition")
)


## 4. Boxplot ----
(p4 <- ggplot(data_analysis, aes(x = type, y = mean_score, fill = group_name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6, position = position_dodge(0.7)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
             alpha = 0.3, size = 1, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "white", 
               position = position_dodge(0.7)) +
   scale_fill_apa() +
   labs(title = "Comparison of Anchor vs. Manipulated Scores",
       subtitle = "White diamonds represent group means",
       x = "Question Category", y = "Mean Score", fill = "Group")
)



## 5. Saving all Plots ----
{
# Plot 1
ggsave(here("plots", "01_distribution_plot.pdf"), 
       plot = p1, 
       width = 8, 
       height = 6
       )
# Plot 2
ggsave(here("plots", "XX_seperate_distribution_plot.pdf"), 
       plot = p2, 
       width = 8, 
       height = 6
       )
# Plot 3
ggsave(here("plots", "02_qq_normality_check.png"), 
       plot = p3, 
       width = 8, 
       height = 6, 
       dpi = 300
       )
# Plot 4
ggsave(here("plots", "07_Boxplot.tif"), 
       plot = p4, 
       device = "tiff", 
       dpi = 600,
       width = 7, 
       height = 5, 
       units = "in"
       )
}
