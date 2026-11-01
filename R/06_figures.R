# R/06_figures.R -- High-Quality Publication Figures (Blue Theme)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(forcats); library(ggplot2); library(patchwork)
  library(ggExtra); library(ggcorrplot); library(scales); library(RColorBrewer)
})

clean   <- readRDS("data/processed/clean_data.rds")
primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

# --- Define Professional Blue Palette ---
blues <- c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF")
bg_blue <- "#F7FBFF"

# --- Global ggplot theme ---
theme_pub <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(10, 10, 10, 10)
  )
theme_set(theme_pub)

save_both <- function(p, name, w, h) {
  ggsave(file.path("output/figures", paste0(name, ".pdf")),  p, width = w, height = h)
  ggsave(file.path("output/figures", paste0(name, ".tiff")), p, width = w, height = h, dpi = 300, compression = "lzw")
  message("  saved ", name)
}

# ==================================================================
# Fig 1: STROBE flow (Fixed box height, light blue background, black text)
# ==================================================================
p_flow <- ggplot() +
  # Main boxes 
  annotate("rect", xmin = 0.1, xmax = 0.9, ymin = c(8, 6, 4, 2),
           ymax = c(9.5, 7.5, 5.5, 3.5), fill = "white", colour = "black", linewidth = 0.6) +
  # Exclusion boxes
  annotate("rect", xmin = 1.0, xmax = 1.4, ymin = c(6.2, 4.2),
           ymax = c(7.3, 5.3), fill = bg_blue, colour = "black", linewidth = 0.5) +
  # Final Analysis box (Taller, Light Blue fill)
  annotate("rect", xmin = 0.1, xmax = 0.9, ymin = -1.8, ymax = 0.8,
           fill = blues[7], colour = "black", linewidth = 0.6) +
  
  # Main text
  annotate("text", x = 0.5, y = c(8.75, 6.75, 4.75, 2.75),
           label = c("Enrolled\nn = 136",
                     "Assessed for eligibility\nn = 136",
                     "Eligible\nn = 116",
                     "Complete data n = 113\nDSM-5 primary cohort n = 78"),
           size = 4.2, lineheight = 1.1) +
  
  # Exclusion text
  annotate("text", x = 1.2, y = c(6.75, 4.75),
           label = c("Excluded\n(abnormal thyroid)\nn = 20",
                     "Excluded\n(incomplete data)\nn = 3"),
           size = 3.8, lineheight = 1.1) +
  
  # Final analysis text (Black text, centered vertically inside the taller box)
  annotate("text", x = 0.5, y = -0.5,
           label = "Analysed\nn = 78\n\n48 Depressive / 21 Anxiety\n9 Stress-related",
           size = 4, colour = "black", lineheight = 1.1) +
  
  # Main arrows
  annotate("segment", x = 0.5, xend = 0.5, y = c(8, 6, 4), yend = c(7.5, 5.5, 3.5),
           arrow = arrow(length = unit(2.5, "mm"), type = "closed"), colour = "black") +
  annotate("segment", x = 0.5, xend = 0.5, y = 2, yend = 0.8,
           arrow = arrow(length = unit(2.5, "mm"), type = "closed"), colour = "black") +
  
  # Dashed exclusion arrows
  annotate("segment", x = 0.9, xend = 1.0, y = c(6.75, 4.75), yend = c(6.75, 4.75),
           arrow = arrow(length = unit(2, "mm"), type = "closed"), linetype = "dashed") +
  
  coord_cartesian(xlim = c(0, 1.5), ylim = c(-2.2, 10)) +
  theme_void() + theme(plot.margin = margin(10, 10, 10, 10))
save_both(p_flow, "fig1_strobe_flow", 7, 7)

# ==================================================================
# Fig 2: Sociodemographic panel
# ==================================================================
make_bar <- function(df, var, title, colors) {
  df |> 
    mutate(!!sym(var) := factor(as.character(!!sym(var)))) |> 
    count(!!sym(var)) |> 
    mutate(!!sym(var) := fct_reorder(!!sym(var), n)) |> 
    ggplot(aes(n, !!sym(var), fill = !!sym(var))) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.3, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = colors) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = NULL, title = title) +
    theme_pub + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())
}

p1 <- make_bar(primary, "gender_clean", "Sex", c(blues[3], blues[6]))
p2 <- make_bar(primary, "married", "Marital status", c(blues[2], blues[4], blues[6]))
p3 <- make_bar(primary, "employed", "Employment", c(blues[2], blues[5])) + 
  scale_y_discrete(labels = c("Not employed", "Employed"))
p4 <- make_bar(primary, "education_graduate", "Education", c(blues[3], blues[5]))
p5 <- make_bar(primary, "dx_group", "Diagnosis Group", c(blues[1], blues[3], blues[5]))

p_demo <- (p1 | p2) / (p3 | p4) / (p5 | plot_spacer()) + 
  plot_annotation(title = "Sociodemographic Profile (Primary Cohort, n = 78)") &
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
save_both(p_demo, "fig2_sociodemographic", 9, 8)

# ==================================================================
# Fig 3: Correlation heatmap
# ==================================================================
cm <- cor(primary |> select(dass_depression, dass_anxiety, dass_stress, dass_total, wemwbs, prolactin),
          use = "pairwise.complete.obs", method = "spearman")
labels <- c("Depression", "Anxiety", "Stress", "DASS total", "WEMWBS", "Prolactin")
dimnames(cm) <- list(labels, labels)

p_heat <- ggcorrplot(cm, type = "full", lab = TRUE, lab_size = 3.5,
                     colors = c("#08519C", "white", "#08306B"),
                     outline.color = "white",
                     title = "Spearman Correlations (n = 78)",
                     ggtheme = theme_minimal(base_size = 12)) +
  theme(plot.title = element_text(face = "bold"))
save_both(p_heat, "fig3_correlation_heatmap", 7, 6)

# ==================================================================
# Fig 4: Scatterplots
# ==================================================================
make_scatter <- function(yvar, ylab) {
  p <- ggplot(primary, aes(prolactin, .data[[yvar]])) +
    geom_point(alpha = 0.5, size = 1.8, colour = blues[3]) +
    geom_smooth(method = "lm", se = TRUE, colour = blues[1], fill = blues[6], alpha = 0.4) +
    labs(x = "Serum prolactin (ng/mL)", y = ylab) + theme_pub + theme(axis.title = element_text(size = 9))
  ggExtra::ggMarginal(p, type = "density", fill = blues[6], alpha = 0.8, size = 6)
}
sc <- list(
  make_scatter("dass_depression", "Depression (DASS-42)"),
  make_scatter("dass_anxiety",    "Anxiety (DASS-42)"),
  make_scatter("dass_stress",     "Stress (DASS-42)"),
  make_scatter("dass_total",      "DASS-42 total"),
  make_scatter("wemwbs",          "WEMWBS")
)
p_scatter <- wrap_plots(sc, ncol = 3) + plot_annotation(title = "Prolactin vs Psychological Outcomes")
save_both(p_scatter, "fig4_scatterplots", 12, 8)

# ==================================================================
# Fig 5: Forest plot
# ==================================================================
corr <- read_csv("output/tables/corr_primary_spearman.csv", show_col_types = FALSE)
forest_df <- corr |> mutate(label = recode(outcome, dass_depression="Depression", dass_anxiety="Anxiety", dass_stress="Stress", dass_total="DASS total", wemwbs="WEMWBS"),
                            label = factor(label, levels = label[order(rho)]))
p_forest <- ggplot(forest_df, aes(rho, label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.15, colour = blues[2], linewidth = 0.8) +
  geom_point(size = 3.5, colour = blues[1]) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)) +
  labs(x = expression(paste("Spearman ", rho, " (95% CI)")), y = NULL,
       title = "Prolactin vs Outcomes (BH-FDR corrected)") +
  theme_pub
save_both(p_forest, "fig5_forest", 7, 4)

# ==================================================================
# Fig 6: Prolactin quartiles
# ==================================================================
prim_q <- primary |> filter(!is.na(prolactin)) |>
  mutate(q = cut(prolactin, quantile(prolactin, 0:4/4), include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4")))
p_q1 <- ggplot(prim_q, aes(q, dass_total, fill = q)) + geom_boxplot(alpha=0.8, outlier.shape=21) + scale_fill_manual(values = blues[3:6], guide="none") + labs(x="Prolactin Quartile", y="DASS-42 Total", title="DASS-42 across Quartiles") + theme_pub
p_q2 <- ggplot(prim_q, aes(q, wemwbs, fill = q)) + geom_boxplot(alpha=0.8, outlier.shape=21) + scale_fill_manual(values = blues[3:6], guide="none") + labs(x="Prolactin Quartile", y="WEMWBS", title="WEMWBS across Quartiles") + theme_pub
save_both(p_q1 | p_q2, "fig6_prolactin_quartiles", 10, 4.5)

# ==================================================================
# Fig 7: Employment x WEMWBS
# ==================================================================
p_emp <- ggplot(primary, aes(employed, wemwbs, fill = employed)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5, colour = "black") +
  scale_fill_manual(values = c(blues[6], blues[2]), guide = "none") +
  scale_x_discrete(labels = c("Not employed", "Employed")) +
  labs(x = NULL, y = "WEMWBS (Well-being)",
       title = "Employment and Well-being",
       subtitle = "Hedges g = -0.89, p = 0.002 (robust)") +
  theme_pub
save_both(p_emp, "fig7_employment_wemwbs", 5, 4.5)

# ==================================================================
# Supplementary Figures (S1, S2)
# ==================================================================
p_prol <- ggplot(primary, aes(gender_clean, prolactin, fill = gender_clean)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5, colour = "black") +
  scale_fill_manual(values = c(blues[3], blues[6]), guide = "none") +
  labs(x = NULL, y = "Serum prolactin (ng/mL)", title = "Prolactin by Sex") +
  theme_pub
save_both(p_prol, "figS1_prolactin_by_sex", 5, 4)

qq_df <- primary |> select(prolactin, dass_depression, dass_anxiety, dass_stress, dass_total, wemwbs) |> pivot_longer(everything())
p_qq <- ggplot(qq_df, aes(sample = value)) +
  stat_qq(alpha = 0.6, size = 1.2, colour = blues[3]) +
  stat_qq_line(colour = blues[1], linewidth = 0.8) +
  facet_wrap(~name, scales = "free", ncol = 3) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plots") +
  theme_pub + theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))
save_both(p_qq, "figS2_qq_plots", 10, 6)

message("\nAll figures saved.\n")
