# R/03_normality.R
# ------------------------------------------------------------------
# Produces:
#   output/tables/shapiro_wilk_overall.csv
#   output/tables/shapiro_wilk_by_sex.csv
#   output/tables/normality_decision.csv
#   output/figures/qq_plots.pdf / .tiff
#   output/figures/prolactin_by_sex.pdf / .tiff
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)
})

clean <- readRDS("data/processed/clean_data.rds")
clean_primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

vars <- c("prolactin", "dass_depression", "dass_anxiety", "dass_stress",
          "dass_total", "wemwbs")

# ---- Overall Shapiro-Wilk -----------------------------------------
shap_overall <- purrr::map_dfr(vars, function(v) {
  x <- clean_primary[[v]]; x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 3)
    return(tibble(variable = v, n = length(x), W = NA, p = NA,
                  interpretation = "Too few unique values"))
  s <- shapiro.test(x)
  tibble(variable = v, n = length(x), W = s$statistic, p = s$p.value,
         interpretation = if_else(s$p.value > 0.05, "Approx normal (p>0.05)",
                                  "Non-normal (p<=0.05)"))
})

# ---- Shapiro-Wilk by sex ------------------------------------------
shap_by_sex <- purrr::map_dfr(vars, function(v) {
  purrr::map_dfr(c("Female","Male"), function(sx) {
    x <- clean_primary |> filter(gender_clean == sx) |> pull(!!sym(v))
    x <- x[!is.na(x)]
    if (length(x) < 3 || length(unique(x)) < 3)
      return(tibble(variable = v, sex = sx, n = length(x), W = NA, p = NA))
    s <- shapiro.test(x)
    tibble(variable = v, sex = sx, n = length(x), W = s$statistic, p = s$p.value)
  })
})

write_csv(shap_overall, "output/tables/shapiro_wilk_overall.csv")
write_csv(shap_by_sex, "output/tables/shapiro_wilk_by_sex.csv")

cat("===== SHAPIRO-WILK (primary cohort, n=78) =====\n")
print(as.data.frame(shap_overall))

# ---- Decision log --------------------------------------------------
decisions <- shap_overall |>
  transmute(variable,
            normal_overall = p > 0.05,
            decision = if_else(p > 0.05,
                               "Parametric OK (Pearson / Welch t)",
                               "Use non-parametric (Spearman / Mann-Whitney)"))
write_csv(decisions, "output/tables/normality_decision.csv")

cat("\n===== NORMALITY DECISIONS (pre-specified gate) =====\n")
print(as.data.frame(decisions))

# ---- QQ plots ------------------------------------------------------
qq_df <- clean_primary |>
  select(all_of(vars)) |>
  pivot_longer(everything())

p_qq <- ggplot(qq_df, aes(sample = value)) +
  stat_qq(alpha = 0.55, size = 1.3) +
  stat_qq_line(colour = "#D55E00", linewidth = 0.7) +
  facet_wrap(~name, scales = "free", ncol = 3) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles",
       title = "Normal Q–Q plots — primary cohort (n=78)") +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave("output/figures/qq_plots.pdf",  p_qq, width = 11, height = 7, dpi = 300)
ggsave("output/figures/qq_plots.tiff", p_qq, width = 11, height = 7,
       dpi = 300, compression = "lzw")

# ---- Prolactin by sex ----------------------------------------------
p_prol <- ggplot(clean_primary, aes(gender_clean, prolactin, fill = gender_clean)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 21, width = 0.55) +
  geom_jitter(width = 0.12, alpha = 0.55, size = 1.3) +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), guide = "none") +
  labs(x = NULL, y = "Serum prolactin (ng/mL)",
       title = "Prolactin by sex — primary cohort") +
  theme_classic(base_size = 11)

ggsave("output/figures/prolactin_by_sex.pdf",  p_prol, width = 5, height = 4, dpi = 300)
ggsave("output/figures/prolactin_by_sex.tiff", p_prol, width = 5, height = 4,
       dpi = 300, compression = "lzw")

cat("\nFigures written:\n")
cat("  output/figures/qq_plots.pdf\n")
cat("  output/figures/qq_plots.tiff\n")
cat("  output/figures/prolactin_by_sex.pdf\n")
cat("  output/figures/prolactin_by_sex.tiff\n")
