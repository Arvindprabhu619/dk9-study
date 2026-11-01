# R/07_null_rigor.R
# ------------------------------------------------------------------
# Strengthen the null:
#   1. TOST equivalence tests (SESOI rho = 0.20)
#   2. Bayes factors (BF01: evidence FOR the null)
#   3. Prolactin quartile analysis (checks for non-monotonic effects)
#
# All results are additional/exploratory and clearly labelled.
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
})

# Check/install specialized packages
for (p in c("BayesFactor", "TOSTER")) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
}

clean   <- readRDS("data/processed/clean_data.rds")
primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

outcomes <- c("dass_depression","dass_anxiety","dass_stress",
              "dass_total","wemwbs")

# ==================================================================
# 1. TOST equivalence test (Fisher-z implementation)
# ==================================================================
# SESOI = |rho| = 0.20 (small effect). We test whether the observed
# 95% CI falls inside [-0.20, +0.20]. Two one-sided tests on Fisher z.

tost_spearman <- function(rho, n, sesoi = 0.20, alpha = 0.05) {
  z     <- atanh(rho)
  se    <- 1 / sqrt(n - 3)
  z_d   <- atanh(sesoi)
  # H0_lower: rho <= -sesoi  vs  H1: rho > -sesoi
  p_low  <- 1 - pnorm((z + z_d) / se)
  # H0_upper: rho >= +sesoi  vs  H1: rho < +sesoi
  p_up   <- pnorm((z - z_d) / se)
  p_tost <- max(p_low, p_up)
  tibble(rho = rho, n = n, sesoi = sesoi,
         p_low = p_low, p_up = p_up, p_tost = p_tost,
         equivalent = p_tost < alpha)
}

tost_res <- map_dfr(outcomes, function(y) {
  x  <- primary$prolactin; yv <- primary[[y]]
  ok <- !is.na(x) & !is.na(yv)
  ct <- suppressWarnings(cor.test(x[ok], yv[ok],
                                  method = "spearman", exact = FALSE))
  tost_spearman(unname(ct$estimate), sum(ok)) |> mutate(outcome = y)
}) |> select(outcome, everything())

write_csv(tost_res, "output/tables/tost_equivalence.csv")
cat("==================================================\n")
cat(" TOST EQUIVALENCE (SESOI |rho| = 0.20)\n")
cat("==================================================\n")
print(as.data.frame(tost_res), digits = 3)
cat("\n")

# ==================================================================
# 2. Bayes factors (evidence FOR the null)
# ==================================================================
# Rank-transform to remove non-normality (Bayesian Spearman equivalent).
# BF10 > 1 means evidence for H1 (association).
# BF01 = 1/BF10  →  evidence for H0 (null). Higher = stronger null.

compute_bf <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x  <- rank(x[ok], ties.method = "average")
  y  <- rank(y[ok], ties.method = "average")
  bf10 <- tryCatch(
    BayesFactor::extractBF(
      BayesFactor::correlationBF(x, y, rscale = "medium"),
      onlybf = TRUE
    ),
    error = function(e) NA_real_
  )
  bf10 <- as.numeric(bf10)
  bf01 <- 1 / bf10
  tibble(bf10 = bf10, bf01 = bf01,
         interpretation = case_when(
           is.na(bf01)      ~ "BF not computed",
           bf01 > 10        ~ "Strong evidence FOR null",
           bf01 > 3         ~ "Moderate evidence FOR null",
           bf01 > 1         ~ "Weak evidence FOR null",
           TRUE             ~ "Evidence favours H1"))
}

bf_res <- map_dfr(outcomes, function(y) {
  compute_bf(primary$prolactin, primary[[y]]) |> mutate(outcome = y)
}) |> select(outcome, everything())

write_csv(bf_res, "output/tables/bayes_factors.csv")
cat("==================================================\n")
cat(" BAYES FACTORS (rank-transformed; evidence for null)\n")
cat("==================================================\n")
print(as.data.frame(bf_res), digits = 3)
cat("\n")

# ==================================================================
# 3. Prolactin quartile analysis (tests for non-monotonic effects)
# ==================================================================
primary_q <- primary |>
  filter(!is.na(prolactin)) |>
  mutate(prolactin_q = cut(prolactin,
                           breaks = quantile(prolactin, probs = 0:4/4),
                           include.lowest = TRUE,
                           labels = c("Q1 (low)","Q2","Q3","Q4 (high)")))

quartile_summary <- primary_q |>
  group_by(prolactin_q) |>
  summarise(n = n(),
            prolactin_range = sprintf("%.1f–%.1f",
                                      min(prolactin), max(prolactin)),
            dass_depression = median(dass_depression),
            dass_anxiety    = median(dass_anxiety),
            dass_stress     = median(dass_stress),
            dass_total      = median(dass_total),
            wemwbs          = median(wemwbs),
            .groups = "drop")

cat("==================================================\n")
cat(" PROLACTIN QUARTILES (medians of outcomes)\n")
cat("==================================================\n")
print(as.data.frame(quartile_summary), digits = 3)
cat("\n")

# Kruskal-Wallis test (does any outcome differ across quartiles?)
kw_res <- map_dfr(outcomes, function(y) {
  kt <- kruskal.test(primary_q[[y]] ~ primary_q$prolactin_q)
  tibble(outcome = y, chi_sq = round(unname(kt$statistic), 3),
         df = unname(kt$parameter), p = kt$p.value)
})

write_csv(kw_res, "output/tables/prolactin_quartile_kruskal.csv")
write_csv(quartile_summary, "output/tables/prolactin_quartile_summary.csv")
cat("Kruskal-Wallis across quartiles:\n")
print(as.data.frame(kw_res), digits = 3)
cat("\n")
