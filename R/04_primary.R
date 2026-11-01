# R/04_primary.R
# ------------------------------------------------------------------
# Primary: Spearman correlations of prolactin with DASS-42 domains,
# total, and WEMWBS in the primary cohort (n=78), BH-FDR corrected.
#
# Secondary: prespecified group comparisons, test chosen per the
# normality_decision table produced by 03_normality.R.
#
# Produces:
#   output/tables/corr_primary_spearman.csv
#   output/tables/secondary_comparisons.csv
#   output/tables/secondary_comparisons_wide.csv   (for manuscript table)
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(effsize)
})

clean   <- readRDS("data/processed/clean_data.rds")
primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

stopifnot(nrow(primary) == 78)

# ==================================================================
# 1. PRIMARY ANALYSIS — Spearman correlations (n=78)
# ==================================================================
corr_one <- function(y) {
  x  <- primary$prolactin
  yv <- primary[[y]]
  ok <- !is.na(x) & !is.na(yv)
  x  <- x[ok]; yv <- yv[ok]; n <- length(x)

  ct  <- suppressWarnings(cor.test(x, yv, method = "spearman", exact = FALSE))
  rho <- unname(ct$estimate)

  z  <- atanh(rho); se <- 1 / sqrt(n - 3)
  lo <- tanh(z - 1.96*se); hi <- tanh(z + 1.96*se)

  tibble(outcome = y, n = n, rho = rho, ci_lo = lo, ci_hi = hi, p = ct$p.value)
}

corr_primary <- map_dfr(
  c("dass_depression","dass_anxiety","dass_stress","dass_total","wemwbs"),
  corr_one
) |>
  mutate(p_adj_BH = p.adjust(p, "BH"),
         sig_adj  = p_adj_BH < 0.05,
         interpretation = if_else(sig_adj,
                                  "Significant after FDR",
                                  "Not significant after FDR"))

write_csv(corr_primary, "output/tables/corr_primary_spearman.csv")
cat("==================================================\n")
cat(" PRIMARY: SPEARMAN ρ (prolactin vs outcomes, n=78)\n")
cat(" BH-FDR across 5 tests\n")
cat("==================================================\n")
print(as.data.frame(corr_primary), digits = 3)
cat("\n")

# ==================================================================
# 2. SECONDARY — group comparisons
# ==================================================================

# ---- Helper: Mann-Whitney with rank-biserial effect size ----------
mw_compare <- function(group_var, value_var) {
  d <- primary |>
    filter(!is.na(.data[[group_var]]), !is.na(.data[[value_var]]))
  d[[group_var]] <- factor(d[[group_var]])
  levs <- levels(d[[group_var]])
  if (length(levs) != 2) return(NULL)

  x <- d[[value_var]][d[[group_var]] == levs[1]]
  y <- d[[value_var]][d[[group_var]] == levs[2]]

  wt   <- suppressWarnings(wilcox.test(x, y, exact = FALSE))
  U    <- unname(wt$statistic)
  n1   <- length(x); n2 <- length(y)
  r_rb <- 1 - 2 * U / (n1 * n2)   # >0 means group2 higher

  tibble(
    group_var   = group_var, value_var = value_var,
    level1      = levs[1], level2      = levs[2],
    median1     = round(median(x), 2), median2 = round(median(y), 2),
    n1          = n1, n2 = n2,
    statistic   = round(U, 2),
    effect_size = round(r_rb, 3),   # rank-biserial r
    effect_name = "rank-biserial r",
    p           = wt$p.value,
    test        = "Mann-Whitney U"
  )
}

# ---- Helper: Welch t with Hedges g -------------------------------
welch_compare <- function(group_var, value_var) {
  d <- primary |>
    filter(!is.na(.data[[group_var]]), !is.na(.data[[value_var]]))
  d[[group_var]] <- factor(d[[group_var]])
  levs <- levels(d[[group_var]])
  if (length(levs) != 2) return(NULL)

  x <- d[[value_var]][d[[group_var]] == levs[1]]
  y <- d[[value_var]][d[[group_var]] == levs[2]]

  tt <- t.test(x, y)
  d_eff <- suppressWarnings(
    cohen.d(x, y, pooled = TRUE, hedges.correction = TRUE)
  )

  tibble(
    group_var   = group_var, value_var = value_var,
    level1      = levs[1], level2      = levs[2],
    median1     = round(mean(x), 2), median2 = round(mean(y), 2),
    n1          = length(x), n2 = length(y),
    statistic   = round(unname(tt$statistic), 2),
    effect_size = round(d_eff$estimate, 3),
    effect_name = "Hedges g",
    p           = tt$p.value,
    test        = "Welch t-test"
  )
}

# ---- Spec table (uses normality_decision.csv) --------------------
# decision: prolactin, dass_depression, dass_total → non-parametric
#           dass_anxiety, dass_stress, wemwbs    → parametric
secondary_spec <- tribble(
  ~group_var,        ~value_var,        ~test,
  "gender_clean",    "prolactin",       "mw",
  "gender_clean",    "dass_depression", "mw",
  "gender_clean",    "dass_anxiety",    "welch",
  "gender_clean",    "dass_stress",     "welch",
  "gender_clean",    "dass_total",      "mw",
  "gender_clean",    "wemwbs",          "welch",
  "married_binary",  "prolactin",       "mw",
  "married_binary",  "wemwbs",          "welch",
  "employed",        "prolactin",       "mw",
  "employed",        "dass_total",      "mw",
  "employed",        "wemwbs",          "welch",
  "on_psychotropic", "prolactin",       "mw"
)

secondary <- secondary_spec |>
  pmap_dfr(function(group_var, value_var, test) {
    if (test == "mw") mw_compare(group_var, value_var)
    else              welch_compare(group_var, value_var)
  })

write_csv(secondary, "output/tables/secondary_comparisons.csv")
cat("==================================================\n")
cat(" SECONDARY COMPARISONS (n varies 73–78 by missingness)\n")
cat("==================================================\n")
print(as.data.frame(secondary), digits = 3)
cat("\n")

# ---- Wide table for manuscript -----------------------------------
secondary_wide <- secondary |>
  transmute(comparison = paste0(group_var, ": ", value_var),
            group1 = level1, group2 = level2,
            summary = paste0(level1, " ", median1, " vs ",
                             level2, " ", median2),
            effect_size = paste0(effect_name, " = ", effect_size),
            test, p = round(p, 4))
write_csv(secondary_wide, "output/tables/secondary_comparisons_wide.csv")

# ==================================================================
# 3. Console summary
# ==================================================================
cat("==================================================\n")
cat(" BOTTOM LINE\n")
cat("==================================================\n")
cat("Primary Spearman correlations (BH-adjusted):\n")
print(corr_primary |> select(outcome, rho, p, p_adj_BH, sig_adj),
      digits = 3)
cat("\nAny primary outcome significant after FDR:",
    any(corr_primary$sig_adj), "\n")
cat("Any secondary comparison significant at p<0.05:",
    any(secondary$p < 0.05, na.rm = TRUE), "\n")
cat("  Significant secondary comparisons:\n")
secondary |>
  filter(p < 0.05) |>
  select(group_var, value_var, test, effect_size, p) |>
  print(digits = 3)
