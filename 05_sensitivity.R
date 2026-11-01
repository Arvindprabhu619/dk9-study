# R/05_sensitivity.R
# ------------------------------------------------------------------
# Runs the primary Spearman analysis across all six prespecified
# sensitivity sets. Confirms the null is robust (or not).
#
# Produces:
#   output/tables/sensitivity_correlations.csv
#   output/tables/sensitivity_summary.csv
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
})

clean <- readRDS("data/processed/clean_data.rds")

run_spear <- function(df, label) {
  outcomes <- c("dass_depression","dass_anxiety","dass_stress",
                "dass_total","wemwbs")
  out <- map_dfr(outcomes, function(y) {
    x  <- df$prolactin; yv <- df[[y]]
    ok <- !is.na(x) & !is.na(yv)
    if (sum(ok) < 10) return(NULL)
    ct <- suppressWarnings(cor.test(x[ok], yv[ok], method = "spearman",
                                    exact = FALSE))
    tibble(analysis = label, outcome = y, n = sum(ok),
           rho = unname(ct$estimate), p = ct$p.value)
  })
  out |> mutate(p_adj_BH = p.adjust(p, "BH"),
                sig_adj  = p_adj_BH < 0.05)
}

primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

sens <- bind_rows(
  run_spear(primary,                                       "S1: Primary n=78"),
  run_spear(primary |> filter(!prolactin_censored),        "S2: Excl censored"),
  run_spear(primary |> filter(!hyperprolactinemia),        "S3: Excl hyperprolactinemic"),
  run_spear(primary |> filter(pid != 58),                  "S4: Excl creatinine 6.8"),
  run_spear(clean,                                         "S5: All 113"),
  run_spear(clean |> filter(thyroid_flag != "Overt hypothyroid (TSH>10)"),
                                                          "S5b: Excl overt hypothyroid")
)

write_csv(sens, "output/tables/sensitivity_correlations.csv")

cat("==================================================\n")
cat(" SENSITIVITY ANALYSES\n")
cat("==================================================\n")
print(as.data.frame(sens |>
        select(analysis, outcome, n, rho, p_adj_BH, sig_adj) |>
        arrange(outcome, analysis)), digits = 3)

# ---- Summary -------------------------------------------------------
summ <- sens |>
  group_by(analysis) |>
  summarise(n_cases = first(n),
            rho_range = sprintf("[%.3f, %.3f]", min(rho), max(rho)),
            any_sig = any(sig_adj), .groups = "drop")
write_csv(summ, "output/tables/sensitivity_summary.csv")

cat("\n===== SUMMARY BY SET =====\n")
print(as.data.frame(summ))

cat("\nTotal BH-significant results across all 6 sets × 5 outcomes:",
    sum(sens$sig_adj), "of", nrow(sens), "\n")
if (sum(sens$sig_adj) == 0) {
  cat("\n→ NULL FINDING ROBUST across all sensitivity sets.\n")
  cat("  This is the key result for the paper: no sensitivity analysis\n")
  cat("  shows a prolactin-psychological-symptom association.\n")
} else {
  cat("\n→ WARNING: some sensitivity analyses show significance:\n")
  print(sens |> filter(sig_adj) |> as.data.frame())
}
