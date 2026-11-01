# R/02b_primary_numbers.R
suppressPackageStartupMessages({library(dplyr); library(readr)})

clean   <- readRDS("data/processed/clean_data.rds")
primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

fmt_msd <- function(x) sprintf("%.1f ± %.1f", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
fmt_np  <- function(n, tot) sprintf("%d (%.1f%%)", n, 100*n/tot)

cat("N =", nrow(primary), "\n")
cat("Age:", fmt_msd(primary$age), "| median:",
    round(median(primary$age),1), "[",
    round(quantile(primary$age,.25),1), "–",
    round(quantile(primary$age,.75),1), "]\n\n")

for (v in c("gender_clean","married","employed","education_graduate",
            "on_psychotropic","substance_any","thyroid_flag","dx_group")) {
  cat("\n", v, ":\n")
  primary |> count(.data[[v]]) |>
    mutate(fmt = fmt_np(n, nrow(primary))) |> print()
}

cat("\nScales:\n")
for (v in c("dass_depression","dass_anxiety","dass_stress",
            "dass_total","wemwbs","prolactin")) {
  cat(sprintf("  %-16s %s | median %.1f [%.1f–%.1f]\n",
              v, fmt_msd(primary[[v]]),
              median(primary[[v]], na.rm=TRUE),
              quantile(primary[[v]],.25,na.rm=TRUE),
              quantile(primary[[v]],.75,na.rm=TRUE)))
}
