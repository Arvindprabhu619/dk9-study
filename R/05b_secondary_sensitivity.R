# R/05b_secondary_sensitivity.R
# Re-tests the significant secondary findings under sensitivity sets.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(purrr)
})

clean   <- readRDS("data/processed/clean_data.rds")
primary <- clean |> filter(dx_group %in% c("Depressive","Anxiety","Stress-related"))

mw <- function(df, gvar, yvar) {
  d <- df |> filter(!is.na(.data[[gvar]]), !is.na(.data[[yvar]]))
  d[[gvar]] <- factor(d[[gvar]])
  if (length(levels(d[[gvar]])) != 2) return(NULL)
  wt <- suppressWarnings(wilcox.test(d[[yvar]] ~ d[[gvar]], exact = FALSE))
  levs <- levels(d[[gvar]])
  n1 <- sum(d[[gvar]] == levs[1]); n2 <- sum(d[[gvar]] == levs[2])
  U  <- unname(wt$statistic)
  tibble(
    var1 = levs[1], var2 = levs[2],
    med1 = round(median(d[[yvar]][d[[gvar]] == levs[1]]), 2),
    med2 = round(median(d[[yvar]][d[[gvar]] == levs[2]]), 2),
    r_rb = round(1 - 2*U/(n1*n2), 3), p = wt$p.value
  )
}

welch <- function(df, gvar, yvar) {
  d <- df |> filter(!is.na(.data[[gvar]]), !is.na(.data[[yvar]]))
  d[[gvar]] <- factor(d[[gvar]])
  if (length(levels(d[[gvar]])) != 2) return(NULL)
  levs <- levels(d[[gvar]])
  tt <- t.test(d[[yvar]] ~ d[[gvar]])
  tibble(
    var1 = levs[1], var2 = levs[2],
    med1 = round(mean(d[[yvar]][d[[gvar]] == levs[1]]), 2),
    med2 = round(mean(d[[yvar]][d[[gvar]] == levs[2]]), 2),
    r_rb = round(unname(tt$statistic), 3), p = tt$p.value
  )
}

sets <- list(
  `S1: Primary`          = primary,
  `S2: Excl censored`    = primary |> filter(!prolactin_censored),
  `S3: Excl hyperprolact`= primary |> filter(!hyperprolactinemia),
  `S5: All 113`          = clean
)

check_secondary <- function(label, df) {
  tibble(analysis = label) |>
    bind_cols(bind_rows(
      mw(df,    "employed",     "prolactin") |> mutate(finding = "Employed x Prolactin"),
      mw(df,    "employed",     "dass_total") |> mutate(finding = "Employed x DASS total"),
      welch(df, "employed",     "wemwbs")    |> mutate(finding = "Employed x WEMWBS"),
      mw(df,    "gender_clean", "prolactin") |> mutate(finding = "Sex x Prolactin")
    ))
}

# imap passes (value, name) — swap so label = name, df = value
res <- imap_dfr(sets, ~ check_secondary(.y, .x)) |>
  select(analysis, finding, var1, var2, med1, med2, r_rb, p) |>
  mutate(sig_05 = p < 0.05)

write_csv(res, "output/tables/secondary_sensitivity.csv")
cat("===== SECONDARY FINDINGS UNDER SENSITIVITY =====\n")
print(as.data.frame(res), digits = 3)

cat("\n===== ROBUSTNESS VERDICT =====\n")
verdict <- res |>
  group_by(finding) |>
  summarise(
    sets_sig = sum(sig_05),
    sets_total = n(),
    direction_consistent = length(unique(sign(r_rb))) == 1,
    .groups = "drop"
  ) |>
  mutate(
    verdict = case_when(
      sets_sig == sets_total & direction_consistent ~ "ROBUST (significant everywhere, same direction)",
      sets_sig == 0                                  ~ "NULL (not significant anywhere)",
      sets_sig >= 3 & direction_consistent           ~ "Fragile (significant in majority)",
      TRUE                                           ~ "INCONSISTENT — do not report"
    )
  )
print(as.data.frame(verdict))
