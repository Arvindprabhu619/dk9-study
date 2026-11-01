# R/02_descriptives.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)
})

clean <- readRDS("data/processed/clean_data.rds")
clean_primary <- clean |> filter(dx_group %in% c("Depressive", "Anxiety", "Stress-related"))

# ---- Helpers -------------------------------------------------------
fmt_msd  <- function(x, dig = 1) sprintf(paste0("%.", dig, "f (%.", dig, "f)"),
                                          mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_med  <- function(x) sprintf("%.1f [%.1f – %.1f]", median(x, na.rm = TRUE),
                                 quantile(x, .25, na.rm = TRUE), quantile(x, .75, na.rm = TRUE))
fmt_npct <- function(n, total) sprintf("%d (%.1f%%)", n, 100 * n / total)

# ---- Corrected employment definition -------------------------------
# Matches manuscript Fig 2:
#   Employed = Self Employed, Non-Govt/Pvt Sector, Skilled/Unskilled Labour, Professional
#   Not employed = Housewife, Unemployed, Retired*, Other
clean <- clean |>
  mutate(
    employed = !(occupation %in% c("Unemployed", "Housewife", "Other") |
                 str_detect(occupation, "^Retired"))
  )
clean_primary <- clean_primary |>
  mutate(
    employed = !(occupation %in% c("Unemployed", "Housewife", "Other") |
                 str_detect(occupation, "^Retired"))
  )

cat("Occupation audit (raw values -> employed/not):\n")
print(clean |> count(occupation, employed, sort = TRUE))

# ---- Table 1: Demographics ----------------------------------------
demo_vars <- c("gender_clean", "married", "employed", "education_graduate",
               "on_psychotropic", "substance_any", "thyroid_flag")
demo_labels <- c("Sex", "Marital status", "Employed", "Education",
                 "Psychotropic treatment", "Substance use", "Thyroid status")

df_list  <- c(list(clean), lapply(c("Depressive","Anxiety","Stress-related"),
              function(g) clean |> filter(dx_group == g)))
df_names <- c("Overall", "Depressive", "Anxiety", "Stress-related")

build_demo_row <- function(var, label) {
  levs <- clean[[var]] |> as.character() |> unique() |> na.omit() |> sort()
  map_dfr(levs, function(lv) {
    vals <- map_chr(df_list, function(d)
      fmt_npct(sum(d[[var]] == lv, na.rm = TRUE), nrow(d)))
    tibble(Variable = label, Level = lv, !!!setNames(as.list(vals), df_names))
  })
}

t1 <- bind_rows(
  tibble(Variable = "Age (years)", Level = "mean (SD)",
         Overall          = fmt_msd(clean$age),
         Depressive       = fmt_msd(clean$age[clean$dx_group == "Depressive"]),
         Anxiety          = fmt_msd(clean$age[clean$dx_group == "Anxiety"]),
         `Stress-related` = fmt_msd(clean$age[clean$dx_group == "Stress-related"])),
  map2_dfr(demo_vars, demo_labels, ~ build_demo_row(.x, .y))
)
write_csv(t1, "output/tables/table1_demographics.csv")
cat("\n===== TABLE 1: DEMOGRAPHICS =====\n"); print(as.data.frame(t1))

# ---- Table 2: Scales ----------------------------------------------
scale_vars <- c("dass_depression", "dass_anxiety", "dass_stress",
                "dass_total", "wemwbs", "prolactin")

t2 <- tibble(
  Variable                    = scale_vars,
  `All mean (SD)`             = map_chr(scale_vars, ~ fmt_msd(clean[[.x]])),
  `All median [IQR]`          = map_chr(scale_vars, ~ fmt_med(clean[[.x]])),
  `Primary mean (SD)`         = map_chr(scale_vars, ~ fmt_msd(clean_primary[[.x]])),
  `Primary median [IQR]`      = map_chr(scale_vars, ~ fmt_med(clean_primary[[.x]]))
)
write_csv(t2, "output/tables/table2_scales.csv")
cat("\n===== TABLE 2: SCALES =====\n"); print(as.data.frame(t2))

# ---- Table 2b: Scales by group ------------------------------------
scale_by_group <- function(v) {
  tibble(
    Variable         = v,
    Depressive       = fmt_msd(clean[[v]][clean$dx_group == "Depressive"]),
    Anxiety          = fmt_msd(clean[[v]][clean$dx_group == "Anxiety"]),
    `Stress-related` = fmt_msd(clean[[v]][clean$dx_group == "Stress-related"]),
    Excluded         = fmt_msd(clean[[v]][clean$dx_group == "Excluded"])
  )
}
t2b <- map_dfr(scale_vars, scale_by_group)
write_csv(t2b, "output/tables/table2_scales_by_group.csv")
cat("\n===== TABLE 2b: SCALES BY GROUP =====\n"); print(as.data.frame(t2b))

cat("\nPrimary cohort size:", nrow(clean_primary), "\n")
cat("Group sizes:\n"); print(table(clean_primary$dx_group))
