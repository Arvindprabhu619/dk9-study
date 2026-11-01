# R/01_clean.R
# ------------------------------------------------------------------
# Reads raw Excel (n=113 as submitted), applies documented cleaning,
# writes:
#   - data/processed/clean_data.csv
#   - data/processed/clean_data.rds
#   - output/tables/dx_classification_audit.csv
#   - output/tables/gender_audit.csv
#
# All decisions are mirrored in DECISIONS.md.
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl); library(janitor); library(dplyr); library(tidyr)
  library(stringr); library(readr)
})

# ---- 1. Read raw data ---------------------------------------------
raw <- read_excel("data/raw/original_n113.xlsx", sheet = "Sheet1", skip = 1)
raw <- clean_names(raw)

cat("Rows read:", nrow(raw), "\n")
cat("Columns read:", ncol(raw), "\n\n")

# ---- 2. Diagnosis classifier --------------------------------------
classify_dx <- function(x) {
  x <- str_to_lower(str_trim(x))
  dplyr::case_when(
    is.na(x)                                                         ~ "Excluded",
    str_detect(x, "^under observation")                              ~ "Excluded",
    str_detect(x, "^migraine")                                       ~ "Excluded",
    str_detect(x, "^k/c/o htn\\s*$")                                 ~ "Excluded",
    str_detect(x, "adjustment|asjustment")                           ~ "Stress-related",
    str_detect(x, "mdd|persistant depressive|persistent depressive|recurrent brief|premenstrual|depression due to") ~ "Depressive",
    str_detect(x, "genarilized anxiety|generalized anxiety|panic|unspecified anxiety|anxiety disorder due to") ~ "Anxiety",
    str_detect(x, "unspecified mood")                                ~ "Other",
    TRUE                                                             ~ "Other"
  )
}

# ---- 3. Prolactin parser ------------------------------------------
parse_prolactin <- function(x) {
  x_chr <- as.character(x)
  dplyr::case_when(
    x_chr == "<0.1" ~ 0.1 / sqrt(2),
    x_chr == "<1.0" ~ 1.0 / sqrt(2),
    TRUE            ~ suppressWarnings(as.numeric(x_chr))
  )
}

# ---- 4. Gender correction (DECISIONS.md D4) -----------------------
# Two records have gendered names that contradict the recorded value.
# Manually reviewed; correction applied. Original kept for audit.
gender_corrections <- tibble::tribble(
  ~pattern,               ~corrected,
  "Dr Ravindra V Shinde", "Male",
  "Mr Mrunal Patel",      "Male"
)

apply_gender_correction <- function(name, recorded) {
  out    <- as.character(recorded)
  reason <- rep(NA_character_, length(name))
  for (i in seq_len(nrow(gender_corrections))) {
    idx <- which(str_detect(name, fixed(gender_corrections$pattern[i])))
    if (length(idx) > 0) {
      out[idx]    <- gender_corrections$corrected[i]
      reason[idx] <- sprintf("Recorded as %s; corrected based on name",
                             recorded[idx])
    }
  }
  list(gender = out, reason = reason)
}

gc <- apply_gender_correction(raw$name, raw$gender)

# ---- 5. Diagnosis audit -------------------------------------------
audit_dx <- raw |>
  transmute(pid = row_number(),
            name = name,
            raw_diagnosis = current_psychiatric_diagnosis,
            dx_group = classify_dx(current_psychiatric_diagnosis))
write_csv(audit_dx, "output/tables/dx_classification_audit.csv")

cat("---- Diagnosis classification ----\n")
print(table(audit_dx$dx_group, useNA = "always"))
cat("\n---- Diagnoses mapped to 'Other' or 'Excluded' (verify these) ----\n")
audit_dx |>
  filter(dx_group %in% c("Other", "Excluded")) |>
  count(raw_diagnosis, dx_group, sort = TRUE) |>
  print(n = 50)
cat("\n")

# ---- 6. Build cleaned dataset -------------------------------------
clean <- raw |>
  mutate(
    pid                 = row_number(),
    age                 = as.numeric(age),

    prolactin_raw       = as.character(serum_prolactin),
    prolactin           = parse_prolactin(serum_prolactin),
    prolactin_censored  = prolactin_raw %in% c("<0.1", "<1.0"),

    dass_depression     = as.numeric(depression),
    dass_anxiety        = as.numeric(anxiety),
    dass_stress         = as.numeric(stress),
    dass_total          = dass_depression + dass_anxiety + dass_stress,
    wemwbs              = as.numeric(wemwbs),

    gender_as_recorded  = gender,
    gender_correction   = gc$reason,
    gender_clean        = factor(gc$gender, levels = c("Female", "Male")),

    married             = case_when(
      marital_status == "Married"   ~ "Married",
      marital_status == "Unmarried" ~ "Unmarried",
      marital_status == "Widowed"   ~ "Widowed",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Married", "Unmarried", "Widowed")),
    married_binary      = case_when(
      married == "Married" ~ "Married",
      is.na(married)       ~ NA_character_,
      TRUE                 ~ "Not married"
    ),

    employed            = !occupation %in% c("Unemployed", "Housewife",
                                             "Retired", "Retired- wrestler"),

    education_graduate  = case_when(
      str_detect(highest_education, "Graduation|PG Degree|Professional") ~ "Graduate+",
      is.na(highest_education)                                            ~ NA_character_,
      TRUE                                                                ~ "Below graduate"
    ),

    dx_group            = classify_dx(current_psychiatric_diagnosis) |>
                            factor(levels = c("Depressive", "Anxiety",
                                              "Stress-related", "Excluded",
                                              "Other")),

    on_psychotropic     = case_when(
      str_starts(treatment_psychotropics, "Y") ~ "On psychotropic",
      treatment_psychotropics == "N"           ~ "Not on psychotropic",
      TRUE                                     ~ NA_character_
    ),

    substance_any       = case_when(
      is.na(substance_use_in_past_12_months)  ~ "None documented",
      substance_use_in_past_12_months == "NA" ~ "None documented",
      TRUE                                    ~ "Use reported"
    ),

    tsh                 = as.numeric(tsh),
    thyroid_flag        = case_when(
      is.na(tsh)  ~ NA_character_,
      tsh > 10    ~ "Overt hypothyroid (TSH>10)",
      tsh > 4.5   ~ "Subclinical hypothyroid",
      tsh < 0.1   ~ "Hyperthyroid",
      TRUE        ~ "Euthyroid"
    ),

    hyperprolactinemia  = case_when(
      gender_clean == "Female" & prolactin > 25 ~ TRUE,
      gender_clean == "Male"   & prolactin > 20 ~ TRUE,
      TRUE                                       ~ FALSE
    ),

    blood_urea          = as.numeric(blood_urea),
    blood_urea_flag     = !is.na(blood_urea) & blood_urea < 10
  ) |>
  select(pid, name, age,
         gender_as_recorded, gender_clean, gender_correction,
         married, married_binary, occupation, employed, education_graduate,
         dx_group,
         dass_depression, dass_anxiety, dass_stress, dass_total, wemwbs,
         prolactin, prolactin_raw, prolactin_censored, hyperprolactinemia,
         on_psychotropic, substance_any,
         tsh, thyroid_flag, blood_urea, blood_urea_flag)

# ---- 7. Gender audit ----------------------------------------------
audit_gender <- clean |>
  mutate(
    prefix = str_extract(name, "^(Mr|Mrs|Miss|Ms|Dr|Me)\\b"),
    prefix_implied_gender = case_when(
      prefix == "Mr"                   ~ "Male",
      prefix %in% c("Mrs","Miss","Ms") ~ "Female",
      TRUE                             ~ NA_character_
    ),
    mismatch = !is.na(prefix_implied_gender) &
               prefix_implied_gender != as.character(gender_clean)
  ) |>
  select(pid, name, gender_as_recorded, gender_clean, gender_correction,
         prefix, prefix_implied_gender, mismatch)
write_csv(audit_gender, "output/tables/gender_audit.csv")

# ---- 8. Save -------------------------------------------------------
write_csv(clean, "data/processed/clean_data.csv")
saveRDS(clean, "data/processed/clean_data.rds")

# ---- 9. Summary ----------------------------------------------------
cat("===== CLEANING SUMMARY =====\n")
cat("N rows:", nrow(clean), "\n\n")

cat("Gender corrections applied:\n")
print(clean |> filter(!is.na(gender_correction)) |>
        select(pid, name, gender_as_recorded, gender_clean, gender_correction))

cat("\nGender distribution (corrected):\n")
print(table(clean$gender_clean, useNA = "always"))

cat("\nRemaining prefix/gender mismatches:\n")
print(audit_gender |> filter(mismatch) |> select(pid, name, gender_clean, prefix))
cat(" (mismatch count:", sum(audit_gender$mismatch), ")\n\n")

cat("Prolactin summary (after LOD/sqrt(2)):\n")
print(summary(clean$prolactin))
cat("  Censored:", sum(clean$prolactin_censored),
    " | Hyperprolactinemic:", sum(clean$hyperprolactinemia), "\n\n")

cat("DSM-5 diagnosis group:\n")
print(table(clean$dx_group, useNA = "always"))
cat("Primary cohort (excludes 'Excluded'):",
    sum(clean$dx_group %in% c("Depressive","Anxiety","Stress-related")), "\n\n")

cat("Thyroid status:\n"); print(table(clean$thyroid_flag, useNA = "always"))
cat("\nPsychotropic treatment:\n"); print(table(clean$on_psychotropic, useNA = "always"))
cat("\nSubstance use:\n"); print(table(clean$substance_any, useNA = "always"))
cat("\nBlood urea flags:", sum(clean$blood_urea_flag), "\n\n")
cat("Scales summary:\n")
print(summary(clean[, c("dass_depression","dass_anxiety","dass_stress",
                        "dass_total","wemwbs")]))
