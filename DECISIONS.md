# Pre-specified analysis decisions

**Project:** Serum prolactin and psychological symptom severity in
depressive, anxiety and stress-related disorders — a cross-sectional study.

**Data freeze:** 2025-xx-xx (before any inferential statistics were run).
**Corresponding author sign-off:** pending.

All decisions below are codified in `R/01_clean.R`. The cleaned dataset
(`data/processed/clean_data.csv`) is the sole input to all downstream
analyses. Nothing was altered in Excel.

---

## D1. Diagnosis classification (PRIMARY DRIVER OF PAPER SCOPE)

Free-text `Current Psychiatric Diagnosis` was classified by pattern-matching
into four DSM-5-aligned groups:

| Raw diagnosis (case-insensitive) | Group |
|---|---|
| MDD, MDD with anxious distress, PDD/Dysthymia, Recurrent brief depression, PMDD, Depression due to another medical condition | **Depressive** |
| GAD, Panic disorder, Unspecified anxiety disorder, Anxiety disorder due to another medical condition | **Anxiety** |
| Adjustment disorder (incl. with anxiety) | **Stress-related** |
| "Under Observation" | **Excluded** |
| "Migraine headache", "K/C/O HTN" alone | **Excluded** |
| "Unspecified mood disorder" | **Other** (sensitivity only) |

**Counts (n=113):** Depressive 48 · Anxiety 21 · Stress-related 9 · Excluded 34 · Other 1.

**Primary cohort = 78.** The 34 "Under Observation" participants were
attending psychiatry OPD but had no documented DSM-5 diagnosis; including
them would misrepresent the cohort. This is disclosed in the manuscript.

**Sensitivity cohort S5 = 113** (all as submitted).

Full audit: `output/tables/dx_classification_audit.csv`.

---

## D2. Censored prolactin values

7 records reported as `<0.1` (n=6) or `<1.0` (n=1).

- **Primary:** substitute with LOD/√2 → 0.071 and 0.707 ng/mL.
- **Sensitivity (S2):** exclude all 7.

Rationale: standard approach for left-censored data when proportion
censored < 10%.

---

## D3. Hyperprolactinemia / outliers

Sex-specific upper reference (Melmed et al., JCEM 2011): >25 ng/mL (F),
>20 ng/mL (M). Applying after gender correction:

- Hyperprolactinemic: **n=7** (77.7, 55.6, 30.2, 26.6, 25.9, 25.7, 22.5)

- **Primary:** retain (pre-specified; not a post-hoc exclusion).
- **Sensitivity (S3):** exclude the 7.

---

## D4. Gender corrections

Two records had a name/gender conflict:
- pid 77  Dr Ravindra V Shinde — recorded Female → **corrected Male**
- pid 109 Mr Mrunal Patel     — recorded Female → **corrected Male**

Original preserved in `gender_as_recorded`. Audit file:
`output/tables/gender_audit.csv`. After correction: 74 F, 39 M; zero
remaining prefix/gender mismatches.

---

## D5. Creatinine anomaly

Row 58 (pid 58): creatinine = 6.8 mg/dL — severe renal-failure range,
which conflicts with the study exclusion criterion ("renal dysfunction").
Likely a typo (0.68) or an overlooked exclusion.

- **Primary:** retain (respects n=113 as submitted).
- **Sensitivity (S4):** exclude this participant.
- No analysis uses creatinine as a covariate.

---

## D6. Thyroid

14 participants with TSH > 4.5 (subclinical) and 1 with TSH = 34.1
(overt hypothyroid). Manuscript states thyroid dysfunction was an exclusion.
We retain all in primary (respecting submitted n=113), flag and run
sensitivity excluding the overt case (S5b).

---

## D7. Menopause

86.7% missing. Not usable as a covariate. Disclosed in limitations.

---

## D8. Statistical analysis plan

### Primary (confirmatory)
- **Question:** is serum prolactin associated with DASS-42 depression,
  anxiety, stress, total, and WEMWBS in DSM-5-diagnosed patients (n=78)?
- **Method:** Spearman's ρ (robust to non-normality; pre-specified to
  preserve 5-test family without power loss from normality testing).
- **Multiplicity:** Benjamini–Hochberg FDR, α = 0.05, family = 5 tests.
- **Report:** ρ, 95% CI (Fisher z), raw p, BH-adjusted p, n.

### Secondary (prespecified, uncorrected, exploratory)
- Sex differences: prolactin, DASS domains, WEMWBS.
- Marital status: prolactin, WEMWBS.
- Employment: prolactin, DASS total, WEMWBS.
- Psychotropic treatment: prolactin.
- Test choice: Welch t-test for approximately normal variables
  (per Shapiro-Wilk at α = 0.10 as gatekeeping threshold);
  Mann-Whitney U otherwise. Report Hedges g (with 95% CI) for
  parametric and rank-biserial r for non-parametric comparisons.

### Sensitivity analyses (report only, do not change conclusions)
| Code | Analysis |
|---|---|
| S1 | DSM-5 diagnosed (n=78) — **= primary** |
| S2 | Exclude censored prolactin (n=71) |
| S3 | Exclude hyperprolactinemic (n=71) |
| S4 | Exclude creatinine anomaly (n=77) |
| S5 | All 113 as submitted |
| S5b | Exclude overt hypothyroid (n=112) |

All sensitivity correlations re-computed with identical Spearman + BH
approach. A finding is declared *robust* if ρ retains the same sign and
non-significance (or significance) across all six analyses.

### Sample size justification
Recomputed post-hoc using `pwr::pwr.r.test` with reference effect from
Elgellaie 2021 (r ≈ 0.30 in MDD). Minimum detectable ρ at n=78 with
80% power and α = 0.05 (two-sided) is reported in
`output/tables/sample_size.xlsx`. This is reported as an achieved
precision statement, not as an a priori justification (the original
paper's calculation cannot be reproduced from any standard formula).

---

## D9. Figures

All figures generated by `R/06_figures.R`, saved as both PDF (vector)
and TIFF (LZW, 300 dpi). Colour palette: Okabe–Ito (colour-blind safe).
Fig 1 (STROBE), Fig 2 (sociodemographic panel), Fig 3 (correlation
heatmap with BH-adjusted p in cells), Fig 4 (scatterplots with loess
and marginal densities), Fig 5 (forest plot of ρ with 95% CI).

---

## D10. Reporting standard

STROBE (Strengthening the Reporting of Observational Studies in
Epidemiology) — checklist included as supplementary file.
