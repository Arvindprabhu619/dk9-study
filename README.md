cat > README.md << 'EOF'
# Serum Prolactin & Mental Well-being Study (DK9)

Reproducible analysis code for the manuscript: *"Serum prolactin is not a biomarker for symptom severity or mental well-being in DSM-5-diagnosed depressive, anxiety and stress-related disorders."*

## Data & Cohort
- **Primary Cohort:** n = 78 (DSM-5 diagnosed patients)
- **Sensitivity Cohort:** n = 113 (all enrolled patients)

## Key Findings
- **Null Finding:** Prolactin showed no association with DASS-42 or WEMWBS scores (proven via TOST equivalence and Bayesian analysis).
- **Robust Finding:** Employment status strongly predicts mental well-being (Hedges *g* = −0.89, *p* = 0.002).

## Reproducibility
To reproduce the analysis from scratch:
```R
Rscript -e 'renv::restore()'
Rscript R/99_run_all.R
