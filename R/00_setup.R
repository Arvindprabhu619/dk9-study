# R/00_setup.R
# One-time package install + session defaults.
# renv is deferred to the end of the project (see README).

required <- c(
  # data wrangling
  "readxl", "writexl", "janitor", "dplyr", "tidyr", "stringr", "forcats", "purrr",
  # stats
  "rstatix", "coin", "effsize", "pwr", "broom",
  # tables
  "gtsummary", "gt", "flextable", "officer",
  # figures
  "ggplot2", "patchwork", "scales", "viridis", "ggExtra", "ggrepel", "ggcorrplot",
  # misc
  "here"
)

installed <- rownames(installed.packages())
new <- setdiff(required, installed)
if (length(new) > 0) {
  message("Installing: ", paste(new, collapse = ", "))
  install.packages(new, repos = "https://cloud.r-project.org")
} else {
  message("All required packages already installed.")
}

# Load the one package we need at setup time (for theme_set)
suppressPackageStartupMessages(library(ggplot2))

set.seed(20241101)
theme_set(theme_classic(base_size = 12))

message("Setup complete. R version: ", getRversion())
