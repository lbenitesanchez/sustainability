# =========================================================
# ARTICLE TITLE
# The effect of value-based teaching on attitudes, values,
# and behaviors associated with sustainability:
# Does national culture matter?
#
# AUTHORS AND AFFILIATIONS
# Luis Benites
# Department of Business, Universidad del Pacífico, Lima, Perú
#
# María Angela Prialé
# Department of Business, Universidad del Pacífico, Lima, Perú
#
# Angela Vera Ruiz
# Academic Department of Psychology,
# Pontificia Universidad Católica del Perú, Lima, Perú
#
# Carlos Ivan Flores Venturi
# Department of Business, Universidad del Pacífico, Lima, Perú
#
# SUPPLEMENTARY SUMMARY STATISTICS FOR SECONDARY ANALYSIS
# Corrected SEM version:
#   - PSB indicators: PP1, PP2, PP3
#   - CSR1_bin coded 0 = No, 1 = Yes
#   - Pooled + country-specific outputs
# =========================================================

# Metadata for the article
article_title <- paste(
  "The effect of value-based teaching on attitudes, values,",
  "and behaviors associated with sustainability:",
  "Does national culture matter?"
)

article_authors <- tibble::tribble(
  ~author,                   ~affiliation,
  "Luis Benites",            "Department of Business, Universidad del Pacífico, Lima, Perú",
  "María Angela Prialé",     "Department of Business, Universidad del Pacífico, Lima, Perú",
  "Angela Vera Ruiz",        "Academic Department of Psychology, Pontificia Universidad Católica del Perú, Lima, Perú",
  "Carlos Ivan Flores Venturi", "Department of Business, Universidad del Pacífico, Lima, Perú"
)

article_author_line <- paste(article_authors$author, collapse = "; ")

article_affiliation_block <- paste(
  paste0(article_authors$author, ": ", article_authors$affiliation),
  collapse = "\n"
)

analysis_subtitle <- "Supplementary summary statistics for secondary analysis"

# install.packages(c(
#   "haven", "dplyr", "tidyr", "purrr", "stringr",
#   "openxlsx", "readr", "tibble"
# ))
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(openxlsx)
library(readr)
library(tibble)

# ---------------------------------------------------------
# 1) Paths and options
# ---------------------------------------------------------
input_file <- "Base_Final.sav"
out_dir <- "Supplementary_SEM_summary_statistics"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# If TRUE, create additional covariance/correlation matrices
# including CSR1_bin as a numeric 0/1 observed variable.
include_binary_in_matrices <- TRUE

# ---------------------------------------------------------
# 2) Read data and harmonize names
# ---------------------------------------------------------
raw <- read_sav(input_file)
names(raw) <- iconv(names(raw), from = "", to = "ASCII//TRANSLIT")

# ---------------------------------------------------------
# 3) Variable names used in the corrected SEM
# ---------------------------------------------------------
sch_items <- c("Benevolencia", "Universalismo", "Poder", "Logro")

sc_items <- c(
  "APS1", "APS2", "APS3", "APS4", "APS5",
  "APS6", "APS7", "APS9", "APS10", "APS14"
)

sbl_items <- c(
  "APS11", "APS12", "APS13", "APS15", "APS16",
  "APS18", "APS19", "APS20", "APS21"
)

psb_items <- c("PP1", "PP2", "PP3")

sem_continuous_vars <- c(sch_items, sc_items, sbl_items, psb_items)

country_var <- "Pais"
sex_var <- "Sexo"
edu_var <- "Estudios"
ses_var <- "NSE"

# ---------------------------------------------------------
# 4) Coerce variables and relabel countries
# ---------------------------------------------------------
all_numeric_candidates <- intersect(
  c(sem_continuous_vars, "Edad", "CSR1", "CSR1_rec"),
  names(raw)
)

dat <- raw %>%
  mutate(across(all_of(all_numeric_candidates), ~ suppressWarnings(as.numeric(.x))))

# Convert factor-like variables using labels if available
if (country_var %in% names(dat)) dat[[country_var]] <- as.character(as_factor(dat[[country_var]]))
if (sex_var %in% names(dat))     dat[[sex_var]]     <- as.character(as_factor(dat[[sex_var]]))
if (edu_var %in% names(dat))     dat[[edu_var]]     <- as.character(as_factor(dat[[edu_var]]))
if (ses_var %in% names(dat))     dat[[ses_var]]     <- as.character(as_factor(dat[[ses_var]]))

# Standardize country names
dat[[country_var]] <- recode(
  dat[[country_var]],
  "Perú" = "Peru",
  "México" = "Mexico",
  .default = dat[[country_var]]
)

country_levels <- c("Argentina", "Colombia", "Peru", "Mexico")
dat[[country_var]] <- factor(dat[[country_var]], levels = country_levels)

# ---------------------------------------------------------
# 5) Create CSR1_bin (0 = No, 1 = Yes)
# ---------------------------------------------------------
if ("CSR1_rec" %in% names(dat)) {
  dat$CSR1_bin <- as.numeric(dat$CSR1_rec)
} else if ("CSR1" %in% names(dat)) {
  csr_chr <- as.character(dat$CSR1)
  dat$CSR1_bin <- case_when(
    csr_chr %in% c("0", "No", "NO", "no") ~ 0,
    csr_chr %in% c("1", "Si", "Sí", "SI", "Yes", "YES", "yes") ~ 1,
    TRUE ~ suppressWarnings(as.numeric(dat$CSR1))
  )
} else {
  stop("Neither CSR1_rec nor CSR1 was found in the dataset.")
}

stopifnot(all(na.omit(dat$CSR1_bin) %in% c(0, 1)))

# Human-readable version for frequency tables
dat$CSR1_label <- factor(dat$CSR1_bin, levels = c(0, 1), labels = c("No", "Yes"))

# ---------------------------------------------------------
# 6) Keep the analytic sample used in the corrected SEM
# ---------------------------------------------------------
sem_vars_all <- c(sem_continuous_vars, "CSR1_bin", country_var)

analytic <- dat %>%
  select(any_of(c(
    sem_vars_all, "Edad", sex_var, edu_var, ses_var, "CSR1_label"
  ))) %>%
  filter(if_all(all_of(sem_vars_all), ~ !is.na(.)))

# Sample size checks
sample_summary_total <- tibble(
  Stage = c("Raw dataset", "Complete cases on SEM variables"),
  N = c(nrow(dat), nrow(analytic))
)

sample_summary_by_country <- bind_rows(
  dat %>%
    filter(!is.na(.data[[country_var]])) %>%
    count(.data[[country_var]], name = "N") %>%
    mutate(Stage = "Raw dataset"),
  analytic %>%
    count(.data[[country_var]], name = "N") %>%
    mutate(Stage = "Complete cases on SEM variables")
) %>%
  rename(Country = !!country_var) %>%
  relocate(Stage, Country, N)

# ---------------------------------------------------------
# 7) Build a variable codebook
# ---------------------------------------------------------
get_label_safe <- function(df, var) {
  lbl <- attr(df[[var]], "label")
  if (is.null(lbl) || length(lbl) == 0 || is.na(lbl) || lbl == "") return(NA_character_)
  as.character(lbl)
}

construct_map <- tibble(
  Variable = c(sch_items, sc_items, sbl_items, psb_items, "CSR1_bin"),
  Construct = c(
    rep("Self-transcendence / Self-enhancement", length(sch_items)),
    rep("SC", length(sc_items)),
    rep("SBL", length(sbl_items)),
    rep("PSB", length(psb_items)),
    "CSR1"
  ),
  Scale = c(
    rep("Likert 1-6", length(sch_items)),
    rep("Likert 1-5", length(sc_items) + length(sbl_items) + length(psb_items)),
    "0 = no; 1 = yes"
  ),
  Type = c(
    rep("Observed item", length(sch_items) + length(sc_items) + length(sbl_items) + length(psb_items)),
    "Observed binary variable"
  ),
  Included_in_SEM = TRUE
)

codebook <- construct_map %>%
  mutate(
    Item_label = map_chr(Variable, ~ get_label_safe(raw, .x)),
    Notes = case_when(
      Variable %in% psb_items ~ "Retained PSB indicator",
      Variable == "CSR1_bin" ~ "Binary exogenous predictor used in corrected SEM",
      TRUE ~ ""
    )
  )

# ---------------------------------------------------------
# 8) Helper functions
# ---------------------------------------------------------
means_sd_table <- function(df, vars, group_name) {
  tibble(
    Group = group_name,
    Variable = vars,
    Mean = sapply(vars, function(v) mean(df[[v]], na.rm = FALSE)),
    SD   = sapply(vars, function(v) stats::sd(df[[v]], na.rm = FALSE)),
    N    = nrow(df)
  )
}

matrix_to_df <- function(mat) {
  out <- as.data.frame(mat)
  out <- tibble::rownames_to_column(out, var = "Variable")
  out
}

cov_table <- function(df, vars) {
  cov(df[, vars, drop = FALSE], use = "everything")
}

cor_table <- function(df, vars) {
  cor(df[, vars, drop = FALSE], use = "everything")
}

freq_table_one_group <- function(df, var, group_name) {
  if (!var %in% names(df)) return(NULL)
  
  df %>%
    filter(!is.na(.data[[var]])) %>%
    count(Level = .data[[var]], name = "n") %>%
    mutate(
      Percent = 100 * n / sum(n),
      Group = group_name,
      Variable = var
    ) %>%
    select(Group, Variable, Level, n, Percent)
}

# ---------------------------------------------------------
# 9) Means and SDs: pooled and by country
# ---------------------------------------------------------
means_sd_pooled <- means_sd_table(analytic, c(sem_continuous_vars, "CSR1_bin"), "Pooled")

means_sd_by_country <- map_dfr(country_levels, function(ctry) {
  df_ctry <- analytic %>% filter(.data[[country_var]] == ctry)
  means_sd_table(df_ctry, c(sem_continuous_vars, "CSR1_bin"), ctry)
})

means_sd_all <- bind_rows(means_sd_pooled, means_sd_by_country)

# ---------------------------------------------------------
# 10) Frequency tables for categorical variables
# ---------------------------------------------------------
# Optional recodes to English if desired
if (sex_var %in% names(analytic)) {
  analytic[[sex_var]] <- recode(
    analytic[[sex_var]],
    "Hombre" = "Male",
    "Mujer" = "Female",
    .default = analytic[[sex_var]]
  )
}

if (edu_var %in% names(analytic)) {
  analytic[[edu_var]] <- recode(
    analytic[[edu_var]],
    "Pregrado" = "Undergraduate studies",
    "Postgrado" = "Graduate studies",
    .default = analytic[[edu_var]]
  )
}

if (ses_var %in% names(analytic)) {
  analytic[[ses_var]] <- recode(
    analytic[[ses_var]],
    "Bajo" = "Lower",
    "Medio-bajo" = "Middle-Lower",
    "Medio" = "Middle",
    "Medio-alto" = "Middle-Upper",
    "Alto" = "Upper",
    .default = analytic[[ses_var]]
  )
}

categorical_vars <- c("CSR1_label", sex_var, edu_var, ses_var)
categorical_vars <- categorical_vars[categorical_vars %in% names(analytic)]

freq_pooled <- map_dfr(categorical_vars, ~ freq_table_one_group(analytic, .x, "Pooled"))

freq_by_country <- map_dfr(country_levels, function(ctry) {
  df_ctry <- analytic %>% filter(.data[[country_var]] == ctry)
  map_dfr(categorical_vars, ~ freq_table_one_group(df_ctry, .x, ctry))
})

freq_all <- bind_rows(freq_pooled, freq_by_country)

# ---------------------------------------------------------
# 11) Covariance and correlation matrices
#     A) Continuous indicators only
#     B) Continuous indicators + CSR1_bin (optional)
# ---------------------------------------------------------
matrix_vars_cont <- sem_continuous_vars
matrix_vars_with_bin <- c(sem_continuous_vars, "CSR1_bin")

make_matrix_sets <- function(df, group_name, vars, prefix) {
  cov_mat <- cov_table(df, vars)
  cor_mat <- cor_table(df, vars)
  
  list(
    cov = matrix_to_df(cov_mat),
    cor = matrix_to_df(cor_mat),
    cov_name = paste0(prefix, "_Cov_", group_name),
    cor_name = paste0(prefix, "_Cor_", group_name)
  )
}

# Pooled
matrices_cont_pooled <- make_matrix_sets(analytic, "Pooled", matrix_vars_cont, "Cont")
if (include_binary_in_matrices) {
  matrices_bin_pooled <- make_matrix_sets(analytic, "Pooled", matrix_vars_with_bin, "WithCSR1")
}

# By country
matrices_cont_by_country <- map(country_levels, function(ctry) {
  df_ctry <- analytic %>% filter(.data[[country_var]] == ctry)
  make_matrix_sets(df_ctry, ctry, matrix_vars_cont, "Cont")
})
names(matrices_cont_by_country) <- country_levels

if (include_binary_in_matrices) {
  matrices_bin_by_country <- map(country_levels, function(ctry) {
    df_ctry <- analytic %>% filter(.data[[country_var]] == ctry)
    make_matrix_sets(df_ctry, ctry, matrix_vars_with_bin, "WithCSR1")
  })
  names(matrices_bin_by_country) <- country_levels
}

# ---------------------------------------------------------
# 12) Write CSV files
# ---------------------------------------------------------
write_csv(sample_summary_total, file.path(out_dir, "S1_sample_summary_total.csv"))
write_csv(sample_summary_by_country, file.path(out_dir, "S2_sample_summary_by_country.csv"))
write_csv(codebook, file.path(out_dir, "S3_codebook_SEM_variables.csv"))
write_csv(means_sd_all, file.path(out_dir, "S4_means_sd_pooled_and_by_country.csv"))
write_csv(freq_all, file.path(out_dir, "S5_categorical_frequencies_pooled_and_by_country.csv"))

# Continuous-only matrices
write_csv(matrices_cont_pooled$cov, file.path(out_dir, "S6_cov_cont_Pooled.csv"))
write_csv(matrices_cont_pooled$cor, file.path(out_dir, "S7_cor_cont_Pooled.csv"))

walk(country_levels, function(ctry) {
  write_csv(
    matrices_cont_by_country[[ctry]]$cov,
    file.path(out_dir, paste0("S6_cov_cont_", ctry, ".csv"))
  )
  write_csv(
    matrices_cont_by_country[[ctry]]$cor,
    file.path(out_dir, paste0("S7_cor_cont_", ctry, ".csv"))
  )
})

# Continuous + CSR1_bin matrices
if (include_binary_in_matrices) {
  write_csv(matrices_bin_pooled$cov, file.path(out_dir, "S8_cov_withCSR1_Pooled.csv"))
  write_csv(matrices_bin_pooled$cor, file.path(out_dir, "S9_cor_withCSR1_Pooled.csv"))
  
  walk(country_levels, function(ctry) {
    write_csv(
      matrices_bin_by_country[[ctry]]$cov,
      file.path(out_dir, paste0("S8_cov_withCSR1_", ctry, ".csv"))
    )
    write_csv(
      matrices_bin_by_country[[ctry]]$cor,
      file.path(out_dir, paste0("S9_cor_withCSR1_", ctry, ".csv"))
    )
  })
}

# ---------------------------------------------------------
# 13) Write Excel workbook
# ---------------------------------------------------------
wb <- createWorkbook()

# README
addWorksheet(wb, "README")
readme_text <- tibble(
  Text = c(
    "Supplementary summary statistics for secondary analysis",
    "",
    "These files correspond to the corrected SEM specification.",
    "Observed indicators included in the corrected SEM:",
    "- Schwartz items: Benevolencia, Universalismo, Poder, Logro",
    "- ASP-S items: APS1, APS2, APS3, APS4, APS5, APS6, APS7, APS9, APS10, APS14, APS11, APS12, APS13, APS15, APS16, APS18, APS19, APS20, APS21",
    "- PSB items: PP1, PP2, PP3",
    "- CSR1_bin coded 0 = no, 1 = yes",
    "",
    "Matrices are reported pooled and separately for Argentina, Colombia, Peru, and Mexico.",
    "Continuous-only matrices are provided to match standard reviewer requests.",
    "Additional matrices including CSR1_bin are also provided to facilitate exact replication of the corrected SEM."
  )
)
writeData(wb, "README", readme_text)

# Sample summaries and codebook
addWorksheet(wb, "S1_total_N")
writeData(wb, "S1_total_N", sample_summary_total)

addWorksheet(wb, "S2_N_by_country")
writeData(wb, "S2_N_by_country", sample_summary_by_country)

addWorksheet(wb, "S3_codebook")
writeData(wb, "S3_codebook", codebook)

addWorksheet(wb, "S4_means_sd")
writeData(wb, "S4_means_sd", means_sd_all)

addWorksheet(wb, "S5_categorical_freq")
writeData(wb, "S5_categorical_freq", freq_all)

# Helper to add matrix sheets
add_matrix_sheet <- function(wb, sheet_name, df_matrix) {
  safe_name <- substr(sheet_name, 1, 31)
  addWorksheet(wb, safe_name)
  writeData(wb, safe_name, df_matrix)
}

# Continuous-only matrices
add_matrix_sheet(wb, "S6_cov_cont_Pooled", matrices_cont_pooled$cov)
add_matrix_sheet(wb, "S7_cor_cont_Pooled", matrices_cont_pooled$cor)

walk(country_levels, function(ctry) {
  add_matrix_sheet(
    wb,
    paste0("S6_cov_cont_", ctry),
    matrices_cont_by_country[[ctry]]$cov
  )
  add_matrix_sheet(
    wb,
    paste0("S7_cor_cont_", ctry),
    matrices_cont_by_country[[ctry]]$cor
  )
})

# Continuous + CSR1_bin matrices
if (include_binary_in_matrices) {
  add_matrix_sheet(wb, "S8_cov_withCSR1_Pooled", matrices_bin_pooled$cov)
  add_matrix_sheet(wb, "S9_cor_withCSR1_Pooled", matrices_bin_pooled$cor)
  
  walk(country_levels, function(ctry) {
    add_matrix_sheet(
      wb,
      paste0("S8_cov_withCSR1_", ctry),
      matrices_bin_by_country[[ctry]]$cov
    )
    add_matrix_sheet(
      wb,
      paste0("S9_cor_withCSR1_", ctry),
      matrices_bin_by_country[[ctry]]$cor
    )
  })
}

# Save workbook
saveWorkbook(
  wb,
  file.path(out_dir, "Supplementary_SEM_summary_statistics.xlsx"),
  overwrite = TRUE
)

# ---------------------------------------------------------
# 14) Console checks
# ---------------------------------------------------------
cat("\n===== FINAL ANALYTIC SAMPLE =====\n")
print(sample_summary_total)
cat("\n===== FINAL ANALYTIC SAMPLE BY COUNTRY =====\n")
print(sample_summary_by_country)

cat("\nFiles written to:\n")
cat(normalizePath(out_dir), "\n")
