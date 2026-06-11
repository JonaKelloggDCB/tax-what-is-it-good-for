# ============================================================
# Medicare-at-65 RD on GSS — theory test of H3 (visibility → support)
# ============================================================
# Not part of the paper — exploratory US comparison, kept for reference.
# Run with the working directory set to the repository root, where the
# expected data folders live (in RStudio: Session > Set Working Directory).

suppressPackageStartupMessages({
  pkgs <- c("haven", "dplyr", "rdrobust", "rddensity", "ggplot2")
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# ---- 0. Path ------------------------------------------------------------
GSS_PATH <- "GSS_sas/gss7224_r3.sas7bdat"
stopifnot(file.exists(GSS_PATH))

# ---- 1. Load only the columns we need (full file is 2.4 GB) -------------
# GSS SAS releases vary on case (year vs YEAR). Read one row to discover names,
# then build a case-insensitive keep list.
header  <- haven::read_sas(GSS_PATH, n_max = 1)
allvars <- names(header)
want    <- c("year", "id", "age", "eqwlth", "helppoor", "helpnot",
             "sex", "race", "educ", "realinc", "polviews", "partyid")
keep <- allvars[tolower(allvars) %in% want]
missing_vars <- setdiff(want, tolower(keep))
if (length(missing_vars)) {
  warning("Variables not found in SAS file: ", paste(missing_vars, collapse = ", "))
}
cat("Loading these columns:\n"); print(keep)

gss_raw <- haven::read_sas(GSS_PATH, col_select = dplyr::all_of(keep))
# Standardise to lowercase so the rest of the script is portable.
names(gss_raw) <- tolower(names(gss_raw))

# GSS uses 0 = "Not applicable", and various 8/9/98/99 codes for DK/NA in
# specific items. Convert SAS missings (`tagged_na`) and known item-level
# refusal codes to NA. eqwlth: 0=NAP, 8=DK, 9=NA. helppoor/helpnot: same.
gss <- gss_raw |>
  mutate(across(everything(), \(x) {
    x <- haven::zap_labels(x)
    x[haven::is_tagged_na(x)] <- NA
    x
  })) |>
  mutate(
    age      = ifelse(age %in% c(0, 98, 99), NA, age),
    eqwlth   = ifelse(eqwlth %in% c(0, 8, 9), NA, eqwlth),
    helppoor = ifelse(helppoor %in% c(0, 8, 9), NA, helppoor),
    helpnot  = ifelse(helpnot  %in% c(0, 8, 9), NA, helpnot),
    polviews = ifelse(polviews %in% c(0, 8, 9), NA, polviews),
    partyid  = ifelse(partyid  %in% c(8, 9),    NA, partyid),
    educ     = ifelse(educ %in% c(97, 98, 99),  NA, educ),
    realinc  = ifelse(realinc <= 0, NA, realinc)
  ) |>
  filter(!is.na(age), !is.na(eqwlth), age >= 55, age <= 75) |>
  mutate(
    redistr     = 8 - eqwlth,           # 7-pt, high = pro-redistribution
    run         = age - 65,             # running variable
    treated     = as.integer(age >= 65),
    helppoor_r  = 6 - helppoor,         # 5-pt, high = govt should help
    helpnot_r   = 6 - helpnot,
    female      = as.integer(sex == 2),
    nonwhite    = as.integer(race != 1),
    log_realinc = log(realinc)
  )

cat("\nSample size after filtering:", nrow(gss), "\n")
cat("Cell sizes by age:\n"); print(table(gss$age))
cat("Years covered:\n"); print(range(gss$year))

# ---- 2. Manipulation density test (you can't manipulate your age) -------
cat("\n=== Manipulation test (Cattaneo-Jansson-Ma) ===\n")
rddensity::rddensity(X = gss$run, c = 0) |> summary()

# ---- 3. Main estimate ---------------------------------------------------
cat("\n=== Main RD: support for redistribution at age 65 ===\n")
main <- rdrobust::rdrobust(
  y = gss$redistr, x = gss$run,
  c = 0, p = 1, kernel = "triangular", bwselect = "mserd"
)
summary(main)

# ---- 4. RD plot ---------------------------------------------------------
dir.create("output", showWarnings = FALSE)
pdf("output/rd_main.pdf", width = 7, height = 5)
rdrobust::rdplot(
  y = gss$redistr, x = gss$run,
  c = 0, p = 1, binselect = "esmv",
  x.label = "Age - 65",
  y.label = "Support for redistribution (EQWLTH reversed, 1-7)",
  title   = "Medicare-at-65 RD on stated redistribution support"
)
dev.off()
cat("\nPlot written to output/rd_main.pdf\n")

# ---- 5. Robustness ------------------------------------------------------
cat("\n=== Quadratic polynomial ===\n")
rdrobust::rdrobust(gss$redistr, gss$run, c = 0, p = 2, kernel = "triangular") |>
  summary()

cat("\n=== Donut RD (drop age == 65) ===\n")
gss_donut <- gss |> filter(age != 65)
rdrobust::rdrobust(gss_donut$redistr, gss_donut$run, c = 0,
                   p = 1, kernel = "triangular") |> summary()

cat("\n=== Uniform kernel ===\n")
rdrobust::rdrobust(gss$redistr, gss$run, c = 0,
                   p = 1, kernel = "uniform") |> summary()

for (cc in c(-5, 5)) {
  cat("\n=== Placebo cutoff at age", 65 + cc, "===\n")
  print(summary(rdrobust::rdrobust(
    y = gss$redistr, x = gss$run, c = cc,
    p = 1, kernel = "triangular"
  )))
}

# ---- 6. Covariate balance ------------------------------------------------
cat("\n=== Covariate balance at cutoff ===\n")
for (v in c("female", "nonwhite", "educ", "log_realinc", "polviews", "partyid")) {
  if (sum(!is.na(gss[[v]])) < 500) next
  cat("\n--- Balance on", v, "---\n")
  print(summary(rdrobust::rdrobust(
    y = gss[[v]], x = gss$run, c = 0,
    p = 1, kernel = "triangular"
  )))
}

# ---- 7. Secondary outcomes ----------------------------------------------
for (y in c("helppoor_r", "helpnot_r")) {
  if (sum(!is.na(gss[[y]])) < 500) next
  cat("\n=== Secondary outcome:", y, "===\n")
  print(summary(rdrobust::rdrobust(
    y = gss[[y]], x = gss$run, c = 0,
    p = 1, kernel = "triangular"
  )))
}

# ---- 8. Heterogeneity (H2: net beneficiaries) ---------------------------
gss <- gss |>
  group_by(year) |>
  mutate(below_med_y = as.integer(realinc < median(realinc, na.rm = TRUE))) |>
  ungroup()

cat("\n=== H2: below-median-income respondents ===\n")
sub <- gss |> filter(below_med_y == 1)
rdrobust::rdrobust(sub$redistr, sub$run, c = 0,
                   p = 1, kernel = "triangular") |> summary()

cat("\n=== H2: above-median-income respondents ===\n")
sup <- gss |> filter(below_med_y == 0)
rdrobust::rdrobust(sup$redistr, sup$run, c = 0,
                   p = 1, kernel = "triangular") |> summary()

cat("\nDone. Headline result in §3, plot in output/rd_main.pdf.\n")
