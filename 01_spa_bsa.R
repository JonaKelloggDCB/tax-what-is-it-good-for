# ============================================================
# State Pension Age RD — British Social Attitudes 2005 (SN 5618)
# Theory test of H3 (visibility → support for redistribution)
# ============================================================
#
# Treatment: crossing UK State Pension Age (in 2005: women 60, men 65).
# Visibility events: 4-weekly DWP State Pension payment + annual Winter Fuel
# Payment lump sum, both separately labelled federal communications.
#
# Cross-section, so we cannot do diff-in-disc — this is a sharp age-RD.
# Same identification caveats as the GSS Medicare design (retirement,
# health peers etc. also change at the cutoff). Use donut to mitigate.

suppressPackageStartupMessages({
  pkgs <- c("haven","dplyr","rdrobust","ggplot2","fixest")
  to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BSA <- "British social surveys/sas/bsa05.sas7bdat"
stopifnot(file.exists(BSA))

# ---- 1. Load and clean --------------------------------------------------
raw <- haven::read_sas(BSA,
  col_select = dplyr::any_of(c("RAGE","RSEX","WTFACTOR",
                               "REDISTRB","TAXSPEND","WEALTH","LEFTRIGH",
                               "WELFHELP","UNEMPJOB","DOLEFIDL",
                               "RClassEr","HHIncQ","HEdQual","MarStat",
                               "HHINCOME","RCLASSGP")))
# Standardise to lowercase
names(raw) <- tolower(names(raw))

bsa <- raw |>
  mutate(across(everything(),
                ~ ifelse(. %in% c(-1, 8, 9, 98, 99), NA, .))) |>
  filter(!is.na(rage), rage >= 40, rage <= 85)

# REDISTRB: 1=Agree strongly ... 5=Disagree strongly. Reverse so high = pro.
bsa <- bsa |>
  mutate(
    redistr   = 6 - redistrb,
    wealth_r  = 6 - wealth,            # workers should get more
    welf_r    = 6 - welfhelp,          # welfare not too generous
    female    = as.integer(rsex == 2),
    male      = as.integer(rsex == 1),
    # Cutoff: women 60, men 65 (in 2005, pre-Pensions Act 2011 acceleration)
    spa       = ifelse(female == 1, 60, 65),
    run       = rage - spa,            # months are unavailable in BSA-EUL
    crossed   = as.integer(rage >= spa),
    wt        = ifelse(is.na(wtfactor), 1, wtfactor)
  )

cat("Sample sizes by sex:\n")
print(table(bsa$rsex, useNA = "ifany"))
cat("\nN at each running-variable value (years from SPA):\n")
print(table(bsa$run, useNA = "ifany"))

# ---- 2. Pooled RD across sexes (run already centred at own SPA) ---------
cat("\n=== Pooled RD: run = age − SPA, both sexes ===\n")
dat_p <- bsa |> filter(!is.na(redistr), abs(run) <= 10)
m <- rdrobust::rdrobust(
  y = dat_p$redistr, x = dat_p$run, c = 0,
  p = 1, kernel = "triangular", bwselect = "mserd",
  weights = dat_p$wt
)
summary(m)

# ---- 3. Separate RDs by sex --------------------------------------------
for (s in c(1, 2)) {
  sub <- bsa |> filter(rsex == s, !is.na(redistr), abs(run) <= 10)
  cat(sprintf("\n=== %s — N = %d, cutoff age %d ===\n",
              ifelse(s == 1, "Men", "Women"), nrow(sub),
              ifelse(s == 1, 65, 60)))
  r <- rdrobust::rdrobust(
    y = sub$redistr, x = sub$run, c = 0,
    p = 1, kernel = "triangular", bwselect = "mserd",
    weights = sub$wt
  )
  print(summary(r))
}

# ---- 4. Robustness ------------------------------------------------------
cat("\n=== Donut RD (drop exactly at cutoff age) ===\n")
sub <- dat_p |> filter(run != 0)
rdrobust::rdrobust(sub$redistr, sub$run, c = 0,
                   p = 1, kernel = "triangular",
                   weights = sub$wt) |> summary()

cat("\n=== Quadratic polynomial ===\n")
rdrobust::rdrobust(dat_p$redistr, dat_p$run, c = 0,
                   p = 2, kernel = "triangular",
                   weights = dat_p$wt) |> summary()

cat("\n=== Placebo cutoffs at SPA-5 and SPA+5 ===\n")
for (cc in c(-5, 5)) {
  cat("\n-- shift =", cc, "--\n")
  print(summary(rdrobust::rdrobust(
    y = dat_p$redistr, x = dat_p$run, c = cc,
    p = 1, kernel = "triangular",
    weights = dat_p$wt
  )))
}

# ---- 5. Secondary outcomes ----------------------------------------------
for (y in c("wealth_r", "welf_r", "leftrigh", "taxspend")) {
  if (!y %in% names(bsa)) next
  v <- bsa[[y]]
  if (sum(!is.na(v)) < 300) next
  cat("\n=== Secondary outcome:", y, "===\n")
  sub <- bsa |> filter(!is.na(.data[[y]]), abs(run) <= 10)
  print(summary(rdrobust::rdrobust(
    y = sub[[y]], x = sub$run, c = 0,
    p = 1, kernel = "triangular", weights = sub$wt
  )))
}

# ---- 5b. Heterogeneity (H2: net beneficiaries) --------------------------
# Two income proxies in BSA 2005:
#   HHINCOME : 17 banded household income (1=low ... 17=high; 97/98 = NA)
#   RCLASSGP : NS-SEC analytic class (1-2 managerial vs 3-5 routine/manual)
bsa <- bsa |>
  mutate(
    hh_lo  = case_when(hhincome %in% 1:8        ~ 1L,
                       hhincome %in% 9:17       ~ 0L,
                       TRUE                     ~ NA_integer_),
    class_lo = case_when(rclassgp %in% c(3,4,5,8) ~ 1L,
                         rclassgp %in% c(1,2)     ~ 0L,
                         TRUE                     ~ NA_integer_)
  )

cat("\nBSA heterogeneity sample sizes (full survey, both sexes):\n")
print(table(bsa$hh_lo, useNA = "ifany"))
print(table(bsa$class_lo, useNA = "ifany"))

run_split <- function(label, mask) {
  s <- bsa[mask & !is.na(bsa$redistr) & abs(bsa$run) <= 10, ]
  if (nrow(s) < 100) { cat(label, ": N too small (", nrow(s), ")\n"); return(invisible()) }
  cat("\n---", label, "(N =", nrow(s), ") ---\n")
  print(summary(rdrobust::rdrobust(
    y = s$redistr, x = s$run, c = 0,
    p = 1, kernel = "triangular", weights = s$wt
  )))
}

cat("\n=== H2: split by household income (HHINCOME) ===\n")
run_split("Below-median household income (pooled)",
          bsa$hh_lo == 1)
run_split("Above-median household income (pooled)",
          bsa$hh_lo == 0)
run_split("Below-median HH income, women only",
          bsa$hh_lo == 1 & bsa$female == 1)
run_split("Above-median HH income, women only",
          bsa$hh_lo == 0 & bsa$female == 1)

cat("\n=== H2: split by NS-SEC class (RCLASSGP) ===\n")
run_split("Lower class (NS-SEC 3-5,8), pooled",
          bsa$class_lo == 1)
run_split("Higher class (NS-SEC 1-2), pooled",
          bsa$class_lo == 0)

# ---- 6. RD plot ---------------------------------------------------------
dir.create("output", showWarnings = FALSE)
pdf("output/rd_spa_bsa.pdf", width = 7, height = 5)
rdrobust::rdplot(
  y = dat_p$redistr, x = dat_p$run, c = 0,
  p = 1, binselect = "esmv",
  x.label = "Age − State Pension Age",
  y.label = "Support for redistribution (REDISTRB reversed, 1-5)",
  title   = "SPA crossing RD — BSA 2005"
)
dev.off()
cat("\nPlot: output/rd_spa_bsa.pdf\n")

cat("\nDone.\n")
