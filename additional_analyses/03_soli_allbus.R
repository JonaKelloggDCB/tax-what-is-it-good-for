# ============================================================
# Soli abolition 2021 — Diff-in-disc on ALLBUS 2018 vs 2023
# Theory test of H3 (visibility → support for redistribution)
# ============================================================
#
# Not part of the paper — exploratory German comparison, kept for reference.
# Run from the repository root. Files expected (under the repo root):
#   albus2018/ZA5272_v1-0-0.dta   (pre-Soli-abolition)
#   albus2023/ZA8833_v1-0-0.dta   (post-Soli-abolition)
#
# Constraints we are working with:
#   1. ALLBUS 2023 has only categorical income (incc) → band midpoints as RV.
#   2. ALLBUS 2023 dropped I002_3 ("gov should reduce income diffs").
#      Common items across waves are im19, im20, im21 — all *acceptance*
#      of inequality. We reverse and average to get a pro-redistribution index.
#   3. Top band of hhincc in 2018 is "€7500+" — kills the joint-filer cutoff.
#      We therefore restrict to *single filers*, where the Soli Freigrenze
#      maps to ~€3,800/month net (bands 19–20 in 2018, 20–21 in 2023).

suppressPackageStartupMessages({
  pkgs <- c("haven","dplyr","fixest","sandwich","lmtest")
  to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# ---- 1. Band-midpoint dictionaries --------------------------------------
mid_2018 <- c(`1`=150,`2`=250,`3`=350,`4`=450,`5`=562,`6`=687,`7`=812,`8`=937,
              `9`=1062,`10`=1187,`11`=1312,`12`=1437,`13`=1625,`14`=1875,
              `15`=2125,`16`=2375,`17`=2625,`18`=2875,`19`=3500,`20`=4500,
              `21`=6250,`22`=9000)

mid_2023 <- c(`1`=150,`2`=250,`3`=350,`4`=450,`5`=550,`6`=675,`7`=812,`8`=937,
              `9`=1062,`10`=1187,`11`=1312,`12`=1437,`13`=1625,`14`=1875,
              `15`=2125,`16`=2375,`17`=2625,`18`=2875,`19`=3250,`20`=3750,
              `21`=4250,`22`=4750,`23`=5500,`24`=6750,`25`=8750,`26`=12000)

# ---- 2. Loader ----------------------------------------------------------
load_allbus <- function(path, year, mid) {
  d <- haven::read_dta(path)
  keep <- intersect(c("respid","incc","hhincc","im19","im20","im21",
                      "I002_3","i002_3","sex","age","mstat","wghtpew"),
                    names(d))
  d <- d[ , keep]
  names(d) <- tolower(names(d))
  # clean missing codes: anything negative is NAP/refusal/DK in ALLBUS
  d <- d |> mutate(across(everything(), ~ ifelse(. < 0, NA, .)))
  # band → midpoint income
  d$inc_eur   <- mid[as.character(d$incc)]
  # outcome: reverse so 4 = pro-redistribution
  d <- d |> mutate(
    im19_r = 5 - im19,
    im20_r = 5 - im20,
    im21_r = 5 - im21,
    pro_redist = rowMeans(cbind(im19_r, im20_r, im21_r), na.rm = TRUE)
  )
  d$wave <- year
  d
}

a18 <- load_allbus("albus2018/ZA5272_v1-0-0.dta", 2018, mid_2018)
a23 <- load_allbus("albus2023/ZA8833_v1-0-0.dta", 2023, mid_2023)

# ---- 3. Single-filer subsample, near Soli cutoff -----------------------
# Single = mstat in {3 widowed, 4 divorced, 5 never married, 7 life-p living apart}
single_codes <- c(3, 4, 5, 7)
SOLI_CUT_EUR <- 3800   # ~Soli Freigrenze, single, net monthly (rough)

prep <- function(d) {
  d |>
    filter(mstat %in% single_codes, !is.na(inc_eur), !is.na(pro_redist)) |>
    mutate(
      run      = inc_eur - SOLI_CUT_EUR,
      below    = as.integer(run < 0),
      post     = as.integer(wave == 2023),
      visible  = below * post
    )
}

dat <- bind_rows(prep(a18), prep(a23))
cat("Sample by wave x below-cutoff:\n")
print(table(dat$wave, dat$below, useNA = "ifany"))
cat("Outcome summary:\n")
print(tapply(dat$pro_redist, dat$wave, summary))

# ---- 4. Diff-in-disc, local-linear by hand (band-midpoint RV) -----------
# rdrobust would also work but mass-points warnings dominate when RV has
# 6–8 unique values. fixest gives an interpretable, robust point estimate.

run_dd <- function(data, bw = 2500) {
  d <- data |> filter(abs(run) <= bw)
  m <- feols(
    pro_redist ~ visible + below + post + run + run:below |
      0,
    data = d,
    weights = ~wghtpew,
    vcov = "hetero"
  )
  list(model = m, n = nrow(d))
}

cat("\n=== Main diff-in-disc (bandwidth ±€2,500) ===\n")
res <- run_dd(dat, bw = 2500)
cat("Effective N =", res$n, "\n")
print(summary(res$model))

cat("\n=== Bandwidth sensitivity ===\n")
for (bw in c(1500, 2000, 2500, 3000, 4000)) {
  r <- run_dd(dat, bw = bw)
  co <- coef(r$model)["visible"]
  se <- sqrt(diag(vcov(r$model)))["visible"]
  cat(sprintf("bw=%5.0f  N=%4d  tau=%6.3f  se=%5.3f  t=%5.2f\n",
              bw, r$n, co, se, co/se))
}

# ---- 5. Robustness: each outcome separately -----------------------------
cat("\n=== Single-item outcomes (reversed, high = pro-redist) ===\n")
for (y in c("im19_r","im20_r","im21_r")) {
  d <- dat |> filter(abs(run) <= 2500, !is.na(.data[[y]]))
  f <- as.formula(paste(y, "~ visible + below + post + run + run:below"))
  m <- feols(f, data = d, weights = ~wghtpew, vcov = "hetero")
  co <- coef(m)["visible"]; se <- sqrt(diag(vcov(m)))["visible"]
  cat(sprintf("%-8s  tau=%6.3f  se=%5.3f  t=%5.2f  p=%5.3f\n",
              y, co, se, co/se, 2*pnorm(-abs(co/se))))
}

# ---- 6. Placebo cutoffs ------------------------------------------------
cat("\n=== Placebo cutoffs (shifted ±€1,500) ===\n")
for (shift in c(-1500, -1000, 1000, 1500)) {
  d <- dat |> mutate(run2 = inc_eur - (SOLI_CUT_EUR + shift),
                     below2 = as.integer(run2 < 0),
                     visible2 = below2 * post) |>
    filter(abs(run2) <= 2500)
  m <- feols(pro_redist ~ visible2 + below2 + post + run2 + run2:below2,
             data = d, weights = ~wghtpew, vcov = "hetero")
  co <- coef(m)["visible2"]; se <- sqrt(diag(vcov(m)))["visible2"]
  cat(sprintf("shift=%+5.0f  N=%4d  tau=%6.3f  se=%5.3f\n",
              shift, nrow(d), co, se))
}

# ---- 7. Bonus: pure cross-section on 2018 with the gold-standard item ---
if ("i002_3" %in% names(a18)) {
  cat("\n=== 2018 only, gold-standard item I002_3 (RD, no diff-in-disc) ===\n")
  d18 <- a18 |>
    filter(mstat %in% single_codes, !is.na(inc_eur), !is.na(i002_3)) |>
    mutate(redistr = 6 - i002_3,
           run = inc_eur - SOLI_CUT_EUR,
           below = as.integer(run < 0)) |>
    filter(abs(run) <= 2500)
  m <- feols(redistr ~ below + run + run:below, data = d18,
             weights = ~wghtpew, vcov = "hetero")
  print(summary(m))
}

cat("\nDone.\n")
