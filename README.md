# Tax, What Is It Good For? — Replication code

Replication code for the empirical section of *Tax, What Is It Good For?
Perceived Government Efficiency and Redistribution Preferences*.

The paper asks whether the visibility of a government transfer raises stated
support for redistribution. The design is a sharp age-based regression
discontinuity at the UK State Pension Age, using the 2005 British Social
Attitudes survey: women crossing the SPA at 60 begin receiving separately
labelled DWP payments (State Pension, plus the annual Winter Fuel Payment),
and the test is whether that visible transfer shifts support for
redistribution.

Headline result: a roughly half-SD positive effect on stated support for
redistribution for women at the UK SPA cutoff (p = 0.034).

## Files

- `01_spa_bsa.R` — the analysis (UK State Pension Age RD, BSA 2005)
- `additional_analyses/` — exploratory cross-country designs that are **not**
  part of the paper, kept for reference: a US Medicare-at-65 RD on the GSS,
  and a German Soli-abolition diff-in-discontinuities on ALLBUS
- `DATA_SOURCES.md` — where to obtain the underlying public-use data
- `.gitignore` — excludes data and build artefacts

## Data

Data are **not in this repository**. The script expects a local copy of the
public-use BSA file at the path defined at the top of the script:

```
British social surveys/sas/bsa05.sas7bdat
```

It is a free public-use file; download requires a one-time UK Data Service
account and End-User Licence registration. See `DATA_SOURCES.md` for details
(and for the GSS and ALLBUS files used by the additional analyses).

## Running

```r
# install.packages(c("haven", "dplyr", "rdrobust", "rddensity", "ggplot2"))
source("01_spa_bsa.R")
```

The script prints headline RD estimates, robustness checks (donut,
quadratic, placebo cutoffs, covariate balance), secondary outcomes, and
heterogeneity by income, and writes an RD plot to `output/rd_spa_bsa.pdf`.

## Environment

- R 4.5.2
- `rdrobust` — local-linear bias-corrected RD inference (Calonico, Cattaneo, Titiunik)
- `rddensity` — Cattaneo–Jansson–Ma manipulation test
- `haven`, `dplyr`, `ggplot2`
