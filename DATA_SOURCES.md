# Data sources

All datasets are free public-use files. UKDS and GESIS require a one-time
account; NORC GSS does not.

## British Social Attitudes 2005 — paper analysis (`01_spa_bsa.R`)

- **File**: `British social surveys/sas/bsa05.sas7bdat`
- **Source**: UK Data Service, study number SN 5618
  https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5618
- **Access**: register a free UKDS account (instant), accept the End-User
  Licence, download the SAS distribution.
- **Variables used**: `REDISTRB`, `WEALTH`, `WELFHELP`, `LEFTRIGH`, `TAXSPEND`,
  `RAGE`, `RSEX`, `WTFACTOR`.

## Additional analyses (not in the paper)

These cross-country designs live in `additional_analyses/` and are kept for
reference only.

### General Social Survey 1972–2024 (`additional_analyses/02_medicare_gss.R`)

- **File**: `GSS_sas/gss7224_r3.sas7bdat` (cumulative cross-year, release R3)
- **Source**: NORC GSS Data Explorer
  https://gssdataexplorer.norc.org/
- **Access**: free download, no registration required for the cumulative
  SAS file. Click "Download" → "GSS 1972–2024 Cross-Section Cumulative Data".
- **Variables used**: `YEAR`, `ID`, `AGE`, `EQWLTH`, `HELPPOOR`, `HELPNOT`,
  `SEX`, `RACE`, `EDUC`, `REALINC`, `POLVIEWS`, `PARTYID`.

### ALLBUS 2018 and 2023 (`additional_analyses/03_soli_allbus.R`)

- **Files**:
  - `albus2018/ZA5272_v1-0-0.dta` (ALLBUS/GGSS 2018)
  - `albus2023/ZA8833_v1-0-0.dta` (ALLBUS/GGSS 2023)
- **Source**: GESIS Data Archive, Cologne
  https://search.gesis.org/research_data/ZA5272 and ZA8833
- **Access**: free GESIS account (instant), accept usage conditions
  (Category A — scientific use), download Stata distribution.
- **Variables used**: `respid`, `incc`, `im19`, `im20`, `im21`, `sex`, `age`,
  `mstat`, `wghtpew`, and (in the 2018 file only) `i002_3`.

## Notes on data availability

Two further designs were considered but not pursued, because access lead
time is long relative to the project horizon:

- **SOEP-Core (German Socio-Economic Panel)** — requires a signed Data
  Distribution Contract with DIW Berlin (~4 weeks).
- **DST registers + ESS-DK linkage (Danish "grøn check")** — requires
  authorised Danish institutional affiliation plus 8–12 weeks for DST
  Forskningsservice approval and a separate link-key request from the
  Danish Data Archive.

The analyses kept here are those feasible with public-use data.
