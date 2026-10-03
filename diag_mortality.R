library(tidyverse)
library(haven)
library(sf)

setwd("c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2")

# ── 1. Load matched households (same as 07bis) ──────────────────────────────
load_matched <- function(path, year, spei_years) {
  read_rds(path) |>
    st_drop_geometry() |>
    mutate(DHSYEAR = year) |>
    filter(GROUP %in% c("Treatment", "Control")) |>
    select(DHSYEAR, hv001, hv002, GROUP, treatment, weights)
}

matched_all <- bind_rows(
  load_matched("data/derived/data_matched_1997.rds", 1997, NULL),
  load_matched("data/derived/data_matched_2008.rds", 2008, NULL),
  load_matched("data/derived/data_matched_2011.rds", 2011, NULL),
  load_matched("data/derived/data_matched_2013.rds", 2013, NULL),
  load_matched("data/derived/data_matched_2016.rds", 2016, NULL),
  load_matched("data/derived/data_matched_2018.rds", 2018, NULL),
  load_matched("data/derived/data_matched_2021.rds", 2021, NULL)
)

cat("=== Matched households per year ===\n")
matched_all |> count(DHSYEAR, name = "n_matched_hh") |> as.data.frame() |> print()

# ── 2. Load raw KR files (all births) ───────────────────────────────────────
load_kr_raw <- function(path, year) {
  read_dta(path, col_select = c(caseid, v001, v002, v005, b5, b7, b3, v008, b4)) |>
    mutate(DHSYEAR = as.integer(year))
}

kr_1997 <- load_kr_raw("data/raw/dhs/DHS_1997/MDKR31DT/MDKR31FL.DTA", 1997)
kr_2008 <- load_kr_raw("data/raw/dhs/DHS_2008/MDKR51DT/MDKR51FL.DTA", 2008)
kr_2011 <- load_kr_raw("data/raw/dhs/DHS_2011/MDKR61DT/MDKR61FL.DTA", 2011)
kr_2013 <- load_kr_raw("data/raw/dhs/DHS_2013/MDKR6ADT/MDKR6AFL.DTA", 2013)
kr_2016 <- load_kr_raw("data/raw/dhs/DHS_2016/MDKR71DT/MDKR71FL.DTA", 2016)
kr_2021 <- load_kr_raw("data/raw/dhs/DHS_2021/MDKR81DT/MDKR81FL.DTA", 2021)

kr_all <- bind_rows(kr_1997, kr_2008, kr_2011, kr_2013, kr_2016, kr_2021)

cat("\n=== Raw KR births per year ===\n")
kr_all |>
  summarise(
    n_births = n(),
    n_hh = n_distinct(paste(v001, v002)),
    n_women = n_distinct(caseid),
    births_per_hh = round(n_births / n_hh, 2),
    births_per_woman = round(n_births / n_women, 2),
    .by = DHSYEAR
  ) |>
  arrange(DHSYEAR) |>
  as.data.frame() |>
  print()

# ── 3. MICS 2018 birth history ──────────────────────────────────────────────
mics_bh <- read_sav("data/raw/mics/2018/SPSS datasets/bh.sav",
                     col_select = c(HH1, HH2, BH5, BH9U, BH9N, BH4Y, BH3))

cat("\n=== MICS 2018 birth history (ALL births, no year filter) ===\n")
cat("Total births:", nrow(mics_bh), "\n")
cat("Unique HH:", n_distinct(paste(mics_bh$HH1, mics_bh$HH2)), "\n")
cat("Birth years range:", min(mics_bh$BH4Y, na.rm=T), "-", max(mics_bh$BH4Y, na.rm=T), "\n")

cat("\n=== MICS 2018 births by birth year ===\n")
mics_bh |> count(BH4Y) |> as.data.frame() |> print()

mics_bh_filtered <- mics_bh |> filter(BH4Y >= 2013 & BH4Y <= 2018)
cat("\nMICS after filter (2013-2018):", nrow(mics_bh_filtered), "\n")

# ── 4. Join: how many births match to matched households? ───────────────────
cat("\n=== After inner_join with matched HH ===\n")

for (yr in c(1997, 2008, 2011, 2013, 2016, 2021)) {
  kr_yr <- kr_all |> filter(DHSYEAR == yr)
  matched_yr <- matched_all |> filter(DHSYEAR == yr)
  
  joined <- kr_yr |>
    inner_join(matched_yr, by = c("DHSYEAR", "v001" = "hv001", "v002" = "hv002"))
  
  cat(sprintf(
    "Year %d: %d raw births, %d matched HH, %d births in matched HH (%d unique HH with births)\n",
    yr, nrow(kr_yr), nrow(matched_yr),
    nrow(joined), n_distinct(paste(joined$v001, joined$v002))
  ))
}

# MICS 2018
matched_2018 <- matched_all |> filter(DHSYEAR == 2018)
mics_joined_all <- mics_bh |>
  mutate(v001 = as.integer(HH1), v002 = as.integer(HH2), DHSYEAR = 2018L) |>
  inner_join(matched_2018, by = c("DHSYEAR", "v001" = "hv001", "v002" = "hv002"))
mics_joined_filt <- mics_bh_filtered |>
  mutate(v001 = as.integer(HH1), v002 = as.integer(HH2), DHSYEAR = 2018L) |>
  inner_join(matched_2018, by = c("DHSYEAR", "v001" = "hv001", "v002" = "hv002"))

cat(sprintf(
  "Year 2018 (MICS): %d total births, %d after 2013-2018 filter, %d matched HH, %d births in matched HH (all), %d births in matched HH (filtered)\n",
  nrow(mics_bh), nrow(mics_bh_filtered), nrow(matched_2018),
  nrow(mics_joined_all), nrow(mics_joined_filt)
))

# ── 5. DHS 2021 deep dive ──────────────────────────────────────────────────
cat("\n=== DHS 2021 deep dive ===\n")
kr_2021_full <- kr_all |> filter(DHSYEAR == 2021)
cat("Total births in KR 2021:", nrow(kr_2021_full), "\n")
cat("Unique women:", n_distinct(kr_2021_full$caseid), "\n")
cat("Unique HH:", n_distinct(paste(kr_2021_full$v001, kr_2021_full$v002)), "\n")

# Birth year (from CMC codes: b3 = child DOB in CMC, year = floor((b3-1)/12) + 1900)
kr_2021_full <- kr_2021_full |>
  mutate(birth_year = floor((as.numeric(b3) - 1) / 12) + 1900)

cat("Birth years range:", min(kr_2021_full$birth_year, na.rm=T), "-",
    max(kr_2021_full$birth_year, na.rm=T), "\n")
cat("\nBirths by birth year (DHS 2021):\n")
kr_2021_full |> count(birth_year) |> tail(25) |> as.data.frame() |> print()

matched_2021 <- matched_all |> filter(DHSYEAR == 2021)
joined_2021 <- kr_2021_full |>
  inner_join(matched_2021, by = c("DHSYEAR", "v001" = "hv001", "v002" = "hv002"))

cat("\nAfter matching join:\n")
cat("Births retained:", nrow(joined_2021), "\n")
cat("Unique HH with births:", n_distinct(paste(joined_2021$v001, joined_2021$v002)), "\n")
cat("Unique women:", n_distinct(joined_2021$caseid), "\n")
cat("Births per matched HH (with births):", 
    round(nrow(joined_2021) / n_distinct(paste(joined_2021$v001, joined_2021$v002)), 2), "\n")
cat("Fraction of matched HH that have births:", 
    round(n_distinct(paste(joined_2021$v001, joined_2021$v002)) / nrow(matched_2021) * 100, 1), "%\n")

# ── 6. Save all results to CSV for inspection ──────────────────────────────
# Summary per year
summary_rows <- list()

for (yr in c(1997, 2008, 2011, 2013, 2016, 2021)) {
  kr_yr <- kr_all |> filter(DHSYEAR == yr)
  matched_yr <- matched_all |> filter(DHSYEAR == yr)
  joined <- kr_yr |>
    inner_join(matched_yr, by = c("DHSYEAR", "v001" = "hv001", "v002" = "hv002"))
  
  summary_rows[[as.character(yr)]] <- tibble(
    year = yr,
    survey = ifelse(yr %in% c(1997, 2008, 2021), "DHS", "MIS"),
    raw_births = nrow(kr_yr),
    raw_hh = n_distinct(paste(kr_yr$v001, kr_yr$v002)),
    raw_women = n_distinct(kr_yr$caseid),
    matched_hh = nrow(matched_yr),
    births_after_join = nrow(joined),
    hh_with_births = n_distinct(paste(joined$v001, joined$v002)),
    women_after_join = n_distinct(joined$caseid),
    pct_matched_hh_with_births = round(n_distinct(paste(joined$v001, joined$v002)) / nrow(matched_yr) * 100, 1),
    births_per_matched_hh_with_births = round(nrow(joined) / max(1, n_distinct(paste(joined$v001, joined$v002))), 2)
  )
}

# Add MICS 2018
summary_rows[["2018"]] <- tibble(
  year = 2018,
  survey = "MICS",
  raw_births = nrow(mics_bh),
  raw_hh = n_distinct(paste(mics_bh$HH1, mics_bh$HH2)),
  raw_women = NA_integer_,
  matched_hh = nrow(matched_2018),
  births_after_join = nrow(mics_joined_all),
  hh_with_births = n_distinct(paste(mics_joined_all$v001, mics_joined_all$v002)),
  women_after_join = NA_integer_,
  pct_matched_hh_with_births = round(n_distinct(paste(mics_joined_all$v001, mics_joined_all$v002)) / nrow(matched_2018) * 100, 1),
  births_per_matched_hh_with_births = round(nrow(mics_joined_all) / max(1, n_distinct(paste(mics_joined_all$v001, mics_joined_all$v002))), 2)
)

diag_summary <- bind_rows(summary_rows) |> arrange(year)
write.csv(diag_summary, "c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2/diag_mortality_summary.csv", row.names = FALSE)

# DHS 2021 birth year distribution
birth_yr_dist <- kr_2021_full |> count(birth_year)
write.csv(birth_yr_dist, "c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2/diag_2021_birth_years.csv", row.names = FALSE)

# MICS birth year distribution
mics_yr_dist <- mics_bh |> count(BH4Y)
write.csv(mics_yr_dist, "c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2/diag_mics_birth_years.csv", row.names = FALSE)

cat("\nDone. Files written.\n")
