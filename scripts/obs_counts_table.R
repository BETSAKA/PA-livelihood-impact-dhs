# Produce a table of observation counts by survey year
# Columns: Year, Survey, Raw_HH_all, Raw_HH_rural, Excluded_group,
#          Treatment_Control, After_NA_drop, Treated, Matched_rows

library(tidyverse)
library(haven)

years <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)
survey_map <- c(
  `1997` = "DHS",
  `2008` = "DHS",
  `2011` = "MIS",
  `2013` = "MIS",
  `2016` = "MIS",
  `2018` = "MICS",
  `2021` = "DHS"
)

derived_dir <- "data/derived"
dhs_raw_dir <- "data/raw/dhs"
mics_hh_file <- "data/raw/mics/2018/SPSS datasets/hh.sav"

# --- Pre-compute Raw_HH_all from raw source files ---

# DHS/MIS: read only first column of each HR .DTA (fast row count)
dhs_hr_paths <- list.files(
  dhs_raw_dir,
  pattern = "HR.*\\.DTA$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
dhs_hr_paths <- dhs_hr_paths[
  !grepl("SPSS|wealth", dhs_hr_paths, ignore.case = TRUE)
]

dhs_all_map <- map_int(dhs_hr_paths, function(f) {
  tryCatch(nrow(read_dta(f, col_select = 1L)), error = function(e) NA_integer_)
}) |>
  set_names(str_extract(dhs_hr_paths, "DHS_(\\d{4})", group = 1))

# MICS 2018: household questionnaire file
mics_all <- tryCatch(
  nrow(read_sav(mics_hh_file, col_select = 1L)),
  error = function(e) NA_integer_
)

# --- Build table row by row ---

out <- map_dfr(years, function(y) {
  y_chr <- as.character(y)
  hr_file <- file.path(derived_dir, paste0("hr_", y_chr, "_final.rds"))
  matched_file <- file.path(derived_dir, paste0("data_matched_", y_chr, ".rds"))
  matchres_file <- file.path(
    derived_dir,
    paste0("matching_result_", y_chr, ".rds")
  )

  # Raw HH (all): from raw source files
  Raw_HH_all <- if (y == 2018) mics_all else as.integer(dhs_all_map[y_chr])

  # Defaults
  Raw_HH_rural <- NA_integer_
  Excluded_group <- NA_integer_
  Treatment_Control <- NA_integer_
  After_NA_drop <- NA_integer_
  Treated <- NA_integer_
  Matched_rows <- NA_integer_

  # hr_final: rural-only file with GROUP assigned and matching covariates
  if (file.exists(hr_file)) {
    hr <- readRDS(hr_file)
    Raw_HH_rural <- nrow(hr)
    if ("GROUP" %in% names(hr)) {
      Excluded_group <- sum(hr$GROUP == "Excluded", na.rm = TRUE)
      Treatment_Control <- sum(
        hr$GROUP %in% c("Treatment", "Control"),
        na.rm = TRUE
      )
      Treated <- sum(hr$GROUP == "Treatment", na.rm = TRUE)
    }
  }

  # matching_result: matchit object — X matrix rows = inputs to matching (after NA drop)
  if (file.exists(matchres_file)) {
    mr <- readRDS(matchres_file)
    if (is.list(mr) && "X" %in% names(mr)) {
      After_NA_drop <- nrow(mr$X)
      if (is.na(Treated) && "treat" %in% names(mr)) {
        Treated <- sum(mr$treat == 1L)
      }
    }
  }

  # Matched rows: actual row count from matched dataset (not 2 × Treated)
  if (file.exists(matched_file)) {
    Matched_rows <- nrow(readRDS(matched_file))
  }

  tibble(
    Year = y,
    Survey = survey_map[y_chr],
    Raw_HH_all = Raw_HH_all,
    Raw_HH_rural = Raw_HH_rural,
    Excluded_group = Excluded_group,
    Treatment_Control = Treatment_Control,
    After_NA_drop = After_NA_drop,
    Treated = Treated,
    Matched_rows = Matched_rows
  )
})

# Write and print
dir.create("output", showWarnings = FALSE)
write_csv(out, file.path("output", "obs_by_year.csv"))
print(out)
