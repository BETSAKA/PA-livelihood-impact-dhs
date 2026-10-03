library(tidyverse)

setwd("c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2")

years <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)
surveys <- c("DHS", "DHS", "MIS", "MIS", "MIS", "MICS", "DHS")

res <- map_dfr(seq_along(years), function(i) {
  yr <- years[i]
  hr <- readRDS(paste0("data/derived/hr_", yr, "_final.rds"))
  m  <- readRDS(paste0("data/derived/data_matched_", yr, ".rds"))
  
  tibble(
    Year           = yr,
    Survey         = surveys[i],
    Total          = nrow(hr),
    Treatment      = sum(hr$GROUP == "Treatment", na.rm = TRUE),
    Control        = sum(hr$GROUP == "Control", na.rm = TRUE),
    Excluded       = sum(hr$GROUP == "Excluded", na.rm = TRUE),
    Matched_Treat  = sum(m$GROUP == "Treatment", na.rm = TRUE),
    Matched_Ctrl   = sum(m$GROUP == "Control", na.rm = TRUE),
    Matched_Total  = nrow(m)
  )
})

write.csv(res, "c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2/temp_obs_counts.csv", row.names = FALSE)
