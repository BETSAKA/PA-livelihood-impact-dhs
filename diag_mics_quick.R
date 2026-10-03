library(haven)
setwd("c:/Users/fbede/Documents/Statistiques/PA-livelihood-impact-dhs_v2")
bh <- read_sav("data/raw/mics/2018/SPSS datasets/bh.sav",
               col_select = c(HH1, HH2, BH5, BH4Y, BH3))
res <- data.frame(
  total_births = nrow(bh),
  unique_hh = length(unique(paste(bh$HH1, bh$HH2))),
  min_year = min(bh$BH4Y, na.rm = TRUE),
  max_year = max(bh$BH4Y, na.rm = TRUE)
)
write.csv(res, "diag_mics_total.csv", row.names = FALSE)
tbl <- as.data.frame(table(bh$BH4Y))
names(tbl) <- c("birth_year", "n_births")
write.csv(tbl, "diag_mics_by_year.csv", row.names = FALSE)
