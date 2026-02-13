library(dplyr)
library(haven)

# write files where WSL can see them
write_dta(dta_out, "C:/Users/fbede/bjs_ready.dta")

do_lines <- c(
  "clear all",
  "set more off",
  "cd /mnt/c/Users/fbede",
  "",
  "cap which require",
  "if _rc ssc install require, replace",
  "cap which ftools",
  "if _rc ssc install ftools, replace",
  "cap which moremata",
  "if _rc ssc install moremata, replace",
  "cap which reghdfe",
  "if _rc ssc install reghdfe, replace",
  "cap which did_imputation",
  "if _rc ssc install did_imputation, replace",
  "",
  "use \"bjs_ready.dta\", clear",
  "replace g = . if g==0",
  "",
  "foreach k in -5 -4 -3 -2 -1 0 1 2 3 4 5 {",
  "  local nm = cond(`k' < 0, \"wtr_bin_m\" + string(abs(`k')), \"wtr_bin_p\" + string(`k'))",
  "  cap drop `nm'",
  "  gen double `nm' = (year>=g & g<.) * (rel_bin==`k')",
  "}",
  "",
  "did_imputation y id year g [aw=w], ///",
  "  fe(year) ///",
  "  controls(spei_wc_n_2 spei_wc_n_1 spei_wc_n hv220) ///",
  "  wtr(wtr_bin_m5 wtr_bin_m4 wtr_bin_m3 wtr_bin_m2 wtr_bin_m1 ///",
  "      wtr_bin_p0 wtr_bin_p1 wtr_bin_p2 wtr_bin_p3 wtr_bin_p4 wtr_bin_p5) ///",
  "  cluster(cluster) autosample",
  "",
  "* export coefficients + vcov to files",
  "tempname b V",
  "matrix `b' = e(b)",
  "matrix `V' = e(V)",
  "local cn : colnames `b'",
  "preserve",
  "clear",
  "set obs `=colsof(`b')'",
  "gen str60 term = \"\"",
  "gen double estimate = .",
  "gen double se = .",
  "forvalues j = 1/`=colsof(`b')' {",
  "  local tj : word `j' of `cn'",
  "  replace term = \"`tj'\" in `j'",
  "  replace estimate = `b'[1,`j'] in `j'",
  "  replace se = sqrt(`V'[`j',`j']) in `j'",
  "}",
  "gen double conf_low  = estimate - invnormal(0.975)*se",
  "gen double conf_high = estimate + invnormal(0.975)*se",
  "save \"bjs_results.dta\", replace",
  "export delimited using \"bjs_results.csv\", replace",
  "restore",
  "",
  "exit, clear"
)

writeLines(do_lines, "C:/Users/fbede/run_bjs.do")

# run Stata inside WSL (batch)
cmd <- 'wsl bash -lc "cd /mnt/c/Users/fbede && /usr/local/stata/stata -b do run_bjs.do"'
status <- system(cmd)

# check log
log_path <- "C:/Users/fbede/run_bjs.log"
stopifnot(file.exists(log_path))

log_lines <- readLines(log_path, warn = FALSE)

# print last part of the log (avoid flooding the console)
cat(tail(log_lines, 200), sep = "\n")

# optional: fail fast if Stata reports an error code in the log
if (any(grepl("^r\\([0-9]+\\);", log_lines))) {
  message("Stata error detected in log. Search for 'r(' in run_bjs.log.")
}


res <- read_dta("C:/Users/fbede/bjs_results.dta") %>%
  as_tibble()

# keep only the event-study terms
es <- res %>%
  filter(str_detect(term, "^tau_wtr_bin_")) %>%
  mutate(
    # recover bin index from term names created by did_imputation
    bin = case_when(
      str_detect(term, "_m5$") ~ -5L,
      str_detect(term, "_m4$") ~ -4L,
      str_detect(term, "_m3$") ~ -3L,
      str_detect(term, "_m2$") ~ -2L,
      str_detect(term, "_m1$") ~ -1L,
      str_detect(term, "_p0$") ~ 0L,
      str_detect(term, "_p1$") ~ 1L,
      str_detect(term, "_p2$") ~ 2L,
      str_detect(term, "_p3$") ~ 3L,
      str_detect(term, "_p4$") ~ 4L,
      str_detect(term, "_p5$") ~ 5L,
      TRUE ~ NA_integer_
    )
  ) %>%
  arrange(bin)

ggplot(es, aes(x = bin, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.15) +
  geom_point() +
  labs(
    x = "Binned relative time (2-year bins)",
    y = "Effect on wealth index (centiles)"
  )
