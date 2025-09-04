library(haven)
library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(scales)
library(sf)

# On essaye avec les enfants pour avoir plus d'observations : 
# HAZ = HW70 Height for age standard deviation (according to WHO) 

# Cleaning function pour HAZ 
clean_haz <- function(x) {
  y <- ifelse(x %in% 9996:9999, NA, x) / 100
  ifelse(!is.na(y) & (y < -6 | y > 6), NA, y)
}

# Import DHS 
# 1997
kr_1997 <- read_dta("data/raw/dhs/DHS_1997/MDKR31DT/MDKR31FL.DTA",
                    col_select = c(caseid, v001, v002)) %>%
  mutate(DHSYEAR = 1997)
hw_1997 <- read_dta("data/raw/dhs/DHS_1997/MDHW31DT/MDHW31FL.DTA") %>%
  select(hwcaseid, hc70)

stopifnot(all(kr_1997$caseid == hw_1997$hwcaseid))
kr_1997 <- kr_1997 %>%
  bind_cols(hw_1997 %>% select(hc70)) %>%
  transmute(caseid, v001, v002, DHSYEAR, haz = clean_haz(hc70))

# 2008
kr_2008 <- read_dta("data/raw/dhs/DHS_2008/MDKR51DT/MDKR51FL.DTA",
                    col_select = c(caseid, v001, v002, hw70)) %>%
  mutate(DHSYEAR = 2008,
         haz = clean_haz(hw70)) %>%
  transmute(caseid, v001, v002, DHSYEAR, haz)

# 2021
kr_2021 <- read_dta("data/raw/dhs/DHS_2021/MDKR81DT/MDKR81FL.DTA",
                    col_select = c(caseid, v001, v002, hw70)) %>%
  mutate(DHSYEAR = 2021,
         haz = clean_haz(hw70)) %>%
  transmute(caseid, v001, v002, DHSYEAR, haz)

# Fusion
kr_all <- bind_rows(kr_1997, kr_2008, kr_2021)


# Fonction de chargement + renommage SPEI
load_matched <- function(path, year, spei_years) {
  read_rds(path) %>%
    mutate(DHSYEAR = year,
           hv220 = as.numeric(hv220) # On enlève les labels pour éviter les conflits
           ) %>%
    rename(
      spei_wc_n_2 = !!sym(paste0("spei_wc_", spei_years[1])),
      spei_wc_n_1 = !!sym(paste0("spei_wc_", spei_years[2])),
      spei_wc_n   = !!sym(paste0("spei_wc_", spei_years[3]))
    )
}

# Charger et fusionner
data_all <- bind_rows(
  load_matched("data/derived/data_matched_1997.rds", 1997, c(1995,1996,1997)),
  load_matched("data/derived/data_matched_2008.rds", 2008, c(2006,2007,2008)),
  load_matched("data/derived/data_matched_2021.rds", 2021, c(2019,2020,2021))
)

# Fusion enfants (haz) avec données matchées des ménages
dat <- kr_all %>%
  inner_join(
    data_all, 
    by = c("DHSYEAR", "v001"="hv001", "v002"="hv002")
  ) %>%
  filter(GROUP %in% c("Treatment","Control"))

# Fonctions DID 
cluster_vcov <- function(mod, cluster){
  cl <- cluster[as.integer(rownames(mod$model))]
  M <- dplyr::n_distinct(cl); N <- length(cl); K <- mod$rank
  dfc <- (M/(M-1)) * ((N-1)/(N-K))
  uj <- apply(estfun(mod), 2, function(x) tapply(x, cl, sum))
  dfc * sandwich(mod, meat = crossprod(uj)/N)
}
did_fit <- function(d, years){
  dd <- d %>%
    filter(DHSYEAR %in% years) %>%
    transmute(
      DHSYEAR, v001, GROUP,
      haz,
      treated = as.integer(GROUP=="Treatment"),
      post = as.integer(DHSYEAR==max(years))
    ) %>%
    filter(!is.na(haz))
  m <- lm(haz ~ treated*post, data = dd)
  vc <- cluster_vcov(m, dd$v001)
  out <- coeftest(m, vcov.=vc) %>% broom::tidy()
  list(data=dd, tidy=out)
}
did_summary_extract <- function(res, label){
  res$tidy %>%
    filter(term=="treated:post") %>%
    transmute(spec=label, estimate, std.error, statistic, p.value)
}

# Estimation
res_placebo <- did_fit(dat, c(1997, 2008))
res_main    <- did_fit(dat, c(2008, 2021))

# Tableau de synthèse
did_summary <- bind_rows(
  did_summary_extract(res_placebo, "HAZ placebo 1997–2008"),
  did_summary_extract(res_main,    "HAZ main 2008–2021")
)
print(did_summary)

# Graph
mean_placebo <- res_placebo$data %>%
  group_by(DHSYEAR, GROUP) %>%
  summarise(mean_value=mean(haz, na.rm=TRUE),
            sd_value=sd(haz, na.rm=TRUE),
            n=n(), se=sd_value/sqrt(n), .groups="drop") %>%
  mutate(Period="Placebo (1997–2008)")
mean_main <- res_main$data %>%
  group_by(DHSYEAR, GROUP) %>%
  summarise(mean_value=mean(haz, na.rm=TRUE),
            sd_value=sd(haz, na.rm=TRUE),
            n=n(), se=sd_value/sqrt(n), .groups="drop") %>%
  mutate(Period="Main (2008–2021)")

evolution_combined <- bind_rows(mean_placebo, mean_main) %>%
  mutate(GroupPeriod = paste(GROUP, Period, sep="_"))


ggplot(evolution_combined,
       aes(x=factor(DHSYEAR), y=mean_value,
           group=interaction(GROUP, Period), color=GROUP)) +
  geom_point(aes(shape=Period), size=3.5) +
  geom_line(aes(linetype=Period), size=1.3) +
  geom_errorbar(aes(ymin=mean_value - 1.96*se,
                    ymax=mean_value + 1.96*se),
                width=0.2, size=0.8, alpha=0.5) +
  geom_text(aes(label=sprintf("%.2f", mean_value)),
            vjust=-1, size=4, color="black") +
  scale_color_manual(values=c("Treatment"="#D55E00","Control"="#0072B2"),
                     name="Group") +
  scale_linetype_manual(values=c("Main (2008–2021)"="solid",
                                 "Placebo (1997–2008)"="dashed"),
                        name="Period") +
  labs(title="HAZ Evolution: Main DID vs Placebo",
       x="Survey Year",
       y="Mean HAZ (Height-for-age z-score)") +
  theme_minimal(base_size=16) +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=18),
        axis.title=element_text(face="bold"),
        legend.position="top",
        panel.grid.major=element_line(size=0.3),
        panel.grid.minor=element_blank())



# Test avec le package did ------------------------------------------------


library(did)

# Formater le dataset au format attendu par le package
data_test_haz <- dat %>%
  mutate(
    id = as.numeric(paste0(DHSYEAR, v001, v002)), # identifiant unique
    group_recode = case_when(
      GROUP == "Treatment" ~ 2021,
      GROUP == "Control" ~ 0,
      GROUP == "Excluded" ~ 1997
    )
  )

# Vérification
table(data_test_haz$DHSYEAR, data_test_haz$GROUP)

# Estimation DID (Callaway & Sant Anna)
did_callaway_haz <- att_gt(
  yname = "haz",
  tname = "DHSYEAR",
  idname = "id",
  gname = "group_recode",
  control_group = "nevertreated",
  weightsname = "weights", # poids issus du matching
  data = data_test_haz,
  panel = FALSE
)

summary(did_callaway_haz)
ggdid(did_callaway_haz)

## Essai avec fixest -------------------------------------------
# remotes::install_github("https://github.com/s3alfisc/fwildclusterboot/")
# remotes::install_github("https://github.com/lrberge/fixest/")
library(fixest)
library(fwildclusterboot)
library(dplyr)
library(broom)

did_fit_fixest <- function(d, years, ssc_small = TRUE){
  dd <- d %>%
    filter(DHSYEAR %in% years) %>%
    transmute(
      DHSYEAR, v001, GROUP,
      haz,
      treated = as.integer(GROUP == "Treatment"),
      post    = as.integer(DHSYEAR == max(years))
    ) %>%
    filter(!is.na(haz))
  
  m <- feols(haz ~ treated*post, data = dd)
  
  # small-sample correction toggle
  ssc_opt <- if (ssc_small) ssc(adj = TRUE, cluster.adj = TRUE) else ssc()
  
  # --- three VCOVs computed separately (avoid etable's multi-vcov path) ---
  s_hc3 <- summary(m, vcov = "hc3", ssc = ssc_opt)
  s_c1  <- summary(m, vcov = ~ v001, ssc = ssc_opt)
  s_tw  <- summary(m, vcov = ~ v001 + DHSYEAR, ssc = ssc_opt)
  
  # extract DID row
  ext <- function(su) {
    broom::tidy(su) |>
      filter(term == "treated:post") |>
      transmute(estimate, std.error, statistic, p.value)
  }
  
  r_hc3 <- ext(s_hc3) |> rename(se_hc3 = std.error,  p_hc3 = p.value,  t_hc3 = statistic)
  r_c1  <- ext(s_c1)  |> rename(se_cl1 = std.error,  p_cl1 = p.value,  t_cl1 = statistic)
  r_tw  <- ext(s_tw)  |> rename(se_tw  = std.error,  p_tw  = p.value,  t_tw  = statistic)
  
  # wild cluster bootstrap p-value (PSU clusters)
  wb <- boottest(
    m,
    param   = "treated:post",
    clustid = ~ v001,
    B       = 9999,
    type    = "rademacher"
  )
  
  # assemble a single tidy row
  out <- bind_cols(
    tibble(period = paste0(min(years), "–", max(years))),
    r_hc3[, c("estimate", "se_hc3", "t_hc3", "p_hc3")],
    r_c1[,  c("se_cl1", "t_cl1", "p_cl1")],
    r_tw[,  c("se_tw",  "t_tw",  "p_tw")],
    tibble(p_wild = wb$p_val)
  )
  
  list(data = dd, model = m, tidy = out)
}

# Run
res_placebo <- did_fit_fixest(dat, c(1997, 2008))
res_main    <- did_fit_fixest(dat, c(2008, 2021))

# View compact DID rows
res_placebo$tidy
res_main$tidy
