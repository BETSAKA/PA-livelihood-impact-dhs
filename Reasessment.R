library(tidyverse)
library(haven)

options(scipen=999)

summary(did_placebo_wi)
summary(did_2008_2021)

# Essai avec le package did
# https://bcallaway11.github.io/did/articles/index.html
library(did)

data_test <- data_all %>%
  mutate(id = paste0(DHSYEAR, hv005),
         id = as.numeric(id),
         group_recode = case_when(GROUP== "Treatment" ~ 2021,
                                  GROUP == "Control" ~ 0,
                                  GROUP == "Excluded" ~ 1997))

table(data_test$DHSYEAR)

did_callaway <- att_gt(
  yname = "wealth_centile_rural_simple",
  tname = "DHSYEAR",
  idname = "id",
  gname = "group_recode",
  control_group = "nevertreated",
  # control_group = "notyettreated",
  # xformla = ~ spei_wc_n_2 +  spei_wc_n_1 + spei_wc_n + hv219 + hv220,
  weightsname = "weights",
  data = data_test,
panel = FALSE
)
summary(did_callaway)
ggdid(did_callaway)

# Constitution d'un jeu de données avec les poids taille des enfants

#kr = children's recode
kr_1997 <- read_dta("data/raw/dhs/DHS_1997/MDKR31DT/MDKR31FL.DTA",
                    col_select = c(caseid, # id comme dans hw 
                              v001, # cluster
                              v002, # household
                              hw1:hw12)) %>% 
                      mutate(DHSYEAR = 1997)
                    # En 1997 les données de poids-taille sont séparées
hw_1997 <- read_dta("data/raw/dhs/DHS_1997/MDHW31DT/MDHW31FL.DTA")  %>%
  select(hwcaseid, # id pour la jointure avec kr
         hc70:hc72) # variables de poids
# Consolidation
if (any(kr_1997$caseid != hw_1997$hwcaseid)) {
  "Error, can't merge like this"
} else {
  kr_1997 <- kr_1997 %>%
    bind_cols(hw_1997)
}

kr_2008 <- read_dta("data/raw/dhs/DHS_2008/MDKR51DT/MDKR51FL.DTA",
                    col_select = c(caseid, v001, v002, hw1:hw12, hw70:hw73)) %>%
  mutate(DHS_YEAR = "2008")
kr_2021 <- read_dta("data/raw/dhs/DHS_2021/MDKR81DT/MDKR81FL.DTA",
                    col_select = c(caseid, v001, v002, hw1:hw12, hw70:hw73))

# Remove 9996. height out of plausible limits; 
# 9997. age in days out of plausible limits,
# 9998. flagged cases
