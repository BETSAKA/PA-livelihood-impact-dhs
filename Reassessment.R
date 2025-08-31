library(tidyverse)
library(haven)

# Code à exécuter après avoir exécuté 07-estimation.qmd

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

