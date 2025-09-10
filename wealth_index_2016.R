# Listes tirées du .sps 2016
common_pca_vars <- c(
  "QH101_11","QH101_12","QH101_13","QH101_14","QH101_21","QH101_31","QH101_32","QH101_41","QH101_42","QH101_71","QH101_81","QH101_96",
  "QH109_11","QH109_12","QH109_13","QH109_14","QH109_21","QH109_22","QH109_23","QH109_31","QH109_41","QH109_51","QH109_61",
  "QH109_11_sh","QH109_12_sh","QH109_13_sh","QH109_14_sh","QH109_21_sh","QH109_22_sh","QH109_23_sh","QH109_31_sh","QH109_41_sh","QH109_51_sh",
  "QH113_1","QH113_2","QH113_4","QH113_6","QH113_7","QH113_8","QH113_9","QH113_10","QH113_95",
  "QH119A","QH119B","QH119C","QH119D","QH119E","QH119F","QH120A","QH120B","QH120C","QH120D","QH120E","QH120F","QH120G","QH121",
  "QH148_11","QH148_12","QH148_21","QH148_22","QH148_23","QH148_31","QH148_32","QH148_33","QH148_34","QH148_35","QH148_96",
  "QH149_12","QH149_13","QH149_21","QH149_22","QH149_23","QH149_31","QH149_32","QH149_33","QH149_34","QH149_35","QH149_96",
  "QH150_12","QH150_13","QH150_21","QH150_22","QH150_23","QH150_24","QH150_26","QH150_31","QH150_32","QH150_33","QH150_34","QH150_35","QH150_36","QH150_37","QH150_96",
  "LAND","memsleep"
)

urban_pca_vars <- c(common_pca_vars, "landarea")  # en 2016 urbain/rural ajoutent landarea
rural_pca_vars <- c(common_pca_vars, "landarea")


recode_household_recodeVII <- function(hh) {
  hh %>%
    filter(hv015 == 1) %>%
    mutate(
      # --- EAU (QH101) ---
      QH101_11 = as.integer(hv201 == 11),
      QH101_12 = as.integer(hv201 == 12),
      QH101_13 = as.integer(hv201 == 13),
      QH101_14 = as.integer(hv201 == 14),                    # robinet public
      QH101_21 = as.integer(hv201 == 21),
      QH101_31 = as.integer(hv201 == 31),
      QH101_32 = as.integer(hv201 == 32),
      QH101_41 = as.integer(hv201 == 41),
      QH101_42 = as.integer(hv201 == 42),
      QH101_71 = as.integer(hv201 %in% c(71, 61)),           # 2016: 71 OU 61
      QH101_81 = as.integer(hv201 == 81),
      QH101_96 = as.integer(hv201 %in% c(96, 51)),           # 2016: inclut pluie (51)
      
      # --- TOILETTES (QH109) + partagé (QH110≈hv225) ---
      QH109_11 = as.integer(hv205 == 11),
      QH109_12 = as.integer(hv205 == 12),
      QH109_13 = as.integer(hv205 == 13),
      QH109_14 = as.integer(hv205 %in% c(14, 15)),           # “autre/dk” groupé en 2016
      QH109_21 = as.integer(hv205 == 21),
      QH109_22 = as.integer(hv205 == 22),
      QH109_23 = as.integer(hv205 == 23),
      QH109_31 = as.integer(hv205 == 31),
      QH109_41 = as.integer(hv205 == 41),
      QH109_51 = as.integer(hv205 == 51),
      QH109_61 = as.integer(hv205 == 61),
      
      QH109_11_sh = as.integer(hv225 == 1 & QH109_11 == 1),
      QH109_12_sh = as.integer(hv225 == 1 & QH109_12 == 1),
      QH109_13_sh = as.integer(hv225 == 1 & QH109_13 == 1),
      QH109_14_sh = as.integer(hv225 == 1 & QH109_14 == 1),
      QH109_21_sh = as.integer(hv225 == 1 & QH109_21 == 1),
      QH109_22_sh = as.integer(hv225 == 1 & QH109_22 == 1),
      QH109_23_sh = as.integer(hv225 == 1 & QH109_23 == 1),
      QH109_31_sh = as.integer(hv225 == 1 & QH109_31 == 1),
      QH109_41_sh = as.integer(hv225 == 1 & QH109_41 == 1),
      QH109_51_sh = as.integer(hv225 == 1 & QH109_51 == 1),
      
      # --- COMBUSTIBLE CUISSON (QH113 ≈ hv226) ---
      QH113_1  = as.integer(hv226 == 1),
      QH113_2  = as.integer(hv226 %in% c(2, 3)),             # GPL ou gaz naturel
      QH113_4  = as.integer(hv226 == 4),
      QH113_6  = as.integer(hv226 == 6),
      QH113_7  = as.integer(hv226 == 7),
      QH113_8  = as.integer(hv226 == 8),
      QH113_9  = as.integer(hv226 == 9),
      QH113_10 = as.integer(hv226 %in% c(10, 11)),           # résidus/bouse
      QH113_95 = as.integer(hv226 == 95),
      
      # --- BIENS (QH119/QH120/QH121 ≈ hv206:hv212 + hv243a + hv121?) ---
      QH119A = as.integer(hv206 == 1),                       # électricité
      QH119B = as.integer(hv207 == 1),                       # radio
      QH119C = as.integer(hv208 == 1),                       # tv
      QH119D = as.integer(ifelse("hv221"  %in% names(.), hv221  == 1, 0)),   # tel. fixe (si dispo)
      QH119E = as.integer(ifelse("hv243e" %in% names(.), hv243e == 1, 0)),   # ordinateur (optionnel)
      QH119F = as.integer(hv209 == 1),                       # frigo
      QH120A = as.integer(ifelse("hv243a" %in% names(.), hv243a == 1, 0)),   # montre
      QH120B = as.integer(ifelse("hv243a" %in% names(.), hv243a == 1, 0)),   # mobile (si tu préfères, garde ton QH122B)
      QH120C = as.integer(hv210 == 1),                       # vélo
      QH120D = as.integer(hv211 == 1),                       # moto
      QH120E = as.integer(ifelse("hv243c" %in% names(.), hv243c == 1, 0)),   # charrette (si dispo)
      QH120F = as.integer(hv212 == 1),                       # voiture
      QH120G = as.integer(ifelse("hv243d" %in% names(.), hv243d == 1, 0)),   # bateau moteur (si dispo)
      QH121  = as.integer(ifelse("hv247"  %in% names(.), hv247  == 1, 0)),   # compte bancaire (si dispo)
      
      # --- MATERIAUX SOL (QH148 ≈ hv213) ---
      QH148_11 = as.integer(hv213 == 11),
      QH148_12 = as.integer(hv213 == 12),
      QH148_21 = as.integer(hv213 == 21),
      QH148_22 = as.integer(hv213 == 22),
      QH148_23 = as.integer(hv213 == 23),
      QH148_31 = as.integer(hv213 == 31),
      QH148_32 = as.integer(hv213 == 32),
      QH148_33 = as.integer(hv213 == 33),
      QH148_34 = as.integer(hv213 == 34),
      QH148_35 = as.integer(hv213 == 35),
      QH148_96 = as.integer(hv213 == 96),
      
      # --- TOIT (QH149 ≈ hv214) ---
      QH149_12 = as.integer(hv214 == 12),
      QH149_13 = as.integer(hv214 == 13),
      QH149_21 = as.integer(hv214 == 21),
      QH149_22 = as.integer(hv214 == 22),
      QH149_23 = as.integer(hv214 == 23),
      QH149_31 = as.integer(hv214 == 31),
      QH149_32 = as.integer(hv214 == 32),
      QH149_33 = as.integer(hv214 == 33),
      QH149_34 = as.integer(hv214 %in% c(34, 36)),           # tuiles/shingles groupés
      QH149_35 = as.integer(hv214 == 35),
      QH149_96 = as.integer(hv214 == 96),
      
      # --- MURS (QH150 ≈ hv215) ---
      QH150_12 = as.integer(hv215 == 12),
      QH150_13 = as.integer(hv215 == 13),
      QH150_21 = as.integer(hv215 == 21),
      QH150_22 = as.integer(hv215 == 22),
      QH150_23 = as.integer(hv215 == 23),
      QH150_24 = as.integer(hv215 %in% c(24, 25)),           # contreplaqué/carton
      QH150_26 = as.integer(hv215 == 26),
      QH150_31 = as.integer(hv215 == 31),
      QH150_32 = as.integer(hv215 == 32),
      QH150_33 = as.integer(hv215 == 33),
      QH150_34 = as.integer(hv215 == 34),
      QH150_35 = as.integer(hv215 == 35),
      QH150_36 = as.integer(hv215 == 36),
      QH150_37 = as.integer(hv215 == 37),
      QH150_96 = as.integer(hv215 == 96),
      
      # --- TERRES AGRICOLES (LAND & landarea ; hv244/245 si présents) ---
      LAND     = as.integer(ifelse("hv244" %in% names(.), hv244 == 1, 0)),
      landarea = dplyr::case_when(
        !"hv245" %in% names(.) ~ 0,
        is.na(hv245) | hv245 >= 99.8 ~ 99.9,
        ifelse("hv244" %in% names(.), hv244 != 1, FALSE) ~ 0,
        TRUE ~ hv245
      ),
      
      # --- densité sommeil (même idée que ton code) ---
      hv216 = if_else(is.na(hv216) | hv216 == 0, hv012, hv216),
      memsleep = hv012 / hv216,
      
      wt = hv005/1e6
    ) %>%
    # Si certaines variables optionnelles n'existent pas dans le fichier, elles sont déjà à 0
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))
}


hh_2016 <- read_dta("data/raw/dhs/DHS_2016/MDHR71DT/MDHR71FL.DTA")
hh_2016_rec <- recode_household_recodeVII(hh_2016)

urban_vars_2016 <- remove_zero_var(filter(hh_2016_rec, hv025 == 1), urban_pca_vars)
rural_vars_2016 <- remove_zero_var(filter(hh_2016_rec, hv025 == 2), rural_pca_vars)

pca_urb_2016 <- perform_pca(filter(hh_2016_rec, hv025 == 1), urban_vars_2016)
pca_rur_2016 <- perform_pca(filter(hh_2016_rec, hv025 == 2), rural_vars_2016)

hh_2016_rec <- hh_2016_rec %>%
  mutate(
    wealth_index_rural = if_else(
      hv025 == 2,
      as.vector(scale(dplyr::select(., all_of(rural_vars_2016))) %*% pca_rur_2016$loadings[, 1]),
      NA_real_
    )
  )

hh_2016_rural <- hh_2016_rec %>%
  filter(hv025 == 2) %>%
  select(hv001, hv002, hv005, hv025, wealth_index_rural) %>%
  add_centile_survey("wealth_index_rural", new_col = "wealth_centile_rural_weighted") %>%
  mutate(wealth_centile_rural_simple = ntile(wealth_index_rural, 100))  %>%
  add_zscore_from_centile("wealth_centile_rural_weighted", "hv001",
                          zscore_col = "zscore_centile_rural")

write_rds(hh_2016_rural, "data/derived/hh_2016_rural.rds")

