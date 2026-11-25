# ---------------------------------------------------------------------------
# pa_legal_dates.R — Reconstruction des dates de début de traitement (PA)
#
# Deux définitions de la date d'entrée en traitement coexistent :
#
#   - date_main (« premier jalon individuel », spécification principale) :
#     le plus précoce des événements juridiques *individuellement attribuables*
#     et *opératoires* du site : arrêté de mise en protection temporaire
#     site-nommé, décret de création définitive (y compris site nommé dans un
#     décret collectif vérifié) ou acte de délégation de gestion. Les actes
#     « blanket » (arrêté interministériel 18633/2008 du 17/10/2008 et ses
#     prorogations 52005/2010 et 9874/2013) ne qualifient jamais : leurs
#     annexes sont des cartes, l'appartenance d'un site n'y est pas vérifiable
#     individuellement. Les états de précision annuelle (placeholder 01-01)
#     ou non datés de la base dynamique ne qualifient pas non plus.
#   - date_blanket_robustness (« chronologie juridique reconstituée », test de
#     sensibilité historique) : premier état de la base dynamique
#     (dynamic_wdpa.gpkg), qui attribue notamment le 17/10/2008 à de nombreux
#     sites via le proxy SAPM — avec incertitude explicite sur l'appartenance.
#   - date_permanent (« création définitive uniquement », test de sensibilité
#     institutionnel) : premier décret de création définitive (permanente)
#     vérifiable et individuellement attribuable — décret individuel ou site
#     nommé dans un décret collectif, uniquement si l'appariement PA-décret
#     est suffisamment documenté (certitude corpus « high »/« medium » ; les
#     cas « uncertain »/« not_matched » ne sont pas convertis en date).
#     La protection temporaire et
#     ses prorogations, la délégation de gestion, les actes blanket et les
#     modifications ultérieures ne qualifient pas ; ni STATUS_YR, ni le
#     premier valid_from de la base dynamique, ni un état définitif sans
#     texte (definitive_state_no_text), ni une citation ultérieure.
#
# Sources : data/legal_texts.rds (corpus curatorisé de textes juridiques) et
# les états de la base dynamic_wdpa déjà chargés par 01. Aucune table locale
# non versionnée n'est nécessaire.
#
# Corrections de dates documentées par le pipeline de curation
# (betsaka.github.io/conservation-dynamic-madagascar, 03_legal_docs) :
#   - décret 2015-713 (Massif d'Itremo) : 1951-04-21 -> 2015-04-21 ;
#   - décret collectif 2015-808 : 2017-05-05 (date de publication JO) ->
#     2015-05-05 (date de signature).
#
# Audit des décrets PA (novembre 2026) — couche de correction documentée :
#   - correction de liens acte -> WDPAID : 2015-758 (Loky -> Ambondrobe),
#     2015-711 (Marolambo -> Manjakatompo), 2015-750 (352240 -> 555790237),
#     23548/2018 et 2021-919 (Angavo -> Ankafobe : mélien résolu) ;
#   - correction de date : 2015-771 (Oronjia) 2018-04-28 -> 2015-04-28
#     (bloc de signature du texte conservé au corpus) ;
#   - arbitrage Angavo : certitude « uncertain » -> « high » au décret
#     collectif 2015-808 (géographie du Grand Sud confirmée) ;
#   - actes ajoutés : 2015-759 (Loky), 2015-720 (Tsitongambarika),
#     2015-782 appliqué aux composants COMATSA Nord/Sud, Bemanevika et
#     Mahimborondro (couverture spatiale vérifiée), 2015-791 (Andreba,
#     certitude « medium »), 2026-1529 (Tsinjoarivo, post-fenêtre).
# ---------------------------------------------------------------------------

# Constantes ---------------------------------------------------------------

PA_LEGAL_BLANKET_DATE <- as.Date("2008-10-17") # arrêté interministériel 18633/2008
PA_LEGAL_BLANKET_NUM <- "18633/2008"
PA_LEGAL_PROROG <- tibble(
  num_texte = c("52005/2010", "9874/2013"),
  act_date = as.Date(c("2010-12-20", "2013-05-06"))
)

# Corrections CNLEGIS documentées (numéro, date observée -> date corrigée).
# 2015-771 ajouté par l'audit des décrets PA (novembre 2026) : le bloc de
# signature du texte conservé au corpus porte « Fait à Antananarivo, le 28
# Avril 2015 » ; la fiche protectedareas.mg (fr/landscape/show/38) et le
# compte rendu du gestionnaire (MBG, avril 2015) concordent ; 03/12/2018 est
# la date de publication au JO.
PA_LEGAL_DATE_CORRECTIONS <- tibble(
  num_texte = c("2015-713", "2015-808", "2015-771"),
  date_observed = as.Date(c("1951-04-21", "2017-05-05", "2018-04-28")),
  date_corrected = as.Date(c("2015-04-21", "2015-05-05", "2015-04-28")),
  correction_basis = c(
    "erreur de métadonnée CNLEGIS documentée par la curation (décret 2015-713 : numéro et JO prouvent 2015 ; décrets frères signés le 21/04/2015)",
    "erreur de métadonnée CNLEGIS documentée par la curation (décret 2015-808 : signature le 05/05/2015 ; 27/11/2017 = publication au JO)",
    "erreur de métadonnée CNLEGIS documentée par l'audit des décrets PA (décret 2015-771 : bloc de signature du texte conservé au corpus « Fait à Antananarivo, le 28 Avril 2015 » ; protectedareas.mg/fr/landscape/show/38 et gestionnaire MBG concordent ; 03/12/2018 = publication au JO)"
  )
)

# Décret collectif 2015-808 : son objet nomme 10 AP ; rapprochement par nom
# exact avec la base dynamique, certitude renseignée par lien. Le cas Angavo,
# d'abord conservé « uncertain », a été arbitré par l'audit des décrets PA
# (novembre 2026) : l'article 1er et le tableau du décret identifient Angavo
# par le district d'Ambovombe et les communes Antanimora Atsimo et
# Ambanisarika (Grand Sud), ce que confirme la position du polygone
# analytique (555549450, centroid ≈ 45,8°E / 24,9°S), distinct de l'ancien
# « Complexe Anjozorobe Angavo » (WDPAID 352242, ≈ 48,0°E / 18,6°S) doté de
# son propre décret le même jour (2015-763). Certitude portée à « high ».
PA_LEGAL_DECREE_808_MAP <- tribble(
  ~name_in_decree      , ~WDPAID     , ~match_certainty , ~match_note                                                                                                                                                                                                                                                                                                                ,
  "Behara Tranomaro"   , 555549451L  , "high"           , "nom unique dans la base dynamique (Behara-Tranomaro)"                                                                                                                                                                                                                                                                     ,
  "Sud-Ouest Ifotaky"  , 555549452L  , "high"           , "nom unique dans la base dynamique"                                                                                                                                                                                                                                                                                        ,
  "Torotorofotsy"      ,    902404L  , "high"           , "nommé dans les arrêtés blanket selon la curation ; STATUS_YR antérieur à 2008 (hors univers de traitement)"                                                                                                                                                                                                               ,
  "Ambohidray"         , 555697915L  , "high"           , "nom unique dans la base dynamique"                                                                                                                                                                                                                                                                                        ,
  "Angavo"             , 555549450L  , "high"           , "arbitrage audit 2026-11 : article 1er et tableau du décret (district d'Ambovombe, communes Antanimora Atsimo et Ambanisarika, Grand Sud) concordent avec la position du polygone (centroid ≈ 45,8°E / 24,9°S) ; distinct du Complexe Anjozorobe Angavo (352242, ≈ 48,0°E / 18,6°S), doté du décret 2015-763 le même jour" ,
  "Ranobe Bay"         , NA_integer_ , "not_matched"    , "aucune AP nommée « Ranobe Bay » dans la base dynamique ; distincte de Ranobe PK 32, nommée séparément dans le même décret"                                                                                                                                                                                                ,
  "Vohidefo"           , 555549449L  , "high"           , "nom unique dans la base dynamique"                                                                                                                                                                                                                                                                                        ,
  "Bombetoka Belobaka" , 555697914L  , "high"           , "nom unique dans la base dynamique (variante orthographique Beloboka)"                                                                                                                                                                                                                                                     ,
  "Ranobe PK 32"       , 555549460L  , "high"           , "nommée séparément de Ranobe Bay dans le même décret"                                                                                                                                                                                                                                                                      ,
  "Ambararata Londa"   , 555697916L  , "high"           , "nom unique dans la base dynamique"
)

# Couche de correction documentée (audit des décrets PA, novembre 2026) -----
#
# 1. Corrections de liens acte -> WDPAID : le corpus rattachait des textes au
#    mauvais WDPAID. Les enregistrements bruts du corpus sont conservés ; la
#    correction est appliquée au moment de la construction du registre, avec
#    la preuve textuelle ou géographique et la source.
PA_LEGAL_ACT_LINK_CORRECTIONS <- tibble(
  num_texte = c("2015-758", "2015-711", "2015-750", "23548/2018", "2021-919"),
  wdpa_raw = c(555697908L, 555697917L, 352240L, 555549450L, 555549450L),
  WDPAID = c(555697875L, 555697911L, 555790237L, 555790172L, 555790172L),
  correction_basis = c(
    "l'objet et les annexes du décret conservés au corpus nomment « COMPLEXE LAC-FORET AMBONDROBE » (communes Andimaky et Aboalimena, district Belo sur Tsiribihina, région Menabe) ; le décret frère 2015-759 du même jour crée Loky Manambato (protectedareas.mg document 163248, JO n° 3685 du 23/05/2016)",
    "les annexes du décret 2015-711 du 21/04/2015 nomment « Manjakatompo Ankaratra » (communes Tsiafajavona et Sabotsy Namatoana, district d'Ambatolampy) ; le décret frère 2015-716 du même jour crée Marolambo (texte local et annexes D2015-716)",
    "le décret 2015-750 nomme « Analalava » dans la commune Amparihintsokatra, district d'Ambatondrazaka, région Alaotra Mangoro ; le polygone analytique 555790237 est dans la région Alaotra-Mangoro (~30 km d'Ambatondrazaka) alors que 352240 est dans la région Atsinanana à ~87 km (hors univers)",
    "l'arrêté 23548/2018 porte « mise en protection temporaire de l'Aire Protégée en création dénommée Ankafobe, district d'Ankazobe, région Analamanga » (texte local A23548-2018) ; la base dynamique place Ankafobe au WDPAID 555790172",
    "le décret 2021-919 du 15/09/2021 porte « création définitive de l'Aire Protégée dénommée Ankafobe, commune urbaine d'Ankazobe, district d'Ankazobe, région Analamanga » (texte local D2021-919-VF) ; la base dynamique place Ankafobe au WDPAID 555790172"
  )
)

# 2. Actes vérifiés absents du corpus, ajoutés par l'audit (preuves citées).
#    2015-782 : le corps du décret nomme les composants du complexe
#    Ambohimirahavavy-Marivorahona (RRN de COMATSA-Nord cat. VI, 237 883 ha ;
#    PHP de COMATSA-Sud cat. V, 80 204 ha ; Bemanevika ; Mahimborondro) et les
#    annexes 3 et 6 en détaillent les limites ; la couverture spatiale des
#    polygones analytiques par le complexe a été vérifiée (premiers états,
#    EPSG:29702) : 98,3 % (COMATSA Nord), 93,7 % (COMATSA Sud), 98,1 %
#    (Bemanevika), 98,2 % (Mahimborondro). 2015-791 (Andreba) : source
#    secondaire FAPBM, texte du décret non disponible (certitude « medium »).
#    2026-1529 (Tsinjoarivo) : postérieur à la dernière enquête 2021, il est
#    conservé pour la traçabilité (statut post_2021_decree, catchments
#    exclus) et jamais converti en date.
PA_LEGAL_EXTRA_ACTS <- tibble(
  WDPAID = c(
    555697908L,
    555697878L,
    555697904L,
    555697905L,
    555697906L,
    555697907L,
    555697903L,
    555790168L
  ),
  ap_nom = c(
    "Loky Manambato",
    "Forêt Naturelle de Tsitongambarika",
    "COMATSA Sud",
    "Mahimborondro",
    "Bemanevika",
    "COMATSA Nord",
    "Andreba",
    "Tsinjoarivo Ambalaomby"
  ),
  act_number = c(
    "2015-759",
    "2015-720",
    "2015-782",
    "2015-782",
    "2015-782",
    "2015-782",
    "2015-791",
    "2026-1529"
  ),
  act_date = as.Date(c(
    "2015-04-28",
    "2015-04-21",
    "2015-04-28",
    "2015-04-28",
    "2015-04-28",
    "2015-04-28",
    "2015-04-28",
    "2026-06-15"
  )),
  matching_certainty = c(
    "high",
    "high",
    "high",
    "high",
    "high",
    "high",
    "medium",
    "high"
  ),
  note = c(
    "décret n° 2015-759 du 28/04/2015 portant création de l'AP « Loky Manambato » (communes Ampasikinana, Maromokotra, Nosibe et Daraina, district de Vohémar, région SAVA) ; document officiel protectedareas.mg/document/show/163248 ; fiche protectedareas.mg/landscape/show/22 ; JO n° 3685 du 23/05/2016",
    "décret n° 2015-720 du 21/04/2015 portant création de l'AP « Forêt Naturelle de Tsitongambarika » (district Tolagnaro, région Anosy) ; texte local D2015-720 (signature « Antananarivo, le 21 Avril 2015 »)",
    "composant nommé dans le corps du décret collectif 2015-782 du 28/04/2015 (PHP de COMATSA-Sud, cat. V, 80 204 ha) et délimité aux annexes 3 et 6 ; couverture spatiale du polygone analytique par le complexe vérifiée : 93,7 % (premiers états, EPSG:29702)",
    "composant nommé dans le corps du décret collectif 2015-782 du 28/04/2015 et délimité aux annexes 3 et 6 (limites externes de Mahimborondro) ; couverture spatiale du polygone analytique par le complexe vérifiée : 98,2 % (premiers états, EPSG:29702)",
    "composant nommé dans le corps du décret collectif 2015-782 du 28/04/2015 et délimité aux annexes 3 et 6 (limites externes de Bemanevika) ; couverture spatiale du polygone analytique par le complexe vérifiée : 98,1 % (premiers états, EPSG:29702)",
    "composant nommé dans le corps du décret collectif 2015-782 du 28/04/2015 (RRN de COMATSA-Nord, cat. VI, 237 883 ha) et délimité aux annexes 3 et 6 ; couverture spatiale du polygone analytique par le complexe vérifiée : 98,3 % (premiers états, EPSG:29702)",
    "décret n° 2015-791 du 28/04/2015 pour Andreba (Analanjirofo, ~39 ha, gestion WCS) ; source secondaire FAPBM (fapbm.org/en/nature-conservation-in-madagascar-through-protected-areas), texte du décret non disponible ; la petite taille (39 ha) et la localisation Maroantsetra concordent avec le polygone analytique",
    "décret n° 2026-1529 du 15/06/2026 portant création définitive de l'AP de Tsinjoarivo Ambalaomby ; postérieur à la dernière enquête 2021 : hors fenêtre d'observation (statut post_2021_decree, catchments exclus) ; corroboré par Rainforest Trust (officialisation le 15/06/2026)"
  )
)

# 3. Notes d'audit pour les cas restant non résolus (preuve manquante
#    spécifique et implication pour l'éligibilité, documentées dans la
#    provenance) — aucun cas arbitré ici : la date reste manquante.
PA_LEGAL_AUDIT_UNRESOLVED_NOTES <- tibble(
  WDPAID = c(20283L, 555790169L),
  audit_note = c(
    "audit 2026-11 : le décret 2015-718 (21/04/2015) crée le complexe « Zones Humides Mahavavy Kinkony » (352248, pré-2008) ; chevauchement partiel (79 % du premier état du polygone analytique dans le complexe) et désignation distincte — « Kinkony Lake Hunting Reserve », état placeholder 2017-01-01 — non nommée par le décret (aucune mention de réserve de chasse dans le texte) ; preuve manquante : texte propre de la réserve de chasse ; catchments exclus tant que non résolu",
    "audit 2026-11 : seul l'arrêté 2645/2020 (protection temporaire, 29/01/2020) est vérifié ; aucun décret de création définitive localisé (CNLEGIS et web, novembre 2026 ; sources secondaires évoquent une officialisation en 2023, postérieure à la fenêtre) ; catchments exclus dans la variante permanente"
  )
)

# Helpers ------------------------------------------------------------------

# min() sur un sous-ensemble vide perd la classe Date et renvoie Inf
pa_min_date <- function(x) {
  if (!length(x)) {
    return(as.Date(NA))
  }
  r <- suppressWarnings(min(x, na.rm = TRUE))
  if (!is.finite(as.numeric(r))) as.Date(NA) else as.Date(r)
}

# Précision d'une date source : « year » si placeholder au 1er janvier,
# « day » sinon ; NA -> « none »
pa_date_precision <- function(d) {
  case_when(
    is.na(d) ~ "none",
    month(d) == 1 & day(d) == 1 ~ "year",
    TRUE ~ "day"
  )
}

# Cohorte effective : année calendaire + 1 si entrée après le 1er juin
# (même convention que survey_reference_date() dans 01 et que g_eff dans 07)
cohort_year <- function(d) {
  y <- as.integer(format(d, "%Y"))
  res <- rep(NA_integer_, length(d))
  idx <- !is.na(d)
  res[idx] <- y[idx] + as.integer(d[idx] > as.Date(sprintf("%d-06-01", y[idx])))
  res
}

# Registre des événements juridiques ---------------------------------------

#' Construire le registre des événements juridiques par AP de l'univers
#'
#' @param states états temporels de la base dynamique (sf ou data.frame),
#'   toutes zones confondues, avec WDPAID, NAME, DESIG, zone_type, valid_from
#' @param universe_ids WDPAIDs de l'univers de traitement (AP créées 2008-2021)
#' @param legal_texts corpus curatorisé ; NULL (défaut) => data/legal_texts.rds
#' @return tibble : une ligne par PA x événement (texte ou état de limite),
#'   avec classe d'événement, date (corrigée le cas échéant), caractère
#'   opératoire et qualifiant, certitude d'appariement et provenance.
build_pa_legal_register <- function(states, universe_ids, legal_texts = NULL) {
  if (is.null(legal_texts)) {
    legal_texts <- readRDS("data/legal_texts.rds")
  }
  universe_ids <- as.integer(universe_ids)

  # --- 1. États de limite de la base dynamique -----------------------------
  # Un état ne documente un acte juridique que s'il marque un CHANGEMENT de
  # désignation (premier état de l'AP, ou passage temporaire <-> définitif) ;
  # les états de continuation (révision géométrique sans changement de statut)
  # ne sont pas des événements. Les états de précision annuelle (placeholder
  # 01-01) et les états blanket/prorogation ne qualifient jamais.
  states_ext <- states |>
    sf::st_drop_geometry() |>
    filter(zone_type == "external_boundary") |>
    transmute(
      WDPAID = as.integer(WDPAID),
      pa_name = NAME,
      DESIG,
      act_date = as.Date(valid_from)
    ) |>
    # Correction documentée du placeholder 0000-01-01 d'Ambohimirahavavy
    # Marivorahona (555697871) — même correctif que pa_states_analysis dans 01
    mutate(
      act_date = if_else(
        WDPAID == 555697871L & act_date < as.Date("2015-01-01"),
        as.Date("2015-01-01"),
        act_date
      )
    ) |>
    filter(WDPAID %in% universe_ids, !is.na(act_date)) |>
    group_by(WDPAID) |>
    arrange(WDPAID, act_date, .by_group = TRUE) |>
    mutate(
      is_temp = DESIG == "Protection Temporaire",
      prev_temp = lag(is_temp),
      prev_desig = lag(DESIG),
      first_state = row_number() == 1
    ) |>
    ungroup() |>
    transmute(
      WDPAID,
      pa_name,
      event_source = "dynamic_wdpa.gpkg (état de limite)",
      act_type = NA_character_,
      act_number = NA_character_,
      act_date,
      date_precision = pa_date_precision(act_date),
      date_corrected = FALSE,
      type_decision = NA_character_,
      event_class = case_when(
        is_temp & act_date == PA_LEGAL_BLANKET_DATE ~
          "blanket_inferred_18633_2008",
        is_temp & act_date %in% PA_LEGAL_PROROG$act_date ~
          "prorogation_inferred",
        is_temp & (first_state | !prev_temp) ~ "site_temp_state",
        !is_temp & (first_state | prev_desig != DESIG) ~
          "definitive_state_no_text",
        TRUE ~ "continuation_state"
      ),
      qualifying = case_when(
        event_class == "site_temp_state" ~ date_precision == "day",
        event_class == "definitive_state_no_text" ~ date_precision == "day",
        TRUE ~ FALSE
      ),
      matching_certainty = case_when(
        event_class %in%
          c(
            "blanket_inferred_18633_2008",
            "prorogation_inferred"
          ) ~ "inferred_proxy",
        date_precision == "year" ~ "placeholder",
        TRUE ~ "high"
      ),
      note = ""
    ) |>
    filter(event_class != "continuation_state")

  # --- 2. Événements issus du corpus de textes curatorisé -------------------
  # Corrections de liens acte -> WDPAID appliquées AVANT le filtre de
  # l'univers (un acte mal rattaché peut sortir d'un WDPAID hors univers, p.
  # ex. 2015-750 lié à 352240) et avant toute dérivation de date. Les
  # enregistrements bruts du corpus sont conservés ; la correction est tracée
  # (note + preuve) et ne modifie jamais la date du texte.
  text_events <- legal_texts |>
    left_join(
      PA_LEGAL_ACT_LINK_CORRECTIONS |>
        transmute(
          num_texte,
          wdpa_raw,
          wdpaid_corrected = WDPAID,
          link_rationale = correction_basis
        ),
      by = "num_texte"
    ) |>
    mutate(
      link_corrected = !is.na(wdpaid_corrected) &
        as.integer(WDPAID) == wdpa_raw,
      WDPAID = if_else(
        link_corrected,
        as.integer(wdpaid_corrected),
        as.integer(WDPAID)
      )
    ) |>
    mutate(WDPAID = as.integer(WDPAID)) |>
    filter(WDPAID %in% universe_ids) |>
    transmute(
      WDPAID,
      pa_name = WDPA_NAME,
      event_source = "data/legal_texts.rds",
      act_type = type_texte,
      act_number = num_texte,
      act_date = as.Date(date_texte),
      date_precision = "day",
      type_decision,
      etat_texte,
      link_corrected,
      link_rationale
    ) |>
    # Corrections CNLEGIS documentées (correspondance exacte numéro + date)
    left_join(
      PA_LEGAL_DATE_CORRECTIONS |>
        transmute(
          act_number = num_texte,
          date_observed,
          date_corrected,
          correction_basis
        ),
      by = "act_number"
    ) |>
    mutate(
      is_corrected = !is.na(date_corrected) & act_date == date_observed,
      act_date = if_else(is_corrected, date_corrected, act_date)
    ) |>
    # Actes blanket et prorogations blanket : jamais qualifiants (l'objet
    # liste des noms mais l'annexe opposable est une carte ; appartenance
    # uniquement inférée par le proxy SAPM)
    mutate(
      is_blanket_text = act_number == PA_LEGAL_BLANKET_NUM |
        act_number %in% PA_LEGAL_PROROG$num_texte,
      is_abrogated = etat_texte == "Abrogé",
      operative = !is_blanket_text & !is_abrogated,
      event_class = case_when(
        !operative ~ case_when(
          is_blanket_text ~ "blanket_inferred_18633_2008",
          TRUE ~ "abrogated_text"
        ),
        str_detect(type_decision, "mise_protection_temporaire") ~
          "site_temp_arrete",
        str_detect(type_decision, "creation_definitive") &
          act_number == "2015-808" ~ "definitive_decree_collective",
        str_detect(type_decision, "creation_definitive") ~
          "definitive_decree",
        str_detect(type_decision, "delegation_gestion") ~
          "management_delegation",
        TRUE ~ "other"
      ),
      qualifying = operative &
        event_class %in%
          c(
            "site_temp_arrete",
            "definitive_decree",
            "definitive_decree_collective",
            "management_delegation"
          ),
      matching_certainty = case_when(
        is.na(pa_name) ~ "medium", # lien WDPAID sans nom stocké
        TRUE ~ "high"
      ),
      note = case_when(
        is_blanket_text ~
          "acte blanket ou prorogation blanket : appartenance inférée, jamais qualifiante",
        is_abrogated ~ "texte abrogé : non opératoire",
        is_corrected ~ paste("date corrigée :", correction_basis),
        TRUE ~ ""
      ),
      note = if_else(
        link_corrected,
        if_else(
          note == "",
          paste("lien AP-décret corrigé :", link_rationale),
          paste(note, "; lien AP-décret corrigé :", link_rationale)
        ),
        note
      )
    ) |>
    select(
      -date_observed,
      -date_corrected,
      -correction_basis,
      -link_corrected,
      -link_rationale,
      -etat_texte,
      -is_corrected,
      -is_blanket_text,
      -is_abrogated
    )

  # --- 2bis. Actes vérifiés absents du corpus (couche de correction) --------
  extra_events <- PA_LEGAL_EXTRA_ACTS |>
    filter(WDPAID %in% universe_ids) |>
    transmute(
      WDPAID,
      pa_name = ap_nom,
      event_source = "couche de correction documentée (audit des décrets PA, novembre 2026)",
      act_type = "Décret",
      act_number,
      act_date,
      date_precision = "day",
      date_corrected = FALSE,
      type_decision = "creation_definitive",
      etat_texte = NA_character_,
      event_class = "definitive_decree",
      qualifying = TRUE,
      matching_certainty,
      note
    ) |>
    select(-date_corrected, -etat_texte)

  # --- 3. Rapprochement par nom du décret collectif 2015-808 ----------------
  # Sites nommés dans l'objet du décret mais non rattachés à ce décret dans le
  # corpus : ajout d'un événement qualifiant à la date corrigée (2015-05-05),
  # certitude par lien (Angavo = « high » depuis l'audit 2026-11, voir la
  # carte ci-dessus).
  linked_808 <- text_events |>
    filter(act_number == "2015-808", operative) |>
    pull(WDPAID)
  extra808 <- PA_LEGAL_DECREE_808_MAP |>
    filter(!is.na(WDPAID), WDPAID %in% universe_ids, !WDPAID %in% linked_808) |>
    transmute(
      WDPAID,
      pa_name = NA_character_,
      event_source = "data/legal_texts.rds (rapprochement par nom, décret collectif 2015-808)",
      act_type = "Décret",
      act_number = "2015-808",
      act_date = as.Date("2015-05-05"),
      date_precision = "day",
      date_corrected = TRUE,
      type_decision = "creation_definitive",
      event_class = "definitive_decree_collective",
      qualifying = TRUE,
      matching_certainty = match_certainty,
      note = paste0(
        "nom dans le décret : « ",
        name_in_decree,
        " » ; ",
        match_note
      )
    )

  bind_rows(states_ext, text_events, extra_events, extra808) |>
    arrange(WDPAID, act_date, event_source)
}

# Dates de début de traitement par PA --------------------------------------

#' Dérivre, pour chaque PA de l'univers, les dates main / robustness /
#' permanent
#'
#' @param register registre produit par build_pa_legal_register()
#' @param pa_first_tbl table PA x {WDPAID, NAME, STATUS_YR, treatment_date}
#'   (premier état de la base dynamique = chronologie reconstituée T0)
#' @param window_end fin de la fenêtre d'observation (dernière enquête 2021) ;
#'   un décret de création définitive postérieur est signalé
#'   (`permanent_status = "post_2021_decree"`) mais ne produit pas de date
#' @return tibble une ligne par PA : date_main (premier jalon individuel,
#'   protection OU délégation de gestion, la plus précoce), date_protection
#'   (jalon de protection seul), date_mgmt (délégation seule),
#'   date_blanket_robustness (T0), date_permanent (premier décret de création
#'   définitive vérifiable), statuts, sources, certitudes et motifs de
#'   non-résolution des deux spécifications.
pa_main_onset_dates <- function(
  register,
  pa_first_tbl,
  window_end = as.Date("2021-12-31")
) {
  per_pa <- register |>
    filter(qualifying) |>
    group_by(WDPAID) |>
    summarise(
      date_protection = pa_min_date(act_date[
        event_class %in%
          c(
            "site_temp_arrete",
            "definitive_decree",
            "definitive_decree_collective"
          )
      ]),
      date_mgmt = pa_min_date(act_date[event_class == "management_delegation"]),
      .groups = "drop"
    ) |>
    mutate(
      # Règle principale : le plus précoce du jalon de protection
      # site-spécifique et de l'acte de délégation, à NA près. Une protection
      # site-spécifique antérieure n'est jamais remplacée par une délégation
      # postérieure (p. ex. Tsitongambarika).
      date_main = pmin(date_protection, date_mgmt, na.rm = TRUE)
    )

  # Source (acte) et classe du jalon retenu
  winning_event <- register |>
    filter(qualifying) |>
    group_by(WDPAID) |>
    group_modify(function(d, g) {
      dm <- pa_min_date(d$act_date)
      d |>
        filter(act_date == dm) |>
        # En cas d'égalité protection/délégation, privilégier la protection
        arrange(event_class == "management_delegation", event_source) |>
        slice(1) |>
        select(act_number, event_class, matching_certainty, event_source, note)
    }) |>
    ungroup() |>
    rename_with(
      ~ paste0("main_", .x),
      c(act_number, event_class, matching_certainty, event_source, note)
    )

  # --- Variante « création définitive uniquement » --------------------------
  # Première date : premier décret de création définitive vérifiable et
  # individuellement attribuable. Ne qualifient que les décrets du corpus ou
  # de la couche de correction (individuels ou sites nommés dans un décret
  # collectif) dont l'appariement est suffisamment documenté ; les
  # appariements « uncertain »/« not_matched » ne sont pas convertis en date
  # (aucun cas restant depuis l'audit de novembre 2026). Un état définitif de
  # la base dynamique sans texte
  # au corpus (definitive_state_no_text), STATUS_YR et le premier valid_from
  # ne sont jamais utilisés.
  perm_events <- register |>
    filter(
      qualifying,
      event_class %in% c("definitive_decree", "definitive_decree_collective"),
      !matching_certainty %in% c("uncertain", "not_matched")
    )

  perm_win <- perm_events |>
    group_by(WDPAID) |>
    arrange(act_date, act_number, .by_group = TRUE) |>
    group_modify(function(d, g) {
      dp <- pa_min_date(d$act_date)
      w <- d |> filter(act_date == dp)
      nts <- unique(w$note[!is.na(w$note) & w$note != ""])
      # Anomalie de cohérence signalée, non corrigée : numérotation
      # « YYYY-NNN » incompatible avec la date du corpus (p. ex. décret
      # « 2015-771 » d'Oronjia daté 28/04/2018)
      an <- unique(w$act_number[
        !is.na(w$act_number) &
          suppressWarnings(as.integer(str_sub(w$act_number, 1, 4))) !=
            as.integer(format(dp, "%Y"))
      ])
      if (length(an)) {
        nts <- c(
          nts,
          paste0(
            "numérotation ",
            paste(an, collapse = ", "),
            " incompatible avec la date du corpus (",
            format(dp, "%d/%m/%Y"),
            ") ; à inspecter"
          )
        )
      }
      tibble(
        permanent_candidate_date = dp,
        permanent_act_number = paste(unique(w$act_number), collapse = " / "),
        permanent_event_class = w$event_class[1],
        permanent_certainty = w$matching_certainty[1],
        permanent_register_note = if (length(nts)) {
          paste(nts, collapse = " ; ")
        } else {
          NA_character_
        }
      )
    }) |>
    ungroup()

  # Attribution ambiguë : appariement « uncertain » ou « not_matched » à un
  # décret de création définitive. Aucun cas restant depuis l'audit des
  # décrets PA (novembre 2026) : Angavo est arbitré « high » au décret
  # collectif 2015-808 (géographie du Grand Sud confirmée) et le mélien
  # Angavo/Ankafobe est résolu par la correction de liens (les textes
  # 23548/2018 et 2021-919 sont attribués à Ankafobe, 555790172).
  perm_ambiguous_ids <- register |>
    filter(
      qualifying,
      event_class %in% c("definitive_decree", "definitive_decree_collective"),
      matching_certainty %in% c("uncertain", "not_matched")
    ) |>
    pull(WDPAID) |>
    unique()

  # États « désignation définitive » de la base dynamique sans texte au
  # corpus : signalés dans le motif, jamais utilisés comme date
  perm_db_definitive_ids <- register |>
    filter(event_class == "definitive_state_no_text") |>
    pull(WDPAID) |>
    unique()

  pa_first_tbl |>
    transmute(
      WDPAID,
      NAME,
      STATUS_YR,
      date_blanket_robustness = as.Date(treatment_date),
      blanket_precision = pa_date_precision(date_blanket_robustness),
      blanket_attribution = date_blanket_robustness == PA_LEGAL_BLANKET_DATE
    ) |>
    left_join(per_pa, by = "WDPAID") |>
    left_join(winning_event, by = "WDPAID") |>
    left_join(perm_win, by = "WDPAID") |>
    mutate(
      main_source = case_when(
        is.na(date_main) ~ NA_character_,
        main_event_class == "site_temp_arrete" ~
          paste("arrêté de protection temporaire site-nommé", main_act_number),
        main_event_class == "definitive_decree" ~
          paste("décret de création définitive", main_act_number),
        main_event_class == "definitive_decree_collective" ~
          paste(
            "décret collectif (site nommé dans le corps du texte) :",
            main_act_number
          ),
        main_event_class == "management_delegation" ~
          paste("acte de délégation de gestion", main_act_number),
        main_event_class == "definitive_state_no_text" ~
          "état de désignation définitive de la base dynamique (texte absent du corpus local)"
      ),
      main_certainty = main_matching_certainty,
      unresolved_main = is.na(date_main),
      unresolved_reason = case_when(
        !unresolved_main ~ NA_character_,
        blanket_attribution ~
          "protection temporaire du 17/10/2008 attribuée uniquement via le proxy SAPM ; aucun acte individuel nommé dans le corpus local",
        blanket_precision == "year" ~
          "date de la source = placeholder annuel (01-01) ; aucun acte individuel nommé dans le corpus local",
        TRUE ~ "aucun événement individuel qualifiant dans le corpus local"
      ),
      # Cohortes effectives (convention du 1er juin, identique à 07)
      cohort_blanket = cohort_year(date_blanket_robustness),
      cohort_main = cohort_year(date_main),
      # --- Variante « création définitive uniquement » ----------------------
      permanent_status = case_when(
        !is.na(permanent_candidate_date) &
          permanent_candidate_date > window_end ~ "post_2021_decree",
        !is.na(permanent_candidate_date) ~ "resolved",
        WDPAID %in% perm_ambiguous_ids ~ "ambiguous_attribution",
        TRUE ~ "no_verified_decree"
      ),
      # date_permanent reste manquant pour toute AP non résolue dans la
      # fenêtre ; la date brute du décret trouvé reste dans
      # permanent_candidate_date (traçabilité des cas post-fenêtre)
      date_permanent = if_else(
        permanent_status == "resolved",
        permanent_candidate_date,
        as.Date(NA)
      ),
      permanent_source = case_when(
        is.na(permanent_candidate_date) ~ NA_character_,
        permanent_event_class == "definitive_decree_collective" ~
          paste(
            "décret collectif",
            permanent_act_number,
            "(site nommé dans l'objet)"
          ),
        TRUE ~ paste("décret de création définitive", permanent_act_number)
      ),
      permanent_unresolved_reason = case_when(
        permanent_status == "post_2021_decree" ~
          paste0(
            "décret de création définitive daté ",
            format(permanent_candidate_date, "%d/%m/%Y"),
            ", après la dernière enquête 2021 : hors fenêtre d'observation, ",
            "catchments exclus"
          ),
        permanent_status == "ambiguous_attribution" ~
          paste(
            "attribution du décret de création définitive ambiguë :",
            "appariement « uncertain » ou « not_matched » à un décret",
            "collectif ; à arbitrer par l'équipe, aucune date déduite"
          ),
        permanent_status == "no_verified_decree" ~
          paste0(
            "aucun décret de création définitive individuellement attribuable ",
            "dans le corpus local",
            ifelse(
              WDPAID %in% perm_db_definitive_ids,
              paste(
                " ; la base dynamique documente une désignation définitive",
                "sans texte au corpus (non utilisée comme date)"
              ),
              ""
            )
          ),
        TRUE ~ NA_character_
      ),
      unresolved_permanent = permanent_status != "resolved",
      permanent_note = permanent_register_note,
      cohort_permanent = cohort_year(date_permanent)
    ) |>
    left_join(PA_LEGAL_AUDIT_UNRESOLVED_NOTES, by = "WDPAID") |>
    mutate(
      unresolved_reason = if_else(
        unresolved_main & !is.na(audit_note),
        paste(unresolved_reason, audit_note, sep = " ; "),
        unresolved_reason
      ),
      permanent_unresolved_reason = if_else(
        unresolved_permanent & !is.na(audit_note),
        paste(permanent_unresolved_reason, audit_note, sep = " ; "),
        permanent_unresolved_reason
      )
    ) |>
    select(-audit_note)
}
