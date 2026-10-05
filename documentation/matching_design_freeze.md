# Design freeze — ATT profile cardinality matching

Date de gel : 2026-10-05 (avant toute inspection d'outcome sous ce design).
Branche : `florent_review_v2`. HEAD diagnostique au moment du gel : `b2e9be5`.

Ce document fige, **avant toute estimation d'effet**, la regle de matching de
production pour `06-matching.qmd`. Le placebo 1997–2008 puis l'effet principal
2008–2021 seront estimes sur ce design fige, sans retouche.

---

## 1. Engagements du PAP (préenregistrement)

Le PAP preregistre explicitement :

- Genetic Matching / appareillement au plus proche voisin (nearest-neighbour) ;
- distance de Mahalanobis ;
- logique de caliper ;
- seuil d'equilibre SMD <= .10 ;
- elargissement du caliper si SMD > .10 ;
- possibility d'exclure les observations non appariées ;
- combinaison des poids de matching/design avec les poids DHS d'enquete ;
- **aucun ratio fixe 1:1 ni regle de remplacement n'est specifie** dans le PAP.

## 2. Raison outcome-blind de la deviation

1. Le Genetic Matching sans caliper satisfaisait SMD <= .10 dans six vagues
   mais echouait en 2013.
2. L'implementation tentee d'un caliper marginal de 0.25 provoquait une
   attrition severe des traites et un mauvais equilibre dans plusieurs vagues.
3. Les diagnostics cardinality et entropy ont demontre que le support complet
   des traites etait faisable (diagnostics `design_diagnostic_2013.R` et
   `design_diagnostic_allwaves.R`).
4. Le profile matching ATT retient tous les traites et maximise le
   sous-echantillon de controles equilibre, en imposant directement le critere
   SMD <= .10 preregistre, avec les cinq covariables de matching du PAP.
5. Le choix a ete fait **avant** d'avoir inspecte le placebo ou tout outcome de
   traitement sous ce design.

## 3. Regle figee (production)

```r
matchit(
  treatment ~ treecover_area_2000 +
              slope_2000 +
              elevation_2000 +
              population_count_2000 +
              traveltime_2000_2000,
  data = dat_m,
  method = "cardinality",
  estimand = "ATT",
  ratio = NA,
  tols = 0.095,      # FINAL_PROFILE_TOL
  std.tols = TRUE,
  solver = "highs"
)
```

Avec :

- critere substantiel d'equilibre externe : `max |SMD| <= .10`
  (cobalt, convention ATT : standardisation par l'ecart-type du groupe traite
  eligible ; verifiee identique au workflow precedent a 1e-6 pres) ;
- exactement les cinq covariables de matching du PAP ;
- matching realise separement par vague d'enquete
  (1997, 2008, 2011, 2013, 2016, 2018, 2021) ;
- toutes les observations traitees retenues (retention 100% exige par vague,
  statut solveur optimal exige) ;
- meme population eligible que le `06-matching.qmd` courant :
  `GROUP %in% c("Treatment","Control")`, `treatment` derive de `GROUP`,
  `drop_na(all_of(matching_variables))` ;
- semantique de traitement inchangee : `GROUP` = geographie de traitement
  eventuelle, `treated_now` = traitement actif a la date de reference
  (1er juin de l'annee d'enquete).

**Le role de `tols = 0.095` est uniquement une garde numerique** : elle garantit
que la solution retenue satisfait la borne externe de .10 avec une marge, car a
`tols = .100` plusieurs vagues se plaçaient exactement a la borne
(max |SMD| = 0.0999–0.1000). Le critere substantiel reste `max |SMD| <= .10`.

## 4. Statut methodologique

Le profile cardinality matching ATT est une **deviation methodologique
documentee du plan preregistre, pas un estimateur preregistre**. Le PAP n'a
jamais preregistre cette methode ; le PAP n'a pas non plus exigé de ratio 1:1.

Reserve a des analyses de robustesse ulterieures uniquement (pas le design
principal) :

- cardinality 1:1 ;
- entropy balancing ATT.

## 5. Evidence diagnostique outcome-blind ayant fonde le gel

Script : `scripts/profile_tolerance_diagnostic.R` (commite avec ce document).
Sorties : `output/review_v2/profile_tolerance_diagnostic/*.csv`.

### A. Test de garde .100 vs .095 sur les deux vagues les plus fragiles

| vague | tols | traites retenus | controles retenus | max \|SMD\| externe | statut |
|-------|------|-----------------|-------------------|---------------------|--------|
| 1997  | .100 | 335/335 (100%)  | 2946 (79.6%)      | 0.099842            | optimal |
| 1997  | .095 | 335/335 (100%)  | 2919 (78.9%)      | 0.094998            | optimal |
| 2011  | .100 | 441/441 (100%)  | 1929 (41.8%)      | 0.099993            | optimal |
| 2011  | .095 | 441/441 (100%)  | 1910 (41.4%)      | 0.094998            | optimal |

Controle croise manuel du pire SMD, `(mean_t - mean_c) / SD_treated_eligible`,
identique a cobalt (|diff| < 1e-6) pour chaque run full-sample.

### B. Stabilite de design (1997 et 2011 ; 20 repetitions ; 80% des clusters
echantillonnes sans remplacement au sein de chaque strate ; graine
deterministe 4821)

| vague | tols | runs | max \|SMD\| > .10 | mean max \|SMD\| | max max \|SMD\| | retention traites min |
|-------|------|------|-------------------|------------------|-----------------|-----------------------|
| 1997  | .095 | 20   | 0/20              | 0.0950           | 0.095           | 100%                  |
| 1997  | .100 | 20   | 0/20              | 0.1000           | 0.100           | 100%                  |
| 2011  | .095 | 20   | 0/20              | 0.0949           | 0.095           | 100%                  |
| 2011  | .100 | 20   | 0/20              | 0.0999           | 0.100           | 100%                  |

A `.100`, les solutions se plaacent systematiquement sur la borne (moyenne
0.0999–0.1000) ; a `.095`, aucune violation externe de .10 et une marge
systematique. Le cout en controles perdus est marginal (0.4–0.7 pp).

### C. Verification de `.095` sur les sept vagues (full sample)

| vague | traites eligibles/retenus | controles retenus | retention controles | max \|SMD\| externe | statut |
|-------|---------------------------|-------------------|---------------------|---------------------|--------|
| 1997  | 335/335 (100%)            | 2919              | 78.9%               | 0.094998            | optimal |
| 2008  | 1155/1155 (100%)          | 6323              | 65.2%               | 0.095000            | optimal |
| 2011  | 441/441 (100%)            | 1910              | 41.4%               | 0.094998            | optimal |
| 2013  | 724/724 (100%)            | 3175              | 71.3%               | 0.094968            | optimal |
| 2016  | 637/637 (100%)            | 4296              | 59.9%               | 0.094999            | optimal |
| 2018  | 1194/1194 (100%)          | 6799              | 63.3%               | 0.094999            | optimal |
| 2021  | 1601/1601 (100%)          | 5922              | 55.2%               | 0.094999            | optimal |

Les quatre conditions de gel de `.095` sont satisfaites :

1. faisable en 1997 et 2011 (statut optimal) ;
2. retention 100% des traites dans les deux vagues ;
3. max |SMD| externe <= .10 full-sample dans les deux vagues ;
4. aucune violation systematique externe de .10 dans les runs de stabilite.

### Decision

```text
FINAL_PROFILE_TOL = 0.095
```

## 6. Confirmation

Aucun outcome (placebo 1997–2008 ou effet principal 2008–2021) n'a ete inspecte
avant le commit de ce gel. Le placebo ne sera execute qu'apres l'implementation
de production (commit separe) et ne pourra pas retroagir sur ce design.
