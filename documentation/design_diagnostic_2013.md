# Diagnostic design 2013 — cardinality matching et entropy balancing

**Statut : diagnostic outcome-blind uniquement.** Aucune variable d'outcome
(riche `hv271`, `zscore_wealth`, centiles) n'a été chargée ; aucun placebo
1997–2008 ; aucune estimation d'effet. Le design de production
(`06-matching.qmd`, `data_matched_*.rds`, `matching_result_*.rds`) n'est pas
modifié. Les pretrends ne seront évalués qu'après gel de la règle de design.

Script : `scripts/design_diagnostic_2013.R` (reproductible, graines fixes).

## A. Configuration courante

- Branche `florent_review_v2`, HEAD `6b02ce4` (contain les commits review-v2 attendus).
- Versions : R 4.5.1, MatchIt 4.7.2, cobalt 5.0.0, WeightIt 2.1.0, highs 1.14.0-2,
  Rglpk 0.6-5.1 (présent, non utilisé).
- Dépendances installées pour ce diagnostic uniquement :
  - `highs` (solveur par défaut de `method = "cardinality"`, documenté dans MatchIt) ;
  - `WeightIt` (implémentation documentée de l'entropy balancing).
  - **Effet collateral à noter** : `WeightIt` 2.1.0 requiert `cobalt (>= 5.0.0)` ;
    cobalt a été mis à jour de 4.6.1 vers 5.0.0 lors de l'installation (upgrade
    forcé par la dépendance, non optionnel). La définition du SMD (standardisation
    par l'écart-type du groupe traité sous ATT) est inchangée entre les deux
    versions. Aucun autre paquet core (MatchIt, Matching) n'a été modifié.
- Échantillon éligible 2013 (construction identique à `06-matching.qmd` :
  `GROUP ∈ {Treatment, Control}`, drop NA sur les 5 covariables) :
  **724 traités, 4 455 contrôles (5 179 ménages)** — conforme à la référence.
- Confirmation : **aucune variable d'outcome utilisée**.

## B. Cardinality matching (benchmark principal)

### Formulation exacte

`matchit(method = "cardinality", estimand = "ATT", ratio = 1, tols = tol,
std.tols = TRUE, solver = "highs", time = 1800)` sur les cinq covariables PAP.

- L'objectif du programme mixte est bien de **maximiser le nombre d'observations
  retenues** sous contraintes de balance `|SMD_k| <= tol` (MILP : 12 lignes,
  5 180 colonnes binaires ; documentation MatchIt 4.7.2 vérifiée, pas supposée).
- `std.tols = TRUE` avec `estimand = "ATT"` standardise par l'écart-type du
  groupe **traité** — même convention que `cobalt::bal.tab(estimand = "ATT")`
  utilisée comme référence dans 06 (vérifié : les SMD MatchIt et cobalt coïncident).
- 1:1 traité-contrôle (`ratio = 1`), sans remplacement (la sélection est par
  construction sans remplacement).
- Sélection et appariement sont séparés : `mahvars` de MatchIt exige `optmatch`
  (licence restreinte, non installé) ; un appariement 1:1 post-hoc (distance de
  Mahalanobis, covariance pooled) est trivialment faisable au sein de l'ensemble
  sélectionné (724/724) et ne change ni la composition ni la balance (sémantique
  documentée de `mahvars`). L'appariement n'entre dans aucun critère de design.

### Résultat à la tolérance préenregistrée SMD <= 0.10

| Quantité | Valeur |
|---|---|
| Traités retenus | **724 / 724 (100 %)** |
| Contrôles retenus | 724 |
| Max SMD post-appariement | 0.0999 |
| Covariable la plus déséquilibrée | `elevation_2000` (contrainte saturée) |
| Statut solveur | Optimal (HiGHS), ~0.2 s |

SMD par covariable (convention cobalt ATT) : treecover 0.0465, slope 0.0993,
elevation 0.0999, population_count 0.0975, traveltime 0.0975 — tous <= 0.10.

**Aucun ménage/cluster/PA traité n'est exclu** (voir
`cardinality_2013_excluded_treated.csv`, vide par construction).

### Frontière balance–rétention (diagnostique, pas du specification fishing)

| Tolérance SMD | Traités retenus | Rétention | Max SMD | Covar. limite |
|---|---|---|---|---|
| 0.05 | 724 | 100 % | 0.0492 | traveltime |
| 0.075 | 724 | 100 % | 0.0738 | traveltime |
| **0.10 (PAP)** | **724** | **100 %** | **0.0999** | elevation |
| 0.125 | 724 | 100 % | 0.1240 | slope |
| 0.15 | 724 | 100 % | 0.1500 | elevation |

La frontière est **plate** : même à une tolérance deux fois plus stricte que le
critère préenregistré, tous les traités sont retenus. Tous les solveurs ont
convergé vers l'optimum (statut « Optimal »).

## C. Entropy balancing (benchmark secondaire)

`WeightIt::weightit(method = "entropy", estimand = "ATT")` (WeightIt 2.1.0),
premiers moments des cinq covariables uniquement (pas de moments supérieurs ni
d'interactions). Poids de design uniquement — aucun poids d'enquete DHS dans le
diagnostic principal.

| Quantité | Valeur |
|---|---|
| Traités | 724 (tous retenus, poids = 1) |
| Contrôles à poids positif | 4 455 / 4 455 |
| Max SMD post-pondération | 0.0002 (balance quasi exacte) |
| Somme des poids contrôles | 724 (= N traités) |
| **ESS contrôles** | **3 117 (70 % des 4 455)** |
| Poids max contrôles | 0.427 (moyenne 0.162 ; ratio 2.6) |
| P99 poids contrôles | 0.388 |
| CV des poids contrôles | 0.655 |
| Part top 1 % / 5 % / 10 % | 2.6 % / 11.2 % / 21.0 % |

Les poids sont **opérationalement sains** : pas de concentration pathologique.

Diagnostic supplémentaire (étiqueté comme tel) : poids entropy × poids d'enquête
DHS (`s.weights = hv005`) : max SMD ≈ 0.00003, ESS contrôles 1 504, poids max 1.30.

## D. Comparaison des quatre stratégies (2013)

| Stratégie | Traités retenus | Rétention | Contrôles | Max SMD | Covar. limite | ESS contrôles |
|---|---|---|---|---|---|---|
| A. GenMatch sans caliper (production, pré-8b02041) | 724 | 100 % | 724 | 0.2535 | population_count | — |
| B. GenMatch caliper 0.25 SD (production, 8b02041) | 185 | 25.6 % | 185 | 0.0741 | population_count | — |
| C. Cardinality SMD <= 0.10 (MatchIt + HiGHS) | 724 | 100 % | 724 | 0.0999 | elevation | 724 |
| D. Entropy balancing ATT (WeightIt) | 724 | 100 % | 4 455 (poids > 0) | 0.0002 | population_count | 3 117 |

(La ligne A provient de `output/review_v2/matching_summary.csv` d'avant le
commit 8b02041, cf. `git show ffdf53f:output/review_v2/matching_summary.csv`.)

## E. Stabilité du design (pas un bootstrap d'inference)

Sous-échantillonnage **au niveau cluster** (jamais au niveau ménage) :
50 répétitions, 80 % des clusters tirés sans remplacement dans chaque strate
traité/contrôle, tous les ménages d'un cluster conservés ; graines
déterministes (20261005 + i). Cardinality résolue à tolérance 0.10 à chaque
répétition ; aucune erreur solveur (50/50).

| Quantité | Médiane | P5 | P95 |
|---|---|---|---|
| Cardinality : rétention traités (%) | 100 | 100 | 100 |
| Cardinality : max SMD | 0.100 | 0.096 | 0.100 |
| Cardinality : clusters traités retenus | 19 | 19 | 19 |
| Cardinality : PAs traités distincts | 14 | 12 | 15 |
| Entropy : max SMD | 0.000 | 0.000 | 0.000 |
| Entropy : ESS contrôles | 2 438 | 1 998 | 2 705 |
| Entropy : poids max contrôles | 0.485 | 0.358 | 0.758 |
| Entropy : part top 5 % | 0.124 | 0.101 | 0.168 |

Plages observées (50 reps) : cardinality max SMD ∈ [0.0675 ; 0.1000] — jamais
au-dessus de 0.10 ; entropy poids max ∈ [0.30 ; 0.82]. La solution de design
est **stable** à des modifications modérées de l'échantillon observé.

## F. Interprétation

1. **Oui** : la cardinality matching retient **724/724** traités (et non ~185)
   en satisfaisant SMD <= 0.10, et même SMD <= 0.05.
2. Le problème 2013 est donc **d'abord un problème d'algorithme** (l'interaction
   caliper × recherche génétique, qui écarte des paires au-delà de 0.25 SD par
   covariable), **pas une limite de support commun** : un sous-ensemble de
   contrôles équilibré au sens des moyennes existe pour la totalité des traités.
3. L'entropy balancing offre une voie crédible pour garder tous les traités :
   balance quasi exacte, ESS contrôles de 3 117, poids bornés et peu concentrés.
   C'est un signal supplémentaire que l'overlap global n'est pas le
   contraignant — sous réserve que le support *individuel* (paires) reste
   limité, ce que la cardinality quantifie à 724 contrôles utilisables.
4. Pour une analyse de robustesse multi-vagues ultérieure, la famille la plus
   prometteuse est cardinality matching (garantie de balance + rétention
   maximale, solveur rapide et déterministe), avec l'entropy balancing en
   benchmark de repondération. Les deux préservent une lecture ATT
   (traités tous retenus).
5. Changements d'estimand/support à documenter si le design évolue :
   - sous B (caliper), l'estimand n'est plus l'ATT sur les 724 mais sur les
     185 traités retenus (sous-population sélectionnée) ;
   - sous C/D, les 724 traités sont conservés ; C n'apparie pas au niveau
     ménage (inférence cluster-robuste requise), D repondère (robust SE requis).
   - Dans tous les cas, la sélection est basée uniquement sur les covariables.

## Fichiers créés

- `scripts/design_diagnostic_2013.R`
- `data/derived/design_diagnostics/cardinality_2013_tol0p10.rds`
- `data/derived/design_diagnostics/entropy_2013.rds`
- `output/review_v2/design_diagnostics/cardinality_2013_summary.csv`
- `output/review_v2/design_diagnostics/cardinality_2013_balance.csv`
- `output/review_v2/design_diagnostics/cardinality_2013_excluded_treated.csv`
- `output/review_v2/design_diagnostics/cardinality_balance_retention_frontier_2013.csv`
- `output/review_v2/design_diagnostics/entropy_2013_summary.csv`
- `output/review_v2/design_diagnostics/entropy_2013_balance.csv`
- `output/review_v2/design_diagnostics/entropy_2013_weight_diagnostics.csv`
- `output/review_v2/design_diagnostics/design_comparison_2013.csv`
- `output/review_v2/design_diagnostics/cardinality_stability_2013.csv`
- `output/review_v2/design_diagnostics/entropy_stability_2013.csv`

## Prochaine étape (gel de design, puis pretrends)

STOP ici conformément au protocole : la règle de design n'est pas figée, le
placebo 1997–2008 et les modèles d'effet ne sont PAS lancés. Séquence prévue :
(1) gel de la règle sur la base balance/rétention/stabilité uniquement ;
(2) application aux vagues pertinentes ; (3) seulement ensuite, placebo
1997–2008, sans re-tuning a posteriori.
