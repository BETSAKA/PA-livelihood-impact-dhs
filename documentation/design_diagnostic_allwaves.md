# Diagnostic design toutes vagues — comparaison de designs équilibrés avant gel de la règle de matching

**Statut : diagnostic outcome-blind. Aucun modèle d'outcome, aucun placebo DiD, aucun coefficient d'event-study, aucun estimateur `did` n'a été exécuté. Le design de production (`06-matching.qmd`, `data/derived/matching_result_*.rds`, `data/derived/data_matched_*.rds`, `output/review_v2/matching_summary.csv`) n'a pas été modifié ni remplacé.**

Script : `scripts/design_diagnostic_allwaves.R` (reproductible, déterministe ; checkpoints RDS dans `data/derived/design_diagnostics_allwaves/`).

---

## A. Mise en place

### 1. Branche et HEAD

- Branche : `florent_review_v2` (inchangée) ; 8 commits d'avance sur `origin/florent_review_v2`, non poussés.
- HEAD avant diagnostic : `a9d2ceb diagnose(06): benchmark cardinality and entropy balancing for 2013` — présent et vérifié.
- Modifications préexistantes non liées (staging area sale) : **non touchées**, non commitées.

### 2. Versions de paquets

| package | version |
|---|---|
| R | 4.5.1 |
| MatchIt | 4.7.2 |
| cobalt | 5.0.0 |
| WeightIt | 2.1.0 |
| highs | 1.14.0.2 |
| Matching | 4.10.15 |

Aucune mise à niveau de paquet. Pas d'`renv` initialisé (le projet n'en utilise pas).

**Vérification de convention cobalt 5.0.0** : sur la référence 2013 (`data/derived/design_diagnostics/cardinality_2013_tol0p10.rds`, commit `a9d2ceb`), les SMD ATT de `cobalt::bal.tab(..., un = TRUE)$Balance$Diff.Un` sont identiques (écart < 1e-6, les 5 covariables) au calcul manuel `|m_t − m_c| / sd(traités éligibles)` du workflow précédent. Convention confirmée, pas d'arrêt.

### 3. Sémantique MatchIt vérifiée (documentation installée 4.7.2, `?method_cardinality`)

- `estimand = "ATT"` : cible = moyenne des traités ; standardisation des tolérances par l'écart-type du groupe traité.
- Cardinality 1:1 : `ratio = 1` (entier strictement positif) ; nombre de contrôles retenus = nombre de traités retenus ; sans remplacement par construction.
- **Profile matching ATT : `estimand = "ATT"` et `ratio = NA`** — le plus grand ensemble équilibré avec **tous les traités fixés comme cible** ; vérifié dans la doc, pas supposé.
- `tols = 0.10, std.tols = TRUE` : contrainte |SMD_k| ≤ 0.10 pour les 5 covariables.
- `solver = "highs"` (recommandé par la doc, installé). Statut solver enregistré par capture des warnings : **« optimal » pour les 14 résolutions plein échantillon**.

### 4. Échantillon éligible par vague (construction identique à `06-matching.qmd`)

`GROUP ∈ {Treatment, Control}` → `treatment = 1L/0L` → `drop_na(matching_variables)` ; mêmes 5 covariables PAP (`treecover_area_2000`, `slope_2000`, `elevation_2000`, `population_count_2000`, `traveltime_2000_2000`) ; source `data/derived/hr_{year}_final.rds`.

| vague | traités éligibles | contrôles éligibles |
|---|---|---|
| 1997 | 335 | 3 699 |
| 2008 | 1 155 | 9 697 |
| 2011 | 441 | 4 617 |
| 2013 | 724 | 4 455 |
| 2016 | 637 | 7 170 |
| 2018 | 1 194 | 10 731 |
| 2021 | 1 601 | 10 737 |

(2013 = 724/4 455 : identique à la référence du diagnostic précédent.)

---

## B. Diagnostic A — Cardinality 1:1, SMD ≤ 0.10 (`cardinality_1to1_summary.csv`)

### 5–6. Rétention des traités et SMD max

| vague | traités retenus | contrôles retenus | rétention traités | rétention contrôles | SMD max | pire covariable | runtime (s) |
|---|---|---|---|---|---|---|---|
| 1997 | 335 | 335 | **100 %** | 9,1 % | 0,0998 | elevation_2000 | 0,09 |
| 2008 | 1 155 | 1 155 | **100 %** | 11,9 % | 0,0999 | traveltime_2000_2000 | 0,29 |
| 2011 | 441 | 441 | **100 %** | 9,6 % | 0,0999 | elevation_2000 | 0,26 |
| 2013 | 724 | 724 | **100 %** | 16,2 % | 0,0999 | elevation_2000 | 0,15 |
| 2016 | 637 | 637 | **100 %** | 8,9 % | 0,0890 | elevation_2000 | 0,33 |
| 2018 | 1 194 | 1 194 | **100 %** | 11,1 % | 0,0779 | population_count_2000 | 0,32 |
| 2021 | 1 601 | 1 601 | **100 %** | 14,9 % | 0,0892 | elevation_2000 | 0,34 |

### 7. Pire covariable

`elevation_2000` dans 5 vagues sur 7 ; la contrainte sature à la borne (SMD ≈ 0,099–0,100) — comportement attendu d'une optimisation de cardinalité qui maximise N sous contrainte.

### 8. Représentation des AP traitées

`cardinality_1to1_excluded_treated.csv` : **0 ligne — aucun traité exclu dans aucune vague**. Par construction, chaque AP traitée garde donc au moins ses ménages retenus (9 à 34 AP traitées par vague, toutes représentées ; cf. `treated_pa_composition.csv`).

### 9. Concentration de clusters contrôles (`control_cluster_concentration.csv`)

| vague | clusters contrôles | ménages/cluster (méd ; max) | part top-1 | part top-5 | HHI | ESS clusters |
|---|---|---|---|---|---|---|
| 1997 | 13 | 23 ; 52 | 0,155 | 0,552 | 0,093 | 10,8 |
| 2008 | 41 | 31 ; 32 | 0,028 | 0,138 | 0,026 | 38,8 |
| 2011 | 24 | 20,5 ; 32 | 0,073 | 0,363 | 0,063 | 15,9 |
| 2013 | 28 | 31 ; 32 | 0,044 | 0,221 | 0,041 | 24,3 |
| 2016 | 23 | 31 ; 34 | 0,053 | 0,254 | 0,047 | 21,2 |
| 2018 | 49 | 26 ; 26 | 0,022 | 0,109 | 0,021 | 46,7 |
| 2021 | 54 | 33 ; 34 | 0,021 | 0,106 | 0,020 | 50,8 |

À titre de comparaison, la référence production (GenMatch + caliper 0.25) est **beaucoup plus concentrée** : 3–21 clusters contrôles, part top-1 jusqu'à 39,7 % (1997), top-5 = 100 % dans 4 vagues, ESS clusters 2,9–17,3.

---

## C. Diagnostic B — Profile matching ATT, SMD ≤ 0.10 (`profile_att_summary.csv`)

### 10–13. Rétention et concentration

| vague | traités retenus | contrôles retenus | rétention contrôles | SMD max | pire covariable | clusters contrôles | ESS clusters |
|---|---|---|---|---|---|---|---|
| 1997 | 335 | 2 946 | 79,6 % | 0,0998 | slope_2000 | 89 | 76,9 |
| 2008 | 1 155 | 6 356 | 65,6 % | 0,0999 | treecover_area_2000 | 214 | 212 |
| 2011 | 441 | 1 929 | 41,8 % | 0,1000 | traveltime_2000_2000 | 66 | 63,7 |
| 2013 | 724 | 3 185 | 71,5 % | 0,0998 | treecover_area_2000 | 105 | 103 |
| 2016 | 637 | 4 332 | 60,4 % | 0,0999 | population_count_2000 | 143 | 138 |
| 2018 | 1 194 | 6 837 | 63,7 % | 0,1000 | traveltime_2000_2000 | 264 | 263 |
| 2021 | 1 601 | 5 967 | 55,6 % | 0,1000 | elevation_2000 | 191 | 188 |

Tous les traités sont retenus dans toutes les vagues (cible ATT fixée) ; runtime 0,13–0,47 s ; statut « optimal » partout.

---

## D. Diagnostic C — Entropy balancing ATT, premiers moments (`entropy_summary.csv`)

### 14–16. Équilibre et poids

| vague | SMD max | pire covariable | ESS contrôles | poids max | p99 | CV | part top-5 % | part top-10 % |
|---|---|---|---|---|---|---|---|---|
| 1997 | 1,4e-05 | elevation_2000 | 2 708 | 0,335 | — | — | 0,139 | 0,235 |
| 2008 | 7e-06 | treecover_area_2000 | 6 358 | 0,444 | — | — | 0,137 | 0,251 |
| 2011 | 9e-06 | traveltime_2000_2000 | 1 729 | 0,665 | — | — | 0,258 | 0,430 |
| 2013 | 2,3e-04 | population_count_2000 | 3 117 | 0,428 | — | — | 0,112 | 0,210 |
| 2016 | 1,9e-04 | population_count_2000 | 3 746 | 0,507 | — | — | 0,177 | 0,302 |
| 2018 | 0 | elevation_2000 | 6 292 | 0,674 | — | — | 0,168 | 0,283 |
| 2021 | 2,1e-05 | population_count_2000 | 4 955 | 1,240 | — | — | 0,220 | 0,353 |

(p99 et CV par vague dans `entropy_weight_diagnostics.csv`.) Tous les traités retenus ; tous les contrôles ont un poids > 0. ESS contrôles = 37 %–70 % des contrôles éligibles ; poids max ≤ 1,24 ; concentration modérée (part top-5 % ≤ 0,26).

### 17. Concentration cluster pondérée

ESS cluster pondéré (1/HHI des poids agrégés par `hv001`) : 70,6 / 213 / 57,3 / 99,8 / 118 / 242 / 155 ; part du top-1 cluster ≤ 3,9 % dans toutes les vagues. Aucune concentration géographique anormale.

---

## E. Équilibre au-delà des moyennes (`distributional_balance.csv`)

Définitions documentées : SMD cobalt ATT (convention vérifiée) ; variance ratio cobalt (`V.Ratio.Adj`, traités/contrôles) ; eCDF calculés exactement (pondérés pour entropy) en chaque valeur observée poolée — `ecdf_max` = KS-like, `ecdf_mean` = moyenne des écarts.

### 18–19. Pires écarts par méthode (max sur les 5 covariables et les 7 vagues)

| méthode | max \|VR − 1\| | max eCDF-mean | max eCDF-max |
|---|---|---|---|
| A0 GenMatch + caliper 0.25 (production) | 3,46 (1997) | 0,400 (1997) | 0,712 (1997) |
| B Cardinality 1:1 | 2,04 (1997) ; 1,91 (2013) | 0,174 (1997) | 0,349 (2011) |
| C Profile ATT | 1,37 (2011) | 0,140 (2011) | 0,351 (2011) |
| D Entropy ATT | 1,44 (2013) | 0,101 (2018) | 0,273 (1997) |

### 20. La balance des moyennes masque-t-elle des mismatchs distributionnels ?

Partiellement. (i) L'équilibre parfait des moyennes d'entropy ne garantit ni variances ni queues : VR s'écarte jusqu'à 44 % de 1 (2013) et eCDF-max atteint 0,27 — acceptable comme diagnostic descriptif, à surveiller. (ii) Cardinality/profile saturent la contrainte de moyenne (SMD ≈ 0,10) tout en laissant des écarts d'échelle (VR jusqu'à ×2 en 1997/2013 pour cardinality 1:1) ; profile matching réduit nettement ce problème (VR ≤ 1,37 partout). (iii) La référence production est dominée sur tous les plans distributionnels dans toutes les vagues.

---

## F. Interprétation (sans utiliser d'outcome)

### 21. Le support ATT complet est-il faisable dans les 7 vagues ?

**Oui.** Les trois designs retennent 100 % des traités dans les 7 vagues ; cardinality 1:1 y parvient même avec des contrôles en nombre égal aux traités.

### 22. Le résultat 2013 se généralise-t-il ?

**Oui.** La conclusion de `a9d2ceb` (cardinality retient tous les traités jusqu'à tolérance 0,05 ; entropy sain) se vérifie sur les 7 vagues, avec des solveurs « optimal » partout et des runtimes < 0,5 s.

### 23. Design le plus proche du PAP qui résout attrition/équilibre

**Cardinality 1:1 (design B).** Il préserve : matching, 1:1, sans remplacement, ATT, critère explicite de balance SMD (le critère préenregistré), et ne change que l'optimiseur (Genetic Matching → optimisation MILP). Il supprime l'attrition massif du design production (rétention des traités 21,8 %–42,3 % avec caliper 0.25 ; SMD 0,05–0,48) tout en satisfaisant SMD ≤ 0.10 dans les 7 vagues.

### 24. Design qui utilise le mieux l'information contrôle

**Profile matching ATT (design C)** : 42 %–80 % des contrôles retenus (1 929–6 836 ménages), ESS clusters 64–263, et meilleure balance distributionnelle parmi les méthodes de matching. Entropy (design D) garde tous les contrôles mais sous forme de repondération (ESS contrôles 1 729–6 358) — efficience comparable, mais déviation méthodologique plus large (plus de « matching » du tout).

### 25. Concentrations géographiques ?

Aucune concentration pathologique. Le point le plus concentré reste cardinality 1:1 en 1997 (13 clusters contrôles, part top-1 15,5 %, ESS clusters 10,8) — néanmoins bien meilleure que la production (3 clusters, ESS 2,9).

### 26. Règle de design figée recommandée (sans outcome)

**Recommandation : cardinality matching 1:1, `MatchIt::matchit(method = "cardinality", estimand = "ATT", ratio = 1, tols = 0.10, std.tols = TRUE, solver = "highs")`, appliqué vague par vague sur l'échantillon éligible inchangé** — cas « Case 1 » de la logique de décision : rétention complète des traités partout, concentration raisonnable, fidélité maximale au PAP parmi les designs qui résolvent le problème.

Robustesse recommandée (designs secondaires, à préenregistrer comme such) :

1. **Profile matching ATT** (`ratio = NA`) : même critère de balance, tous les traités, 2–8× plus de contrôles ; à privilégier si la précision prime et si l'écart « sélection de contrôles sans ratio fixe » est acceptable.
2. **Entropy balancing ATT** (premiers moments, tous les traités, contrôles repondérés) : benchmark non-paramétrique de l'équilibre exact des moyennes ; signaler l'écart au PAP (pas de matching, pas de caliper).

### 27. Stabilité (section 12 — allégée, ciblée)

Vagues signalées : 1997 (concentration cardinality la plus élevée) et 2011 (ESS entropy le plus faible). 20 répétitions, sous-échantillonnage 80 % des clusters dans chaque strate (`cardinality_stability_1997_2011.csv`, `entropy_stability_1997_2011.csv`) :

- Cardinality 1:1 : rétention des traités = 100 % (p5 = p95 = 100 %) dans les deux vagues ; SMD max médian 0,104 (1997) / 0,106 (2011), p5–p95 [0,082 ; 0,109] — léger dépassement médian de la borne (tolérance numérique du MILP à la frontière, attendu) ; toutes les AP traitées restent représentées (8/9 et 9/11 AP en médiane).
- Entropy : SMD max ≤ 1,5e-04 dans toutes les répétitions ; 88 (1997) et 122 (2011) clusters contrôles pondérés.

Aucune fragilité de solveur, aucune exclusion de traités, aucune concentration surprenante : **pas de problème de support commun ; pas de « Case 4 »**.

---

## G. Fichiers et git

### 28. Fichiers créés

- `scripts/design_diagnostic_allwaves.R`
- `documentation/design_diagnostic_allwaves.md` (ce document)
- `output/review_v2/design_diagnostics_allwaves/package_versions.csv`
- `output/review_v2/design_diagnostics_allwaves/cardinality_1to1_summary.csv`
- `output/review_v2/design_diagnostics_allwaves/cardinality_1to1_balance.csv`
- `output/review_v2/design_diagnostics_allwaves/cardinality_1to1_excluded_treated.csv` (vide : 0 exclu)
- `output/review_v2/design_diagnostics_allwaves/profile_att_summary.csv`
- `output/review_v2/design_diagnostics_allwaves/profile_att_balance.csv`
- `output/review_v2/design_diagnostics_allwaves/entropy_summary.csv`
- `output/review_v2/design_diagnostics_allwaves/entropy_balance.csv`
- `output/review_v2/design_diagnostics_allwaves/entropy_weight_diagnostics.csv`
- `output/review_v2/design_diagnostics_allwaves/control_cluster_concentration.csv`
- `output/review_v2/design_diagnostics_allwaves/distributional_balance.csv`
- `output/review_v2/design_diagnostics_allwaves/treated_pa_composition.csv`
- `output/review_v2/design_diagnostics_allwaves/design_comparison_allwaves.csv`
- `output/review_v2/design_diagnostics_allwaves/cardinality_stability_1997_2011.csv`
- `output/review_v2/design_diagnostics_allwaves/entropy_stability_1997_2011.csv`
- Checkpoints (non commités) : `data/derived/design_diagnostics_allwaves/allwaves_{year}.rds`

Non modifiés : `06-matching.qmd`, `07-estimation_staggered.qmd`, `07b-estimation-staggered-did.qmd`, les RDS de production, `output/review_v2/matching_summary.csv`.

### 29. Commit

`diagnose(06): compare balanced designs across all survey waves`

### 30. Statut git final

Voir sortie `git status` après commit : uniquement les modifications préexistantes non liées restent en dehors du commit.

---

## STOP — prochaines étapes interdites avant gel

Ne pas exécuter le placebo 1997–2008 ni aucun modèle d'effet avant revue et gel de la règle de design. La règle recommandée (cardinality 1:1, tolérance 0.10, ATT, HiGHS) doit être validée puis implémentée dans `06-matching.qmd` dans une tâche séparée.
