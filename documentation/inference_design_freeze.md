# Gel de l'inférence — design figé (profile matching ATT), 07

Date : 2026-10-05
Prompt de référence : `documentation/inference_audit_main_staggered_robustness_prompt.md`
Script d'audit : `scripts/inference_audit_placebo.R`
Sorties : `output/review_v2/inference_audit/*.csv`

## Règle d'inférence gelée

> Primary inference follows the PAP: enumeration-area clustered robust standard
> errors using the existing fixest implementation. Because the placebo models
> triggered a non-PSD covariance warning, additionally report a prespecified
> cluster-robust robustness check (CRV3 and/or wild-cluster bootstrap where
> technically supported). The primary point estimate and specification remain
> unchanged.

Ce gel n'est **pas** fondé sur la p-value la plus favorable : la méthode
primaire (fixest clusterisé) est celle du PAP ; CRV3 et le wild cluster
bootstrap ne sont que des robustness checks prédéfinis.

Explicitement :

- le matching est déjà gelé (commit `1fc6dea`, règle `b89b8c2`) ;
- la spécification de régression est gelée (07, placebo committé `7e561af`) ;
- la définition primaire des clusters est gelée (`interaction(DHSYEAR, hv001)`) ;
- l'inférence alternative est uniquement une robustesse, jamais un remplacement.

## 1. Audit de l'observation écartée (Phase A)

L'observation écartée par `fixest` (NA sur une variable RHS) est unique :

| Champ | Valeur |
|---|---|
| DHSYEAR | 1997 |
| GROUP | Control |
| hv001 | 159 |
| hv002 | 10 |
| WDPAID | NA (control) |
| Variable manquante | `hv220` (âge du chef de ménage) |

- N sample apparié : 10 732 ; N régression : 10 731 ; part exclue :
  1/10 732 = 0,0093 % — très en dessous du seuil PAP de 2 %.
- Statut : Control. Pas d'imputation (PAP : exclusion autorisée).
- Vérification ex ante : **aucune** RHS manquante dans les vagues 2008 et
  2021 du design figé (0/7 478 et 0/7 523) ; 1997 n'a que cette unique
  observation. Le problème ne peut donc pas se reproduire dans l'effet
  principal 2008–2021.
- Export : `regression_missingness_audit.csv`.

## 2. Diagnostic du VCOV clusterisé « non PSD » (Phase B)

Reproduction exacte des modèles placebo committés confirmée
(point estimates et SE identiques aux sorties de `7e561af`).

Diagnostic des eigenvalues du VCOV clusterisé **brut** (`vcov_fix = FALSE`) :

| Outcome | n eig. | eig. < 0 | eig. < 1e-12 | min eig. | max eig. | Var(treat_post)>0 | SE avant réparation | SE après fix |
|---|---|---|---|---|---|---|---|---|
| H1 wealth centile | 12 | 0 | 0 | 1,64e-09 | 86,52 | oui | 7,3211 | 7,3211 |
| H2 z-score | 12 | 0 | 1 | 2,41e-15 | 1,18e-03 | oui | 0,0072115 | 0,0072115 |

Conclusion : **aucune eigenvalue négative**. Le warning de fixest provient de
sa règle interne `any(eigenvalues < 1e-12)` (source : `fixest:::vcov.fixest`,
v0.13.2), déclenchée uniquement par H2 dont la plus petite eigenvalue est
2,4e-15 — positive mais en dessous du seuil numérique. La réparation
(eigen-décomposition à la Cameron–Gelbach–Miller 2011) laisse la variance du
coefficient DiD et son SE **strictement inchangés**. Il s'agit d'une
near-singularité numérique mineure (colinéarité faible), pas d'une covariance
instable. La condition d'arrêt « VCOV sévèrement indéfini » n'est pas
déclenchée.

- Export : `vcov_eigen_audit.csv`.

## 3. Structure de clusters (Section 5)

Sample placebo (10 731 obs. utilisées) :

| Métrique | Valeur |
|---|---|
| Clusters totaux | 351 |
| 1997 : clusters treated | 11 |
| 1997 : clusters control | 88 |
| 2008 : clusters treated | 38 |
| 2008 : clusters control | 214 |
| Ménages/cluster (min ; p25 ; médiane ; p75 ; p90 ; max) | 3 ; 28 ; 31 ; 32 ; 34 ; 60 |
| Part de masse de poids du cluster max | 0,80 % |
| Part de masse des 5 plus gros clusters | 3,82 % |

La définition `cluster_uid = interaction(DHSYEAR, hv001)` fait qu'aucun
cluster ne couvre deux vagues. L'identification de `treat_post` repose sur
11 + 38 = 49 cluster-vagues treated — un nombre modeste mais non dégénéré ;
aucune concentration anormale de poids. Aucun cluster n'est écarté.

- Export : `placebo_cluster_structure.csv`.

## 4. Convention d'inférence primaire (Section 6A)

`feols(..., weights = ~w_all, cluster = ~cluster_uid)` avec les réglages `ssc`
par défaut de fixest 0.13.2 :

```
ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min",
    t.df = "min", K.exact = FALSE)
```

## 5. Robustesse CRV3 (Section 6B)

`clubSandwich` 0.7.0 n'a pas de méthode `fixest`, mais les modèles figés n'ont
**aucun fixed effect** : le refit `lm()` (WLS) est numériquement identique
(vérifié coefficient par coefficient). CRV3 est donc calculé exactement via
`vcovCR(lm_fit, cluster, type = "CR3")` + `coef_test` (Satterthwaite).

## 6. Wild cluster bootstrap (Section 6C)

`fwildclusterboot` 0.14.3 (installé depuis GitHub `s3alfisc/fwildclusterboot`
— le package a été retiré de CRAN le 2024-05-29). `boottest.fixest` supporte
les poids (condition : `fe = NULL`, vérifiée — pas de FE) :

- type : Rademacher (défaut documenté) ;
- null imposée (WCR), `p_val_type = "two-tailed"` (défauts documentés) ;
- B = 9 999 ; clusters = `cluster_uid` (351) ;
- seed déterministe : 8607 (`set.seed` + `dqrng::dqset.seed`, requis par
  fwildclusterboot >= 0.13) ;
- `ssc` de boottest calqué sur fixest (défauts).

Les modèles pour le bootstrap sont refittés sur le sample complet
(sans ligne NA interne, condition technique de `boottest`) : coefficients
identiques aux modèles figés (vérifié).

## 7. Résultats comparés sur le placebo

| Outcome | Méthode | Estim. | SE / CI 95 % | p |
|---|---|---|---|---|
| H1 | fixest clusterisé (PRIMAIRE) | -4,50 | [-18,85 ; 9,85] | 0,539 |
| H1 | CRV3 (Satt. df = 16,9) | -4,50 | [-21,67 ; 12,66] | 0,587 |
| H1 | WCR bootstrap (Rad., B=9999) | -4,50 | [-21,08 ; 11,48] | 0,574 |
| H2 | fixest clusterisé (PRIMAIRE) | +0,00868 | [-0,00545 ; 0,0228] | 0,229 |
| H2 | CRV3 (Satt. df = 16,9) | +0,00868 | [-0,00816 ; 0,0255] | 0,292 |
| H2 | WCR bootstrap (Rad., B=9999) | +0,00868 | [-0,00662 ; 0,0246] | 0,253 |

Les trois méthodes sont cohérentes : aucune ne rejette l'hypothèse nulle du
placebo. Les EC robustes (CRV3, bootstrap) sont légèrement plus larges que
l'EC primaire, comme attendu avec des clusters treated peu nombreux.

- Export : `placebo_inference_comparison.csv`.

## 8. Décision

Aucune condition d'arrêt déclenchée. L'inférence est gelée comme suit pour
l'effet principal 2008–2021 et les analyses suivantes :

1. Estimation primaire : spec figée 07, EC clusterisés fixest (`cluster_uid`),
   réglages `ssc` par défaut.
2. Robustesse prédéfinie (rapportée systématiquement, sans jamais remplacer
   la primaire) : CRV3 (clubSandwich sur refit lm) et WCR wild cluster
   bootstrap (fwildclusterboot, Rademacher, B = 9999, null imposée, seed 8607).
3. Aucune modification du matching, des contrôles, des outcomes, du clustering
   ou des poids sur la base d'un résultat (placebo, principal, staggered ou
   robustesse).
