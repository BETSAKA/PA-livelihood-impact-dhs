# Audit technique du test conjoint de pré-tendance `did` (`Wpval = 0`)

Date : 2026-10-06
Prompt de référence : `documentation/audit_erratic_staggered_pretrends_prompt.md`
Script d'audit : `scripts/audit_did_pretrends.R`
Sorties : `output/review_v2/did_pretrend_audit/*.csv`, `event_study_support.png`

Portée : **diagnostic et présentation uniquement**. Le dessin d'appariement (profile
cardinality, `tols = 0.095`, 5 covariables PAP), la spécification 2×2 primaire,
l'inférence clusterisée gelée et les définitions H1/H2 ne sont **pas** modifiés.

## 1. Chemin de code exact de `Wpval` (did 2.1.2)

Dans `did:::att_gt` (source inspectée dans la bibliothèque installée, pas seulement
la documentation) :

```r
V   <- Matrix::t(inffunc) %*% inffunc / n        # covariance ANALYTIQUE
se  <- sqrt(Matrix::diag(V) / n)
# ... les SE affichés, eux, proviennent du bootstrap multiplieur clusterisé (bstrap=TRUE)
pre <- which(group > tt)                          # cellules pré-traitement (g > t)
pre <- pre[!(pre %in% zero_na_sd_entry)]          # exclusion des cellules à SE nul/NA
preatt <- as.matrix(att[pre])
preV   <- as.matrix(V[pre, pre])
if (rcond(preV) <= .Machine$double.eps) { W <- NULL; Wpval <- NULL }  # singularité
else {
  W <- n * t(preatt) %*% solve(preV) %*% preatt
  Wpval <- round(1 - pchisq(W, q), 5)             # q = nombre de cellules pré
}
```

Points clés :

1. **Seules les cellules pré-traitement entrent** dans le test (`group > t`) ;
   les cellules non estimables (SE analytique ≤ `sqrt(.Machine$double.eps)*10`)
   sont écartées **avant** le test.
2. Le test utilise la covariance **analytique** des fonctions d'influence
   (`V_analytical`), **pas** la covariance bootstrap clusterisée qui produit les
   SE affichés. `clustervars` n'entre **pas** dans `V_analytical` : chaque
   observation (ménage) y est traitée comme une unité indépendante, en
   contradiction avec l'inférence clusterisée gelée du projet.
3. Si la matrice est singulière (`rcond`), le test est simplement **omis** (avec un
   avertissement) — ce n'est pas le cas ici.
4. La p-value est **arrondie à 5 décimales** : `Wpval = 0` signifie
   p < 5×10⁻⁶, pas p = 0 exactement.

## 2. Cellules pré-traitement incluses (modèle figé, `ipw`, never-treated)

| cellule (g, t) | ATT | SE bootstrap (affiché) | SE analytique (test) | SE cluster-robuste |
|---|---|---|---|---|
| (2011, 2008) | −6.85 | 8.87 | 2.27 | 8.77 |
| (2016, 2008) | +1.06 | 14.18 | 4.62 | 13.90 |
| (2016, 2011) | +22.75 | 19.90 | 4.65 | 18.96 |
| (2016, 2013) | −27.64 | 66.64 | 6.89 | 23.08 |

La cohorte 2021 n'a **aucune** cellule pré-traitement estimable (cellule 2018
non estimable : grappe unique, SPEI constant) ; la cellule (2016, 2016) est
non estimable (aucune observation appariée 2016). Le test porte donc sur
exactement 4 cellules.

## 3. Structure de la covariance (`preV`, 4×4)

- rang : 4/4 (plein) ; `rcond` = 0.091 ; nombre de condition = 9.8 ;
  aucune singularité, aucun problème numérique de généralisé-inverse.
- valeurs propres : 5.02, 0.61, 0.062, 0.0092 (unités : centiles² × 10⁴ environ ;
  voir `pretrend_eigenvalues.csv`).
- corrélations modestes (max |ρ| = 0.45 entre (2016, 2011) et (2016, 2013)) ;
  voir `pretrend_correlation.csv`.

La covariance est donc **bien conditionnée** : la cause n'est ni C (singularité)
ni D (cellules pathologiques — les 4 cellules incluses sont toutes estimables).

## 4. Reconstruction manuelle du test

Avec la covariance du package (`V_analytical`, telle quelle) :

- W = 46.11, df = 4, p = 2.33×10⁻⁹ → arrondi à **0** : reproduction **exacte** de
  `attgt_out$W = 46.114` et `Wpval = 0`.

Le classment est donc formellement **A (statistique de Wald réellement grande)**
sous la covariance du package — mais la raison substantielle de sa grandeur est :

**la covariance analytique ignore le cluster.** Les SE analytiques (2.27–6.89)
sont 3–4 fois plus petits que les SE bootstrap **clusterisés** affichés
(8.87–66.64), car les ménages d'une même grappe partagent des chocs communs que
`V_analytical` compte comme des informations indépendantes. Sous la définition
de grappe **gelée** du projet (`interaction(DHSYEAR, hv001)`), la covariance
cluster-robuste reconstruite à partir des mêmes fonctions d'influence (somme des
IF par grappe, sans ré-estimation) donne :

- W_cluster-robuste = **3.91**, df = 4, **p = 0.418** : aucune rejection.

Les SE cluster-robustes analytiques (8.77, 13.90, 18.96, 23.08) confirment la
validité de cette reconstruction : ils coïncident avec les SE bootstrap
clusterisés (8.87, 14.18, 19.90) partout sauf dans la cellule la plus pauvre
((2016, 2013) : 23.08 vs 66.64 — bootstrap plus prudent avec 1 grappe traitée).

## 5. Classification de `p = 0`

Le package n'a **pas de bug arithmétique** : `Wpval = 0` est l'arrondi correct de
p = 2.3×10⁻⁹ pour W = 46.11 sur 4 df. Le problème est **un problème
d'adéquation** : le test construit avec `V_analytical` n'implémente pas
l'inférence clusterisée gelée du projet (catégorie E du prompt — configuration
RCS sparse + covariance non clusterisée ; la cause numérique précise étant
l'absence de `clustervars` dans `V_analytical`). Un test conjoint calculé avec
la même statistique mais la covariance cluster-robuste (la norme d'inférence du
projet) ne rejette pas (p = 0.42).

Vérification supplémentaire : l'`att_gt` restreint à la cohorte 2011 + contrôles
(same settings) donne `Wpval = 0.0026` — l'artefact se généralise à toute
configuration où `V_analytical` est utilisé avec des données clusterisées.

## 6. Recommandation de rapport

**Qualifier, ne pas reporter tel quel.** Wording suggéré pour le supplément :

> Le test conjoint de pré-tendance rapporté par `att_gt()` (p = 0) est construit
> sur la covariance analytique des fonctions d'influence, qui ne tient pas
> compte du cluster ; reconstruit avec la covariance cluster-robuste du dessin
> figé, le même test ne rejette pas (W = 3.91, 4 dl, p = 0.42). Les cellules
> pré-traitement estimables sont au nombre de quatre, toutes individuellement
> imprécises. Le diagnostic principal de tendances parallèles demeure le
> placebo 2×2 1997–2008 (chapitre 07).

Ne pas interpréter le `Wpval` du package comme un rejet des tendances parallèles ;
ne pas l'omettre silencieusement non plus — l'expliquer.

## 7. Décomposition dynamique (résumé ; détail dans `dynamic_cell_contributions.csv`)

Les poids d'agrégation dynamique de `aggte(type = "dynamic")` sont les parts de
poids des ménages par cohorte (`pg`), normalisées au sein de chaque temps
d'événement ; reconstruction exacte vérifiée contre le package pour les 8 temps :

| e | estimateur | cellules | part 2011 | part 2016 | part 2021 |
|---|---|---|---|---|---|
| −8 | +1.06 | (2016,2008) | 0 | 1.00 | 0 |
| −5 | +22.75 | (2016,2011) | 0 | 1.00 | 0 |
| −3 | −9.49 | (2011,2008)+(2016,2013) | 0.87 | 0.13 | 0 |
| 0 | +4.16 | (2011,2011)+(2021,2021) | 0.97 | 0 | 0.03 |
| 2 | +0.90 | (2011,2013)+(2016,2018) | 0.87 | 0.13 | 0 |
| 5 | +0.64 | (2011,2016)+(2016,2021) | 0.87 | 0.13 | 0 |
| 7 | +2.74 | (2011,2018) | 1.00 | 0 | 0 |
| 10 | +6.04 | (2011,2021) | 1.00 | 0 | 0 |

**Les temps pré-traitement erratiques sont générés par la cohorte 2016** :
e = −8 et e = −5 proviennent à 100 % de cette cohorte (respectivement 4 et 1 AP,
1 grappe côté traités à e = −5), et le piquet négatif de e = −3 est tiré à 13 %
par la cellule (2016, 2013) (−27.6, 1 grappe, SE 66.6). La cohorte 2011 seule
(87–100 % de poids à tous les temps sauf −8/−5) dessine une séquence pré-traitement
regularre et sans piquet : −6.85 → +4.75 → −3.07 → …

## 8. Déclaration anti-redesign

> Aucun choix d'appariement, de tolérance, de contrôles de régression, d'outcome
> ou de clusterisation n'a été modifié en réponse au profil de pré-tendances
> échelonnées.
>
> L'analyse de la cohorte 2011 est descriptive et orientée soutien ; elle ne
> remplace ni l'estimation primaire pré-spécifiée ni l'ATT échelonné
> toutes cohortes.
