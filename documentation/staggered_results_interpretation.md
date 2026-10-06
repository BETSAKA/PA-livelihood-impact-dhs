# Wording de manuscrit — résultats échelonnés et diagnostics de soutien

Date : 2026-10-06
Références : `documentation/did_pretrend_wald_audit.md`,
`output/review_v2/did_pretrend_audit/`, chapitre 07b.

Texte proposé pour le manuscrit, en trois sections. Les chiffres sont ceux du
design figé (2×2 placebo 1997–2008, 2×2 principal 2008–2021, ATT échelonné
`did` en coupes répétées) ; ils ne remplacent pas les tableaux générés par les
scripts.

---

## A. Paragraphe résultats principaux

> L'estimation 2×2 préenregistrée suggère un effet positif de l'accès aux aires
> protégées sur le niveau de vie des ménages ruraux (+4.4 centiles de richesse
> rurale pondérée, SE 4.9), mais cet effet est imprécis (p = 0.37). Le placebo
> 1997–2008, construit sur le même dessin avant toute entrée en traitement, ne
> détecte pas de différence différentielle pré-traitement (−4.5, SE 7.3) — il
> est lui aussi imprécis et ne constitue donc pas une validation formelle.
> L'estimateur échelonné en coupes répétées (Callaway & Sant'Anna 2021),
> estimé sur le même échantillon apparié, donne un ATT global agrégé de
> +2.8 centiles (SE 4.9 ; IC 95 % [−6.8 ; 12.3]), de magnitude comparable et
> tout aussi imprécis. Les variantes d'appariement (cardinalité 1:1,
> entropie) conduisent aux mêmes conclusions qualitatives. L'ensemble des
> résultats est donc cohérent avec un effet positif de taille modérée, non
> distingué de zéro à ces niveaux de puissance.

## B. Paragraphe tendances parallèles

> Le diagnostic principal de tendances parallèles est le placebo 2×2 1997–2008
> du dessin figé, qui ne détecte pas de tendance différentielle mais reste
> imprécis. Les ATT(g,t) pré-traitement de l'analyse échelonnée sont
> individuellement imprécis et irréguliers, et le soutien nécessaire à des
> diagnostics fins est limité : seules quatre cellules pré-traitement sont
> estimables, dont trois reposent sur la cohorte 2016 (1 à 4 aires protégées).
> Le test conjoint de pré-tendance produit par `att_gt()` (p = 0) a fait
> l'objet d'un audit technique : il est construit sur la covariance analytique
> des fonctions d'influence, qui ne tient pas compte de la structure de grappe ;
> reconstruit avec la covariance cluster-robuste du dessin figé, le même test ne
> rejette pas l'hypothèse de tendances parallèles (W = 3.91, 4 dl, p = 0.42).
> Nous n'interprétons donc ni ce test ni l'absence de rejection comme une
> validation des tendances parallèles ; les magnitudes pré-traitement, le
> placebo 2×2 et les limites de soutien documentées ci-dessous priment.

## C. Paragraphe hétérogénéité / soutien

> L'analyse échelonnée distingue trois cohortes de traitement d'appui très
> inégal : 36 aires protégées pour la cohorte 2011, 5 pour la cohorte 2016 et
> 2 pour la cohorte 2021. Les effets par cohorte pour 2016 (+34.2) et 2021
> (−17.6) sont instables et doivent être lus comme exploratoires : ils se
> concentrent précisément dans les cellules de plus faible soutien (1 à 5 aires
> protégées, une à huit grappes traitées), où l'équilibre cohorte × vague peut
> être très dégradé alors même que l'équilibre global par vague — objectif du
> design d'appariement figé — est bon. La cohorte 2016 n'est par exemple
> représentée dans aucune observation appariée de la vague 2016, et la cohorte
> 2021 n'a aucune cellule pré-traitement estimable. Un diagnostic de soutien
> centré sur la cohorte dominante 2011 (36 des 43 aires protégées) est présenté
> en supplément ; il est descriptif et ne remplace ni l'estimation primaire
> préenregistrée ni l'ATT échelonné toutes cohortes.

---

## Hiérarchie de présentation recommandée

**Texte principal** : (1) placebo 2×2 1997–2008 figé ; (2) effet principal 2×2
2008–2021 figé ; (3) robustesse aux méthodes d'appariement ; (4) mention compacte
de l'ATT échelonné agrégé (+2.8, IC [−6.8 ; 12.3]).

**Supplément** : (5) ATT(g,t) complets ; (6) figure event-study annotée par
soutien (`event_study_support.png`) ; (7) effets par cohorte ; (8) tableau de
soutien cohorte × vague ; (9) audit technique du test conjoint
(`did_pretrend_wald_audit.md`).

Les effets des cohortes 2016/2021 ne sont **pas** masqués : ils figurent dans le
supplément avec leurs SE, leurs bandes simultanées et le tableau de soutien
cohorte × vague. Ils ne sont pas mis en avant parce que leur soutien effectif
(1–5 aires protégées, équilibre cohorte × vague dégradé) ne permet pas de les
distinguer du bruit d'échantillonnage — non pas parce qu'ils sont défavorables.

## Déclaration anti-redesign (à reproduire dans le supplément)

> Aucun choix d'appariement, de tolérance, de contrôles de régression, d'outcome
> ou de clusterisation n'a été modifié en réponse au profil de pré-tendances
> échelonnées. L'analyse de la cohorte 2011 est descriptive et orientée soutien ;
> elle ne remplace ni l'estimation primaire pré-spécifiée ni l'ATT échelonné
> toutes cohortes.
