# Review v2 — synthesis report

Branch `florent_review_v2` (continuation of the interrupted session; final HEAD after this session: `03c7588` + render commit). Base: `florent_review` at the merge commit of PR #73 (`3e00f95`).

The primary design is the **H1 2×2 DiD**; the staggered/event-study specification remains **unresolved** (see §10) and its coefficients must not be presented as final.

---

## 1. Validated bug fixes

| Fix | Verification |
|---|---|
| **01 — eventual treatment group vs. activation** (`GROUP == "Treatment"` = membership of the eventual treatment geography; `treated_now` = time-varying activation; future PAs later in the same calendar year no longer dropped by `STATUS_YR > année` — 39 PAs of the 2008 cohort have Oct.–Dec. 2008 onsets) | Invariants T1–T9 pass for the seven waves; audit: 1997 = 11 Treatment clusters (all pre-activation), 2008 = 38 (all `treated_now = 0`), 2021 = 52 (all treated). Endpoint check in 07: **0** Treatment observations already treated at the June-2008 baseline; 2021 share not-yet-treated ≤ 5 % (asserted, render fails otherwise). |
| **03 — preregistered signed within-cluster z-score preserved** (no `abs()`); weighted MICS centile thresholds on `wscore`; MICS household ID `HH2`; 2018 `hh_2018_rural_simpler.rds` | Rendered and unchanged; cluster SD/IQR added only as descriptive diagnostics. |
| **04 — fixed-vintage covariate names** (`treecover_area_2000`, `slope_2000`, `elevation_2000`, `population_count_2000`, `traveltime_2000_2000`), POSIXct/time-zone year correction, corrected 2018 join, pre-matching variable assertion | Rendered; 06 reads the covariates under the fixed-vintage names. |
| **06 — reproducible genetic matching**: `matchit(method = "genetic", distance = "mahalanobis")`, ATT, 1:1 without replacement, `pop.size = 1000`, `max.generations = 100`, `wait.generations = 4`, wave seeds `20261004L + year` (integer), specification hash over treatment + row IDs + X + arguments + seed, cache reuse only on identical hash | All **seven** cached `spec_hash` values re-validated in this session against the current `hr_YEAR_final.rds` inputs with the exact integer seed — all `TRUE`. The 06 render reused all seven caches (no recomputation). Matched counts reproduced exactly: 670 / 2310 / 882 / 1448 / 1274 / 2388 / 3202. |
| **06 — `bal.tab()` wrapper fix** (`cached$m_out` unwrapping) and invalid-GPS guard (households of invalid-GPS clusters may have `GROUP = NA`; eligible matching rows must have valid labels) | 06 renders; the two `bal.tab()` chunks run on the wrapper format. |
| **06 — hard assertion on the matching summary** (`nrow == 7` and wave set equality) so a `tryCatch()`-dropped wave can no longer produce a silent partial summary | The stale five-wave `matching_summary_all_years.rds` left by the interrupted render was confirmed stale (it predated the rematched 2008/2011 caches); the new render rebuilt and validated the seven-wave summary. |
| **07 — 2×2 semantics aligned with the PAP**: `treat = as.integer(GROUP == "Treatment")` (not `treated_now`), placebo 1997–2008, main 2008–2021, weights = survey weight × matching weight, SEs clustered by `interaction(DHSYEAR, hv001)`, PAP controls (SPEI t-1, head sex/age, the five 2000-vintage covariates), endpoint assertions, cohort 2008 not dropped | Rendered on the real matched samples; exports `output/review_v2/did_2x2_h1.csv` and `did_2x2_h2.csv`. |

New fixes made in this continuation session (separate commits):

- `fix(01)`: placebo wording — the 1997 wave shares the **same PA treatment universe and classification rule** as 2008, **not the same sampled units** (DHS waves are independent repeated cross-sections; cluster IDs are not longitudinal identifiers). Wording only; logic unchanged.
- `fix(06)`: seven-wave assertion + balance-diagnostic harmonization (§8) + corrected stale pre-matching prose.
- `fix(07)`: **date-based event time** (§10) + missing `library(sf)` that broke the render.
- Rendered 06/07 committed; earlier session had committed renders of 01/03/04 only (`981d205`).

## 2. Methodological refinements consistent with the PAP

- Genetic matching (Mahalanobis) with ATT, 1:1, no replacement — as preregistered.
- Eventual-treatment classification with date-based activation, matching the PAP's intention-to-treat geography.
- 2×2 controls restricted to the PAP set.
- Supplementary (non-preregistered) cluster dispersion diagnostics (SD, IQR) in 03 — descriptive only.

## 3. Deviations / unresolved choices

- **Caliper**: the preregistered 0.25-SD caliper is **not applied** (deliberate; deferred to a separate task). Nothing in 06/07 should be read as caliper-matched. TODO comments kept in 06.
- **Staggered estimator**: `did2s` on repeated cross-sections with wave fixed effects — see §10.
- **Treatment onset operationalization**: first dynamic `valid_from` (§11).
- H2 dispersion alternatives not estimated as causal outcomes (§6).

## 4. Final review-v2 sample and matching diagnostics

Matching summary (cobalt SMD = |mean diff| standardised by the treated-group SD, ATT reference definition; custom `smd_by_var()` = pooled-SD definition):

| Wave | Eligible | Matched | Max SMD before (cobalt) | Max SMD after (cobalt) | Max SMD after (pooled, custom) | Seed |
|---|---:|---:|---:|---:|---:|---:|
| 1997 | 4,034 | 670 | 2.16 | 0.090 | 0.099 | 20263001 |
| 2008 | 10,852 | 2,310 | 1.82 | 0.088 | 0.093 | 20263012 |
| 2011 | 5,058 | 882 | 4.78 | 0.086 | 0.084 | 20263015 |
| 2013 | 5,179 | 1,448 | 7.30 | **0.253** | 0.197 | 20263017 |
| 2016 | 7,807 | 1,274 | 4.98 | 0.067 | 0.068 | 20263020 |
| 2018 | 11,925 | 2,388 | 0.93 | 0.059 | 0.060 | 20263022 |
| 2021 | 12,338 | 3,202 | 3.17 | 0.059 | 0.060 | 20263025 |

Six of seven waves meet the ≤ 0.1 criterion under the cobalt definition; 2013 does not (§8).

## 5. Final H1 2×2 estimates (preregistered outcome: wealth centile)

| Period | Estimate | SE | 95 % CI |
|---|---:|---:|---|
| Placebo 1997–2008 | **−3.93** | 9.35 | [−22.2 ; 14.4] |
| Main 2008–2021 | **+7.85** | 5.65 | [−3.2 ; 18.9] |

Placebo consistent with zero (no pretrend detectable); main effect positive but not statistically distinguishable from zero at conventional levels. Exports: `output/review_v2/did_2x2_h1.csv`, `paper/tables/table_DID_WI.rds`, `paper/figures/h1_2x2_plot.rds`.

## 6. Final H2 estimates (preregistered signed within-cluster z-score)

| Period | Estimate | SE | 95 % CI |
|---|---:|---:|---|
| Placebo 1997–2008 | **−0.0435** | 0.0350 | [−0.112 ; 0.025] |
| Main 2008–2021 | **+0.0224** | 0.0157 | [−0.008 ; 0.053] |

Reminder of the conceptual limitation: the signed z-score is standardised within cluster, so its unweighted cluster mean is mechanically zero. The cluster-level SD/IQR added in 03 are **non-preregistered descriptive diagnostics only**; they have **not** been estimated as causal H2 outcomes, and no such claim is made. No Gini added (clusters of ~20–30 households).

## 7. Comparison to `main` and `florent_review_matching`

Recovered from archived, branch-specific outputs **without checkout**: `output/obs_by_year.csv`, rendered `docs/06-matching.html` and `docs/07-estimation_staggered.html` (LFS pointers smudged offline where needed). Full table: `output/review_v2/comparison_branches.csv`.

| Metric | main | florent_review_matching | review v2 |
|---|---:|---:|---:|
| **H1 placebo 1997–2008** (SE) | +3.297 (8.779) | +17.36 (9.642) | −3.93 (9.35) |
| **H1 main 2008–2021** (SE) | +1.743 (5.722) | +3.735 (5.126) | +7.85 (5.65) |
| **H2 placebo** (SE) | +0.0381 (0.0350) | +0.0249 (0.0401) | −0.0435 (0.0350) |
| **H2 main** (SE) | −0.0106 (0.0182) | +0.0164 (0.0274) | +0.0224 (0.0157) |
| Treatment households 1997 / 2008 / 2021 | 429 / 1,407 / 2,073 | 82 / 303 / 1,485 | 335 / 1,155 / 1,601 |
| Matched households (7 waves, sum) | 14,128 | 10,552 | 12,172 |
| Treatment **clusters** 1997 / 2008 / 2021 | not archived | not archived | 11 / 38 / 52 |
| Max post-matching SMD (cobalt), worst wave | 0.248 (2016) | 0.426 (1997) | 0.253 (2013) |

Provenance caveats (stated, not hidden): `main`'s rendered 06/07 date from 2026-03-09 / 2026-05-04 and may not reflect `main`'s tip; `florent_review_matching`'s renders date from 2026-10-04 19:43, i.e. one commit before its tip (`4b5c424`, a Ramsar guard in 01 that was **not** re-rendered). Per-wave **cluster** counts for the old branches are not recoverable from archived outputs: the only per-cluster classification file (`classification_all_clusters.csv`) is a single stale snapshot (blob identical on both branches, dated 2026-06-29), so **classification transitions by wave could not be established**; old-branch household counts come from their archived `obs_by_year.csv`.

Causal debugging — what accounts for the differences:

1. **main → florent_review_matching**: the Treatment universe was drastically narrowed (Treatment households 429 → 82 in 1997), consistent with the stricter exclusion of pre-2008-PA proximity introduced in the pa_assignation review line; matching was already genetic/Mahalanobis on that branch. Fewer, more-selected treated units ⇒ larger and less precise estimates (H1 placebo 17.4, SE 9.6).
2. **florent_review_matching → review v2**: the treatment group was redefined as the **eventual** treatment geography (PR #73 semantics), re-including future-PA geographies at baseline with `treated_now` separating activation; late-2008 onsets no longer dropped; date-based `treatment_date`; matching re-run reproducibly (seeds + spec-hash cache); 2×2 controls aligned to the PAP; weights = survey × matching. Treatment counts land between the two ancestors (335 / 1,155 / 1,601), matched counts likewise.

## 8. 2013 balance failure

Under the reference (cobalt, treated-group-SD) definition, 2013 after-matching balance is driven by `population_count_2000`: `Diff.Un ≈ 7.30 → Diff.Adj ≈ 0.253` — **2013 does not meet the intended ≤ 0.1 criterion**. The internal `smd_by_var()` (pooled SD, unweighted means) reports 0.197 for the same wave: the difference is definitional (pooled vs. treated-group SD), not a contradiction. 06 now reports **both** definitions, designates cobalt as the reference, and states the 2013 failure explicitly. 2013's imbalance must be kept in mind when reading any wave-specific result.

## 9. Caliper deferred

The preregistered 0.25-SD caliper is **not** solved in this review and is not applied anywhere; the TODOs remain in 06. Handling it is a separate task (it will also mechanically reduce matched samples).

## 10. Staggered / event-study: unresolved, flagged

Two distinct issues:

1. **Event-time construction (fixed in this session, commit `2551eca`)**: event time is now `floor((survey_ref_date − treatment_date) / 365.25)` instead of `DHSYEAR − treatment_year`. This removes the inconsistency where June-2008 observations of PAs with Oct.–Dec. 2008 onsets carried event time 0 while being untreated (`treat_on = 0`). Verified on the real matched samples: all 1,580 not-yet-treated Treatment observations now have strictly negative event time; all 4,507 treated observations have non-negative event time; 1,037 of 1,155 cohort-2008 observations change bin. `treat_on` itself was already date-based and is unchanged. The construction remains an annual approximation (an observation at +0.4 year counts as year 0).
2. **Repeated cross-sections (unresolved)**: `did2s` is estimated with wave fixed effects (`DHSYEAR`), not household/cluster unit fixed effects; cohort fixed effects are **not** equivalent to unit fixed effects. The event-study/staggered coefficients are therefore **not presented as final**. A separate methodological review should select an estimator explicitly supporting repeated cross-sections before relying on these results. The 2×2 is the primary design and is not held hostage to this.

## 11. Treatment-onset operationalization caveat

The implemented `treatment_date` is the **first legal/dynamic state `valid_from`** of the PA. Its year agrees with the corrected creation year for the treatment universe (apart from the documented `555697871` placeholder correction), which is useful but does **not** establish the PAP's stronger notion that treatment begins when a PA is both legally declared **and operationally managed**. Operational management may have started later for some PAs. No dates were changed; this is flagged as a remaining identification issue and a natural sensitivity-analysis candidate.

## 12. Remaining dynamic-PA data issues

- **Historical gaps**: gaps persist for Tsimembo/Manambolomaty (`166880`) and Mandrozo (`555547961`) — both appear in the 2008 `untreated_pas` audit list. The code's fallback to the latest historical boundary after a gap follows the creation-based PAP treatment group but must **not** be read as proof of continuous institutional activity. If the upstream dynamic PA database has since acquired later national-designation states, they should be verified; otherwise source-data correction belongs to a separate PA-database task.
- **POINT/MULTIPOINT geometry omissions (checked, immaterial for now)**: `terra::vect("data/PA_creation/dynamic_wdpa.gpkg")` silently drops the 3 MULTIPOINT features (warning only), i.e. WDPAIDs **901250** (Littoral de Toliara) and **20017** (Mananara Nord), leaving 504 of 507 features. Materiality check: no `hr_YEAR_final` row references these WDPAIDs, and exactly **one** 2008 cluster (hv001 312) lies within 10 km of a dropped feature (Mananara Nord, 4.2 km). It is classified `Control` where the pre-2008-proximity rule would arguably make it `Excluded`, but it is **not** part of the matched sample, so the primary results are unaffected. Recommended follow-up: read the gpkg with `sf::st_read` (or `terra::svc`) so no geometry type is silently dropped, and re-audit; left to a separate task per the review scope.

## Reproduction notes

- `quarto render 06-matching.qmd 07-estimation_staggered.qmd` reuses all seven caches (hash-validated); 06 asserts a complete seven-wave summary; 07 asserts the 2008/2021 endpoint conditions.
- All hash validations used the exact integer seed `MATCHING_BASE_SEED + as.integer(year)` to avoid the integer/double digest pitfall.
- Pre-existing cosmetic issue: pandoc citeproc warnings for `abadie2021`, `austin2009`, `borusyak2024`, `ho2007` (citations not found in `references.bib`) — harmless to results, worth fixing separately.
