# Caliper semantics — PAP wording vs. software implementation

Status: methodological note prepared for the caliper decision. No custom
implementation has been made; the production specification remains 8b02041
(provisional).

## 1. What the PAP says

> "a caliper is initially set at 0.25 standard deviations of the Mahalanobis distance."

and, as an explicit adjustment rule:

> "If the SMD exceeds 0.1, we will increase the caliper interval to achieve a SMD ≤ 0.1."

The PAP therefore (a) names the Mahalanobis distance as the quantity the
caliper is expressed in units of, and (b) prescribes a balance-driven
*widening* of the caliper, not a fixed width.

## 2. What 8b02041 implements

`06-matching` calls:

```r
matchit(method = "genetic", distance = "mahalanobis", estimand = "ATT",
        replace = FALSE, ratio = 1,
        caliper = setNames(rep(0.25, 5), matching_variables),
        std.caliper = TRUE, ...)
```

This is a **simultaneous marginal standardized caliper on each of the five
matching covariates**: a treated–control pair is admissible only if, for every
covariate, the pair differs by at most 0.25 standard deviations of that
covariate (SDs computed on the eligible sample; `std.caliper = TRUE`). It is
*not* a scalar threshold on the pairwise Mahalanobis distance.

Two documented MatchIt facts matter here (MatchIt 4.x reference,
`method_genetic`):

1. With `method = "genetic"` and `distance = "mahalanobis"`, the string
   `"mahalanobis"` has **no bearing on how the distance matrix is computed**;
   it only signals that no propensity score is estimated. The actual matching
   distance is the **generalized** Mahalanobis distance with per-covariate
   scaling weights chosen by the genetic algorithm (Diamond & Sekhon).
2. When a caliper is specified, the caliper variables are added to the
   matching variables used to form the generalized Mahalanobis distance
   matrix, because `Matching` does not allow separating caliper variables from
   matching variables in genetic matching. (In our case they are already the
   matching variables, so this changes nothing.)

## 3. Why the marginal caliper and a scalar Mahalanobis-distance caliper differ

- The marginal caliper is a **rectangular** restriction in covariate space:
  each covariate is individually constrained. It ignores correlations between
  covariates and can forbid pairs that are jointly very close in Mahalanobis
  terms but differ on a single covariate.
- A caliper on the pairwise Mahalanobis distance is an **elliptical**
  restriction: a pair is admissible if its joint (correlation-aware) distance
  is small, even if one covariate deviates substantially when that deviation
  is offset by correlation with others.
- Consequently the two restrictions are neither nested nor monotone in each
  other: widening the marginal caliper does not approximate the distance
  caliper, and vice versa. The 0.25-marginal experiment showed severe
  retention loss (e.g. 2013: 185/724 treated) without fixing balance in most
  waves; that behaviour is characteristic of a rectangular restriction on
  skewed covariates such as `population_count_2000`.

## 4. Is the marginal implementation the closest native implementation?

Yes, essentially. In both MatchIt and the underlying `Matching` package,
`caliper` is defined over **covariates** (optionally plus the propensity
score); neither package supports a caliper on the pairwise Mahalanobis
distance itself. The named-covariate standardized caliper is the standard,
native operationalization available in the current stack.

## 5. Would a literal pairwise-Mahalanobis caliper require custom machinery?

Yes. The feasible routes are:

- a static admissibility mask via `Matching`'s `restrict` mechanism (or a
  pre-masked distance matrix), computing plain (unweighted) Mahalanobis
  distances once and forbidding pairs beyond the threshold; or
- a custom check inside the genetic search, which the packages do not expose.

Note a conceptual tension: the genetic algorithm *reweights* the distance at
each generation, so a caliper fixed on the plain Mahalanobis distance is not
the same quantity the optimizer is minimizing.

## 6. Extra choices a custom version would require

- The SD of the pairwise-distance distribution: computed over all eligible
  treated–control pairs, or over pairs of treated units only, or over
  within-group distances? (These differ.)
- Whether the SD is computed before or after any common-support trimming.
- Whether the distance entering the caliper is the plain Mahalanobis distance
  or the genetically weighted one (and, if the latter, re-evaluated at every
  generation).
- How the caliper interacts with 1:1 without-replacement matching and with
  the estimand (excluded treated units change the target population).

## 7. Which interpretation is most defensible relative to the preregistration?

- The PAP's *operative* content is the balance-driven rule: start at 0.25,
  widen until max SMD ≤ 0.1. That rule is operationalization-agnostic and is
  the part that must be honored.
- The marginal standardized covariate caliper is the closest standard
  implementation, is fully reproducible, and is what 8b02041 documents. It is
  defensible **if reported transparently** as such (i.e., the paper should say
  "calipers of 0.25 SD on each matching covariate", not "caliper of 0.25 SD of
  the Mahalanobis distance").
- A literal pairwise-Mahalanobis caliper would require custom machinery and
  several underdetermined choices (Section 6); it is harder to preregister
  cleanly and does not resolve the empirical problem anyway (see diagnostic:
  2013 fails at SMD ≈ 0.28 on `population_count_2000` even with the caliper
  effectively non-binding).

**Empirical caveat from the diagnostic (outcome-blind):** for 2013, no
marginal-caliper width in {0.50, 1.00, 1.50} achieves SMD ≤ 0.1; the imbalance
tracks the no-caliper reference (0.253 at production settings), so the 2013
failure is a common-support/balance limitation of the wave, not a caliper-width
problem. The decision on the final rule should account for this.
