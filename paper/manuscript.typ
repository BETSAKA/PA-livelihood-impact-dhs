// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "linux libertine",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "linux libertine",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#show: doc => article(
  title: [Socioeconomic Impacts of Terrestrial Protected Areas],
  subtitle: [Evidence from Large-Scale National Surveys in Madagascar],
  authors: (
    ( name: [Iriana Razafimahenina],
      affiliation: [],
      email: [] ),
    ( name: [Florent Bédécarrats],
      affiliation: [],
      email: [] ),
    ( name: [Ingrid Dallmann],
      affiliation: [],
      email: [] ),
    ( name: [Holimalala Randriamanampisoa],
      affiliation: [],
      email: [] ),
    ),
  lang: "en",
  abstract: [Protected areas are the most widely used instrument for biodiversity conservation, yet their socioeconomic effects on nearby populations remain contested, particularly in low income contexts where livelihoods depend heavily on natural resources. This paper evaluates the impact of terrestrial protected areas on rural household well-being in Madagascar, exploiting geolocated socio-demographic surveys covering the period of rapid protected area expansion between 2008 and 2021. To support identification, we draw on an earlier survey wave from 1997 to assess the plausibility of parallel trends prior to treatment. We combine spatially derived socio environmental indicators with genetic matching and difference-in-difference to construct a credible counterfactual. The study follows a pre-analysis plan registered before any outcome analysis, including an ex ante power calculation indicating the ability to detect effects of approximately 7.5 wealth percentiles. We find no statistically significant average effect of protected area creation on the wealth index of households located within 10 km of protected areas established after 2008. A series of robustness checks using alternative data sources and specifications supports this result. The absence of an average effect may reflect offsetting positive and negative mechanisms, heterogeneous impacts across households or sites, or limited implementation capacity in a context of weak governance and chronic underfunding of protected areas.

],
  abstract-title: "Abstract",
  sectionnumbering: "1.1.a",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#strong[Keywords:] Biodiversity Conservation, Well-being, Demographic and Health surveys, Genetic matching, difference-in-difference, Madagascar

#strong[JEL classification];: Q57, I31, C31, Q56, O55

#pagebreak()
= Introduction
<introduction>
The reconciliation between conservation and economic development has long been debated in the scientific literature @Adams2004, but the issue has gained renewed prominence over the past decade with the rapid global expansion of protected areas (PAs). This debate is particularly salient in the context of the Kunming–Montreal Global Biodiversity Framework, under which 195 signatory states committed to protecting 30 percent of terrestrial land by 2030.

In theory, PAs can affect local livelihoods through multiple and potentially opposing channels. While they are a central instrument for biodiversity conservation @Maxwell2020, their establishment may restrict access to land and natural resources that support income generating activities such as agriculture, hunting, fishing, or forest product collection. At the same time, PAs may generate benefits through compensation schemes, employment opportunities linked to conservation or tourism, and the provision of ecosystem services such as water regulation, erosion control, or fire prevention @Kandel2022.

Despite these ambivalent mechanisms, rigorous quantitative evidence on the socioeconomic impacts of PAs remains limited, in particular in poor and fragile governance contexts. Among the 1,043 studies reviewed across 104 countries by McKinnon et al.~(#cite(<McKinnon2016>, form: "year");), only 19 used quantitative methods to assess effects on material living conditions or economic well-being. More recent syntheses confirm substantial heterogeneity in estimated impacts across contexts and methodologies. Focusing on 30 quantitative evaluations of household income, Kandel et al.~(#cite(<Kandel2022>, form: "year");) find that PAs are associated with modest average gains, with effects that depend strongly on local conditions. This heterogeneity underscores the need for context specific evaluations using transparent and robust empirical strategies.

Madagascar provides a particularly relevant setting for such an analysis. It ranks among the poorest countries globally under SDG 1.1, with the highest share of the population living below the international poverty line @Conceicao2024[ pp.~298-299]. During the study period, terrestrial PAs coverage expanded from 3.6 percent of national territory in 2008 to 10.8 percent in 2021, while the share of the population living within 10 km of a PAs rose from 9 to 28 percent. At the same time, Madagascar exhibits low state capacity @Hanson2021, which constrains the implementation of conservation policies and associated social measures. Combined with high rural dependence on natural resources, these features suggest that the socioeconomic impacts of PAs may differ from those observed in less precarious institutional contexts.

Yet national scale impact evaluations remain scarce. None of the quantitative studies reviewed by McKinnon et al.~(#cite(<McKinnon2016>, form: "year");) focus on Madagascar. The only study cited by Kandel et al.~(#cite(<Kandel2022>, form: "year");) that includes the country relies on cross sectional municipality level data from the 1993 census @Mammides2019, predates the major expansion of PAs, and does not allow for before and after comparisons.

This article contributes to the literature in two ways. Empirically, it provides the first national level impact evaluation of terrestrial PAs in Madagascar, covering 71 PAs created between 2008 and 2021. Methodologically, it combines recent advances in matching and difference-in-differences methods with geolocated household survey data to construct a credible counterfactual. To enhance transparency and limit researcher discretion, the empirical strategy was fully specified in a pre-analysis plan registered on the Open Science Framework prior to conducting any outcome analysis. Our analysis plan was submitted with a dated and verifiable certification on the OSF portal in November 2024 and updated in March 2025.

The remainder of the paper proceeds as follows. @theoryofchange, presents the conceptual framework and hypotheses. @pre-specifiedempiricalstrategy describes the data sources, outlines the identification strategy, and details the econometric methods used to assess the effect of PAs on household livelihoods, on inequalities between households, and the heterogeneity of effects according to the type of PAs governance. @departuresandcomplementstothepreanalysisplan presents an additional specification integrated into our analysis, followed by descriptive statistics in the @descriptivestatistics. @results reports the main findings and provides a series of robustness tests. Finally, @discussion discusses the result, and @conclusion concludes.

= Theory of change
<theoryofchange>
Our conceptual framework is based on a theory of change that links the implementation of PAs (the treatment) to local household well-being (outcome) through multiple possible channels, summarised in (@fig-theory-change). The objective here is to determine the impact of PAs implementation on observed changes in household living standards. Existing evidence suggests that average impacts are modest but highly context dependent. Kandel et al.~(#cite(<Kandel2022>, form: "year");) report a slightly positive average impact, alongside substantial heterogeneity across settings. Building on this literature, we represent potential mechanisms in the form of a directed acyclic graph @Hunermund2023. If the mechanisms represented affect all residents of a locality in a convergent manner, they should translate into a significant average impact (positive or negative) on the average well-being (Hypothesis 1). If, on the contrary, they affect households in very different ways, the average effects may be close to zero, but inequalities within local communities may increase (Hypothesis 2).

#figure([
#box(image("figures/theory_change.png", width: 80%))
], caption: figure.caption(
position: top, 
[
Logic diagram of the theory of change tested in the study
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-theory-change>


#emph[Source: Authors]

The factors likely to lead to a decline in well-being seem particularly significant in the Malagasy context, where the population is predominantly rural and living in extreme poverty (the last assessment was in 2021, with 69.2% of the population below the \$2.15 a day threshold at 2017 PPP). Six studies conducted in Madagascar between 1995 and 2006 estimated the opportunity cost of restricted access to natural resources following the creation of PAs (slash-and-burn agriculture, hunting, gathering, timber, etc.) at between USD 39 and 177 per household per year @Neudert2017. Golden et al.~(#cite(<Golden2014>, form: "year");) estimated that income from hunting accounted for 57% of household’s cash income in areas adjacent to the Makira and Masoala PAs. Another survey of people living near Makira estimated the value of pharmaceutical use at USD 30-44 per year per household, based on the subsidized price of equivalent treatments in the Malagasy market @Golden2012.

Several factors that could help improve livelihoods through conservation appear to be fragile in Madagascar, starting with tourism. Naidoo et al.~(#cite(<Naidoo2019>, form: "year");) aggregate data from DHS surveys conducted between 2001 and 2011 in 34 developing countries. Their study is based on matching households near and far from PAs, but with no pre-post conservation comparison. They highlight positive impacts, but only for a subset of PAs "with documented tourism". According to their study, households living near the PAs "with tourism" are 17% wealthier and 16% less likely to be poor than similar households living far from these areas. However, tourism in Madagascar’s PAs remains low. According to data from Madagascar National Parks (MNP), only seven PAs recorded more than 10,000 visitors in 2023 (with a maximum of 30,744 in Isalo), which is low compared to the average of 356,405 visitors per year and per PAs recorded across 929 PAs worldwide in the global study by Chung et al.~(#cite(<Chung2018>, form: "year");).

When new PAs are created in Madagascar, compensation mechanisms for local populations remain rare, ineffective and insufficient @Riviere2017@Bertrand2014. The most in-depth study on this subject, conducted by Poudyal et al.~(#cite(<Poudyal2018>, form: "year");) with support from the World Bank, focuses on the Ankeniheny Zahamena Corridor (CAZ), created in 2015 to connect several existing PAs. Five study sites were selected: two adjacent to the new CAZ PA (one eligible for compensation, the other not), two adjacent to long-established PAs, and one far from the forest boundary. The median cost of the conservation restriction is estimated at USD 2,375 per household per year, representing between 27% and 84% of the average annual income. The amounts set aside to compensate beneficiary households were assessed to be insufficient relative to the losses incurred, and around 50% of households eligible for compensation received nothing @Poudyal2018@Poudyal2016.

Our first set of results therefore consists of determining whether PAs in Madagascar, by limiting access to natural resources, have negative impacts on the standard of living of households living nearby, potentially exceeding the benefits of compensation and ecosystem services, and whether these impacts are more adverse than those documented in other contexts (Hypothesis 1).

The impact mechanisms represented in @fig-theory-change are likely to affect households differently depending on their prior characteristics, which would increase inequality (Hypothesis 2). Compensation measures are generally implemented in the form of projects to promote income-generating activities (agriculture, livestock, handicrafts) in surrounding communities @Poudyal2018a. In the context of such development projects, individuals known as "development brokers" frequently emerge as intermediaries between local communities and implementing organizations. By mobilizing their social networks and specific skills, these brokers manage to capture a disproportionate share of the benefits of interventions, whether in the form of income or privileged access to opportunities. This dynamic can reinforce pre-existing inequalities within communities, limiting the access of the most vulnerable households to the expected benefits of compensation programs. Although tourism development is often presented as an opportunity for economic growth, it also tends to exacerbate socioeconomic inequalities, particularly in developing countries. Adeniyi et al.~(#cite(<Adeniyi2024>, form: "year");) show that in Southern Africa, tourism can initially exacerbate inequalities by concentrating benefits in the most attractive regions, while leaving marginalized communities out of the economic benefits. According to Ghosh and Mitra (#cite(<Ghosh2021>, form: "year");), the relationship between tourism and inequality follows an inverted Kuznets curve in developing countries: when tourism remains moderate, its growth reduces inequalities, but when tourism becomes massive, further expansion worsens inequalities. Finally, Xuanming et al.~(#cite(<Xuanming2024>, form: "year");) point out that while tourism helps to improve certain socioeconomic indicators, it can also generate inflationary pressures and strain local resources, particularly affecting the most vulnerable households. Taken together, these mechanisms suggest that PAs may exacerbate economic inequalities within neighboring communities by creating opportunities that primarily benefit individuals with higher levels of education or dominant social positions, granting them access to rents and jobs related to tourism and associated activities (Hypothesis 2).

IUCN status of PAs are frequently used to explain differences in effectiveness between them. For example, Naidoo et al.~(#cite(<Naidoo2019>, form: "year");) show that multiple-use PAs (statuses V and VI) tend to have more beneficial effects than strict areas (statuses I to IV), partly due to greater flexibility in integrating local needs. Beyond status alone, governance plays a central role. Eklund et al.~(#cite(<Eklund2017>, form: "year");) highlight the importance of transparent and inclusive structures to maximize the positive effects of PAs on conservation and social justice. They call for management approaches to be adapted to local contexts, with greater involvement of communities in decision-making processes, to better reconcile conservation and development objectives.

This diversity is particularly evident in Madagascar. Although governed by similar formal statuses, PAs follow different paths depending on the local context and the way in which they are implemented. Froger and Méral (#cite(<Froger2009>, form: "year");) show that the early initiatives of shared governance, gradually introduced with in-depth mediation efforts, achieved encouraging results by strengthening local community support. However, from the 2000s onward, the accelerated deployment of management transfers, driven by quantitative targets, often led to hasty and less contextually adapted implementations, undermining the effectiveness of these mechanisms. These experiences demonstrate that, beyond the PAs’ status, their establishment period, management approach, and level of community participation significantly influence their socioeconomic impacts. We therefore anticipate that the impacts of PAs on well-being and inequalities are heterogeneous, and that PAs with higher levels of local community participation are more likely to generate greater benefits and distribute them more equitably (Hypothesis 3).

Based on this theory of change, we define two main outcome variables to capture changes in living standards: household wealth index (the main outcome) and the standardized Z-score of the wealth index (the secondary outcome). Household wealth will be the outcome variable used to determine the overall impact of PAs, and the standardized Z-score of the wealth index will capture inequalities in living standards across localities. We also use variables that may be predictive of the outcome under study. The appropriate covariates for our model are variables that are likely to influence both the probability of treatment (whether a PAs has been created near the household) and the outcome (household living standard and inequalities between households). The literature shows that PAs tend to be created in less dense, less accessible, higher and steeper regions @Joppa2010. These variables may also affect living standards: areas that are more dense, flat, low-lying and accessible (in terms of travel time and geography) tend to be wealthier @Gallup1999. We propose five covariates (forest cover in 2000, slope, elevation, population density in 2000, and accessibility in 2000).

= Pre-specified empirical strategy
<pre-specifiedempiricalstrategy>
We developed a pre-analysis plan to structure the study, specifying the data processing tools, the selected observations and variables, and the statistical methods used to test the hypotheses. The empirical strategy was registered on the Open Science Framework prior to any analysis, with a dated and verifiable record submitted in November 2024 and updated in March 2025 (https:\/\/osf.io/bgu5n/).

== Data
<data>
This study assesses the impact of the creation of PAs on rural household well-being between 2008 and 2021, based on geolocated data on PAs (World Database on Protected Areas (WDPA)), environmental characteristics (downloaded from the #emph[mapme.biodiversity] package), and the socioeconomic conditions of households (Demographic and Health Surveys (DHS)). Household living standards are the primary outcome, with inequality as a secondary outcome, and demographic and environmental variables are included as controls to improve the accuracy of treatment effect estimates. We use three DHS surveys (1997, 2008, 2021), taking 2008 as the reference year. The treatment group includes rural households located in clusters less than 10 km from PAs created since 2008 in rural areas (shown in red), and households in the control group are those located in clusters more than 10 km from PAs created since 2008 in rural areas (shown in blue). Rural populations living within 10 km of PAs before 2008 or located in urban areas are excluded from the study (shown in grey). The GPS data used comes from DHS clusters. A comprehensive overview of the data set along with the selected variables is outlined within the pre-analysis plan.

#figure([
#box(image("figures/map-clust.png", width: 80%))
], caption: figure.caption(
separator: "", 
position: top, 
[
#block[
]
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-map-clust>


#emph[Source: Author’s calculation based on DHS data (1997, 2008, 2021).]

== Methods
<methods>
To assess the impact of PAs on household well-being, it is essential to identify the counterfactual scenario, that is, what would have occurred in the absence of protection. We applied genetic matching with Mahalanobis distance matching and a caliper of 0.25 to improve covariate balance between treated and control units. This procedure strengthens the robustness of the empirical analysis (details in the PAP). Post-matching, we used a difference-in-difference (DID) approach to estimate the causal effect of PAs creation by comparing changes in household wealth index between treated and control groups before and after the intervention. The DID strategy relies on the parallel trends assumption, which posits that treated and untreated areas followed similar trajectories prior to the intervention.

The equation for the difference-in-difference @Daw2018 is as follows:

$ Y_(i c t) = alpha + delta dot.op (upright(T r e a t m e n t)_c times upright(P o s t)_t) + X_(i c t)' theta + mu_c + lambda_t + epsilon_(i c t) $ With the parameters defined as:

$Y_(i c t)$: Wealth index for household $i$ in cluster $c$ at year $t$ (1997, 2008 or 2021)

$alpha$: Intercept, representing the average wealth index of households in control clusters before the treatment

$upright(T r e a t m e n t)_c$: Binary variable that takes the value of 1 if cluster $c$ is located in an area affected by a protected area, and 0 otherwise (control households)

$upright(P o s t)_t$: Binary variable that takes the value of 1 for the post-treatment year 2021 and 0 for the pre-treatment year 2008

$(upright("Treatment")_c times upright("Post")_t)$: Interaction term between $T r e a t m e n t_c$ and $P o s t_t$; It is equal to 1 for treated clusters in the post-treatment period and 0 for the other households

$delta$: Coefficient of interest (DID estimator), measuring the causal effect of protected area creation on household well-being

$X_(i c t)$: Vector of observable control variables at the household or cluster level (age and sex of head of household, SPEI, accessibility, population density, forest cover area, slope and altitude)

$theta$: Vector of coefficients associated with the control variables $X_(i c t)$

$mu_c$: Cluster fixed effects, capturing unobserved characteristics that are constant over time within each cluster

$lambda_t$: Time fixed effects, capturing shocks common to all clusters in a given year

$epsilon_(i c t)$: Error term, representing unobserved factors affecting the household wealth index for household $i$ in cluster $c$ at year $t$

== Statistical power
<statisticalpower>
Statistical power aims to estimate the probability of detecting an effect when one exists, typically set at 0.8. To increase sensitivity and limit biases associated with artificial thresholds, the wealth index was segmented into percentiles rather than quintiles. The calculations show a high intra-cluster correlation coefficient (ICC) of 0.4854, significant dispersion with an adjusted standard deviation of 28.82, and a moderate effect size (Cohen’s $d$ = 0.27). The minimum detectable effect (MDE) is 7.5 percentiles, meaning that the study can reliably identify differences of this magnitude between treatment and control groups.

== Robustness and sensitivity tests
<robustnessandsensitivity>
We re-estimate all effects using distances thresholds 5 km and 15 km to assess the robustness of our results to the choice of the treatment radius. While confidence intervals may widen when restricting the study area to 5 km, comparing coefficients across specifications allows us to assess the consistency of the estimated effects.

We apply Benjamini-Hochberg’s (#cite(<Benjamini1995>, form: "year");) False Discovery Rate method to test Hypothesis 2 (PAs impact on inequalities between households) and Hypothesis 3 (the role of PAs governance model). These tests are performed to mitigate the risk of incorrectly inferring significant effects by controlling the average proportion of false positives among the results reported as significant. Hypothesis 2 is evaluated using the Z-score outcome of the wealth index, and Hypothesis 3 is evaluated using PAs governance categories based on IUCN status of PAs.

In the analysis, the outcome variable may be correlated with unobserved factors or shocks at the household level. Fixed effects methods can correct for many of these factors, but only repeated cross-sectional data are available.

Genetic matching and Doubly Robust DID estimation on a cross-section do not fully control for unobserved characteristics that may simultaneously affect PAs and the outcome variable (wealth index). To assess the robustness of the results in the face of these potential biases resulting from unobserved confounding variables, we perform a sensitivity analysis using Rosenbaum’s method (#cite(<Rosenbaum2002>, form: "year", supplement: [,pp.\~105–170]);).

= Departures and complements to the preanalysis plan
<departuresandcomplementstothepreanalysisplan>
== Inclusion of MICS
<inclusion-of-mics>
We still do not have sufficient data to stabilize our estimates. Incorporating MIS data alone introduces substantial noise due to the irregular spacing of the available survey waves. To remedy this problem, we therefore included the 2018 Multiple Indicator Cluster Surveys (MICS) in the analysis. The MICS samples provide cross-sectional data at both the individual and household levels and are designed using methodologies comparable to those of the MIS and DHS surveys @bolgrien_harmonized_2025. These 2018 MICS cover 17,870 households during a period for which no alternative data sources are available, allowing for more comprehensive coverage of both the preceding and subsequent years. It is worth noting that recent versions of the MICS and DHS surveys have been harmonized, facilitating their integration @bolgrien_harmonized_2025.

== Years-to-treatment binning
<years-to-treatment-binning>
The decision to incorporate a binning period into the estimate is motivated by the irregular spacing of the available surveys, which means that certain relative time values are represented by very few observations and produce unstable coefficients. A two-year binning strategy will strengthen the stability of our estimates. As Borusyak, Jaravel and Spiess #cite(<Borusyak2024>, form: "year");: "In practice, it is common to bin distant leads and lags into a single category, both to improve statistical precision and to avoid presenting very noisy and uninformative coefficients". This approach facilitates the reduction of noise at the extremes, thereby enabling the focus on interpreting the main dynamics surrounding the treatment.

== Choice of staggered DID estimator
<choice-of-staggered-did-estimator>
Our study used Malaria Indicator Surveys (MIS) data because our two-period estimate lacked sufficient statistical power. MIS data are surveys focused specifically on malaria-related issues, which do not include certain health or demographic questions found in DHS surveys. However, all variables relating to household living conditions used in DHS surveys are present in MIS surveys. The inclusion of these data requires an estimator adapted to staggered adoption and multiple study periods with potential heterogeneous treatment effects @Borusyak2024. The approach of Borusyak, Jaravel, and Spiess #cite(<Borusyak2024>, form: "year") with repeated cross-sectional data using the didimputation package is theoretically feasible, but it is impossible with unevenly spaced time-to-treatment events, even for a repeated cross-section in R. We therefore apply Gardner’s approach (#cite(<Gardner2022>, form: "year");), which is very similar in principle, with slightly wider confidence intervals. This approach is applied with binning of the years relative to the treatment, grouping all observations beyond plus or minus two years into common categories.

== PAs governance classification
<pas-governance-classification>
PAs are classified internationally according to the IUCN management status, which range from strict protection with limited or no resource extraction (statuses I–II) to areas that explicitly allow sustainable use (statuses V–VI). The literature does not provide a consistent treatment of status IV, which is variously grouped with strictly protected areas or with multi-use areas depending on the study. After reviewing this debate, we classify category IV together with strict protected areas and distinguish two governance regimes: strict protected areas (IUCN statuses I–IV) and multi-use protected areas (IUCN statues V–VI). The diversity of alternative classifications used in the literature and the rationale for this choice are detailed in the Supplementary Material.

= Descriptive statistics
<descriptivestatistics>
The unit of analysis in our study is the household. It is at this level that a significant proportion of individual resources are pooled, and it is at this level that data on living standards - the outcome variable in our study - are available in national surveys @Deaton1997. Two outcome variables are taken into account: household living standards (primary outcome) and inequalities in living standards at the level of the localities studied (secondary outcome). Household living standards are estimated using the wealth index, calculated specifically for rural areas. We will translate this wealth index into an integer between 1 and 100, corresponding to the household’s wealth percentile relative to the distribution of the entire sample. In addition to the impact of PAs on household living standards, we seek to understand their influence on socioeconomic inequalities within the populations concerned. To do this, we propose using a standardized Z-score of the wealth index, which allows us to compare the relative distribution of wealth around the mean within the study population at the level of each survey group.

@fig-distr-wi shows that the control and treatment groups have very similar wealth profiles, with averages between 47 and 50, and medians ranging from 46 to 51. In contrast, the excluded group is at the top of the national distribution of wealth, with an average above 50 and a median between 63 and 77. Its distribution is generally shifted upwards, with higher values across the distribution. This suggests that there is no difference between the control and treatment groups; however, the standard deviations of the two groups indicate a high degree of heterogeneity in household living standards.

#figure([
#box(image("figures/distr_wi.png", width: 80%))
], caption: figure.caption(
position: top, 
[
Distribution of the rural wealth index (percentile)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-distr-wi>


#emph[Source: Authors]

The graph shows boxplots of the wealth index distribution in percentiles (blue for the control group, red for the treatment group, and grey for the excluded groups). Across all surveys, for all years of the study, the wealth index distributions of the treatment and control groups are roughly similar.

#figure([
#box(image("figures/distr_zs.png", width: 80%))
], caption: figure.caption(
position: top, 
[
Distribution of the Zscore of the rural wealth index (percentile)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-distr-zs>


#emph[Source: Authors]

The graph shows boxplots of the zscore wealth index distribution in percentiles (blue for the control group, red for the treatment group, and grey for the excluded groups). Across all survey years, the wealth index Z scores of the treatment and control group is roughly similar.

= Results
<results>
== Matching between treatment and control
<matchingbetweentreatmentandcontrol>
The distribution analysis of covariates prior to matching shows a substantial imbalance between the treatment and control groups (2008 case in the @tbl-balance-long-2008). After matching, the SMDs improve for all variables in each year. However, the population density in 2000 remains the most difficult variable to balance. This reflects the fact that PAs are not randomly distributed. The @fig-before-2008 shows the difference in covariates between the two groups.

#figure([
#[
#set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); #table(
  columns: (20%, 20%, 20%, 20%, 20%),
  align: (left,center,center,center,center,),
  table.header(table.cell(align: center, colspan: 5, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.25em , fill: rgb("#333333")); Covariates balance before and after matching],
    table.cell(align: bottom + left, rowspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); Variable], table.cell(align: center, colspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); #block[
    2008
    ]], table.cell(align: center, colspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); #block[
    2021
    ]],
    table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); Before matching], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); After matching], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); Before matching], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); After matching],),
  table.hline(),
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Elevation (m)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.568], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.056], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.433], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.092],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Population density in 2000 (km²)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.976], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.050], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[1.172], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.086],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Slope (%)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.105], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.018], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.150], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.012],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Accessibility in 2000 (min)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.365], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.018], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.379], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.047],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Forest cover rates in 2000 (%)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.729], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.007], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.925], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.101],
)
]
], caption: figure.caption(
position: top, 
[
Standardized Mean Difference of the Year 2008 and 2021
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-balance-long-2008>


#emph[Source: Authors]

#figure([
#box(image("figures/density_before_2008.png", width: 80%))
], caption: figure.caption(
position: top, 
[
Covariate balance before matching (2008))
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-before-2008>


#emph[Source: Authors]

#figure([
#box(image("figures/density_after_2008.png", width: 80%))
], caption: figure.caption(
position: top, 
[
Covariate balance after matching (2008))
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-after-2008>


#emph[Source: Authors]

These graphs show the distribution of covariates before matching between clusters located near protected areas (purple) and those located far from them (green). Each panel illustrates the covariates used in the matching, namely forest cover in 2000, slope and elevation, population density in 2000, slope and accessibility in 2000 (estimated travel time by car from households to the nearest towns).

== Overall impact on livelihoods
<overallimpactonlivelihoods>
=== Estimation DID 2X2
<estimation-did-2x2>
In our DID estimation, we used robust standard errors. The results indicate no statistically significant difference at the 5% level between treated and control households. @fig-h1-2x2 presents the results of the 2x2 DID estimation comparing the treatment groups (households living within 10 km of a PAs) and the control groups (households more than 10 km away) prior to (1997-2008) and after (2008-2021) the PAs establishment.

#figure([
#[
#set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); #table(
  columns: 3,
  align: (left,center,center,),
  table.header(table.cell(align: bottom + left, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); ], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); Placebo 97-08], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , fill: rgb("#333333")); Traitement 08-21],),
  table.hline(),
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(Intercept)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[47.181], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[36.538],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(7.371)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(3.143)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[treat], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-4.341], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-1.850],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(7.518)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(4.446)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[post], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-12.514], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[20.344],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(9.174)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(5.857)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[treat\_post], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[3.297], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[1.743],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(8.779)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(5.722)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[spei\_wc\_n\_2], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.973], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-6.617],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(7.249)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(4.530)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[spei\_wc\_n\_1], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-1.494], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-8.422],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(3.648)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(2.678)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[spei\_wc\_n], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.649], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[18.015],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(6.288)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(2.927)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[hv219Femme], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-2.025], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[-5.503],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(1.453)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(0.967)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[hv220], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.125], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.097],
  table.cell(align: horizon + left, stroke: (bottom: (paint: rgb("#000000"), thickness: 0.75pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[], table.cell(align: horizon + center, stroke: (bottom: (paint: rgb("#000000"), thickness: 0.75pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(0.035)], table.cell(align: horizon + center, stroke: (bottom: (paint: rgb("#000000"), thickness: 0.75pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[(0.024)],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Num.Obs.], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[3672], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[6960],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[R2], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.025], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.098],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[R2 Adj.], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.023], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.097],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[AIC], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[34688.6], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[65113.5],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[BIC], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[34744.5], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[65175.2],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[RMSE], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[27.17], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[25.98],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Std.Errors], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[by: cluster\_uid], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[by: cluster\_uid],
)
]
], caption: figure.caption(
position: top, 
[
Static treatment effect estimates on well-being
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-did2s-static-WI>


#emph[Source: Authors]

The placebo test for the treatment period (1997-2008 period) yields small and insignificant coefficient (+3.3 wealth percentiles) indicating that no differential trend is detected prior to intervention (@tbl-h1-staggered-static). The supports the parallel trends assumption as the wealth indices of the two groups vary similarly before PAs creation.

Climatic conditions, as measured by the SPEI have no significant effect on rural wealth, and the effect of household characteristics appears to be minimal. On average, households headed by women are slightly poorer, while those with older members tend to be wealthier.

For the treatment period (2008-2021), the estimated effect is positive but statistically insignificant (+1.7 wealth percentiles). Households living near PAs appear to have slightly lower living standards, but the confidence interval clearly includes zero. Given the study’s ex ante power calculation, which indicates that effects smaller than approximately 7.5 wealth percentiles cannot be reliably detected (Supplementary materials). This result suggests that any true effect is either close to zero or below our detection threshold.

#figure([
#box(image("manuscript_files/figure-typst/fig-h1-2x2-1.svg"))
], caption: figure.caption(
position: top, 
[
Impact on well-being: 2x2 DiD impact on local population (\< 10km)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-h1-2x2>


#emph[Source: Authors]

=== Event study estimation
<event-study-estimation>
The 2×2 DID estimates indicate no statistically significant average effect of PAs creation on household living standards. Consistent with the pre analysis plan, our empirical design is powered to detect effects of approximately 7.5 wealth percentiles, and no such effect is detected. To further explore the possibility of heterogeneous effects over time, we therefore apply an estimator adapted to staggered treatment adoption and multiple periods, which allows treatment effects to vary dynamically @Borusyak2024.

To increase temporal coverage before and after treatment, we integrate additional survey waves from the Malaria Indicator Surveys (MIS) (1997, 2011, and 2013) and MICS 2018, which follows a survey design comparable to DHS and MIS and includes 17,870 households. These additional surveys improve coverage of both pre treatment and post treatment periods.

• Staggered DiD: static

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 3, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 10, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [did2s (static)],
    ),
    // tinytable header end

    // tinytable cell content after
[treat_on = 1], [4.519],
[], [(4.234)],
[Num.Obs.], [4246],
[R2], [0.006],
[R2 Adj.], [0.006],
[AIC], [41025.8],
[BIC], [41032.2],
[RMSE], [30.32],
[Std.Errors], [Corrected Clustered (cluster_uid)],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Static treatment effect estimates on well-being
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-h1-staggered-static>


#emph[Source: Author’s calculation]

The static staggered DID estimate suggests a positive but statistically insignificant association between treatment and household wealth (+4.5 wealth percentiles, standard error 4.23). Given the magnitude of uncertainty, this estimate does not allow us to reject the null hypothesis of no effect, motivating a dynamic event study analysis.

• Staggered DiD: dynamic event study

For the event study, each observation is indexed by its relative year to treatment, defined as the difference between the survey year and the year of PAs establishment for treated locations. This transformation allows outcomes to be compared along a common event time dimension across cohorts. Because surveys are conducted at irregular intervals and some relative years are represented by very few observations, we group relative years into two-year bins. Observations occurring more than ten years before or after treatment are further aggregated into open-ended bins, as shown in @fig-rel-years. This binning strategy improves the stability and precision of the estimated dynamic effects.

#figure([
#box(image("manuscript_files/figure-typst/fig-rel-years-1.svg"))
], caption: figure.caption(
position: top, 
[
Binning of relative years-to-treatment for the event study
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-rel-years>


#emph[Source: Author’s calculation]

#figure([
#box(image("manuscript_files/figure-typst/fig-h1-event-study-1.svg"))
], caption: figure.caption(
position: top, 
[
Impact on well-being: Event-study with 2-year bins
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-h1-event-study>


@fig-h1-event-study shows that the estimated event time coefficients are close to zero in the pre treatment period, and none of the pre-treatment coefficients are statistically distinguishable from zero, supporting the plausibility of the parallel trends assumption. Following PAs establishment, point estimates fluctuate across event time bins and are generally imprecise. However, the estimate for the longest horizon (more than 10 years after treatment) is large and negative, and its confidence interval lies entirely below zero. This pattern suggests a potential adverse effect emerging only at long horizons, while effects in the short and medium run remain statistically indistinguishable from zero. This long horizon estimate should nonetheless be interpreted cautiously, as it may reflect a specific subset of PAs. Although it includes 851 treated observations from 27 clusters, it primarily concerns PAs created at the beginning of the study period, which may differ systematically from later ones (for instance, lower tourism potential or other unobserved characteristics). Subject to further investigation, this estimate could reflect a genuine mechanism, such as a gradual decline in compensatory measures by PAs managers or the cumulative effects of land use restrictions on younger farming generations.

== Effect on inequalities
<effectoninequalities>
=== Estimation DID 2X2
<estimation-did-2x2-1>
The DID estimates indicate that PAs creation does not have a statistically significant average effect on wealth inequalities, as measured by the Z score of the wealth index. The placebo estimation for the pre treatment period (1997–2008) yields a small and statistically insignificant treatment effect (treat × post = 0.038, s.e. 0.035), supporting the plausibility of the parallel trends assumption for inequality outcomes. For the treatment period (2008–2021), the estimated treatment effect is negative but small and statistically insignificant (treat × post = −0.011, s.e. 0.018). As shown in Figure 7, the clustered confidence interval clearly overlaps zero, indicating no detectable causal effect of PAs creation on within-cluster wealth inequality. Hypothesis 2 is therefore not supported in terms of an average effect.

#figure([
#box(image("manuscript_files/figure-typst/fig-h2-2x2-1.svg"))
], caption: figure.caption(
position: top, 
[
Impact on inequalities: 2x2 DiD impact on local population (\< 10km)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-h2-2x2>


#emph[Source: Authors]

The estimated average effect (black dot) is slightly positive for the placebo period, but still not significant (the error bar crosses the horizontal line). During treatment, the estimated average effect is slightly below 0, pointing to a negative effect, but as the clustered confidence interval overlaps 0, the effect is not significant

The coefficient on the post indicator is positive and statistically significant in the main specification (post = 0.055, p \< 0.01), indicating a general increase in inequality over time between 2008 and 2021 that affects both treated and control clusters alike. This pattern reflects a time trend rather than a treatment effect, and should not be attributed to PAs creation. Climatic conditions, as measured by the SPEI, exhibit statistically significant associations with inequality in the treatment period, suggesting that climatic shocks disproportionately affect poorer households and contribute to widening disparities. In contrast, household demographic characteristics play a limited role: households headed by women exhibit slightly lower relative wealth positions in the inequality distribution, while the age of the household head is positively associated with relative wealth. Finally, the very low values of R² and adjusted R² indicate that the model explains only a small share of the variation in inequality outcomes. This is expected in short panel DID specifications using repeated cross sections and does not undermine the identification of the treatment effect, but it cautions against interpreting the model as predictive.

=== Event study estimation
<event-study-estimation-1>
• Staggered DiD: static specification

@tbl-h2-staggered-static reports the static staggered DID estimates for inequality outcomes. The estimated treatment effect is negative but very small (−0.007), and not statistically significant. This indicates that, on average, exposure to PAs creation is not associated with a detectable change in within cluster wealth inequality. The estimate is imprecise, and the null hypothesis of no effect should not be rejected

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 3, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 10, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [did2s (static)],
    ),
    // tinytable header end

    // tinytable cell content after
[treat_on = 1], [-0.007],
[], [(0.019)],
[Num.Obs.], [4246],
[R2], [-0.000],
[R2 Adj.], [-0.000],
[AIC], [7251.5],
[BIC], [7257.9],
[RMSE], [0.57],
[Std.Errors], [Corrected Clustered (cluster_uid)],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Static treatment effect estimates on inequalities
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-h2-staggered-static>


#emph[Source: Author’s calculation]

• Staggered DiD: dynamic event study

@fig-h2-event-study presents the dynamic event study estimates for inequality outcomes using two-year bins. Pre treatment coefficients are close to zero and statistically insignificant, supporting the plausibility of the parallel trends assumption for inequality measures. Following PAs creation, estimated effects remain small and statistically insignificant across all post treatment horizons.

#figure([
#box(image("manuscript_files/figure-typst/fig-h2-event-study-1.svg"))
], caption: figure.caption(
position: top, 
[
Impact on inequalities: Event-study with 2-year bins
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-h2-event-study>


#emph[Source: Authors]

== Heterogeneity
<heterogeneity>
We assess whether the average effect of PAs creation on household living standards differs by governance regime, distinguishing between strict PAs and multi-use PAs (supplementary materials). The 2×2 DID estimates indicate no statistically significant effect for either governance type, both in the placebo period (1997–2008) and in the treatment period (2008–2021). Point estimates differ in sign across regimes, but confidence intervals are wide and overlap zero, providing no evidence of systematic heterogeneity in average impacts by PAs governance.

#figure([
#box(image("manuscript_files/figure-typst/fig-h3-2x2-1.svg"))
], caption: figure.caption(
position: top, 
[
Heterogeneity of impact on well-being
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-h3-2x2>


#emph[Source: Authors]

The graph highlights the change in household living standards before and after the PAs implementation, by governance type of PAs. Point estimates differ slightly between multi-use and strict PAs in both periods, but confidence intervals overlap zero throughout, indicating no statistically significant differences.

As previous estimates show, staggered DID and event study specifications that allow for dynamic and cohort specific effects lead to consistent conclusions. While some coefficients are statistically significant at specific horizons, they are not stable across specifications or governance types, and do not alter the overall finding of no detectable average differential effect by PAs governance regime.

== Robustness tests
<Robustnesstests>
In this section, we discuss the sensitivity analyses and robustness checks that support our main results. First, to verify the robustness of our conclusions, we performed additional analyses using distances of 5 km and 15 km from the PA to define the treated group. We find that the creation of PAs did not lead to significant changes in household wealth percentiles for either distance. In neither case is the interaction coefficient significant. Conversely, the temporal effect is consistently positive and significant, reflecting a general increase in wealth after 2008, regardless of the distance considered. Climatic variables are also statistically significant, and socio-demographic indicators consistently show that female-headed households have lower wealth and that wealth levels are positively correlated with the age of the household head. The stability of these results across the three distance thresholds suggests that climatic and socio-demographic dynamics largely dominate the effect of proximity to PAs. These tests showed that our conclusions are robust to the definition of treated households.

Benjamini’s #cite(<Benjamini1995>, form: "year") False Discovery rate (FDR) method showed that, prior to multiple testing, the coefficients did not show a stable trend of improvement or deterioration in the wealth index. After controlling for the risk of false positives, only a marked negative effect on the wealth index remains statistically significant. This suggests a transient effect of impoverishment.

The pseudo panel approach shows that the average wealth index of household cohorts is influenced by environmental factors. The results indicate that households located at high altitudes and in densely populated areas have a higher wealth index. Forest cover has a positive and significant effect, while slope and accessibility have a negative effect on the wealth index. In contrast, socio-demographic variables have a weakly significant effect, suggesting that spatial and environmental conditions play a more decisive role than individual household characteristics.

Rosenbaum’s sensitivity test @Rosenbaum1983 shows that the results are not robust in the face of unobserved assignment biases. In 1997, the absence of any systematic difference between households close to and far from PAs before the introduction of PAs confirms the hypothesis of parallel trends. In 2008, the test suggests a significant effect when assuming no bias ($Gamma = 1$), but this quickly disappears as soon as a small unmeasured imbalance is introduced ($Gamma > 1$). The Hodges-Lehmann intervals include zero and the p-value bounds exceed the 0.05 threshold. In 2021, the estimated effect is small and marginally significant, but it is not robust to hidden bias assumptions. Limiting ourselves to these three years, the observed wealth inequalities cannot be causally and robustly attributed to proximity to PAs.

The DID estimates revealed that, on average, female-headed households have a lower wealth percentile (-2.307) than male-headed households (+0.7440). However, the interaction between the treatment and the gender of the household head does not reveal a significant difference between men and women. In other words, living near PAs has no particular effect on gender inequality among household heads. Similarly, no interaction between the treatment and the age of the household head was evident, even though households headed by people aged 45-59 appeared to be significantly wealthier (2.920) than others.

= Discussion
<discussion>
This study assesses the potentially contradictory effects of PAs on the household living standards and socioeconomic inequalities among rural households in Madagascar. By relying on a pre-analysis plan and combining matching with difference-in-difference approaches.

Hypothesis 1 posits that, on average, PAs reduce the living standards of rural households. Our results do not support this hypothesis. Across multiple specifications, we do not detect a statistically significant average effect of PAs creation on the household wealth index. Point estimates are generally negative, but confidence intervals are wide and include zero, implying that any true effect is either close to zero or below our detection capacity. A plausible explanation is that losses associated with restrictions on access to natural resources are offset, at least partially, by compensatory mechanisms, ecosystem services, or adaptation strategies, even if these mechanisms remain limited or uneven. On the other hand, the absence of a detectable effect may also reflect the presence of compensatory forces that offset impacts in the short and medium term. While some households experience income losses dues to restrictions on forest exploitation, others may benefit from employment opportunities in conservation projects or from diversifying their livelihood activities. As a result, losses and gains may counterbalance each other, leading to no observable net change in the short and medium run. At the same time, our dynamic results are consistent with the literature suggesting that the socioeconomic costs of PAs are often delayed and cumulative rather than immediate @Brockington2015. In the early stages, land-use restrictions may be absorbed through adaptation mechanisms. However, over the longer term, particularly in the context of population growth, these constraints can progressively erode household’s resilience and reduce their capacity to cope with shocks. This dynamic may explain the significantly negative effect observed ten years after the PAs’ creation. From a policy perspective, these findings underline the importance of strengthening inclusive measures, such as transparent revenue sharing from tourism or targeted transfers, to reduce the risk of vulnerability among households located near PAs.

Hypothesis 2 concerns the effect of PAs creation on within cluster wealth inequalities. The results show no statistically significant average impact of PAs on inequality, as measured by the Z score of the wealth index. Instead, observed changes in inequality are primarily associated with broader socio demographic and climatic dynamics. In particular, climatic shocks, as captured by drought indicators, are associated with increased inequality, suggesting that poorer households are less able to buffer adverse shocks. Household characteristics also matter: households headed by women tend to occupy lower relative positions in the wealth distribution, while those headed by older individuals tend to be better off, likely reflecting accumulated assets and social capital. These patterns point to structural drivers of inequality that operate largely independently of PAs creation. In the absence of inclusive policies, conservation interventions may nonetheless interact with these dynamics by reinforcing existing asymetries in access to opportunities, which calls for governance arrangements that explicitly address equity concerns.

Hypothesis 3 examines whether the effects of PAs creation differ by governance regime. The 2x2 DID estimates do not provide evidence of statistically significant heterogeneity between strict and multi-use PAs. While point estimates differ in sign across governance types, confidence intervals are wide and overlap zero, preventing any firm conclusion regarding differential average impacts. Staggered DID and event study analyses lead to consistent conclusions: although some coefficients are significant at specific horizons, they are not stable across specifications and do not alter the overall finding of no detectable systematic heterogeneity by governance type. These results suggest that, in the Malagasy context, formal governance categories alone are insufficient to explain variation in socioeconomic outcomes, and that implementation quality, timing, and local context likely play a more important role.

= Conclusion
<conclusion>
For decades, scientific literature has debated the impacts of PAs on the living conditions of local communities. These effects are highly context-dependent, varying according to geographic, ecological and socioeconomic factors. As a result, it is difficult to make generalized statements about whether PAs are beneficial or detrimental to households.

In the Malagasy context, overall, our results do not allow us to rule out that PAs have socioeconomic impacts on rural households. However, they suggest that the widespread concern, notably in Madagascar, that PAs creation substantially undermines rural living conditions is not supported at the temporal and spatial scale of our analysis. At the same time, these findings do not preclude the existence of longer term effects, nor of important sources of heterogeneity that are not captured by the dichotomous classifications used in this study.

In particular, the negative effects observed at long horizons in the dynamic analysis may point to delayed or cumulative mechanisms, or to differentiated impacts across specific subsets of PAs. This calls for further work to better document long term trajectories and to move beyond coarse governance typologies in order to understand how conservation policies interact with local socioeconomic dynamics over time. In this context, the integration of systematic socioeconomic monitoring into PA governance frameworks could help decision-makers identify emerging vulnerabilities before they become entrenched.

Finally, these findings should be interpreted in light of the scale and nature of the data used. The analysis is designed to detect average effects of PA creation on household wealth at a national scale, using repeated cross-sectional surveys and spatial proximity as a treatment proxy. As such, it is well suited to identifying large and systematic impacts, but less capable of capturing small, highly localized, or institution-specific effects. In particular, welfare changes that operate through non-monetary channels, gradual adaptation processes, or uneven implementation of compensation and enforcement are unlikely to be fully reflected in standard wealth indices. This limitation is not specific to the Malagasy case, but reflects a more general challenge for evaluating the development effectiveness of conservation policies using multi-purpose household surveys.

#pagebreak()
= CRediT authorship contribution statement
<credit-authorship-contribution-statement>
Iriana Razafimahenina: Conceptualization; Data curation; Formal analysis; Methodology; Writing - Original draft

Florent Bédécarrats: Conceptualization; Data curation; Formal analysis; Funding acquisition; Investigation; Methodology; Project administration; Resources; Software; Supervision; Validation; Writing - Original draft

Ingrid Dallmann: Conceptualization; Methodology; Supervision; Validation; Writing; Review & Editing

Holimalala Randriamanampisoa: Conceptualization; Formal analysis; Project administration; Supervision; Validation; Writing - Review & Editing

= Funding
<funding>
The study is performed in the framework of the BETSAKA project. The BETSAKA project is cofunded by the Development Impact Lab of the German KfW Development Bank; the Agence Française de Développement (AFD), through the PAIRES program, the French National Research Agency (ANR), and the French Research Institute for Sustainable Development (IRD).

= Declaration of interest
<declaration-of-interest>
One of the authors is an evaluation officer at AFD, and the BETSAKA project is funded by the Evaluation department of both AFD and KfW. While the operational departments of AFD and KfW also fund conservation projects in Madagascar and other countries, the Evaluation departments operate independently. They are committed to rigorous, unbiased studies and are supervised by independent entities within both institutions.

#pagebreak()
#block[
] <refs>


 

#set bibliography(style: "chicago-author-date")


#bibliography("references.bib")

