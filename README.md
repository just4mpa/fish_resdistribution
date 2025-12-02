# Fish redistribution from climate change risks human nutritional security 🌍🐟

The aim of this project is to estimate changes nutrition security, as fishes redistribute from climate change. This repository contains code to generate species distribution models (SDMs) for target fish species, and projections to future conditions according to climate models. We based nutritional values of fish species from the database of [Hicks et al. 2019](Hicks et al. 2019).

Hicks et al. (2019) provide nutrient data for 5907 fish species. We removed those identified to the genus level, resulting in 5853 species. The nutrients the group compiled were:

-   *Selenium*, *Zinc*, *Protein*, *Omega 3*, *Calcium*, *Iron* and *Vitamin_A*

We download occurrence records from GBIF for all the 5885 species, and run enssemble SDMs for XX species with more than 30 occurrence records.

**Nutritional Security Index (NSI)** should capture two main thi:

1.  **Nutrient quality per species** (e.g., protein, iron, zinc, omega-3, etc.).
2.  **Projected availability in MPAs** (from your SDMs, i.e., habitat suitability or biomass proxy).

So the index becomes a **weighted sum of nutritional contributions** of species present in an MPA under a given scenario.

------------------------------------------------------------------------

### 🔹 General Formula

For an MPA $j$ under climate scenario $t$:

$$
NSI_{j,t} = \sum_{i=1}^{S} \, A_{i,j,t} \times NQ_i
$$

where:

-   $S$ = number of species considered
-   $A_{i,j,t}$ = availability of species $i$ in MPA $j$ at time $t$ (e.g., binary presence/absence, probability of occurrence, or biomass proxy)
-   $NQ_i$ = nutrient quality score of species $i$ (can be a composite of multiple nutrients)

------------------------------------------------------------------------

### 🔹 Constructing the **Nutrient Quality Score (NQ)**

Several options, depending on your data:

-   **Single nutrient**: use concentration (mg/100 g).

-   **Multiple nutrients**: normalize each nutrient to [0,1] (relative to recommended dietary allowance or across species) and average or weight them.

    $$
    NQ_i = \sum_{k=1}^{K} w_k \cdot \frac{N_{i,k}}{RDA_k}
    $$

    where $k$ = nutrient, $w_k$ = weight (e.g., equal or based on dietary importance).

------------------------------------------------------------------------

### 🔹 Changes over time / scenarios

You can express **relative nutritional change** in an MPA:

$$
\Delta NSI_{j} = \frac{NSI_{j,t_{future}} - NSI_{j,t_{current}}}{NSI_{j,t_{current}}}
$$

This gives % gain/loss of nutritional capacity due to species redistribution.

### Interpretation

-   **Higher NSI** → MPA supplies more/better nutrients (per projected species pool).
-   **Lower NSI** → nutritional insecurity risk.
-   You can map NSI across MPAs and track change under climate scenarios.
