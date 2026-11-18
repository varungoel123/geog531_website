# GEOG 531: South Carolina-inspired regression labs

The main deliverable is `GEOG531_SC_Heat_OLS_to_Spatial.qmd`. It is self-contained: upload that single file to Posit Cloud, install its core packages in the Console once, and run the chunks in order or Render. The simulation, causal diagram, source audit, weights, helpers and analyses are inside the Quarto file. No external data download, API key, spatial file or Python is needed by students.

## Posit Cloud setup

1. Open a new RStudio project in Posit Cloud.
2. Upload and open `GEOG531_SC_Heat_OLS_to_Spatial.qmd`.
3. In the Console run:

```r
install.packages(c("tidyverse", "broom", "lmtest", "sandwich", "knitr", "rmarkdown"))
```

4. Run chunks sequentially, discussing each checkpoint. Click Render for the HTML document. Use R 4.1 or newer for `|>`.
5. Leave `params: run_spatial: false` for the core lab. For the continuation, install `spdep` and `sphet` and change that parameter to true. The instructor should prepare the base Cloud project before sharing if using the spatial section, as its spatial dependencies can take longer to install.

## What students investigate

- A known total effect, confounders and a mediator in a plausible extreme-heat/health scenario.
- Data auditing and a verified source correction.
- Crude and adjusted OLS with a numeric count exposure and continuous outcome.
- Single-sample estimation error versus conditional expected bias.
- A near-duplicate poverty measure, VIFs and a causally justified response.
- A curved adjustment relationship, heteroskedasticity, Q–Q and influence diagnostics.
- County fixed effects with repeated tract observations per county.
- Residual maps and a descriptive Moran's I that remains positive after county effects.
- An optional heteroskedastic spatial-error model, with explicit distinctions from spatial lag and hierarchical models.

The study has 736 artificial tracts in 46 artificial county groups in a hypothetical 2025. It is inspired by a South Carolina question but has no real county locations, boundaries or empirical SC values. Coordinates form a schematic grid. The annual mean health measure is a continuous average of residents' poor-physical-health days in a 30-day window, not an annual count of health events. Each tract receives equal analytical weight.

## Included files

| File/folder | Purpose |
|---|---|
| `GEOG531_SC_Heat_OLS_to_Spatial.qmd` | OLS, spatial bridge, categorical predictors and all three interaction types |
| `GEOG531_SC_Heat_Logistic_Poisson.qmd` | Standalone companion GLM lab and diagnostics |
| Matching `.R` | Optional plain-R version of the supplied chunks |
| `data/` | Optional raw, source-audit and clean CSV copies for inspection |
| `instructor/INSTRUCTOR_GUIDE.html` | Teaching notes, expected results and interpretation cautions |
| `instructor/numerical_reference.json` | Independently computed numerical reference |
| `instructor/validate_simulation.py` | Python QA implementation; not a student dependency |
| `preview_assets/` | Independent numerical plot previews used by the instructor guide |

The raw CSV includes one intentionally impossible health entry, identified in the audit CSV. The clean CSV corrects it from the verified value. Use the simulation in the Quarto file for the exact intended workflow and spatial weights.

## Verification boundary

The data generator uses the same simple Park–Miller uniforms and inverse-normal/inverse-Poisson transforms in R and Python. The Python implementation verifies the DAG-consistent mean equations, known effect, ordering, data integrity, multicollinearity, conditional-bias calculation, OLS/HC3 calculations, and descriptive residual spatial correlation. This is independent numerical checking, not a native R execution.

R and Quarto were unavailable during creation. The `.qmd` therefore still needs a local or Posit Cloud render before teaching. The optional `sphet` estimator was not run here. The instructor numerical report includes an oracle GLS calculation using known simulated covariance as a validation benchmark, not as an alleged `sphet` result or a student-available real-world estimator.

To independently rerun Python QA, install numpy, pandas and scipy and run `python instructor/validate_simulation.py` from the project folder. Students do not need Python. The Quarto file requires no extra file from this ZIP.


## Categorical predictors, interactions and the GLM companion

The original OLS file now adds a three-level cooling predictor, two binary baseline factors, and all three requested two-way interaction types. The original outcome still has its original constant heat effect and oracle bias check. A clearly named related variant, `poor_health_days_modified`, adds effect modification; it must not be assessed with the earlier constant-effect oracle.

| Type | Example |
|---|---|
| Continuous × continuous | Heat days × poverty |
| Continuous × categorical | Heat days × cooling-access category |
| Categorical × categorical | Warning program × housing profile |

The joint model retains all lower-order terms and all the known interaction terms. It includes prediction plots and finite contrasts, reference-level explanations, centering at 15 heat days / 20% poverty, and an optional spatial-error refit of the full interaction formula.

The companion `GEOG531_SC_Heat_Logistic_Poisson.qmd` can also be uploaded by itself to Posit Cloud. It recreates the covariates and factors internally. Core libraries are tidyverse and broom, plus the recommended MASS package for the negative-binomial extension. MASS is used through `MASS::glm.nb()` to avoid masking dplyr's select.

The GLM lab covers binary coding, conditional ORs and IRRs, heat-by-cooling interactions, probability contrasts, count predictions with person-year offsets, convergence/sparsity, calibration, Brier score, mean shape, influence, conditional dispersion, zero-count checks, quasi-Poisson and negative binomial. Ordinary AIC is compared only between likelihood models fitted to the same response/rows.

These GLM outcomes are generated independently conditional on the specified measured predictors, unlike the original continuous outcome's spatial errors. Their simplified outcome-specific DAG is documented. The binary and count outcomes cover the same complete hypothetical year, but the binary response is one sampled resident per tract and the count covers tract episode experience over positive population person-years. Neither outcome is a downstream control variable for the other.

`data/sc_heat_health_2025_extended.csv` contains the covariates, factors, original and modified continuous outcomes, and GLM outcomes. It is an optional reference copy; neither Quarto file reads it. The instructor folder contains separate numerical references and generator checks for the original and extended examples. New plots and worked interpretations are appended to the instructor HTML guide.

Allow additional class time: roughly 45–60 minutes for the OLS additions, 45–60 for logistic, and 45–60 for Poisson, with count overdispersion as an optional continuation. Installation, interpretation and discussion will vary with student preparation.

Native R/Quarto execution remains to be checked in Posit Cloud. Independent Python checks validate the extended generator, factor support, exact oracle interactions, model score equations, offset scaling and NB likelihood/dispersion behavior. They do not constitute native R or spatial-GMM execution.
