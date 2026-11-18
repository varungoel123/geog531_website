# GEOG 531 · Regression week

Start with **01_lecture.html**, the Reveal.js slide presentation for two 75-minute classes. The editable Quarto source is **01_lecture.qmd**. Expanded reading notes remain in **01_lecture_notes.html** / `.qmd`. Use **02_lab_linear.html** and **03_lab_glm.html** for the guided exercises. The lab and handout HTML files are self-contained reading copies. Analysis code runs in RStudio. The supplied slide preview embeds the figures but loads the Reveal.js runtime from a CDN, so it requires an internet connection for slideshow features. A local Quarto render embeds its dependencies using the source’s embed-resources option.

## What is included

| File/folder | Purpose |
|---|---|
| `01_lecture.qmd` / `.html` | Reveal.js lecture slides with speaker notes, spanning both sessions |
| `01_lecture_notes.qmd` / `.html` | Original expanded lecture handout |
| `slides.css` | Presentation typography and figure sizing |
| `02_lab_linear.qmd` / `.html` | 25-minute linear regression lab; optional extensions |
| `03_lab_glm.qmd` / `.html` | Logistic and Poisson lab in two 15-minute parts |
| `data/urban_heat.csv` | 400 synthetic household records; no real participants or places |
| `R/helpers.R` | Legacy helpers used by the expanded lecture notes; the revised labs use broom and tidyverse directly |
| `R/install_packages.R` | One-time setup for the core workflow and optional packages |
| `R/lab_linear.R`, `R/lab_glm.R` | Plain R copies of the lab chunks, including optional extensions |
| `assets/` | Original teaching figures, including the temperature DAG |
| `instructor/` | Teaching guide, worked answer key, numerical reference results and simulation truth; keep private |
| `GEOG531_Regression_Week.Rproj` | RStudio project; double-click before running chunks |
| `_quarto.yml`, `styles.css` | Shared rendering and appearance settings |

The **student ZIP** excludes the instructor folder. Its Quarto configuration renders only the lecture, expanded notes and labs. The complete package includes both student and instructor materials.

## RStudio setup

1. Unzip the package, preserving its folders. Do not open the Quarto files inside the ZIP.
2. Install R and a current RStudio with Quarto support, if needed.
3. Double-click `GEOG531_Regression_Week.Rproj`.
4. Once per computer, run `install.packages(c("tidyverse", "broom", "knitr", "rmarkdown"))` in the R console if those packages are missing, or run the supplied `R/install_packages.R` once. The labs require `tidyverse` and `broom`; optional extensions use `infer`, `lmtest` and `sandwich`.
5. Open a lab `.qmd`. Run the setup chunk and then each code chunk in order. Insert prose answers in the answer spaces.
6. Click **Render** for a fresh HTML copy. The lecture source uses `format: revealjs`; the labs and expanded notes use HTML. From a terminal in the project folder, `quarto render` renders all documents listed in `_quarto.yml`.

Alternative: from the open RStudio project, run `source("R/lab_linear.R", echo = TRUE)` or `source("R/lab_glm.R", echo = TRUE)`. These scripts run the core model code and put optional extensions inside `if (FALSE)` blocks. Select the code inside a block to run an extension after installing any optional packages. The scripts do not contain the lab’s question text. Use the lab HTML beside the script.

All code uses paths relative to the project root. The Quarto project sets the execution directory to the project root, including for instructor documents. If a file cannot be found, confirm that you opened the project and preserved the `data`, `R`, and `assets` folders. When rendering only an instructor file, still open it inside this project.

## Presenting the slides

Unzip the package before opening `01_lecture.html`. Use the arrow keys or Space to advance, Esc for the overview and S for speaker notes. The main lecture ends at the exit ticket, followed by five optional reference slides. The presentation has 49 slides including its title.

For a local preview with speaker notes, run `quarto preview 01_lecture.qmd`. For an HTML render, use `quarto render 01_lecture.qmd --to revealjs` from the project folder or click Render in RStudio. The lecture source uses static examples, so only Quarto is needed to render the slides. R remains necessary for the lab analyses.

The supplied HTML preview needs internet access to load Reveal.js from jsDelivr. If that runtime is unavailable, it displays the slides as a reading page and labels the missing slideshow runtime. A local Quarto render with `embed-resources: true` is the route to a presentation with embedded dependencies. Browser slideshow behavior and native Quarto rendering could not be tested in the creation environment.

Speaker notes are part of the editable lecture source and HTML. They include conceptual answers to lecture prompts. The student ZIP excludes the separate instructor folder and worked lab answer key.

## Sequence

Class 1: conceptual linear regression, continuous/binary predictors, DAG-based adjustment and diagnostics (40 minutes); Lab 1 (25); logistic bridge (10).

Class 2: retrieval and logistic interpretation (20 minutes); Lab 2A (15); Poisson rates (13); Lab 2B (15); GLM diagnostics (8); exit ticket and next week (4).

## Data dictionary

| Variable | Meaning |
|---|---|
| `household_id` | Unique synthetic household identifier |
| `canopy_pct` | Residential tree canopy, in percentage points, 5–78 |
| `canopy10` | `canopy_pct / 10`; one unit = 10 percentage points |
| `deprivation` | Synthetic pre-exposure category: 0 lower, 1 higher |
| `cool_roof` | Pre-existing cool roof: 0 absent, 1 present; independent of canopy/deprivation in the simulation |
| `shade_index` | Arbitrary-unit synthetic mediator of canopy's effect on temperature |
| `indoor_temp_c` | Continuous indoor afternoon temperature, degrees Celsius |
| `heat_illness` | One respondent per household: 1 illness, 0 no illness, during a common fixed summer window |
| `household_members` | 1–5 household members |
| `weeks_observed` | 8–16 weeks of count follow-up |
| `person_weeks` | Members multiplied by count follow-up weeks; all positive |
| `heat_episodes` | Nonnegative household episode count during count follow-up; repeats allowed |
| `episodes_extra` | Separate overdispersed teaching outcome with the same specified conditional mean structure |

The temperature, respondent illness and household count outcomes are generated separately, conditional on the specified predictors. Do not treat them as an observed longitudinal biological chain. The temperature DAG includes shade; the GLM working DAGs are simplified and outcome-specific. There are no real geographic coordinates or neighborhood groupings. Hypothetical households are independent for this week's exercises.

The person-week offset assumes counts are proportional to exposure at the same covariate profile. This is a modeling assumption. It neither guarantees constant susceptibility nor replaces adjustment for measured common causes.

## Numerical and rendering verification

The supplied CSV was generated with a fixed NumPy seed. OLS, logistic and Poisson estimates, covariance matrices, intervals, predicted probabilities/counts, and Pearson dispersion screens were calculated with independent Python/SciPy implementations. The models still use `lm()` and `glm()`, while the revised lab workflow uses tidyverse and broom. The revised GLM lab intervals follow broom’s profile-likelihood method, independently checked numerically; the original lecture displays retain normal-approximation Wald intervals. Coefficients and predictions are unchanged. Small printing and plotting differences are expected.

**Build-environment limit:** Native R and Quarto were unavailable. The lab and expanded-notes HTML previews were assembled with Pandoc from the Quarto text, supplied figures, and independently computed numerical outputs. The slide preview was produced with Pandoc’s Reveal.js writer using the same Quarto Markdown source and static numerical displays. They are not evidence of a completed knitr/Quarto execution. The labs and expanded handout retain executable R chunks. The slide source uses ordinary R code blocks and precomputed numerical tables, so rendering the slide source itself does not require R. If changing the data or model specification, refit the models and update the slide tables. Re-render once in RStudio/Quarto before teaching, especially after editing. HTML previews have been checked for missing assets, output consistency and document structure. Browser layout could not be tested because browser execution is restricted in the build environment. Preview diagnostic figures use the same data/model but are not exact replicas of R's `plot.lm`.

`instructor/validate_models.py` independently refits the saved CSV and checks the reference results. It requires NumPy, Pandas and SciPy and is for instructor QA, not student setup. `R/check_models.R` provides equivalent native-R checks to run before class; these were supplied but could not be executed in the build environment.

## Sources

- The Effect, Chapter 13: https://theeffectbook.net/ch-StatisticalAdjustment.html
- OpenIntro IMS Chapters 24–26: https://openintro-ims.netlify.app/inferential-modeling
- Official R GLM/family/offset documentation, linked in the lecture.

Poisson is an instructor-provided extension to the assigned OpenIntro chapters. All narratives, data and figures in this package are original teaching materials.


## Revised lab workflow

The linear and GLM labs now use `library(tidyverse)` and `library(broom)`. Use R 4.1 or newer for the `|>` pipe. Tables use `tidy()`, new-data predictions and observation diagnostics use `augment()`, and visual diagnostics use `ggplot2`. Core lab code does not source `R/helpers.R`. The supplied data and DAG-based modeling questions are unchanged.

The optional `infer` example bootstraps the crude linear slope and is marked `eval: false`; it is for interactive exploration after the core activity. It does not change confounder adjustment or correct spatial dependence. The optional HC3 section uses `lmtest` and `sandwich` with broom. Install these optional packages only when using those extensions.

Native R/Quarto execution remains unverified in the creation environment. Run `R/check_tidy_workflow.R` and render the labs on the teaching computer before class. The HTML copies use independently computed reference results and representative plot previews, not executed tidyverse output.
