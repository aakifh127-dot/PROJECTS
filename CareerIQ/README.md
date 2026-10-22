# CareerIQ (Aurora Light edition): Student Placement and Career Recommendation Using R

100% R: Shiny + shinydashboard, ggplot2 + plotly, DT, dplyr/tidyr, glm / rpart / randomForest (+ optional xgboost).
Only the *motion* (animations) lives in `www/custom.css` and `www/custom.js`; all logic and UI are R.

## What is new in this edition

**Look and motion**
- Light "aurora" theme: glass cards, drifting gradient background, Plus Jakarta Sans font (needs internet; falls back to system fonts offline)
- Splash screen, sliding sidebar pill, direction-aware page transitions (pages slide forward or back)
- Staggered rise-in of cards and list items, animated ring gauge, animated progress bars with a shine sweep
- Count-up numbers (they animate from the previous value when you move a slider), 3-D tilt on career cards, hero spotlight that follows the mouse
- Button ripple, pulsing Predict button, confetti when placement probability is 70% or more, toast notifications, shimmer while charts recalculate
- Respects "reduce motion" system settings

**New features**
| Feature | Where |
|---|---|
| Load any dataset student into the form | Student Prediction |
| Tabbed pill form with sliders (Academic / Skills / Experience / Soft skills) | Student Prediction |
| Readiness level (Getting Started to Placement Star) and percentile vs all students | Student Prediction |
| Skill radar vs benchmark | Prediction, Skill Gap |
| One-page HTML report download | Student Prediction |
| 12-week animated roadmap + tools to learn for each top career | Career Recommendation |
| 9-domain radar and ranked animated bars | Career Recommendation |
| "Top 3 moves": every gap ranked by probability gain if closed | Skill Gap Analysis |
| Scenario presets (Code sprint, Experience push, Academic focus, All-rounder) | What-If Simulator |
| Goal Planner: shortest realistic route to a target probability | Goal Planner (new page) |
| Percentile ranks, you vs department, 15 most similar students | Peer Insights (new page) |
| Expected package (random-forest regression), k-means student personas, risk watch, AI coach summary, list of AI/ML techniques | AI Insights (new page) |
| Effort-aware Goal Planner: set a target, weeks available and a focus (Balanced / Academics / Skills / Experience). It ranks improvements by probability gain per week, shows a week-by-week route, total effort, and how many weeks the full target needs. If you are already above target it builds a stretch plan and a risk watch | Goal Planner |
| New department AI&ML (dataset regenerated and model retrained) | everywhere |
| Department filter on all charts, CSV download, live KPIs | Analytics, Dataset Explorer |
| Viva Q&A accordion | About |

Your existing `models/trained_model.rds` and `student_data.csv` are included and fully compatible. No re-training is needed.

## Folder structure

```
CareerIQ/
├── app.R                      Shiny UI + server (12 pages)
├── install_packages.R         one-time package installation
├── helper_functions.R         constants, feature metadata, validation, ggplot theme (light palette)
├── generate_dataset.R         creates student_data.csv (800 students)
├── data_preprocessing.R       load, clean, stratified split, single-student input builder
├── recommendation_engine.R    career top-3, skill gaps, advice, predict + local explanation
├── model_training.R           trains/compares models, selects best, saves the model
├── plot_functions.R           ggplot2 charts + native plotly radar charts
├── career_content.R     NEW   roadmaps and tools for each career domain
├── insights.R           NEW   percentile, readiness, gap payoff, similar students, goal planner, report,
│                              package regression, k-means personas, risk watch, coach summary
├── ui_components.R      NEW   KPI cards, ring gauge, animated bars, timeline builders
├── student_data.csv
├── models/trained_model.rds
└── www/custom.css, custom.js  NEW motion and theme
```

## Run in RStudio

```r
setwd("C:/path/to/CareerIQ")          # 1. set working directory (use forward slashes)
source("install_packages.R")          # 2. install packages (once)
shiny::runApp()                       # 3. launch the dashboard
```
The CSV and model are already included (retrained with the AI&ML department; run `source("model_training.R")` yourself if you want XGBoost included too). Only run `source("generate_dataset.R")` and
`source("model_training.R")` if you want to regenerate or retrain.

Pages 3 and 4 (Career, Skill Gap) need you to click **Predict Placement** once on page 2.
What-If, Goal Planner and Peer Insights work straight from the sliders on page 2.

## Common errors and solutions

| Problem | Solution |
|---|---|
| `cannot open file 'helper_functions.R'` | Wrong working directory. Run `setwd(...)`, check with `getwd()`. |
| `there is no package called 'xxx'` | Run `source("install_packages.R")`. |
| Red "Setup problem" box on Home | `student_data.csv` or `models/trained_model.rds` is missing. Regenerate/retrain. |
| `Model file is outdated` | Re-run `source("model_training.R")`. |
| XGBoost skipped | Optional. The app now starts even if xgboost is missing (that one model is just hidden). |
| Animations look plain / unstyled | Hard-refresh the browser (Ctrl+F5) so the new CSS/JS load. |
| Port already in use | `shiny::runApp(port = 5050)`. |

## Viva preparation
The answers are built into the **About Project** page (accordion). Extra points to mention:
- **Gap payoff:** for each skill below benchmark, the model is re-run with that skill set to the benchmark, so gaps are ranked by real probability gain.
- **Goal Planner:** greedy search; each step takes the single realistic improvement with the largest gain, with caps so no skill jumps unrealistically.
- **Honest limitation:** the dataset is synthetic and career weights are expert rules, not learned from outcomes.
