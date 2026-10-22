# CareerIQ – Student Placement Prediction & Career Recommendation

CareerIQ is an R-based intelligent career guidance system that predicts student placement probability, recommends suitable career domains, identifies skill gaps, and provides personalized improvement plans through an interactive R Shiny dashboard.

## Project Structure

```
CareerIQ/
├── app.R                     # Main Shiny application
├── install_packages.R        # Required package installation
├── generate_dataset.R        # Generate sample student dataset
├── data_preprocessing.R      # Data cleaning and preprocessing
├── model_training.R          # Train and evaluate ML models
├── recommendation_engine.R   # Career and skill recommendations
├── helper_functions.R        # Shared functions and validation
├── insights.R                # Prediction insights and goal planning
├── plot_functions.R          # Charts and visualizations
├── career_content.R          # Career roadmap content
├── ui_components.R           # Dashboard UI components
├── student_data.csv          # Student dataset
├── models/
│   └── trained_model.rds     # Pretrained placement model
└── www/
    ├── custom.css            # Dashboard styling
    └── custom.js             # UI animations
```

## Requirements

Before running CareerIQ, install:

- R programming language
- RStudio (recommended) or VS Code with R support
- A modern web browser
- Internet connection for the initial package installation

### Required R Packages

- shiny
- shinydashboard
- dplyr
- tidyr
- tibble
- ggplot2
- scales
- plotly
- DT
- randomForest
- rpart

Optional package: `xgboost`

## Development Setup

### Step 1 – Clone the Repository

```
git clone https://github.com/aakifh127-dot/PROJECTS.git
```

Open the `CareerIQ` folder inside the downloaded repository.

### Step 2 – Open RStudio

Open RStudio and set the working directory to the CareerIQ project folder.

```
setwd("C:/path/to/PROJECTS/CareerIQ")
```

Alternatively, open the CareerIQ folder as an RStudio Project or set it as your working directory using the RStudio interface.

### Step 3 – Install Required Packages

Run this command in the R console:

```
source("install_packages.R")
```

This automatically installs the required R packages if they are missing.

### Step 4 – Launch CareerIQ

```
shiny::runApp()
```

The CareerIQ dashboard will launch in RStudio's viewer or a web browser.

To open it on a specified port:

```
shiny::runApp(port = 5050)
```

Then visit:

`http://localhost:5050`

## Optional – Dataset Generation

A sample dataset is already included.

To regenerate the dataset:

```
source("generate_dataset.R")
```

## Optional – Model Training

A pretrained model is included in `models/trained_model.rds`.

To retrain the machine learning models:

```
source("model_training.R")
```

Retrain the model after changing or regenerating the dataset.

## Machine Learning Methods

CareerIQ uses:

- Logistic Regression
- Decision Tree
- Random Forest
- Optional XGBoost
- K-Means Clustering
- Weighted Career Recommendation
- Skill Gap Analysis

## Main Features

- Student placement probability prediction
- Career domain recommendations
- Skill gap identification
- Personalized improvement suggestions
- Interactive What-If Simulator
- Career Goal Planner
- Peer comparison and insights
- Interactive analytics dashboard
- Model performance evaluation
- Downloadable student reports

## Troubleshooting

| Problem                   | Solution                                  |
| ------------------------- | ----------------------------------------- |
| Missing R package         | Run `source("install_packages.R")`        |
| Dataset not found         | Check `student_data.csv` or regenerate it |
| Model not found           | Run `source("model_training.R")`          |
| File sourcing error       | Verify the working directory              |
| Port already in use       | Use `shiny::runApp(port = 5050)`          |
| Dashboard styling missing | Confirm the `www` folder exists           |

## Note

CareerIQ currently uses a synthetic student dataset for academic demonstration. Placement predictions and career recommendations are educational estimates, not guaranteed real-world outcomes.

## Quick setup for your GitHub visitors

You can also place just this short version near the beginning of your README, like HavNav:

Development Setup

Requirements: R, RStudio and required R packages.

```
source("install_packages.R")
shiny::runApp()
```

Optional checks:

```
source("generate_dataset.R")
source("model_training.R")
```

The first two commands are sufficient when using the sample dataset and pretrained model included in your uploaded CareerIQ ZIP.

Important: Keep `app.R`, the supporting R files, `student_data.csv`, `models/` and `www/` together inside the `CareerIQ` folder when uploading to GitHub. Your application uses relative file paths, so separating them could prevent it from running.
