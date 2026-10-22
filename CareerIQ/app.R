# =====================================================================
# app.R  --  CareerIQ (Aurora Light edition)
# Student Placement Prediction + Career Recommendation, pure R.
# Launch with:  shiny::runApp()   (working directory = CareerIQ folder)
#
# Pages (12): Home | Student Prediction | Career Recommendation |
#   Skill Gap Analysis | What-If Simulator | Goal Planner | Peer Insights | AI Insights |
#   Dataset Explorer | Analytics Dashboard | Model Performance | About
#
# Prerequisites: run generate_dataset.R and model_training.R once
# (or keep the ready-made student_data.csv + models/trained_model.rds).
# =====================================================================

source("helper_functions.R")
require_pkgs(c("shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2", "DT", "plotly", "scales"))
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(ggplot2)
  library(DT); library(plotly)
})
source("data_preprocessing.R")
source("recommendation_engine.R")
source("plot_functions.R")
source("career_content.R")
source("insights.R")
source("ui_components.R")

# ---- Load data and model (never crash the app if they are missing) ----
DATA <- tryCatch(load_clean_data(), error = function(e) e)
DATA_ERROR <- NULL
if (inherits(DATA, "error")) { DATA_ERROR <- conditionMessage(DATA); DATA <- NULL }

BUNDLE <- tryCatch(load_model_bundle(), error = function(e) e)
BUNDLE_ERROR <- NULL
if (inherits(BUNDLE, "error")) { BUNDLE_ERROR <- conditionMessage(BUNDLE); BUNDLE <- NULL }

if (!is.null(DATA)) {
  S <- career_score_matrix(DATA)
  DATA$Recommended_Career <- factor(DOMAINS[max.col(S, ties.method = "first")], levels = DOMAINS)
  DATA$Career_Match <- round(apply(S, 1, max), 1)
  if (!is.null(BUNDLE)) {
    DATA$Placement_Probability <- tryCatch(round(100 * predict_prob(BUNDLE$model, DATA), 1),
                                           error = function(e) NA_real_)
  }
}
SUMMARY <- if (!is.null(DATA)) dataset_summary(DATA) else NULL

# ---- AI / ML extras trained once at start-up (a second or two) ------------
PKG  <- if (!is.null(DATA)) tryCatch(fit_package_model(DATA), error = function(e) NULL) else NULL
CLUS <- if (!is.null(DATA)) tryCatch(fit_clusters(DATA, 4),  error = function(e) NULL) else NULL

# ---- Input wiring ---------------------------------------------------------
INPUT_MAP <- c(
  CGPA = "p_cgpa", Attendance_Percentage = "p_att", Number_of_Backlogs = "p_back",
  Aptitude_Score = "p_apt", Coding_Score = "p_code", Communication_Score = "p_comm",
  Technical_Skill_Score = "p_tech", Number_of_Projects = "p_proj",
  Number_of_Internships = "p_intern", Number_of_Certifications = "p_cert",
  Leadership_Score = "p_lead", Teamwork_Score = "p_team", Problem_Solving_Score = "p_ps"
)
DEFAULTS <- c(CGPA = 7.0, Attendance_Percentage = 80, Number_of_Backlogs = 0,
              Aptitude_Score = 60, Coding_Score = 55, Communication_Score = 60,
              Technical_Skill_Score = 55, Number_of_Projects = 1, Number_of_Internships = 0,
              Number_of_Certifications = 1, Leadership_Score = 55, Teamwork_Score = 60,
              Problem_Solving_Score = 55)

WHATIF_MAP <- c(CGPA = "w_cgpa", Coding_Score = "w_code", Aptitude_Score = "w_apt",
                Communication_Score = "w_comm", Technical_Skill_Score = "w_tech",
                Number_of_Internships = "w_intern", Number_of_Projects = "w_proj",
                Number_of_Certifications = "w_cert")

feat_slider <- function(feature) {
  i <- match(feature, FEATURE_INFO$feature)
  sliderInput(INPUT_MAP[[feature]], FEATURE_INFO$label[i],
              min = FEATURE_INFO$min[i], max = FEATURE_INFO$max[i],
              value = DEFAULTS[[feature]], step = if (feature == "CGPA") 0.1 else 1,
              ticks = FALSE, width = "100%")
}

w_slider <- function(feature, step = 1) {
  i <- match(feature, FEATURE_INFO$feature)
  sliderInput(WHATIF_MAP[[feature]], FEATURE_INFO$label[i],
              min = FEATURE_INFO$min[i], max = FEATURE_INFO$max[i],
              value = DEFAULTS[[feature]], step = step, ticks = FALSE, width = "100%")
}

cap1 <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))

qa <- function(q, a) tags$details(class = "qa", tags$summary(q), div(class = "ans", a))

flow_card <- function(i, icon_name, title, text) {
  div(class = "flow-card lift", style = sprintf("--i:%d;", i - 1),
      div(class = "n", i), div(class = "fi", icon(icon_name)), tags$b(title), p(text))
}

# =============================== UI ===========================================
ui <- dashboardPage(
  title = "CareerIQ - Placement & Career Intelligence",
  skin = "blue",
  dashboardHeader(
    title = tags$span(class = "brand", tags$span(class = "brand-mark", icon("graduation-cap")),
                      tags$span("Career", tags$b("IQ"))),
    titleWidth = 250,
    tags$li(class = "dropdown", div(class = "nav-pill", span(class = "dot"), "Aurora Light  |  100% R"))
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      tags$li(class = "header", "OVERVIEW"),
      menuItem("Home", tabName = "home", icon = icon("home")),
      tags$li(class = "header", "YOUR PROFILE"),
      menuItem("Student Prediction", tabName = "predict", icon = icon("user")),
      menuItem("Career Recommendation", tabName = "career", icon = icon("briefcase")),
      menuItem("Skill Gap Analysis", tabName = "gap", icon = icon("chart-bar")),
      menuItem("What-If Simulator", tabName = "whatif", icon = icon("sliders-h")),
      menuItem("Goal Planner", tabName = "goal", icon = icon("flag-checkered")),
      menuItem("Peer Insights", tabName = "peers", icon = icon("user-friends")),
      menuItem("AI Insights", tabName = "ai", icon = icon("brain")),
      tags$li(class = "header", "DATA & MODEL"),
      menuItem("Dataset Explorer", tabName = "data", icon = icon("table")),
      menuItem("Analytics Dashboard", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Model Performance", tabName = "perf", icon = icon("cogs")),
      menuItem("About Project", tabName = "about", icon = icon("info-circle"))
    ),
    div(class = "sidebar-card",
        tags$b("Built 100% in R"), "Shiny, ggplot2, plotly, DT",
        div(class = "chips", chip("glm", "indigo"), chip("rpart", "cyan"), chip("randomForest", "violet")))
  ),
  dashboardBody(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    div(id = "splash",
        div(class = "splash-logo", icon("graduation-cap")),
        div(class = "splash-title", "Career", tags$b("IQ")),
        div(class = "splash-bar", tags$i())),
    tabItems(

      # ---------------- Home ----------------
      tabItem("home",
        uiOutput("startup_alert"),
        div(class = "hero",
            div(class = "orb o1"), div(class = "orb o2"), div(class = "orb o3"),
            h1("Career", tags$b("IQ")),
            h3("Your ", span(class = "rot", span("placement chances"), span("dream career"),
                             span("skill gaps"), span("next best move")), ", decoded."),
            p("An R-based system that predicts placement probability, recommends careers, analyses skill gaps,",
              "explains its decisions, and lets students simulate and plan their improvement."),
            actionButton("hero_go", "Start predicting", icon = icon("bolt"), class = "btn-hero")),
        uiOutput("home_kpis"),
        div(class = "flow",
            flow_card(1, "user", "Enter a profile", "Use sliders or load any student from the dataset."),
            flow_card(2, "bolt", "Get a prediction", "Probability, level, percentile and the reasons behind it."),
            flow_card(3, "compass", "Explore careers and gaps", "Top 3 careers, a roadmap, and a ranked gap plan."),
            flow_card(4, "rocket", "Simulate and plan", "What-If presets and a Goal Planner for your target.")),
        fluidRow(
          box(title = "Placement rate by department", width = 7, solidHeader = TRUE, status = "primary",
              plotlyOutput("home_dept", height = 320)),
          box(title = "Where students are heading", width = 5, solidHeader = TRUE, status = "primary",
              uiOutput("home_careers"))
        )
      ),

      # ---------------- Student Prediction ----------------
      tabItem("predict",
        page_head("Student Prediction", "Build a profile and see the placement outlook instantly.", "user"),
        fluidRow(
          box(title = "Student profile", width = 5, solidHeader = TRUE, status = "primary",
              fluidRow(column(7, textInput("p_name", "Name", placeholder = "e.g. Priya Kumar")),
                       column(5, selectInput("p_dept", "Department", choices = DEPARTMENTS))),
              fluidRow(column(8, selectizeInput("p_pick", "Or load a sample student", choices = NULL,
                                                options = list(placeholder = "Search by name or ID..."))),
                       column(4, div(style = "margin-top:25px;",
                                     actionButton("p_load", "Load", icon = icon("download"), class = "btn-soft btn-block")))),
              tabsetPanel(
                id = "p_groups", type = "pills",
                tabPanel("Academic", icon = icon("book"),
                         feat_slider("CGPA"), feat_slider("Attendance_Percentage"), feat_slider("Number_of_Backlogs")),
                tabPanel("Skills", icon = icon("code"),
                         feat_slider("Aptitude_Score"), feat_slider("Coding_Score"), feat_slider("Technical_Skill_Score"),
                         feat_slider("Problem_Solving_Score"), feat_slider("Communication_Score")),
                tabPanel("Experience", icon = icon("briefcase"),
                         feat_slider("Number_of_Projects"), feat_slider("Number_of_Internships"),
                         feat_slider("Number_of_Certifications")),
                tabPanel("Soft skills", icon = icon("users"),
                         feat_slider("Leadership_Score"), feat_slider("Teamwork_Score"))
              ),
              actionButton("predict_btn", "Predict Placement", icon = icon("bolt"),
                           class = "btn-primary btn-lg btn-block btn-pulse")),
          column(7, uiOutput("pred_results"))
        ),
        uiOutput("pred_lower")
      ),

      # ---------------- Career ----------------
      tabItem("career",
        page_head("Career Recommendation", "Your three best-fit domains, with a plan for each.", "briefcase"),
        uiOutput("career_page")),

      # ---------------- Skill gap ----------------
      tabItem("gap",
        page_head("Skill Gap Analysis", "Where you stand against the benchmark, and what closing each gap is worth.", "chart-bar"),
        uiOutput("gap_page")),

      # ---------------- What-If ----------------
      tabItem("whatif",
        page_head("What-If Simulator", "Slide, compare, and see the probability move in real time.", "sliders-h"),
        fluidRow(
          box(title = "Adjust the profile", width = 4, solidHeader = TRUE, status = "primary",
              p(class = "muted", "Sliders start from the profile on the Student Prediction page."),
              div(class = "btn-row",
                  actionButton("pre_code", "Code sprint", icon = icon("code"), class = "btn-soft"),
                  actionButton("pre_exp", "Experience push", icon = icon("briefcase"), class = "btn-soft"),
                  actionButton("pre_acad", "Academic focus", icon = icon("book"), class = "btn-soft"),
                  actionButton("pre_all", "All-rounder", icon = icon("magic"), class = "btn-soft")),
              tags$hr(),
              w_slider("CGPA", 0.1), w_slider("Coding_Score"), w_slider("Aptitude_Score"),
              w_slider("Communication_Score"), w_slider("Technical_Skill_Score"),
              w_slider("Number_of_Internships"), w_slider("Number_of_Projects"),
              w_slider("Number_of_Certifications"),
              actionButton("w_reset", "Reset to my profile", icon = icon("undo"), class = "btn-default btn-block")),
          column(8,
            uiOutput("w_kpis"),
            fluidRow(
              box(title = "Before vs after", width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("w_compare", height = 300)),
              box(title = "Impact of each change", width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("w_effects", height = 300))
            ),
            uiOutput("w_msg")
          )
        )
      ),

      # ---------------- Goal planner ----------------
      tabItem("goal",
        page_head("Goal Planner", "Pick a target probability and CareerIQ searches for the shortest realistic route.", "flag-checkered"),
        fluidRow(
          box(title = "Plan settings", width = 4, solidHeader = TRUE, status = "primary",
              sliderInput("goal_target", "Target placement probability", min = 50, max = 99, value = 80,
                          step = 1, post = "%", ticks = FALSE, width = "100%"),
              sliderInput("goal_weeks", "Time available (weeks)", min = 4, max = 52, value = 24,
                          step = 2, ticks = FALSE, width = "100%"),
              tags$label(class = "control-label", "Focus area"),
              div(class = "pill-radio",
                  radioButtons("goal_focus", NULL, inline = TRUE,
                               choices = c("Balanced" = "all", "Academics" = "Academics",
                                           "Skills" = "Skills", "Experience" = "Experience"))),
              actionButton("goal_btn", "Build my plan", icon = icon("route"), class = "btn-primary btn-block"),
              p(class = "muted", style = "margin-top:12px;",
                "Starts from your profile and, step by step, picks the improvement that gains the most",
                "probability per week of effort, until your target is reached or time runs out.")),
          column(8, uiOutput("goal_out"))
        )
      ),

      # ---------------- Peer insights ----------------
      tabItem("peers",
        page_head("Peer Insights", "See how your profile compares with students like you.", "user-friends"),
        uiOutput("peer_page")),

      # ---------------- AI insights ----------------
      tabItem("ai",
        page_head("AI Insights", "Regression, clustering, risk watch and a plain-language coach, all trained in R.", "brain"),
        uiOutput("ai_page")),

      # ---------------- Dataset explorer ----------------
      tabItem("data",
        page_head("Dataset Explorer", "Filter, search and export the student records.", "table"),
        fluidRow(box(width = 12, solidHeader = TRUE, status = "primary", title = "Filters",
          column(4, selectInput("f_dept", "Department", c("All", DEPARTMENTS))),
          column(4, selectInput("f_status", "Placement Status", c("All", "Placed", "Not_Placed"))),
          column(4, selectInput("f_career", "Career Domain (recommended)", c("All", DOMAINS)))
        )),
        uiOutput("data_kpis"),
        fluidRow(box(width = 12, solidHeader = TRUE, status = "primary", title = "Student dataset",
                     div(class = "btn-row", downloadButton("dl_data", "Download filtered CSV", class = "btn-soft")),
                     DTOutput("data_table")))
      ),

      # ---------------- Analytics ----------------
      tabItem("analytics",
        page_head("Analytics Dashboard", "Ten views of the dataset. Filter them by department.", "chart-line"),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE, title = "Filter",
                     column(4, selectInput("a_dept", "Department", c("All", DEPARTMENTS))))),
        fluidRow(box(width = 6, plotlyOutput("a1")), box(width = 6, plotlyOutput("a2"))),
        fluidRow(box(width = 6, plotlyOutput("a3")), box(width = 6, plotlyOutput("a4"))),
        fluidRow(box(width = 6, plotlyOutput("a5")), box(width = 6, plotlyOutput("a6"))),
        fluidRow(box(width = 6, plotlyOutput("a7")), box(width = 6, plotlyOutput("a9"))),
        fluidRow(box(width = 6, plotOutput("a8", height = 460)), box(width = 6, plotlyOutput("a10", height = 460)))
      ),

      # ---------------- Model performance ----------------
      tabItem("perf",
        page_head("Model Performance", "How the models were compared and which one was selected.", "cogs"),
        uiOutput("perf_banner"),
        fluidRow(box(title = "Model comparison", width = 12, solidHeader = TRUE, status = "primary",
                     DTOutput("perf_table"))),
        fluidRow(box(width = 12, status = "primary",
                     selectInput("perf_model", "Show details for model",
                                 choices = if (!is.null(BUNDLE)) names(BUNDLE$models) else character(0),
                                 selected = if (!is.null(BUNDLE)) BUNDLE$final_name else NULL))),
        uiOutput("perf_cards"),
        fluidRow(
          box(title = "Confusion matrix", width = 6, solidHeader = TRUE, status = "primary",
              plotlyOutput("perf_cm", height = 340)),
          box(title = "ROC curves", width = 6, solidHeader = TRUE, status = "primary",
              plotlyOutput("perf_roc", height = 340))
        ),
        fluidRow(
          box(title = "Variable importance (permutation, final model)", width = 6, solidHeader = TRUE,
              status = "primary", plotlyOutput("perf_imp", height = 360)),
          box(title = "Variable importance (Random Forest Gini)", width = 6, solidHeader = TRUE,
              status = "primary", plotlyOutput("perf_imp_rf", height = 360))
        )
      ),

      # ---------------- About ----------------
      tabItem("about",
        page_head("About CareerIQ", "Innovation, technology and viva-ready answers.", "info-circle"),
        fluidRow(
          box(width = 5, solidHeader = TRUE, status = "primary", title = "Innovation",
              p("CareerIQ does not simply predict whether a student will be placed. It combines placement prediction,",
                "career recommendation, skill-gap analysis, explainable AI, personalised recommendations,",
                "What-If simulation and goal planning in one R-based system."),
              h4("Technology stack (R only)"),
              bullet_list(c("R Shiny + shinydashboard for the interface; custom CSS/JS only for motion",
                            "dplyr / tidyr for preprocessing, ggplot2 + plotly for graphs, DT for tables",
                            "glm, rpart, randomForest (and optional xgboost) for modelling",
                            "Model selection by 5-fold cross-validated AUC; evaluation on a 20% test set"), "cog"),
              p(class = "muted", "The dataset is synthetic and generated for demonstration. Predictions are",
                "guidance for students, not a guarantee of placement.")),
          box(width = 7, solidHeader = TRUE, status = "info", title = "Viva preparation",
            qa("What is innovative in this project?",
               "Beyond a placed / not-placed label, CareerIQ explains why, recommends careers, quantifies skill gaps, simulates improvements and plans a route to a target probability."),
            qa("Why R and Shiny?",
               "R covers cleaning (dplyr), modelling (glm, rpart, randomForest), visualisation (ggplot2) and the web app (Shiny) in one language, with reactive inputs that update instantly."),
            qa("Which algorithms are used?",
               "Logistic Regression (interpretable baseline), Decision Tree (rule-based), Random Forest (ensemble for non-linear interactions) and optionally XGBoost."),
            qa("Why was the final model selected?",
               "Each model gets a 5-fold cross-validated AUC on the training set; the highest wins automatically and is then evaluated once on the untouched 20% test set."),
            qa("What is placement probability?",
               "The model's estimated chance (0-100%) that a student with this profile is placed. At or above 50% the status is 'Likely to be Placed'."),
            qa("How is career recommendation calculated?",
               "Each of 9 domains has a weighted formula over normalised scores (e.g. Software Development weights coding and problem solving). The weighted average is the match %, and the top 3 are shown."),
            qa("What is feature importance and explainable AI?",
               "Importance ranks inputs by how much they drive predictions (here: AUC drop when a feature is shuffled). The local explanation shows how each feature moves one student's probability versus a typical student."),
            qa("How does the skill-gap payoff work?",
               "For every skill below its benchmark, CareerIQ sets that skill to the benchmark, re-runs the model, and reports the probability gain, so gaps are ranked by real impact."),
            qa("How does the Goal Planner work?",
               "A greedy search: at each step it tries every realistic improvement, keeps the one with the largest probability gain, and repeats until the target is reached or no gain remains."),
            qa("What are the limitations?",
               "The data is synthetic, so accuracy reflects the generator's relationships. Career weights are expert rules, not learned from outcomes. Real deployment needs real institutional data.")
          )
        )
      )
    )
  )
)

# ============================== SERVER ========================================
server <- function(input, output, session) {

  need_data  <- function() validate(need(!is.null(DATA), paste("Dataset unavailable.", DATA_ERROR)))
  need_model <- function() validate(need(!is.null(BUNDLE), paste("Model unavailable.", BUNDLE_ERROR)))

  gg <- function(p, ...) {
    suppressWarnings(plotly::ggplotly(p, ...)) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(font = list(family = "Plus Jakarta Sans, Segoe UI, sans-serif", color = "#4B4F7A"),
                     paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                     legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.22))
  }

  go <- function(tab) shinydashboard::updateTabItems(session, "tabs", selected = tab)
  observeEvent(input$hero_go,      go("predict"))
  observeEvent(input$go_predict_c, go("predict"))
  observeEvent(input$go_predict_g, go("predict"))
  observeEvent(input$go_career,    go("career"))
  observeEvent(input$go_gap,       go("gap"))
  observeEvent(input$go_whatif,    go("whatif"))

  # ---------- startup alert ----------
  output$startup_alert <- renderUI({
    msgs <- c(DATA_ERROR, BUNDLE_ERROR)
    if (length(msgs) == 0) return(NULL)
    div(class = "alert alert-danger", tags$b("Setup problem: "), paste(msgs, collapse = " | "))
  })

  # ---------- Home ----------
  output$home_kpis <- renderUI({
    na <- function(label, ic) kpi_card(label, ic, "rose", text = "N/A")
    kpi_grid(
      if (is.null(SUMMARY)) na("Total students", "users")
      else kpi_card("Total students", "users", "indigo", count = SUMMARY$total_students, key = "h1"),
      if (is.null(SUMMARY)) na("Placement percentage", "percent")
      else kpi_card("Placement percentage", "percent", "green", count = 100 * SUMMARY$placement_rate,
                    decimals = 1, suffix = "%", key = "h2"),
      if (is.null(SUMMARY)) na("Average CGPA", "book")
      else kpi_card("Average CGPA", "book", "cyan", count = SUMMARY$avg_cgpa, decimals = 2, key = "h3"),
      if (is.null(DATA)) na("Most recommended career", "briefcase")
      else kpi_card("Most recommended career", "briefcase", "amber",
                    text = names(which.max(table(DATA$Recommended_Career)))),
      if (is.null(SUMMARY)) na("Average package", "rupee-sign")
      else kpi_card("Average package (placed)", "rupee-sign", "violet", count = SUMMARY$avg_package,
                    decimals = 1, suffix = " LPA", key = "h5"),
      if (is.null(BUNDLE)) na("Final model", "cogs")
      else kpi_card("Selected prediction model", "cogs", "indigo", text = BUNDLE$final_name),
      if (is.null(BUNDLE)) na("Test AUC", "bullseye")
      else kpi_card("Test AUC of final model", "bullseye", "pink",
                    count = BUNDLE$metrics[[BUNDLE$final_name]]$auc, decimals = 3, key = "h7")
    )
  })
  output$home_dept <- renderPlotly({ need_data(); gg(plot_rate_by_dept(DATA)) })
  output$home_careers <- renderUI({
    need_data()
    tb <- utils::head(sort(table(DATA$Recommended_Career), decreasing = TRUE), 6)
    tones <- c("indigo", "violet", "cyan", "green", "amber", "pink")
    tagList(lapply(seq_along(tb), function(i)
      match_bar(100 * as.numeric(tb)[i] / max(as.numeric(tb)), tones[i], delay = 0.12 * i,
                label = names(tb)[i], right = sprintf("%d students", as.numeric(tb)[i]))))
  })

  # ---------- Sample-student loader ----------
  observe({
    req(DATA)
    ch <- stats::setNames(DATA$Student_ID, paste0(DATA$Name, " (", DATA$Student_ID, ")"))
    updateSelectizeInput(session, "p_pick", choices = ch, server = TRUE)
  })
  observeEvent(input$p_load, {
    req(DATA)
    pick <- input$p_pick %||% ""
    req(nzchar(pick))
    s <- DATA[DATA$Student_ID == pick, , drop = FALSE]
    req(nrow(s) == 1)
    updateTextInput(session, "p_name", value = s$Name)
    updateSelectInput(session, "p_dept", selected = as.character(s$Department))
    for (f in names(INPUT_MAP)) updateSliderInput(session, INPUT_MAP[[f]], value = s[[f]])
    showNotification(paste("Loaded", s$Name, "- now press Predict Placement."), type = "message", duration = 4)
  })

  # ---------- Shared profile + prediction ----------
  profile_vals <- reactive({
    vals <- lapply(INPUT_MAP, function(id) input[[id]])
    vals$Department <- input$p_dept
    vals
  })

  profile_row <- reactive({
    need_model()
    vals <- profile_vals()
    errs <- validate_student_input(vals)
    validate(need(length(errs) == 0, paste(errs, collapse = " ")))
    make_student_row(vals)
  })

  result <- eventReactive(input$predict_btn, {
    need_model()
    vals <- profile_vals()
    errs <- validate_student_input(vals)
    validate(need(length(errs) == 0, paste(errs, collapse = "\n")))
    out <- tryCatch(predict_student(BUNDLE, make_student_row(vals)), error = function(e) e)
    if (inherits(out, "error")) validate(need(FALSE, paste("Prediction failed:", conditionMessage(out))))
    nm <- trimws(input$p_name %||% "")
    out$name <- if (nzchar(nm)) nm else "Student"
    out
  })

  get_result <- function() {
    if (input$predict_btn == 0) validate(need(FALSE, "Enter student details and click 'Predict Placement'."))
    result()
  }

  # celebrate a strong result
  observeEvent(result(), {
    r <- result()
    if (r$probability >= 0.7) session$sendCustomMessage("confetti", list())
    showNotification(sprintf("%s: %s placement chance (%s)", r$name, fmt_pct(r$probability, 1), r$band),
                     type = "message", duration = 5)
  })

  # ---------- Prediction page ----------
  output$pred_results <- renderUI({
    if (input$predict_btn == 0) {
      return(empty_state("Ready when you are",
                         "Set the profile on the left, then press Predict Placement to see your outlook.",
                         icon_name = "bolt"))
    }
    r <- result()
    p <- r$probability; ok <- p >= r$threshold
    lvl <- readiness_level(p); top <- r$careers[1, ]
    cols <- if (p >= 0.7) c("#10B981", "#06B6D4") else if (p >= 0.4) c("#F59E0B", "#F472B6") else c("#F43F5E", "#F59E0B")
    pctl <- if (!is.null(DATA) && "Placement_Probability" %in% names(DATA))
      percentile_rank(100 * p, DATA$Placement_Probability) else NA_real_
    div(class = "glass",
      div(class = "result-hero",
        ring_gauge(100 * p, cols[1], cols[2], size = 210, caption = "placement chance", decimals = 1, key = "pred_ring"),
        div(class = "rh-info",
          h3(r$name),
          div(class = "rh-row",
              span(class = paste("status-badge", if (ok) "ok" else "risk"), r$status), " ",
              span(class = paste("level-badge", paste0("tone-", lvl$tone)), icon(lvl$icon), lvl$name)),
          div(class = "rh-row", tags$b("Probability band: "), sprintf("%s (%s)", r$band, fmt_pct(p, 1))),
          div(class = "rh-row", tags$b("Percentile: "),
              if (is.na(pctl)) "n/a" else sprintf("ahead of %d%% of %s students", pctl, format(nrow(DATA), big.mark = ","))),
          div(class = "rh-row", tags$b("Best career fit: "), top$Role,
              chip(sprintf("%s - %d%% match", top$Career, round(top$Match)), "violet")),
          div(class = "rh-row muted", lvl$blurb))),
      div(class = "next-steps",
          span(class = "muted", "Keep going:"),
          div(class = "btn-row",
              actionButton("go_career", "Careers", icon = icon("briefcase"), class = "btn-soft"),
              actionButton("go_gap", "Skill gaps", icon = icon("chart-bar"), class = "btn-soft"),
              actionButton("go_whatif", "What-if", icon = icon("sliders-h"), class = "btn-soft"),
              downloadButton("dl_report", "Download report", class = "btn-soft"))))
  })

  output$pred_lower <- renderUI({
    if (input$predict_btn == 0) return(NULL)
    r <- result()
    tagList(
      fluidRow(
        box(title = "Strengths", width = 4, status = "success", solidHeader = TRUE,
            bullet_list(r$strengths, "check-circle", "green")),
        box(title = "Areas to improve", width = 4, status = "warning", solidHeader = TRUE,
            bullet_list(r$weaknesses, "exclamation-circle", "amber")),
        box(title = "Recommendations", width = 4, status = "info", solidHeader = TRUE,
            bullet_list(r$recommendations, "lightbulb", "cyan"))),
      fluidRow(
        box(title = "Skill radar vs benchmark", width = 4, solidHeader = TRUE, status = "primary",
            plotlyOutput("pred_radar", height = 360)),
        box(title = "Why this prediction?", width = 4, solidHeader = TRUE, status = "primary",
            plotlyOutput("pred_influence", height = 360)),
        box(title = "Top factors (model-wide)", width = 4, solidHeader = TRUE, status = "primary",
            plotlyOutput("pred_global", height = 360))))
  })
  output$pred_radar     <- renderPlotly({ plotly_skill_radar(get_result()$gaps) })
  output$pred_influence <- renderPlotly({ gg(plot_local_influence(get_result()$influence)) })
  output$pred_global    <- renderPlotly({
    need_model(); get_result()
    gg(plot_importance(BUNDLE$importance, "Top factors affecting prediction", 8))
  })

  output$dl_report <- downloadHandler(
    filename = function() paste0("CareerIQ_report_", gsub("[^A-Za-z0-9]+", "_", result()$name), ".html"),
    content  = function(file) writeLines(build_report_html(result()), file, useBytes = TRUE)
  )

  # ---------- Career ----------
  output$career_page <- renderUI({
    if (input$predict_btn == 0) {
      return(empty_state("Predict first to unlock your careers",
                         "Your top three domains, roadmap and fit chart appear here once you run a prediction.",
                         "go_predict_c", "Go to Student Prediction", "briefcase"))
    }
    r <- result(); tp <- r$careers
    cards <- lapply(seq_len(nrow(tp)), function(i)
      column(4, career_card_ui(i, tp$Career[i], tp$Role[i], tp$Match[i], tp$Tip[i])))
    tagList(
      do.call(fluidRow, cards),
      fluidRow(
        box(title = "Your 12-week roadmap", width = 7, solidHeader = TRUE, status = "primary",
            div(class = "pill-radio", radioButtons("road_pick", NULL, choices = tp$Career, inline = TRUE)),
            uiOutput("roadmap")),
        box(title = "Fit across all 9 domains", width = 5, solidHeader = TRUE, status = "info",
            plotlyOutput("career_radar", height = 400))),
      fluidRow(box(title = "Every domain, ranked", width = 12, solidHeader = TRUE, status = "primary",
                   uiOutput("career_bars"))))
  })
  output$roadmap <- renderUI({
    r <- get_result()
    roadmap_ui(input$road_pick %||% r$careers$Career[1])
  })
  output$career_radar <- renderPlotly({ plotly_career_radar(get_result()$all_scores) })
  output$career_bars <- renderUI({
    a <- get_result()$all_scores
    tones <- c("indigo", "cyan", "amber")
    tagList(lapply(seq_len(nrow(a)), function(i)
      match_bar(a$Match[i], if (i <= 3) tones[i] else "violet", delay = 0.06 * i,
                label = paste0(i, ". ", a$Career[i], "  (", a$Role[i], ")"), right = paste0(round(a$Match[i]), "%"))))
  })

  # ---------- Skill gap ----------
  output$gap_page <- renderUI({
    if (input$predict_btn == 0) {
      return(empty_state("Predict first to see your gaps",
                         "Run a prediction and CareerIQ will rank every gap by how much closing it helps.",
                         "go_predict_g", "Go to Student Prediction", "chart-bar"))
    }
    r <- result()
    imp <- gap_impact(BUNDLE, r$row)
    prio <- if (nrow(imp) == 0) {
      p(class = "muted", "Every benchmark is met. Nothing to close.")
    } else {
      tagList(lapply(seq_len(min(3, nrow(imp))), function(i)
        div(class = "prio", style = sprintf("--i:%d;", i - 1),
            div(class = "prio-num", i),
            div(class = "prio-main", tags$b(imp$Skill[i]),
                sprintf("Raise from %s to %s. %s.", num_txt(imp$From[i]), num_txt(imp$To[i]),
                        cap1(REC_TIPS[[imp$Feature[i]]]))),
            div(class = "prio-gain", sprintf("%+.1f pp", imp$Gain[i])))))
    }
    tagList(
      fluidRow(
        box(title = "Progress towards benchmarks", width = 6, solidHeader = TRUE, status = "primary",
            gap_rows_ui(r$gaps)),
        box(title = "Skill radar", width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("gap_radar", height = 460))),
      fluidRow(
        box(title = "Your top 3 moves", width = 5, solidHeader = TRUE, status = "success", prio),
        box(title = "Payoff of closing each gap", width = 7, solidHeader = TRUE, status = "primary",
            plotlyOutput("gap_impact_plot", height = 340))),
      fluidRow(
        box(title = "Gap table", width = 12, solidHeader = TRUE, status = "primary", DTOutput("gap_table"))),
      fluidRow(
        box(title = "Personalised advice", width = 12, solidHeader = TRUE, status = "info",
            bullet_list(r$recommendations, "lightbulb", "cyan"))))
  })
  output$gap_radar <- renderPlotly({ plotly_skill_radar(get_result()$gaps) })
  output$gap_impact_plot <- renderPlotly({
    need_model(); r <- get_result()
    gg(plot_gap_impact(gap_impact(BUNDLE, r$row)))
  })
  output$gap_table <- renderDT({
    g <- get_result()$gaps[, c("Skill", "Student", "Recommended", "Gap", "Achieved", "Status")]
    datatable(g, rownames = FALSE, options = list(dom = "t", pageLength = 13, scrollX = TRUE), selection = "none") %>%
      formatStyle("Status", color = styleEqual(c("Strong", "Close", "Needs Work"), c("#059669", "#D97706", "#E11D48")),
                  fontWeight = "bold")
  })

  # ---------- What-If ----------
  sync_sliders <- function() {
    vals <- profile_vals()
    for (f in names(WHATIF_MAP)) {
      v <- safe_numeric(vals[[f]])
      if (length(v) == 1 && !is.na(v)) updateSliderInput(session, WHATIF_MAP[[f]], value = v)
    }
  }
  observeEvent(profile_vals(), sync_sliders())
  observeEvent(input$w_reset, sync_sliders())

  apply_preset <- function(changes, label) {
    vals <- profile_vals()
    for (f in names(WHATIF_MAP)) {
      v <- safe_numeric(vals[[f]])
      if (length(v) != 1 || is.na(v)) next
      if (f %in% names(changes)) {
        i <- match(f, FEATURE_INFO$feature)
        v <- min(FEATURE_INFO$max[i], v + changes[[f]])
      }
      updateSliderInput(session, WHATIF_MAP[[f]], value = v)
    }
    showNotification(paste("Preset applied:", label), type = "message", duration = 3)
  }
  observeEvent(input$pre_code, apply_preset(c(Coding_Score = 20, Technical_Skill_Score = 10), "Code sprint"))
  observeEvent(input$pre_exp,  apply_preset(c(Number_of_Internships = 1, Number_of_Projects = 2,
                                              Number_of_Certifications = 2), "Experience push"))
  observeEvent(input$pre_acad, apply_preset(c(CGPA = 0.5, Aptitude_Score = 15), "Academic focus"))
  observeEvent(input$pre_all,  apply_preset(c(CGPA = 0.3, Coding_Score = 10, Aptitude_Score = 10,
                                              Communication_Score = 10, Technical_Skill_Score = 10,
                                              Number_of_Internships = 1, Number_of_Projects = 1,
                                              Number_of_Certifications = 1), "All-rounder"))

  whatif <- reactive({
    base_row <- profile_row()
    new_row <- base_row
    for (f in names(WHATIF_MAP)) {
      v <- safe_numeric(input[[WHATIF_MAP[[f]]]])
      validate(need(length(v) == 1 && !is.na(v), "Slider values are not ready yet."))
      new_row[[f]] <- v
    }
    cur <- predict_prob(BUNDLE$model, base_row)
    new <- predict_prob(BUNDLE$model, new_row)
    eff <- lapply(names(WHATIF_MAP), function(f) {
      if (isTRUE(all.equal(as.numeric(new_row[[f]]), as.numeric(base_row[[f]])))) return(NULL)
      r2 <- base_row; r2[[f]] <- new_row[[f]]
      data.frame(Feature = feature_label(f), Change = 100 * (predict_prob(BUNDLE$model, r2) - cur))
    })
    list(cur = cur, new = new, effects = do.call(rbind, eff))
  })

  output$w_kpis <- renderUI({
    w <- whatif(); d <- 100 * (w$new - w$cur)
    kpi_grid(
      kpi_card("Current probability", "user", "indigo", count = 100 * w$cur, decimals = 1, suffix = "%", key = "w_cur"),
      kpi_card("After improvement", "rocket", "violet", count = 100 * w$new, decimals = 1, suffix = "%", key = "w_new"),
      kpi_card("Change (percentage points)", if (d >= 0) "arrow-up" else "arrow-down", if (d >= 0) "green" else "rose",
               count = d, decimals = 1, prefix = if (d >= 0) "+" else "", suffix = " pp", key = "w_delta"))
  })
  output$w_compare <- renderPlotly({ w <- whatif(); gg(plot_whatif_compare(w$cur, w$new)) })
  output$w_effects <- renderPlotly({ w <- whatif(); gg(plot_whatif_effects(w$effects)) })
  output$w_msg <- renderUI({
    w <- whatif(); d <- 100 * (w$new - w$cur)
    txt <- if (d > 0.05) sprintf("These improvements would raise the placement probability by %.1f percentage points.", d)
           else if (d < -0.05) sprintf("These changes would lower the placement probability by %.1f percentage points.", abs(d))
           else "No change yet. Try a preset or move a slider above your current value."
    best <- w$effects
    lever <- if (!is.null(best) && nrow(best) > 0 && max(best$Change) > 0.05) {
      b <- best[which.max(best$Change), ]
      sprintf(" Biggest single lever: %s (%+.1f pp).", b$Feature, b$Change)
    } else ""
    div(class = "alert alert-info", icon("info-circle"), " ", txt, lever)
  })

  # ---------- Goal planner ----------
  goal <- eventReactive(input$goal_btn, {
    row <- profile_row()
    withProgress(message = "Searching for your best route...", value = 0.3, {
      tgt <- input$goal_target / 100; foc <- input$goal_focus %||% "all"
      g <- goal_plan(BUNDLE, row, target = tgt, focus = foc, budget_weeks = input$goal_weeks %||% 24)
      g$needed_weeks <- NA_real_
      if (!g$reached && g$start < tgt) {
        full <- goal_plan(BUNDLE, row, target = tgt, focus = foc, budget_weeks = Inf)
        if (full$reached) g$needed_weeks <- full$weeks
      }
      # already above the target: also prepare a stretch plan (+10 points, max 99%)
      g$stretch <- NULL
      if (g$start >= tgt && g$start < 0.97) {
        st <- min(0.99, g$start + 0.10)
        g$stretch <- goal_plan(BUNDLE, row, target = st, focus = foc, budget_weeks = input$goal_weeks %||% 24)
      }
      g
    })
  })

  risk_bars <- function(rk, n = 5) {
    rk <- utils::head(rk[rk$Loss > 0.05, , drop = FALSE], n)
    if (nrow(rk) == 0) return(p(class = "muted", "No single slip would change your odds noticeably."))
    mx <- max(rk$Loss)
    tagList(lapply(seq_len(nrow(rk)), function(i)
      match_bar(100 * rk$Loss[i] / mx, "rose", delay = 0.08 * i, label = rk$Skill[i],
                right = sprintf("-%.1f pp if %s to %s", rk$Loss[i], num_txt(rk$From[i]), num_txt(rk$To[i])))))
  }

  plan_ui <- function(g, title_ok, title_no, note_no = NULL) {
    steps <- nrow(g$path) - 1
    cols <- if (g$reached) c("#10B981", "#06B6D4") else c("#F59E0B", "#F472B6")
    gain <- 100 * (g$final - g$start)
    tagList(
      div(class = "glass",
        div(class = "result-hero",
          ring_gauge(100 * g$final, cols[1], cols[2], size = 190,
                     caption = if (g$reached) "target reached" else "best reachable", decimals = 1,
                     key = paste0("goal_ring_", substr(title_ok, 1, 4))),
          div(class = "rh-info",
            h3(if (g$reached) title_ok else title_no),
            div(class = "rh-row", sprintf("From %.1f%% to %.1f%% (target %d%%): %d step%s, about %s week%s of effort.",
                                          100 * g$start, 100 * g$final, round(100 * g$target), steps,
                                          if (steps == 1) "" else "s", g$weeks, if (g$weeks == 1) "" else "s")),
            div(class = "rh-row", lapply(seq_len(min(6, nrow(g$changes))), function(i)
              chip(sprintf("%s: %s to %s", g$changes$Skill[i], num_txt(g$changes$From[i]), num_txt(g$changes$To[i])),
                   if (g$changes$Gain[i] > 0) "green" else "indigo"))),
            if (!is.null(note_no) && !g$reached) div(class = "rh-row muted", note_no)))),
      kpi_grid(
        kpi_card("Total gain", "arrow-up", "green", count = gain, decimals = 1, prefix = "+", suffix = " pp", key = paste0("pg1", steps)),
        kpi_card("Effort needed", "clock", "violet", count = g$weeks, decimals = 0, suffix = " weeks", key = paste0("pg2", steps)),
        kpi_card("Steps in the plan", "route", "cyan", count = steps, key = paste0("pg3", steps)),
        kpi_card("Focus", "crosshairs", "amber", text = if (identical(g$focus, "all")) "Balanced" else g$focus)))
  }

  output$goal_out <- renderUI({
    if (input$goal_btn == 0) {
      return(empty_state("Where do you want to be?",
                         "Set a target, your time and a focus on the left, then press Build my plan.",
                         icon_name = "flag-checkered"))
    }
    g <- goal()
    steps <- nrow(g$path) - 1

    # ---- already above the target: stretch goal + risk watch ----
    if (g$start >= g$target) {
      rk <- risk_factors(BUNDLE, profile_row())
      top <- if (!is.null(g$stretch) && nrow(g$stretch$path) > 1) {
        tagList(
          div(class = "alert alert-info", icon("info-circle"), sprintf(
            " You are already above your %d%% target (%.1f%%). Here is a stretch plan to %.0f%%.",
            round(100 * g$target), 100 * g$start, 100 * g$stretch$target)),
          plan_ui(g$stretch, "Stretch goal within reach", "Stretch goal not fully reachable"),
          fluidRow(
            box(title = "Your stretch route", width = 6, solidHeader = TRUE, status = "primary",
                div(style = "max-height:430px;overflow-y:auto;padding-right:6px;", journey_ui(g$stretch$path, g$stretch$target))),
            box(title = "Probability along the route", width = 6, solidHeader = TRUE, status = "primary",
                plotlyOutput("goal_plot", height = 400))))
      } else {
        div(class = "glass",
          div(class = "result-hero",
            ring_gauge(100 * g$start, "#10B981", "#06B6D4", size = 190, caption = "current chance", decimals = 1, key = "goal_ring"),
            div(class = "rh-info",
              h3("You are already above your target"),
              div(class = "rh-row", sprintf("Your profile gives %.1f%%, above the %d%% goal.", 100 * g$start, round(100 * g$target))),
              div(class = "rh-row muted", "At this level extra effort changes little. Spend it on interview practice and choosing the best offer."))))
      }
      return(tagList(top, fluidRow(
        box(title = "Risk watch: what would hurt most", width = 12, solidHeader = TRUE, status = "warning",
            p(class = "muted", "If each skill slipped by two steps, this is how far your probability could fall."),
            risk_bars(rk)))))
    }

    # ---- normal plan ----
    note <- if (!is.na(g$needed_weeks)) {
      sprintf("With %d weeks you can reach %.1f%%. The full %d%% target needs about %d weeks, so raise the time slider to %d or more.",
              g$budget, 100 * g$final, round(100 * g$target), g$needed_weeks, ceiling(g$needed_weeks / 2) * 2)
    } else "Within realistic limits the model sees no further gain. Try a lower target or a different focus area."
    tagList(
      plan_ui(g, "Your target is within reach", "Target not fully reachable", note),
      fluidRow(
        box(title = "Your route", width = 6, solidHeader = TRUE, status = "primary",
            div(style = "max-height:430px;overflow-y:auto;padding-right:6px;", journey_ui(g$path, g$target))),
        box(title = "Probability along the route", width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("goal_plot", height = 400))),
      if (nrow(g$changes) > 0) fluidRow(
        box(title = "What to change", width = 12, solidHeader = TRUE, status = "success", DTOutput("goal_table"))))
  })
  output$goal_plot <- renderPlotly({
    g <- goal(); if (g$start >= g$target && !is.null(g$stretch)) g <- g$stretch
    gg(plot_goal_path(g$path, g$target))
  })
  output$goal_table <- renderDT({
    g <- goal()$changes[, c("Skill", "From", "To", "Weeks", "Gain")]
    names(g) <- c("Skill", "From", "To", "Effort (weeks)", "Probability gain (pp)")
    datatable(g, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 12)) %>%
      formatRound("Probability gain (pp)", 1)
  })

  # ---------- AI insights ----------
  live_pred <- reactive({
    need_model()
    r <- predict_student(BUNDLE, profile_row())
    nm <- trimws(input$p_name %||% "")
    r$name <- if (nzchar(nm)) nm else "Student"
    r
  })
  output$ai_page <- renderUI({
    r <- live_pred(); row <- r$row
    pkg <- predict_package(PKG, row)
    per <- assign_persona(CLUS, row)
    rk  <- risk_factors(BUNDLE, row)
    coach <- coach_summary(r, pkg, per, rk)
    tones <- c("indigo", "cyan", "amber", "pink")

    persona_ui <- if (is.null(CLUS)) p(class = "muted", "Clustering unavailable.") else {
      sm <- CLUS$summary
      tagList(lapply(seq_len(nrow(sm)), function(i) {
        me <- !is.null(per) && per$index == i
        div(class = paste("persona-row", if (me) "me"), style = sprintf("--i:%d;", i - 1),
            div(class = paste("persona-ic", paste0("tone-", tones[i])), icon(PERSONA_ICONS[[sm$Persona[i]]] %||% "user")),
            div(class = "persona-main",
                match_bar(100 * sm$PlacementRate[i], tones[i], delay = 0.1 * i,
                          label = paste0(sm$Persona[i], if (me) "  (you)" else ""),
                          right = sprintf("%d%% placed | %d students | %s", round(100 * sm$PlacementRate[i]), sm$Size[i], sm$Level[i]))))
      }))
    }

    tech <- list(
      c("Classification", "Logistic regression, decision tree, random forest and optional XGBoost predict placed or not placed."),
      c("Model selection", "5-fold cross-validated AUC chooses the final model automatically."),
      c("Regression", sprintf("A random forest predicts the expected package%s.",
                              if (is.null(PKG)) "" else sprintf(" (out-of-bag RMSE %.2f LPA, R-squared %.2f)", PKG$rmse, PKG$r2))),
      c("Clustering", "k-means (k = 4) groups students into data-driven personas without using labels."),
      c("Explainable AI", "Permutation importance (global) and per-student effects (local) show why a prediction was made."),
      c("Expert system", "Weighted rules score the 9 career domains from normalised skills."),
      c("Optimisation", "A greedy search plans the shortest realistic route to a target probability."),
      c("Natural language", "A template-based coach writes the summary. It is not a large language model."))

    tagList(
      kpi_grid(
        kpi_card("Placement probability", "bolt", "indigo", count = 100 * r$probability, decimals = 1, suffix = "%", key = "ai1"),
        kpi_card("Expected package if placed", "rupee-sign", "violet", count = pkg, decimals = 1, suffix = " LPA", key = "ai2"),
        kpi_card("Your student persona", "users", "cyan", text = if (is.null(per)) "N/A" else per$name),
        kpi_card("Models compared", "cogs", "amber", count = length(BUNDLE$models), key = "ai4")),
      fluidRow(
        box(title = "AI coach summary", width = 7, solidHeader = TRUE, status = "primary",
            bullet_list(coach, "magic", "violet"),
            p(class = "muted", "Generated from the model outputs with templates. It updates as you move the sliders on the Student Prediction page.")),
        box(title = "Student personas (k-means clustering)", width = 5, solidHeader = TRUE, status = "info", persona_ui)),
      fluidRow(
        box(title = "Risk watch: what would hurt most", width = 5, solidHeader = TRUE, status = "warning",
            p(class = "muted", "Probability lost if each skill slipped by two steps."), risk_bars(rk)),
        box(title = "AI and ML techniques used in CareerIQ", width = 7, solidHeader = TRUE, status = "primary",
            div(class = "tech-grid", lapply(seq_along(tech), function(i)
              div(class = "tech-card", style = sprintf("--i:%d;", i - 1), tags$b(tech[[i]][1]), span(tech[[i]][2])))))))
  })

  # ---------- Peer insights ----------
  peers <- reactive({ need_data(); similar_students(DATA, profile_row(), 15) })
  output$peer_page <- renderUI({
    need_data()
    row <- profile_row(); sim <- peers()
    feats <- c("CGPA", "Coding_Score", "Aptitude_Score", "Communication_Score", "Technical_Skill_Score",
               "Problem_Solving_Score", "Number_of_Projects", "Number_of_Internships")
    ranks <- vapply(feats, function(f) percentile_rank(as.numeric(row[[f]]), DATA[[f]]), numeric(1))
    bars <- tagList(lapply(seq_along(feats), function(i)
      match_bar(ranks[i], if (ranks[i] >= 70) "green" else if (ranks[i] >= 40) "amber" else "rose", delay = 0.07 * i,
                label = feature_label(feats[i]), right = sprintf("P%d", ranks[i]))))
    tagList(
      kpi_grid(
        kpi_card(sprintf("Placed among %d most similar students", sim$k), "users", "green",
                 count = 100 * sim$placed_rate, decimals = 0, suffix = "%", key = "pe1"),
        kpi_card("Average package of placed peers", "rupee-sign", "violet", count = sim$avg_package,
                 decimals = 1, suffix = " LPA", key = "pe2"),
        kpi_card("Average CGPA of similar students", "book", "cyan", count = sim$avg_cgpa, decimals = 2, key = "pe3"),
        kpi_card("Your department", "graduation-cap", "amber", text = as.character(row$Department))),
      fluidRow(
        box(title = "Where you rank (percentile)", width = 5, solidHeader = TRUE, status = "primary", bars,
            p(class = "muted", "P70 means you score at or above 70% of all students.")),
        box(title = "You vs your department", width = 7, solidHeader = TRUE, status = "primary",
            plotlyOutput("peer_bars", height = 380))),
      fluidRow(box(title = "Students most like you", width = 12, solidHeader = TRUE, status = "info",
                   DTOutput("peer_table"))))
  })
  output$peer_bars <- renderPlotly({ need_data(); gg(plot_peer_bars(profile_row(), DATA)) })
  output$peer_table <- renderDT({
    n <- peers()$neighbours
    d <- data.frame(Name = n$Name, Department = as.character(n$Department), CGPA = n$CGPA,
                    Coding = n$Coding_Score, Internships = n$Number_of_Internships,
                    Status = gsub("_", " ", as.character(n$Placement_Status)),
                    `Package (LPA)` = n$Placement_Package, Similarity = paste0(n$Similarity, "%"),
                    check.names = FALSE)
    datatable(d, rownames = FALSE, selection = "none", options = list(dom = "tp", pageLength = 8)) %>%
      formatStyle("Status", color = styleEqual(c("Placed", "Not Placed"), c("#059669", "#E11D48")), fontWeight = "bold")
  })

  # ---------- Dataset explorer ----------
  filtered <- reactive({
    need_data()
    d <- DATA
    if (input$f_dept != "All")   d <- d[d$Department == input$f_dept, , drop = FALSE]
    if (input$f_status != "All") d <- d[as.character(d$Placement_Status) == input$f_status, , drop = FALSE]
    if (input$f_career != "All") d <- d[as.character(d$Recommended_Career) == input$f_career, , drop = FALSE]
    d
  })
  output$data_kpis <- renderUI({
    d <- filtered()
    if (nrow(d) == 0) return(NULL)
    kpi_grid(
      kpi_card("Students in view", "users", "indigo", count = nrow(d), key = "d1"),
      kpi_card("Placement rate", "percent", "green", count = 100 * mean(d$Placement_Status == POSITIVE_CLASS),
               decimals = 1, suffix = "%", key = "d2"),
      kpi_card("Average CGPA", "book", "cyan", count = mean(d$CGPA), decimals = 2, key = "d3"),
      kpi_card("Average coding score", "code", "violet", count = mean(d$Coding_Score), decimals = 1, key = "d4"))
  })
  output$data_table <- renderDT({
    d <- filtered()
    validate(need(nrow(d) > 0, "No students match the selected filters."))
    datatable(d, filter = "top", rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, lengthMenu = c(10, 25, 50, 100))) %>%
      formatStyle("Placement_Status", color = styleEqual(c("Placed", "Not_Placed"), c("#059669", "#E11D48")),
                  fontWeight = "bold")
  })
  output$dl_data <- downloadHandler(
    filename = function() paste0("careeriq_students_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) utils::write.csv(filtered(), file, row.names = FALSE)
  )

  # ---------- Analytics ----------
  adf <- reactive({
    need_data()
    d <- DATA
    if (input$a_dept != "All") d <- d[d$Department == input$a_dept, , drop = FALSE]
    validate(need(nrow(d) > 5, "Too few students in this department to chart."))
    d
  })
  output$a1  <- renderPlotly({ gg(plot_status_bar(adf())) })
  output$a2  <- renderPlotly({ gg(plot_rate_by_dept(adf())) })
  output$a3  <- renderPlotly({ gg(plot_cgpa_status(adf())) })
  output$a4  <- renderPlotly({ gg(plot_coding_status(adf())) })
  output$a5  <- renderPlotly({ gg(plot_aptitude_dist(adf())) })
  output$a6  <- renderPlotly({ gg(plot_internship_rate(adf())) })
  output$a7  <- renderPlotly({ gg(plot_career_dist(adf())) })
  output$a8  <- renderPlot({ plot_corr_heatmap(adf()) }, bg = "transparent")
  output$a9  <- renderPlotly({ gg(plot_package_dist(adf())) })
  output$a10 <- renderPlotly({ need_model(); gg(plot_importance(BUNDLE$importance, "Top Factors Affecting Placement", 10)) })

  # ---------- Model performance ----------
  output$perf_banner <- renderUI({
    need_model()
    m <- BUNDLE$metrics[[BUNDLE$final_name]]
    div(class = "final-model-banner", icon("trophy"),
        tags$b(" Final model: "), BUNDLE$final_name,
        sprintf(" | selected by highest 5-fold CV AUC | Test accuracy %.1f%% | Test AUC %.3f | trained on %d, tested on %d students",
                100 * m$accuracy, m$auc, BUNDLE$n_train, BUNDLE$n_test))
  })
  output$perf_table <- renderDT({
    need_model()
    datatable(BUNDLE$comparison, rownames = FALSE, selection = "none", options = list(dom = "t")) %>%
      formatRound(c("CV_AUC", "Accuracy", "Precision", "Recall", "F1", "Test_AUC"), 3) %>%
      formatStyle("Status", target = "row",
                  backgroundColor = styleEqual("\u2605 Selected", "#D1FAE5"),
                  fontWeight = styleEqual("\u2605 Selected", "bold"))
  })
  sel_metrics <- reactive({ need_model(); req(input$perf_model); BUNDLE$metrics[[input$perf_model]] })
  output$perf_cards <- renderUI({
    m <- sel_metrics()
    kpi_grid(
      kpi_card(if (input$perf_model == BUNDLE$final_name) "Final model" else "Model shown", "cogs", "indigo",
               text = input$perf_model),
      kpi_card("Accuracy", "bullseye", "violet", count = 100 * m$accuracy, decimals = 1, suffix = "%", key = "m1"),
      kpi_card("Precision", "crosshairs", "cyan", count = m$precision, decimals = 3, key = "m2"),
      kpi_card("Recall", "search", "green", count = m$recall, decimals = 3, key = "m3"),
      kpi_card("F1 score", "balance-scale", "amber", count = m$f1, decimals = 3, key = "m4"),
      kpi_card("AUC", "chart-area", "pink", count = m$auc, decimals = 3, key = "m5"))
  })
  output$perf_cm  <- renderPlotly({ m <- sel_metrics(); gg(plot_confusion(m$confusion, paste("Confusion Matrix -", input$perf_model))) })
  output$perf_roc <- renderPlotly({ need_model(); gg(plot_roc(BUNDLE$roc, BUNDLE$comparison)) })
  output$perf_imp <- renderPlotly({ need_model(); gg(plot_importance(BUNDLE$importance, "Permutation importance (final model)", 10)) })
  output$perf_imp_rf <- renderPlotly({
    need_model()
    validate(need(nrow(BUNDLE$importance_rf) > 0, "Random Forest importance not available."))
    gg(plot_importance(BUNDLE$importance_rf, "Random Forest importance (Gini)", 10))
  })
}

shinyApp(ui, server)
