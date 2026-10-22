# =====================================================================
# generate_dataset.R  --  RUN ONCE to create student_data.csv
# Usage (working directory = CareerIQ folder):
#   source("generate_dataset.R")
#
# Design: three hidden "latent" traits per student (academic, practical,
# social) drive the visible scores, so columns are realistically
# correlated. Placement is drawn from a logistic model of those scores,
# so the ML models have a genuine signal to learn (but with noise, so
# accuracy is not unrealistically perfect).
# =====================================================================

source("helper_functions.R")

set.seed(2026)
n <- 800
clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# ---- Identity -------------------------------------------------------
male_names   <- c("Arun", "Karthik", "Vignesh", "Rahul", "Suresh", "Harish", "Naveen",
                  "Praveen", "Dinesh", "Ajay", "Manoj", "Sanjay", "Vijay", "Gokul",
                  "Ashwin", "Bharath", "Deepak", "Ganesh", "Hari", "Kishore")
female_names <- c("Priya", "Divya", "Anitha", "Kavya", "Meena", "Sneha", "Swathi",
                  "Nandhini", "Lakshmi", "Pooja", "Ramya", "Shalini", "Deepika",
                  "Harini", "Janani", "Keerthi", "Madhu", "Nithya", "Revathi", "Sowmya")
last_names   <- c("Kumar", "Raj", "Krishnan", "Subramanian", "Iyer", "Nair", "Reddy",
                  "Sharma", "Patel", "Menon", "Pillai", "Rao", "Devi", "Selvam",
                  "Murugan", "Balaji", "Chandran", "Natarajan", "Venkat", "Anand")

Gender <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))
first  <- ifelse(Gender == "Male",
                 sample(male_names, n, replace = TRUE),
                 sample(female_names, n, replace = TRUE))
Name   <- paste(first, sample(last_names, n, replace = TRUE))
Student_ID <- sprintf("STU%04d", seq_len(n))
Age <- sample(20:23, n, replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1))

Department <- sample(DEPARTMENTS, n, replace = TRUE,
                     prob = c(0.26, 0.18, 0.12, 0.10, 0.14, 0.11, 0.09))
tech_boost <- c(CSE = 8, IT = 7, `AI&DS` = 9, `AI&ML` = 9, ECE = 2, EEE = 0, Mechanical = -4)[Department]
tech_boost <- as.numeric(tech_boost)

# ---- Hidden traits --------------------------------------------------
academic  <- rnorm(n)
practical <- rnorm(n)
social    <- rnorm(n)

# ---- Visible features ----------------------------------------------
CGPA <- round(clip(7.4 + 0.9 * academic + rnorm(n, 0, 0.3), 5, 9.9), 2)
Attendance_Percentage <- round(clip(82 + 3 * academic + rnorm(n, 0, 7), 55, 100), 1)
Number_of_Backlogs <- pmin(rpois(n, pmax(0.05, 0.9 - 0.4 * academic)), 6)

Aptitude_Score        <- round(clip(62 + 6.6 * academic + 2.4 * practical + rnorm(n, 0, 9), 20, 100))
Coding_Score          <- round(clip(55 + tech_boost + 13 * practical + 4 * academic + rnorm(n, 0, 8), 15, 100))
Technical_Skill_Score <- round(clip(55 + 0.8 * tech_boost + 10 * practical + 5 * academic + rnorm(n, 0, 8), 15, 100))
Problem_Solving_Score <- round(clip(58 + 8 * academic + 9 * practical + rnorm(n, 0, 8), 20, 100))
Communication_Score   <- round(clip(62 + 12 * social + rnorm(n, 0, 8), 25, 100))
Leadership_Score      <- round(clip(58 + 10 * social + rnorm(n, 0, 10), 20, 100))
Teamwork_Score        <- round(clip(62 + 8 * social + rnorm(n, 0, 9), 25, 100))

Number_of_Projects       <- pmin(rpois(n, pmax(0.3, 1.6 + 0.5 * practical)), 6)
Number_of_Internships    <- pmin(rpois(n, pmax(0.05, 0.6 + 0.35 * practical + 0.2 * academic)), 4)
Number_of_Certifications <- pmin(rpois(n, pmax(0.2, 1.4 + 0.4 * practical)), 8)

# ---- Preferred domain (soft, score-driven) -------------------------
affinity <- cbind(
  (Coding_Score + Problem_Solving_Score) / 2,
  (Aptitude_Score + Problem_Solving_Score + 0.5 * Communication_Score) / 2.5,
  (Aptitude_Score + 0.5 * Coding_Score + CGPA * 10) / 2.5,
  (Coding_Score + CGPA * 10) / 2,
  (Technical_Skill_Score + 0.5 * Coding_Score) / 1.5,
  (Technical_Skill_Score + 0.5 * Coding_Score + 0.3 * Teamwork_Score) / 1.8,
  (0.6 * Coding_Score + 0.4 * Communication_Score),
  (Communication_Score + 0.3 * Leadership_Score + 0.3 * Teamwork_Score) / 1.6,
  (Technical_Skill_Score + 0.3 * Aptitude_Score) / 1.3
)
Preferred_Domain <- vapply(seq_len(n), function(i) {
  w <- exp((affinity[i, ] - max(affinity[i, ])) / 6)
  sample(DOMAINS, 1, prob = w / sum(w))
}, character(1))

# ---- Placement outcome (logistic, with noise) -----------------------
z <- 0.7 +
  0.90 * (CGPA - 7) +
  0.03 * (Coding_Score - 60) +
  0.02 * (Aptitude_Score - 60) +
  0.02 * (Communication_Score - 60) +
  0.015 * (Technical_Skill_Score - 60) +
  0.01 * (Leadership_Score - 60) +
  0.01 * (Attendance_Percentage - 80) +
  0.35 * Number_of_Internships +
  0.20 * Number_of_Projects +
  0.12 * Number_of_Certifications -
  0.70 * Number_of_Backlogs +
  rnorm(n, 0, 0.5)
Placement_Status <- ifelse(rbinom(n, 1, plogis(z)) == 1, "Placed", "Not Placed")

# ---- Package in LPA (0 for not placed) -----------------------------
pkg <- 3.2 + 0.05 * (Coding_Score - 60) + 0.03 * (Technical_Skill_Score - 60) +
  0.5 * Number_of_Internships + rnorm(n, 0, 1.2) + rexp(n, 1.5)
Placement_Package <- ifelse(Placement_Status == "Placed", round(clip(pkg, 2.5, 35), 2), 0)

# ---- Assemble & save -------------------------------------------------
student_data <- data.frame(
  Student_ID, Name, Gender, Age, Department, CGPA, Attendance_Percentage,
  Number_of_Backlogs, Aptitude_Score, Coding_Score, Communication_Score,
  Technical_Skill_Score, Number_of_Projects, Number_of_Internships,
  Number_of_Certifications, Leadership_Score, Teamwork_Score,
  Problem_Solving_Score, Preferred_Domain, Placement_Status, Placement_Package,
  stringsAsFactors = FALSE
)

write.csv(student_data, DATA_PATH, row.names = FALSE)

cat("Saved", nrow(student_data), "records to", DATA_PATH, "\n")
cat("Placement rate:", round(100 * mean(student_data$Placement_Status == "Placed"), 1), "%\n")
print(table(student_data$Placement_Status))
print(head(student_data, 5))
