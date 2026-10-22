# =====================================================================
# career_content.R
# Static knowledge used by the animated "Career Roadmap" timeline:
# a 4-stage plan and a short list of tools to learn for each domain.
# Pure data, no dependencies.
# =====================================================================

CAREER_ICONS <- c(
  "Software Development"    = "code",
  "Data Analytics"          = "chart-bar",
  "Data Science"            = "flask",
  "Artificial Intelligence" = "robot",
  "Cybersecurity"           = "shield-alt",
  "Cloud Computing"         = "cloud",
  "Web Development"         = "globe",
  "UI/UX"                   = "pencil-ruler",
  "Networking"              = "network-wired"
)

CAREER_ROADMAP <- list(
  "Software Development" = c(
    "Weeks 1-3 | Foundations: revise data structures, complexity and one language in depth.",
    "Weeks 4-6 | Practice: solve 3 problems a week on a coding platform; join one contest.",
    "Weeks 7-10 | Build: ship two GitHub projects with clean READMEs and tests.",
    "Weeks 11-12 | Apply: mock interviews, resume polish, apply to 10 roles."),
  "Data Analytics" = c(
    "Weeks 1-3 | Foundations: SQL joins, aggregations and spreadsheet analysis.",
    "Weeks 4-6 | Practice: clean a messy dataset and answer 5 business questions.",
    "Weeks 7-10 | Build: publish a dashboard project with a clear story.",
    "Weeks 11-12 | Apply: case-study practice, portfolio link on resume."),
  "Data Science" = c(
    "Weeks 1-3 | Foundations: statistics, probability and Python/R data handling.",
    "Weeks 4-6 | Practice: regression, classification and cross-validation on public data.",
    "Weeks 7-10 | Build: an end-to-end project (data -> model -> app or report).",
    "Weeks 11-12 | Apply: explain your models in interview-style walkthroughs."),
  "Artificial Intelligence" = c(
    "Weeks 1-3 | Foundations: linear algebra, Python and core ML concepts.",
    "Weeks 4-6 | Practice: train neural networks on a small dataset with a framework.",
    "Weeks 7-10 | Build: an AI mini-project (vision, NLP or recommender) with a demo.",
    "Weeks 11-12 | Apply: write a short project blog and prepare ML interview questions."),
  "Cybersecurity" = c(
    "Weeks 1-3 | Foundations: networking basics, Linux and the CIA triad.",
    "Weeks 4-6 | Practice: beginner CTF labs and common web vulnerabilities.",
    "Weeks 7-10 | Build: a home-lab or write-ups of solved challenges; start a certification.",
    "Weeks 11-12 | Apply: security-analyst interview prep and resume with lab evidence."),
  "Cloud Computing" = c(
    "Weeks 1-3 | Foundations: virtualisation, networking and one cloud's core services.",
    "Weeks 4-6 | Practice: deploy a VM, storage and a database; learn IAM basics.",
    "Weeks 7-10 | Build: host a small app with CI/CD; sit an entry-level cloud certification.",
    "Weeks 11-12 | Apply: architecture-style questions and a public deployment link."),
  "Web Development" = c(
    "Weeks 1-3 | Foundations: HTML, CSS, JavaScript and responsive layout.",
    "Weeks 4-6 | Practice: a front-end framework and REST API consumption.",
    "Weeks 7-10 | Build: a full-stack project with auth and a database; deploy it.",
    "Weeks 11-12 | Apply: polish a portfolio site and practise live-coding rounds."),
  "UI/UX" = c(
    "Weeks 1-3 | Foundations: design principles, typography, colour and user research basics.",
    "Weeks 4-6 | Practice: redesign two existing apps and test with 5 users.",
    "Weeks 7-10 | Build: 2-3 case studies showing problem, process and outcome.",
    "Weeks 11-12 | Apply: portfolio review, design-challenge practice."),
  "Networking" = c(
    "Weeks 1-3 | Foundations: OSI/TCP-IP, addressing and subnetting.",
    "Weeks 4-6 | Practice: routing and switching labs in a simulator.",
    "Weeks 7-10 | Build: design a small office network; prepare for an associate-level certification.",
    "Weeks 11-12 | Apply: troubleshooting scenarios and interview practice.")
)

CAREER_TOOLS <- list(
  "Software Development"    = c("Git", "Data structures", "OOP", "Unit testing", "System design basics"),
  "Data Analytics"          = c("SQL", "Excel", "Power BI / Tableau", "Statistics", "Storytelling"),
  "Data Science"            = c("Python / R", "Statistics", "scikit-learn / caret", "Feature engineering", "Visualisation"),
  "Artificial Intelligence" = c("Python", "TensorFlow / PyTorch", "Linear algebra", "NLP / Vision", "MLOps basics"),
  "Cybersecurity"           = c("Linux", "Wireshark", "OWASP Top 10", "Burp Suite", "CTF labs"),
  "Cloud Computing"         = c("AWS / Azure / GCP", "Docker", "Linux", "IAM", "CI/CD"),
  "Web Development"         = c("HTML / CSS", "JavaScript", "React or similar", "Node / REST", "SQL / NoSQL"),
  "UI/UX"                   = c("Figma", "User research", "Wireframing", "Prototyping", "Accessibility"),
  "Networking"              = c("Cisco Packet Tracer", "Subnetting", "Routing", "Switching", "Firewalls")
)
