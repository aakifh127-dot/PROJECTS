# =====================================================================
# ui_components.R
# Reusable HTML building blocks for the animated interface. Each one is
# plain R (htmltools tags); the motion itself lives in www/custom.css
# (keyframes, transitions) and www/custom.js (counters, ripple, tilt).
# =====================================================================

library(shiny)

# ---- Animated KPI card ----------------------------------------------------
# If `count` is given, the number counts up from 0 (see custom.js).
# `key` lets the counter animate from its previous value on re-render.
kpi_card <- function(label, icon_name, tone = "indigo", count = NULL, decimals = 0,
                     prefix = "", suffix = "", text = NULL, note = NULL, key = NULL) {
  num <- if (!is.null(count) && is.finite(count)) {
    tags$span(class = "kpi-num", `data-count` = count, `data-decimals` = decimals,
              `data-prefix` = prefix, `data-suffix` = suffix, `data-key` = key,
              paste0(prefix, formatC(count, format = "f", digits = decimals, big.mark = ","), suffix))
  } else {
    tags$span(class = "kpi-num kpi-text", text %||% "N/A")
  }
  div(class = paste("kpi lift", paste0("tone-", tone)),
      div(class = "kpi-icon", icon(icon_name)),
      div(class = "kpi-body", num, div(class = "kpi-label", label),
          if (!is.null(note)) div(class = "kpi-note", note)))
}

kpi_grid <- function(...) div(class = "kpi-grid", ...)

# ---- Ring gauge (pure SVG, animated with CSS) --------------------------------
ring_gauge <- function(pct, color = "#6366F1", color2 = "#06B6D4", size = 200, stroke = 15,
                       caption = NULL, decimals = 0, key = NULL) {
  pct  <- max(0, min(100, pct))
  r    <- (size - stroke) / 2
  circ <- 2 * pi * r
  off  <- circ * (1 - pct / 100)
  gid  <- paste0("rg", sample.int(1e8, 1))
  svg  <- sprintf(
    paste0('<svg viewBox="0 0 %1$d %1$d" class="ring-svg">',
           '<defs><linearGradient id="%2$s" x1="0" y1="0" x2="1" y2="1">',
           '<stop offset="0%%" stop-color="%3$s"/><stop offset="100%%" stop-color="%4$s"/></linearGradient></defs>',
           '<circle class="ring-bg" cx="%5$s" cy="%5$s" r="%6$s" stroke-width="%7$d"/>',
           '<circle class="ring-fg" cx="%5$s" cy="%5$s" r="%6$s" stroke-width="%7$d" stroke="url(#%2$s)" ',
           'style="--c:%8$s;--off:%9$s" transform="rotate(-90 %5$s %5$s)"/></svg>'),
    size, gid, color, color2, size / 2, r, stroke, round(circ, 2), round(off, 2))
  div(class = "ring", style = sprintf("width:%dpx;height:%dpx;", size, size),
      HTML(svg),
      div(class = "ring-center",
          tags$span(class = "ring-num", `data-count` = pct, `data-decimals` = decimals,
                    `data-suffix` = "%", `data-key` = key, paste0(round(pct, decimals), "%")),
          if (!is.null(caption)) tags$span(class = "ring-cap", caption)))
}

# ---- Horizontal animated bar -------------------------------------------------
# `tone` is one of: indigo, cyan, green, amber, rose
match_bar <- function(pct, tone = "indigo", delay = 0, label = NULL, right = NULL) {
  pct <- max(0, min(100, pct))
  div(class = "mrow",
      if (!is.null(label) || !is.null(right))
        div(class = "mrow-head", span(class = "mrow-label", label), span(class = "mrow-right", right)),
      div(class = "mbar", div(class = paste("mbar-fill", paste0("tone-", tone)),
                              style = sprintf("--w:%s%%;--d:%ss;", round(pct, 1), round(delay, 2)))))
}

# ---- Lists with staggered slide-in ---------------------------------------------
bullet_list <- function(items, icon_name = "check-circle", tone = "indigo") {
  tags$ul(class = paste("fancy-list", paste0("tone-", tone)),
          lapply(seq_along(items), function(i)
            tags$li(style = sprintf("--i:%d;", i - 1), icon(icon_name), tags$span(items[[i]]))))
}

chip <- function(text, tone = "indigo", icon_name = NULL) {
  span(class = paste("chip", paste0("tone-", tone)), if (!is.null(icon_name)) icon(icon_name), text)
}

# ---- Section header ---------------------------------------------------------------
page_head <- function(title, subtitle = NULL, icon_name = NULL) {
  div(class = "page-head",
      if (!is.null(icon_name)) div(class = "ph-icon", icon(icon_name)),
      div(h2(title), if (!is.null(subtitle)) p(subtitle)))
}

# ---- Empty state ---------------------------------------------------------------------
empty_state <- function(title, text, button_id = NULL, button_label = NULL, icon_name = "compass") {
  div(class = "empty glass",
      div(class = "empty-orb", icon(icon_name)),
      h3(title), p(text),
      if (!is.null(button_id))
        actionButton(button_id, button_label, icon = icon("arrow-right"), class = "btn-grad"))
}

# ---- Skill progress rows (skill-gap page) -------------------------------------------------
gap_rows_ui <- function(g) {
  tagList(lapply(seq_len(nrow(g)), function(i) {
    tone <- switch(g$Status[i], Strong = "green", Close = "amber", "rose")
    div(class = "gap-row", style = sprintf("--i:%d;", i - 1),
        div(class = "gap-head",
            span(class = "gap-skill", g$Skill[i]),
            span(class = "gap-nums", sprintf("You %s / Goal %s", num_txt(g$Student[i]), num_txt(g$Recommended[i])))),
        div(class = "mbar thick",
            div(class = paste("mbar-fill", paste0("tone-", tone)),
                style = sprintf("--w:%s%%;--d:%ss;", max(3, round(g$Achieved[i])), round(0.05 * i, 2)),
                span(class = "mbar-txt", paste0(round(g$Achieved[i]), "%")))))
  }))
}

# ---- Career card -------------------------------------------------------------------------------------
career_card_ui <- function(rank, career, role, match, tip) {
  tones <- c("indigo", "cyan", "amber")
  ic <- CAREER_ICONS[[career]] %||% "briefcase"
  div(class = paste("career-card tilt", paste0("rank-", rank)),
      style = sprintf("--i:%d;", rank - 1),
      div(class = "rank-badge", paste0("#", rank)),
      div(class = "cc-icon", icon(ic)),
      h3(career), p(class = "role", role),
      ring_gauge(match, size = 118, stroke = 11, caption = "match", key = paste0("cc", rank)),
      p(class = "tip", icon("lightbulb"), tags$span(tip)))
}

# ---- Roadmap timeline ----------------------------------------------------------------------------------------
roadmap_ui <- function(career) {
  steps <- CAREER_ROADMAP[[career]]
  tools <- CAREER_TOOLS[[career]]
  if (is.null(steps)) return(NULL)
  parts <- strsplit(steps, " \\| ", perl = TRUE)
  div(class = "roadmap",
      div(class = "rm-line"),
      lapply(seq_along(parts), function(i)
        div(class = paste("rm-step", if (i %% 2 == 1) "from-left" else "from-right"),
            style = sprintf("--i:%d;", i - 1),
            div(class = "rm-dot", i),
            div(class = "rm-body", tags$b(parts[[i]][1]), p(parts[[i]][2])))),
      div(class = "rm-tools", span(class = "muted", "Tools to learn: "),
          lapply(tools, function(t) chip(t, "violet"))))
}

# ---- Journey (goal planner) -------------------------------------------------------------------------------------
journey_ui <- function(path, target) {
  div(class = "journey",
      lapply(seq_len(nrow(path)), function(i) {
        p <- path$Prob[i]
        div(class = "jrow", style = sprintf("--i:%d;", i - 1),
            div(class = "jstep", if (i == 1) icon("flag") else i - 1),
            div(class = "jbody",
                div(class = "jlabel", path$Label[i],
                    if (i > 1) span(class = "jweek", sprintf("week %s", path$Weeks[i]))),
                div(class = "mbar slim",
                    div(class = paste("mbar-fill", if (p >= 100 * target) "tone-green" else "tone-indigo"),
                        style = sprintf("--w:%s%%;--d:%ss;", round(p, 1), round(0.07 * i, 2)))),
                span(class = "jprob", sprintf("%.1f%%", p))))
      }))
}
