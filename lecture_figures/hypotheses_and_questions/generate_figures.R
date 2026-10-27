# BB852 lecture 02, Hypotheses and questions: R code for every figure
# --------------------------------------------------------------------
# Recreates and extends the lecture's figures in ggplot2, using the BB852
# theme shared with the other lecture_figures generators.
#
# Data sources:
# - cholesterol.csv (CourseData): the drug experiment used in the book's
#   Randomisation tests chapter and lecture 09. The exact one-sided
#   randomisation p-value (31/924 = 0.0335) is recomputed here, as in the book.
# - All other figures use SIMULATED or ANALYTICAL values, stated in the
#   comments for each figure. Simulations use set.seed() so they reproduce.
#
# Run from the repository root:
# source("lecture_figures/hypotheses_and_questions/generate_figures.R")
#
# PNG files are written to
# lecture_figures/hypotheses_and_questions/output/<layout>/, numbered in the
# order the figures appear in the deck (BB852_02_Hypotheses_and_questions_v2).
#
# Labels use plotmath and \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)


# Define the BB852 theme and export functions -----------------------------

bb852 <- list(
  bg       = "#FBFAF6",
  text     = "#1B2230",
  muted    = "#6B6E75",
  grid     = "#DDE2E8",
  axis     = "#BCC6D2",
  slate    = "#24405F",
  coral    = "#D0745A",
  teal     = "#2A7F76",
  ochre    = "#C4932F",
  grey     = "#6B6E75",
  light    = "#9FB0C4",
  panel    = "#EDEFF1",
  cover    = "#2C4D70"
)

available_fonts <- unique(systemfonts::system_fonts()$family)
bb852_font <- if ("Avenir Next" %in% available_fonts) {
  "Avenir Next"
} else {
  "Helvetica"
}

theme_bb852 <- function(base_size = 18, base_family = bb852_font) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = bb852$bg, colour = NA),
      panel.background = element_rect(fill = bb852$bg, colour = NA),
      panel.grid.major = element_line(colour = bb852$grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = bb852$text, size = base_size),
      axis.text = element_text(colour = bb852$muted, size = base_size - 2),
      axis.ticks = element_blank(),
      legend.position = "none",
      strip.text = element_text(colour = bb852$text, size = base_size - 2,
                                face = "bold", hjust = 0),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.margin = margin(12, 16, 12, 12)
    )
}

# Blank canvas for schematic diagrams drawn with ggplot2 geometry.
theme_diagram <- function() {
  theme_void(base_family = bb852_font) +
    theme(
      plot.background = element_rect(fill = bb852$bg, colour = NA),
      panel.background = element_rect(fill = bb852$bg, colour = NA),
      plot.margin = margin(4, 4, 4, 4)
    )
}

# Sizes (inches) match the figure placeholders in the BB852 slide theme.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "hypotheses_and_questions", "output")
unlink(output_root, recursive = TRUE)

save_bb852 <- function(plot, filename, layout = "half", bg = bb852$bg) {
  size <- bb852_sizes[[layout]]
  if (is.null(size)) {
    stop("layout must be one of: ", paste(names(bb852_sizes), collapse = ", "))
  }
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(output_directory, paste0(filename, ".png")), plot,
         width = size[1], height = size[2], units = "in", dpi = 300,
         device = ragg::agg_png, bg = bg)
  invisible(filename)
}

# Every content figure is written in its slide layout and also at full and
# half width, as generate_all.R expects matching full/half sets.
save_all <- function(plot, filename, slide_layout) {
  save_bb852(plot, filename, slide_layout)
  for (layout in setdiff(c("full", "half"), slide_layout)) {
    save_bb852(plot, filename, layout)
  }
}

plots <- list()


# Diagram helpers -----------------------------------------------------------

box <- function(x, y, label, w = 13, h = 5.2, fill = bb852$panel,
                col = bb852$text, size = 5.1, face = "plain") {
  list(
    annotate("rect", xmin = x - w / 2, xmax = x + w / 2,
             ymin = y - h / 2, ymax = y + h / 2,
             fill = fill, colour = NA),
    annotate("text", x = x, y = y, label = label, colour = col,
             size = size, family = bb852_font, fontface = face,
             lineheight = 0.9)
  )
}

arrow_seg <- function(x, y, xend, yend, col = bb852$muted, lw = 0.6) {
  annotate("segment", x = x, y = y, xend = xend, yend = yend,
           colour = col, linewidth = lw,
           arrow = arrow(length = unit(0.12, "inches"), type = "closed"))
}

small_label <- function(x, y, label, col = bb852$muted, size = 5,
                        hjust = 0.5, face = "italic") {
  annotate("text", x = x, y = y, label = label, colour = col, size = size,
           family = bb852_font, fontface = face, hjust = hjust,
           lineheight = 0.9)
}

# 95% t-interval and two-sided p for a difference between two means, equal
# group sizes n and common SD.
ci_diff <- function(est, n, sd) {
  se <- sd * sqrt(2 / n)
  half <- qt(0.975, 2 * n - 2) * se
  p <- 2 * pt(-abs(est / se), 2 * n - 2)
  c(est = est, lo = est - half, hi = est + half, p = p)
}


# 00 Title slide: white swans and one black swan -----------------------------
# Decorative. The asymmetry of induction: many white swans never prove
# "all swans are white"; one black swan disproves it. Deterministic grid.

cover_grid <- expand.grid(x = 1:7, y = 1:11)
cover_grid$swan <- ifelse(cover_grid$x == 5 & cover_grid$y == 4, "black", "white")
plots$cover <- ggplot(cover_grid, aes(x, y, colour = swan)) +
  geom_point(size = 9) +
  scale_colour_manual(values = c(white = bb852$bg, black = bb852$coral)) +
  coord_fixed(xlim = c(0.3, 7.7), ylim = c(0.3, 11.7), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bb852$cover, colour = NA),
        panel.background = element_rect(fill = bb852$cover, colour = NA))
save_bb852(plots$cover, "00_cover", "cover", bg = bb852$cover)


# 01 The inductive method (one hypothesis, revised) --------------------------
# Redrawn from the original slide (after Gotelli & Ellison, A Primer of
# Ecological Statistics, ch. 4). "Accepted truth" is replaced by
# "provisionally accepted": induction never proves a hypothesis.

plots$inductive <- ggplot() +
  box(8, 20, "Initial\nobservations") +
  box(27, 20, "Hypothesis", fill = bb852$slate, col = bb852$bg, face = "bold") +
  box(46, 20, "Prediction") +
  box(65, 20, "New\nobservations") +
  box(84, 20, "Match the\nprediction?", fill = bb852$light) +
  arrow_seg(14.5, 20, 20.5, 20) + small_label(17.5, 23.8, "suggests") +
  arrow_seg(33.5, 20, 39.5, 20) + small_label(36.5, 23.8, "generates") +
  arrow_seg(52.5, 20, 58.5, 20) + small_label(55.5, 23.8, "test") +
  arrow_seg(71.5, 20, 77.5, 20) +
  # No: modify the hypothesis and go round again
  annotate("segment", x = 84, xend = 84, y = 17.4, yend = 7,
           colour = bb852$coral, linewidth = 0.6) +
  annotate("segment", x = 84, xend = 27, y = 7, yend = 7,
           colour = bb852$coral, linewidth = 0.6) +
  arrow_seg(27, 7, 27, 17.4, col = bb852$coral) +
  small_label(55.5, 4.6, "No: modify the hypothesis and test again",
              col = bb852$coral, face = "bold") +
  # Yes: provisionally accepted
  arrow_seg(84, 22.6, 84, 28) +
  small_label(86, 25.3, "Yes", col = bb852$teal, hjust = 0, face = "bold") +
  box(84, 31, "Provisionally accepted", w = 27, h = 4.6,
      fill = bb852$teal, col = bb852$bg, size = 5.1, face = "bold") +
  coord_cartesian(xlim = c(0, 100), ylim = c(1, 34), expand = FALSE) +
  theme_diagram()
save_all(plots$inductive, "01_inductive_method", "full")


# 02 The hypothetico-deductive method (multiple working hypotheses) ----------
# Redrawn from the original slide. Four hypotheses make distinct predictions;
# new data falsify A, C and D. B survives, but only provisionally.

hyp_y <- c(29.5, 21.5, 13.5, 5.5)
hyp_lab <- c("A", "B", "C", "D")
surv <- c(FALSE, TRUE, FALSE, FALSE)

hd <- ggplot() + box(7, 17.5, "Initial\nobservations", w = 12)
for (i in 1:4) {
  hd <- hd +
    arrow_seg(13, 17.5, 20, hyp_y[i]) +
    box(27, hyp_y[i], paste("Hypothesis", hyp_lab[i]), w = 13, h = 5,
        fill = bb852$slate, col = bb852$bg, size = 5, face = "bold") +
    arrow_seg(33.5, hyp_y[i], 38.5, hyp_y[i]) +
    box(45, hyp_y[i], paste("Prediction", hyp_lab[i]), w = 13, h = 5,
        size = 5) +
    annotate("segment", x = 51.5, xend = 57, y = hyp_y[i], yend = 17.5,
             colour = bb852$muted, linewidth = 0.6)
  out_fill <- if (surv[i]) bb852$teal else bb852$panel
  out_col <- if (surv[i]) bb852$bg else bb852$muted
  out_lab <- if (surv[i]) {
    paste(hyp_lab[i], "survives")
  } else {
    paste(hyp_lab[i], "falsified")
  }
  hd <- hd + arrow_seg(77, 17.5, 81.5, hyp_y[i], col = bb852$muted, lw = 0.45) +
    box(90, hyp_y[i], out_lab, w = 16, h = 5, fill = out_fill, col = out_col,
        size = 5, face = if (surv[i]) "bold" else "plain")
  if (!surv[i]) {
    hd <- hd + annotate("segment", x = 84, xend = 96, y = hyp_y[i],
                        yend = hyp_y[i], colour = bb852$coral, linewidth = 0.8)
  }
}
plots$hd <- hd +
  arrow_seg(57, 17.5, 60, 17.5) +
  box(68.5, 17.5, "New data:\ndo they match\neach prediction?", w = 16, h = 8,
      fill = bb852$light, size = 5) +
  small_label(45, 33.6, "each prediction unique to one hypothesis", size = 5) +
  coord_cartesian(xlim = c(0, 100), ylim = c(1.5, 35), expand = FALSE) +
  theme_diagram()
save_all(plots$hd, "02_hd_method", "full")


# 03 Cholesterol experiment (book: Randomisation tests chapter; lecture 09) ---
# Box plot with jittered points, the plot type students make in the book.

ch <- read.csv("CourseData/cholesterol.csv")
ch$Treatment <- factor(ch$Treatment, levels = c("Control", "Drug"))
ch_means <- tapply(ch$Cholesterol, ch$Treatment, mean)
observed_diff <- unname(diff(ch_means))
all_drug_groups <- combn(nrow(ch), 6)
exact_diffs <- apply(all_drug_groups, 2, function(rows) {
  mean(ch$Cholesterol[rows]) - mean(ch$Cholesterol[-rows])
})
exact_b <- sum(exact_diffs <= observed_diff + 1e-9)
exact_p <- exact_b / ncol(all_drug_groups)
exact_p2 <- sum(abs(exact_diffs) >= abs(observed_diff) - 1e-9) /
  ncol(all_drug_groups)
message(sprintf(paste("Cholesterol: means %.1f vs %.1f, difference %.1f,",
                      "exact one-sided p = %d/%d = %.4f, two-sided p = %.4f"),
                ch_means[1], ch_means[2], observed_diff, exact_b,
                ncol(all_drug_groups), exact_p, exact_p2))

set.seed(42)
plots$cholesterol <- ggplot(ch, aes(Treatment, Cholesterol)) +
  geom_boxplot(width = 0.5, fill = bb852$panel, colour = bb852$slate,
               outlier.shape = NA, linewidth = 0.6) +
  geom_jitter(width = 0.08, height = 0, colour = bb852$coral, size = 2.8) +
  # Diamond = group mean (the slide quotes means; the box line is the median)
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4.2,
               fill = bb852$slate, colour = bb852$bg) +
  labs(x = NULL, y = "Cholesterol (mg/dL)") +
  theme_bb852()
save_all(plots$cholesterol, "03_cholesterol", "half")


# 04 Type I and Type II errors ------------------------------------------------
# Two-by-two table: what is true versus what we decide.

truth_labels <- c(
  "H0 true" = 'paste(H[0], " true (no wolf)")',
  "H0 false" = 'paste(H[0], " false (wolf)")'
)
decision_labels <- c(
  "Reject" = 'paste("Reject ", H[0])',
  "Do not reject" = 'paste("Do not reject ", H[0])'
)
err <- data.frame(
  truth = factor(rep(c("H0 true", "H0 false"), each = 2),
                 levels = c("H0 true", "H0 false")),
  decision = factor(rep(c("Reject", "Do not reject"), 2),
                    levels = c("Do not reject", "Reject")),
  label = c("Type I error\nfalse positive\n(probability α)",
            "Correct\n(probability 1 − α)",
            "Correct\n(probability = power)",
            "Type II error\nfalse negative\n(probability β)"),
  kind = c("error", "ok", "ok", "error")
)
plots$errors <- ggplot(err, aes(truth, decision, fill = kind)) +
  geom_tile(colour = bb852$bg, linewidth = 3) +
  geom_text(aes(label = label, colour = kind), size = 5.6,
            family = bb852_font, lineheight = 0.95) +
  scale_fill_manual(values = c(error = bb852$coral, ok = bb852$panel)) +
  scale_colour_manual(values = c(error = bb852$bg, ok = bb852$text)) +
  scale_x_discrete(position = "top",
                   labels = function(z) parse(text = truth_labels[z])) +
  scale_y_discrete(labels = function(z) parse(text = decision_labels[z])) +
  labs(x = "What is actually true", y = "What we decide") +
  theme_bb852() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(colour = bb852$text, size = 17),
        axis.text.y = element_text(colour = bb852$text, size = 17, hjust = 1),
        axis.title = element_text(colour = bb852$muted, size = 16))
save_all(plots$errors, "04_error_table", "takeaway")


# 05 Three simulated studies from the same truth ---------------------------
# SIMULATED. The setup slide for the simulation that follows: we choose the
# truth (control mean 260, drug mean 220 mg/dL, SD 30) and draw three
# independent studies with 5 people per group. Two-sample t-tests,
# var.equal = TRUE, two-sided. Seed 358 was chosen (searching seeds 1 to 358)
# because the three p-values span the range clearly: 0.008, 0.18 and 0.54.
# Disclose this in the slide notes: the spread is typical, the exact values
# are illustrative.

set.seed(358)
three <- do.call(rbind, lapply(1:3, function(i) {
  control <- rnorm(5, 260, 30)
  drug <- rnorm(5, 220, 30)
  p <- t.test(control, drug, var.equal = TRUE)$p.value
  data.frame(study = i, p = p,
             Treatment = rep(c("Control", "Drug"), each = 5),
             Cholesterol = c(control, drug))
}))
three$Treatment <- factor(three$Treatment, levels = c("Control", "Drug"))
study_p <- tapply(three$p, three$study, unique)
study_labels <- setNames(sprintf("Study %d\np = %s", 1:3,
                                 ifelse(study_p < 0.01, sprintf("%.3f", study_p),
                                        sprintf("%.2f", study_p))),
                         1:3)
three$label <- factor(study_labels[as.character(three$study)],
                      levels = study_labels)
message("Three studies, p = ", paste(sprintf("%.3f", study_p), collapse = ", "))

plots$three_studies <- ggplot(three, aes(Treatment, Cholesterol)) +
  geom_boxplot(width = 0.55, fill = bb852$panel, colour = bb852$slate,
               outlier.shape = NA, linewidth = 0.5) +
  geom_jitter(width = 0.1, height = 0, colour = bb852$coral, size = 2.2) +
  facet_wrap(~label) +
  scale_x_discrete(labels = c("Control", "Drug")) +
  labs(x = NULL, y = "Cholesterol (mg/dL)") +
  theme_bb852(base_size = 16) +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 14, face = "bold", hjust = 0.5,
                                  lineheight = 0.95))
save_all(plots$three_studies, "05_three_studies", "half")


# 06 Simulated p-values: real difference versus no difference ----------------
# SIMULATED, using the values from the original lecture: means 260 and 220
# mg/dL, SD 30 (a real difference of 40 mg/dL), versus a scenario with no
# difference (both means 240, SD 30). Two-sample t-tests, var.equal = TRUE.
# The proportions below 0.05 (power and Type I error rate) are also
# estimated from 5,000 simulated tests per sample size and printed, for the
# slide caption and notes; lecture 18 covers power analysis in depth.

set.seed(852)
n_values <- seq(2, 20, by = 2)
reps_points <- 30      # dots per sample size per panel
reps_rate <- 5000      # replicates per sample size for the printed rates

sim_p <- function(n, delta) {
  t.test(rnorm(n, 240 + delta / 2, 30), rnorm(n, 240 - delta / 2, 30),
         var.equal = TRUE)$p.value
}
scen <- c("Real difference (40 mg/dL)" = 40, "No difference" = 0)

p_points <- do.call(rbind, lapply(names(scen), function(s) {
  do.call(rbind, lapply(n_values, function(n) {
    data.frame(scenario = s, n = n,
               p = replicate(reps_points, sim_p(n, scen[[s]])))
  }))
}))
p_points$scenario <- factor(p_points$scenario, levels = names(scen))

plots$pvalues <- ggplot(p_points, aes(n, p)) +
  geom_hline(yintercept = 0.05, colour = bb852$coral, linewidth = 0.6,
             linetype = "dashed") +
  geom_jitter(width = 0.18, height = 0, colour = bb852$slate, alpha = 0.55,
              size = 1.6) +
  facet_wrap(~scenario) +
  scale_x_continuous(breaks = c(2, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0.05, 0.25, 0.5, 0.75, 1),
                     labels = c("0.05", "0.25", "0.50", "0.75", "1")) +
  labs(x = "Sample size per group", y = "p-value") +
  theme_bb852(base_size = 17) +
  theme(panel.spacing = unit(2, "lines"))
save_all(plots$pvalues, "06_pvalues_vs_n", "takeaway")

for (s in names(scen)) {
  rates <- sapply(c(5, 10, 20), function(n) {
    mean(replicate(reps_rate, sim_p(n, scen[[s]])) < 0.05)
  })
  message(s, ": proportion p < 0.05 at n = 5, 10, 20: ",
          paste(sprintf("%.2f", rates), collapse = ", "))
}


# 07-08 Rocky shores: one effect, two surveys (after Amrhein et al. 2019) ---
# ANALYTICAL illustration with ILLUSTRATIVE numbers (not published data).
# Question: does wave disturbance let more species persist? Response: mean
# number of species per quadrat on each shore; the shore is the unit.
# Both surveys estimate the same difference (exposed minus sheltered:
# +3 species per quadrat; SD among shores 3.5). 95% t-intervals for a
# difference between two means; two-sided p-values.
# Small survey: 4 shores of each type. Large survey: 20 of each.

rocky <- as.data.frame(rbind(ci_diff(3, 4, sd = 3.5), ci_diff(3, 20, sd = 3.5)))
rocky$study <- factor(c("Small survey\n4 + 4 shores", "Large survey\n20 + 20 shores"),
                      levels = c("Large survey\n20 + 20 shores", "Small survey\n4 + 4 shores"))
rocky$verdict <- ifelse(rocky$p < 0.05,
                        sprintf("p = %.2f\n\"significant\"", rocky$p),
                        sprintf("p = %.2f\n\"not significant\"", rocky$p))
message("Rocky shores: ", paste(sprintf("est %.1f [%.1f, %.1f], p = %.3f",
        rocky$est, rocky$lo, rocky$hi, rocky$p), collapse = "; "))

rocky_plot <- function(d, xlim = c(-5, 15)) {
  ggplot(d, aes(est, study)) +
    geom_vline(xintercept = 0, colour = bb852$grey, linewidth = 0.5) +
    annotate("text", x = 0, y = Inf, label = "no effect", vjust = 1.4,
             hjust = 1.1, colour = bb852$grey, size = 5, family = bb852_font) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.12, orientation = "y",
                  colour = bb852$slate, linewidth = 0.9) +
    geom_point(size = 3.6, colour = bb852$coral) +
    geom_text(aes(x = 9.8, label = verdict), hjust = 0, colour = bb852$text,
              size = 5, family = bb852_font, lineheight = 0.9) +
    scale_x_continuous(limits = xlim, breaks = seq(-4, 8, 2)) +
    scale_y_discrete(expand = expansion(add = 0.6)) +
    labs(x = "Extra species per quadrat on exposed shores", y = NULL) +
    theme_bb852() +
    theme(axis.text.y = element_text(colour = bb852$text, size = 15,
                                     lineheight = 0.9),
          panel.grid.major.y = element_blank())
}

# The small survey alone, for the half-width figure: no row label (the
# slide text names the survey) and the verdict above the interval.
small <- rocky[1, ]
plots$rocky_small <- ggplot(small, aes(est, 1)) +
  geom_vline(xintercept = 0, colour = bb852$grey, linewidth = 0.5) +
  annotate("text", x = 0, y = 1.75, label = "no effect", hjust = 1.1,
           colour = bb852$grey, size = 5, family = bb852_font) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.12, orientation = "y",
                colour = bb852$slate, linewidth = 0.9) +
  geom_point(size = 3.6, colour = bb852$coral) +
  annotate("text", x = 0.5, y = 1.3, hjust = 0,
           label = sprintf("p = %.2f, \"not significant\"", small$p),
           colour = bb852$text, size = 5, family = bb852_font) +
  scale_x_continuous(limits = c(-5, 11), breaks = seq(-4, 10, 2)) +
  scale_y_continuous(limits = c(0.5, 1.8)) +
  labs(x = "Extra species per quadrat\n(exposed minus sheltered shores)",
       y = NULL) +
  theme_bb852() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_all(plots$rocky_small, "07_rocky_small_survey", "half")

plots$rocky_both <- rocky_plot(rocky)
save_all(plots$rocky_both, "08_rocky_two_surveys", "takeaway")


# 09 Ideal free distribution: two non-significant results ----------------------
# ANALYTICAL illustration for the ideal free distribution example. The
# quantity is the difference in mean offspring per female between two
# habitats; the true difference is zero in both cases. SD 1.5 offspring.
# Study 1: 8 females per habitat, estimate 0.3. Study 2: 400 per habitat,
# estimate 0.02. Both are non-significant; only Study 2 is informative,
# because its whole interval lies inside the negligible band (+/- 0.25
# offspring), which is set in advance on biological grounds.

band <- 0.25
null_res <- as.data.frame(rbind(ci_diff(0.30, 8, sd = 1.5),
                                ci_diff(0.02, 400, sd = 1.5)))
stopifnot(null_res$lo[2] > -band, null_res$hi[2] < band)
null_res$study <- factor(c("Study 1\n8 per habitat", "Study 2\n400 per habitat"),
                         levels = c("Study 2\n400 per habitat", "Study 1\n8 per habitat"))
message("Null-result CIs: ", paste(sprintf("est %.2f [%.2f, %.2f], p = %.2f",
        null_res$est, null_res$lo, null_res$hi, null_res$p), collapse = "; "))

plots$null_results <- ggplot(null_res, aes(est, study)) +
  annotate("rect", xmin = -band, xmax = band, ymin = -Inf, ymax = Inf,
           fill = bb852$panel) +
  geom_vline(xintercept = 0, colour = bb852$grey, linewidth = 0.5) +
  annotate("rect", xmin = -0.62, xmax = 0.62, ymin = 0.3, ymax = 0.6,
           fill = bb852$panel, colour = NA) +
  annotate("text", x = 0, y = 0.45, label = "negligible difference",
           colour = bb852$grey, size = 5, family = bb852_font) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.12, orientation = "y",
                colour = bb852$slate, linewidth = 0.9) +
  geom_point(size = 3.6, colour = bb852$coral) +
  scale_x_continuous(limits = c(-1.6, 2.2), breaks = seq(-1.5, 2, 0.5)) +
  scale_y_discrete(expand = expansion(add = c(0.75, 0.5))) +
  labs(x = "Difference in offspring per female\n(habitat A minus habitat B)",
       y = NULL) +
  theme_bb852() +
  theme(axis.text.y = element_text(colour = bb852$text, size = 15,
                                   lineheight = 0.9),
        panel.grid.major.y = element_blank())
save_all(plots$null_results, "09_null_results", "takeaway")



message("Done: figures written to ", output_root)
