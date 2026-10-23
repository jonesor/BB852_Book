# BB852 two-way ANOVA, ANCOVA and multiple regression lecture: figures
# --------------------------------------------------------------------
# Every statistical figure in the lecture, in the BB852 theme shared with the
# other lecture_figures generators (see lecture_figures/t-tests).
#
# Course book chapters:
# https://jonesor.github.io/BB852_Book/n-way-anova-linear-models-with-1-categorical-explanatory-variables.html
# https://jonesor.github.io/BB852_Book/ancova-linear-models-with-categorical-and-continuous-explanatory-variables.html
#
# Data:
#   CourseData/insectDiet.csv   two-way ANOVA, diet x genotype, 12 per cell
#                               (simulated in scripts/SimulateData.R)
#   CourseData/morphometry.csv  ANCOVA and multiple regression, n = 155
#                               (simulated in scripts/SimulateData.R);
#                               converted from mm to cm as in the book
# Simulated here (seed stated): Simpson's paradox example (slide 5).
# Schematic panels (slides 9 and 16) use hand-chosen values, not data.
#
# Run from the repository root:
# source("lecture_figures/ancova/generate_figures.R")
#
# PNG files are written to lecture_figures/ancova/output/<layout>/. Every
# content figure is written at full and half width; figures placed on a
# "Title, Bullets & Photo" slide are also written at takeaway size. The R
# output shown on the slides is written to output/text/.
# Object and file names use the slide numbers of the rebuilt deck
# (BB852_14_ANCOVA_and_Multiple_Regression_v3), e.g. plots$slide_19_parallel.
#
# Labels use \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)
library(patchwork)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/t-tests and lecture_figures/anova.

bb852 <- list(
  bg       = "#FBFAF6",
  text     = "#1B2230",
  muted    = "#6B6E75",
  grid     = "#DDE2E8",
  axis     = "#BCC6D2",
  slate    = "#24405F",
  coral    = "#D0745A",
  teal     = "#2A7F76",
  grey     = "#6B6E75",
  light    = "#9FB0C4",
  panel    = "#EDEFF1",
  cover    = "#2C4D70"
)

# Avenir Next on the Mac; Helvetica otherwise. BB852_FONT overrides both.
available_fonts <- unique(systemfonts::system_fonts()$family)
bb852_font <- if (nzchar(Sys.getenv("BB852_FONT"))) {
  Sys.getenv("BB852_FONT")
} else if ("Avenir Next" %in% available_fonts) {
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

# Sizes (inches) match the figure placeholders in the BB852 slide theme.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "ancova", "output")
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

# Write R output exactly as printed, for copying onto slides.
text_directory <- file.path(output_root, "text")
dir.create(text_directory, recursive = TRUE, showWarnings = FALSE)
save_output <- function(expr_text, filename) {
  out <- capture.output(eval(parse(text = expr_text), envir = globalenv()))
  writeLines(c(paste(">", expr_text), out),
             file.path(text_directory, paste0(filename, ".txt")))
}

# Text size helper: ggplot text size is in mm; 14 pt is the minimum.
pt <- function(points) points / .pt

# Two groups: slate and coral (instructions: main data, contrast group).
sex_cols  <- c(Female = bb852$slate, Male = bb852$coral)
diet_cols <- c(highProtein = bb852$slate, lowProtein = bb852$coral)

plots <- list()
half_plots <- list()


# Data and models -------------------------------------------------------------

insectDiet <- read.csv(file.path("CourseData", "insectDiet.csv"))
mod_insect <- lm(lengthMM ~ genotype + diet + genotype:diet, data = insectDiet)

morph <- read.csv(file.path("CourseData", "morphometry.csv"))
morph$Height <- morph$Height / 10          # mm -> cm, as in the book
morph$HandLength <- morph$HandLength / 10
morph$FootLength <- morph$FootLength / 10

mod_A <- lm(Height ~ HandLength + Sex + HandLength:Sex, data = morph)
mod_B <- lm(Height ~ HandLength + Sex, data = morph)
mod_overall <- lm(Height ~ HandLength, data = morph)
mod_sex_first <- lm(Height ~ Sex + HandLength, data = morph)
mod_multi <- lm(Height ~ HandLength + FootLength, data = morph)
mod_foot <- lm(Height ~ FootLength, data = morph)

hand_lab <- "Hand length (cm)"
height_lab <- "Height (cm)"
hand_range <- range(morph$HandLength)


# R output shown on slides ------------------------------------------------------

save_output("anova(mod_insect)", "slide_12_anova_insect")
save_output("summary(mod_insect)", "slide_13_summary_insect")
save_output("anova(mod_A)", "slide_18_anova_ancova")
save_output("anova(mod_B, mod_A)", "slide_19_model_comparison")
save_output("summary(mod_B)", "slide_20_summary_parallel")
save_output("summary(mod_A)", "slide_21_summary_full")
save_output("anova(mod_B)", "slide_18_anova_hand_first")
save_output("anova(mod_sex_first)", "slide_18_anova_sex_first")
save_output("summary(mod_multi)", "slide_23_summary_multiple")
save_output("summary(mod_foot)", "slide_23_summary_foot_only")
save_output("cor(morph$HandLength, morph$FootLength)", "slide_23_correlation")
if (requireNamespace("agricolae", quietly = TRUE)) {
  save_output(
    'agricolae::HSD.test(mod_insect, trt = c("diet", "genotype"), console = TRUE)',
    "slide_14_hsd"
  )
}


# Slide 1: title-slide chart ----------------------------------------------------
# Parallel lines for the two sexes, on the cover colour.

cb <- coef(mod_B)
cover_lines <- data.frame(
  Sex = c("Female", "Male"),
  intercept = c(cb[["(Intercept)"]], cb[["(Intercept)"]] + cb[["SexMale"]]),
  slope = cb[["HandLength"]]
)
plots$slide_01_cover <- ggplot(morph, aes(HandLength, Height)) +
  geom_point(aes(colour = Sex), size = 2.6, alpha = 0.75) +
  geom_abline(data = cover_lines, aes(intercept = intercept, slope = slope),
              colour = bb852$bg, linewidth = 1.2) +
  scale_colour_manual(values = c(Female = bb852$light, Male = bb852$coral)) +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852() +
  theme(
    plot.background = element_rect(fill = bb852$cover, colour = NA),
    panel.background = element_rect(fill = bb852$cover, colour = NA),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = bb852$light, linewidth = 0.4),
    axis.title = element_text(colour = bb852$light),
    axis.text = element_text(colour = bb852$light),
    plot.margin = margin(140, 24, 110, 12)
  )


# Slides 4 and 17: height vs hand length by sex (separate lines, as in the book) --

sex_ends <- do.call(rbind, lapply(split(morph, morph$Sex), function(d) {
  fit <- lm(Height ~ HandLength, data = d)
  x <- max(d$HandLength)
  data.frame(Sex = d$Sex[1], x = x, y = predict(fit, data.frame(HandLength = x)))
}))

by_sex <- ggplot(morph, aes(HandLength, Height, colour = Sex)) +
  geom_point(alpha = 0.55, size = 2.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.2) +
  geom_text(data = sex_ends, aes(x = x + 0.25, y = y, label = Sex),
            hjust = 0, size = pt(16), family = bb852_font, fontface = "bold") +
  scale_colour_manual(values = sex_cols) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.2))) +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()

plots$slide_17_ancova_data <- by_sex

# Slide 4 version adds the overall line (ignoring sex), dashed grey, so both
# slopes can be compared in one figure.
overall_end <- data.frame(x = hand_range[2],
                          y = predict(mod_overall, data.frame(HandLength = hand_range[2])))
plots$slide_04_by_sex <- by_sex +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE,
              colour = bb852$grey, linetype = "22", linewidth = 1) +
  annotate("text", x = hand_range[1], y = 189, hjust = 0, size = pt(16),
           family = bb852_font, colour = bb852$grey, fontface = "bold",
           label = sprintf("Ignoring sex: slope %.1f", coef(mod_overall)[2])) +
  annotate("text", x = hand_range[1], y = 184.5, hjust = 0, size = pt(16),
           family = bb852_font, colour = bb852$text, fontface = "bold",
           label = sprintf("Within each sex: slope %.1f", coef(mod_B)[["HandLength"]]))


# Slide 5: Simpson's paradox (simulated) ----------------------------------------
# Four groups. Within each, y rises with x (slope 0.8); group means fall as x
# rises, so the pooled slope is negative.

set.seed(852)
n_per <- 25
simpson <- data.frame(
  group = rep(c("A", "B", "C", "D"), each = n_per),
  centre_x = rep(c(2, 5, 8, 11), each = n_per),
  centre_y = rep(c(13, 9, 5, 1), each = n_per)
)
simpson$x <- simpson$centre_x + rnorm(nrow(simpson), 0, 1)
simpson$y <- simpson$centre_y + 0.8 * (simpson$x - simpson$centre_x) +
  rnorm(nrow(simpson), 0, 0.8)
simpson_cols <- c(A = bb852$slate, B = bb852$coral, C = bb852$teal, D = bb852$grey)

simpson_pooled <- ggplot(simpson, aes(x, y)) +
  geom_point(colour = bb852$light, alpha = 0.9, size = 2.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = bb852$slate, linewidth = 1.2) +
  annotate("text", x = 13.5, y = 15, hjust = 1, size = pt(16),
           family = bb852_font, fontface = "bold", colour = bb852$slate,
           label = "Ignoring group: negative") +
  labs(x = "x", y = "y") +
  theme_bb852(16)

simpson_grouped <- ggplot(simpson, aes(x, y, colour = group)) +
  geom_point(alpha = 0.7, size = 2.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.2) +
  annotate("text", x = 13.5, y = 15, hjust = 1, size = pt(16),
           family = bb852_font, fontface = "bold", colour = bb852$text,
           label = "Within each group: positive") +
  scale_colour_manual(values = simpson_cols) +
  labs(x = "x", y = "y") +
  theme_bb852(16)

plots$slide_05_simpson <- simpson_pooled + simpson_grouped
half_plots$slide_05_simpson <- simpson_pooled / simpson_grouped &
  theme_bb852(14) & theme(plot.margin = margin(4, 12, 4, 8))


# Slides 9 and 16: schematic outcome panels --------------------------------------

panel_theme <- function() {
  theme_bb852(14) +
    theme(
      axis.text.y = element_blank(),
      plot.title = element_text(size = 14, colour = bb852$text,
                                family = bb852_font, face = "bold",
                                margin = margin(b = 2)),
      plot.margin = margin(4, 10, 4, 6)
    )
}

twoway_panel <- function(values, label, show_key = FALSE) {
  d <- expand.grid(genotype = c("gt1", "gt2"),
                   diet = c("highProtein", "lowProtein"))
  d$y <- values
  p <- ggplot(d, aes(genotype, y, colour = diet, group = diet)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = diet_cols) +
    scale_y_continuous(limits = c(0, 10)) +
    scale_x_discrete(expand = expansion(add = 0.35)) +
    labs(x = NULL, y = NULL, title = label) +
    panel_theme() +
    theme(panel.grid.major.x = element_blank())
  if (show_key) {
    p <- p +
      annotate("text", x = 1.5, y = 9.2, label = "high protein", size = pt(14),
               colour = bb852$slate, family = bb852_font, fontface = "bold") +
      annotate("text", x = 1.5, y = 1.2, label = "low protein", size = pt(14),
               colour = bb852$coral, family = bb852_font, fontface = "bold")
  }
  p
}

# Values: (gt1, high), (gt2, high), (gt1, low), (gt2, low).
# Labels list the effects present.
twoway_specs <- list(
  list(c(5, 5, 5.3, 5.3),   "A: none"),
  list(c(7, 4, 7.3, 4.3),   "B: genotype"),
  list(c(7, 7, 3.5, 3.5),   "C: diet"),
  list(c(8, 5, 5.5, 2.5),   "D: diet + genotype"),
  list(c(8.5, 5, 4, 3.5),   "E: diet + genotype + int"),
  list(c(7, 3, 3, 7),       "F: interaction only")
)
twoway_panels <- lapply(seq_along(twoway_specs), function(i) {
  twoway_panel(twoway_specs[[i]][[1]], twoway_specs[[i]][[2]], show_key = i == 3)
})
plots$slide_09_twoway_outcomes <- wrap_plots(twoway_panels, nrow = 2)
half_plots$slide_09_twoway_outcomes <- wrap_plots(twoway_panels, ncol = 2)

ancova_panel <- function(values, label, show_key = FALSE) {
  d <- expand.grid(x = c(1, 5), group = c("A", "B"))
  d$y <- values
  p <- ggplot(d, aes(x, y, colour = group, group = group)) +
    geom_line(linewidth = 1.4) +
    scale_colour_manual(values = c(A = bb852$slate, B = bb852$coral)) +
    scale_y_continuous(limits = c(0, 10)) +
    scale_x_continuous(limits = c(0.6, 5.4)) +
    labs(x = NULL, y = NULL, title = label) +
    panel_theme() +
    theme(axis.text.x = element_blank())
  if (show_key) {
    p <- p +
      annotate("text", x = 3, y = 9.2, label = "group A", size = pt(14),
               colour = bb852$slate, family = bb852_font, fontface = "bold") +
      annotate("text", x = 3, y = 1.2, label = "group B", size = pt(14),
               colour = bb852$coral, family = bb852_font, fontface = "bold")
  }
  p
}

# Values: (x = 1, A), (x = 5, A), (x = 1, B), (x = 5, B).
ancova_specs <- list(
  list(c(5, 5, 5.3, 5.3),       "A: none"),
  list(c(2.5, 7.5, 2.8, 7.8),   "B: continuous"),
  list(c(7, 7, 3.5, 3.5),       "C: categorical"),
  list(c(4, 8.5, 1.5, 6),       "D: cont + categ"),
  list(c(2.5, 9, 2.5, 5),       "E: cont + categ + int"),
  list(c(7.5, 2.5, 2.5, 7.5),   "F: interaction only")
)
ancova_panels <- lapply(seq_along(ancova_specs), function(i) {
  ancova_panel(ancova_specs[[i]][[1]], ancova_specs[[i]][[2]], show_key = i == 3)
})
plots$slide_16_ancova_outcomes <- wrap_plots(ancova_panels, nrow = 2)
half_plots$slide_16_ancova_outcomes <- wrap_plots(ancova_panels, ncol = 2)


# Slide 10: the insect diet data ----------------------------------------------------

insect_means <- aggregate(lengthMM ~ genotype + diet, data = insectDiet, FUN = mean)
insect_se <- aggregate(lengthMM ~ genotype + diet, data = insectDiet,
                       FUN = function(v) sd(v) / sqrt(length(v)))
insect_means$se <- insect_se$lengthMM
names(insect_means)[names(insect_means) == "lengthMM"] <- "mean"

# Deterministic offsets (no random jitter): diets side by side, points spread.
dodge <- c(highProtein = -0.18, lowProtein = 0.18)
insectDiet$x <- as.numeric(factor(insectDiet$genotype)) + dodge[insectDiet$diet] +
  ave(seq_len(nrow(insectDiet)), insectDiet$genotype, insectDiet$diet,
      FUN = function(i) ((seq_along(i) - 1) %% 4 - 1.5) * 0.035)
insect_means$x <- as.numeric(factor(insect_means$genotype)) + dodge[insect_means$diet]

plots$slide_10_insect_data <- ggplot(insectDiet, aes(x, lengthMM, colour = diet)) +
  geom_point(alpha = 0.55, size = 2.6) +
  geom_segment(data = insect_means,
               aes(x = x - 0.13, xend = x + 0.13, y = mean, yend = mean),
               linewidth = 1.2) +
  annotate("text", x = 2.45, y = 30, label = "high protein", hjust = 1,
           colour = bb852$slate, size = pt(16), family = bb852_font,
           fontface = "bold") +
  annotate("text", x = 2.45, y = 28.2, label = "low protein", hjust = 1,
           colour = bb852$coral, size = pt(16), family = bb852_font,
           fontface = "bold") +
  scale_colour_manual(values = diet_cols) +
  scale_x_continuous(breaks = 1:2, labels = c("gt1", "gt2"),
                     limits = c(0.5, 2.5)) +
  labs(x = "Genotype", y = "Adult length (mm)") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Slide 13: coefficients as steps from the reference group ---------------------------

cf <- coef(mod_insect)
b0 <- cf[["(Intercept)"]]
b_gt <- cf[["genotypegt2"]]
b_diet <- cf[["dietlowProtein"]]
b_int <- cf[["genotypegt2:dietlowProtein"]]
cell_mean <- c(b0, b0 + b_diet, b0 + b_gt, b0 + b_gt + b_diet + b_int)
stopifnot(isTRUE(all.equal(
  sort(cell_mean), sort(insect_means$mean)
)))

term_cols <- c(gt = bb852$teal, diet = bb852$slate, inter = bb852$coral)
steps <- data.frame(
  x    = c(2, 3, 3.72, 3.97),
  y    = c(b0, b0, b0 + b_gt, b0 + b_gt + b_diet),
  yend = c(b0 + b_diet, b0 + b_gt, b0 + b_gt + b_diet, cell_mean[4]),
  term = c("diet", "gt", "diet", "inter")
)

coef_steps <- ggplot() +
  geom_hline(yintercept = b0, linetype = "dashed", linewidth = 0.6,
             colour = bb852$grey) +
  geom_segment(data = steps,
               aes(x = x, xend = x, y = y, yend = yend, colour = term),
               linewidth = 1.3,
               arrow = arrow(length = unit(0.12, "in"), type = "closed")) +
  geom_point(aes(x = 1:4, y = cell_mean), size = 3.5, colour = bb852$text) +
  annotate("text", x = 1.1, y = b0 + 1, hjust = 0, size = pt(15),
           family = bb852_font, fontface = "bold", colour = bb852$text,
           label = sprintf("(Intercept) %.2f", b0)) +
  annotate("text", x = 2.08, y = b0 + b_diet / 2, hjust = 0, size = pt(15),
           family = bb852_font, fontface = "bold", colour = term_cols[["diet"]],
           label = sprintf("diet %.2f", b_diet)) +
  annotate("text", x = 3.08, y = b0 + b_gt / 2 + 1.2, hjust = 0, size = pt(15),
           family = bb852_font, fontface = "bold", colour = term_cols[["gt"]],
           label = sprintf("genotype\n%.2f", b_gt), lineheight = 0.9) +
  annotate("text", x = 3.64, y = b0 + b_gt + b_diet / 2, hjust = 1,
           size = pt(15), family = bb852_font, fontface = "bold",
           colour = term_cols[["diet"]], label = sprintf("diet %.2f", b_diet)) +
  annotate("text", x = 4.08, y = b0 + b_gt + b_diet + b_int / 2 - 0.6,
           hjust = 0, size = pt(15), family = bb852_font, fontface = "bold",
           colour = term_cols[["inter"]], lineheight = 0.9,
           label = sprintf("interaction\n+%.2f", b_int)) +
  annotate("text", x = 4.1, y = cell_mean[4] + 0.6, hjust = 0, size = pt(15),
           family = bb852_font, colour = bb852$text,
           label = sprintf("= %.2f", cell_mean[4])) +
  scale_colour_manual(values = term_cols) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("gt1\nhigh", "gt1\nlow", "gt2\nhigh", "gt2\nlow"),
                     expand = expansion(add = c(0.35, 0.95))) +
  scale_y_continuous(limits = c(11.5, 27.5)) +
  labs(x = NULL, y = "Mean length (mm)")

plots$slide_13_coefficient_steps <- coef_steps + theme_bb852(16)


# Slide 14: interaction plot with Tukey groupings ----------------------------------
# Letters as reported by agricolae::HSD.test(mod_insect, trt = c("diet",
# "genotype")): highProtein:gt1 a, lowProtein:gt1 b, highProtein:gt2 c,
# lowProtein:gt2 c. Base-R TukeyHSD() on the four cells agrees: every pair
# differs except gt2 high vs gt2 low (adjusted p = 0.70).

tukey <- TukeyHSD(aov(lengthMM ~ interaction(diet, genotype), data = insectDiet))[[1]]
stopifnot(sum(tukey[, "p adj"] > 0.05) == 1,
          tukey["lowProtein.gt2-highProtein.gt2", "p adj"] > 0.05)

letters_df <- data.frame(
  genotype = c("gt1", "gt1", "gt2", "gt2"),
  diet = c("highProtein", "lowProtein", "highProtein", "lowProtein"),
  letter = c("a", "b", "c", "c")
)
insect_means <- merge(insect_means, letters_df)
nudge <- c(highProtein = -0.05, lowProtein = 0.05)
insect_means$xi <- as.numeric(factor(insect_means$genotype)) + nudge[insect_means$diet]

plots$slide_14_interaction_tukey <- ggplot(
  insect_means, aes(xi, mean, colour = diet, group = diet)
) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.06, linewidth = 0.9) +
  geom_point(size = 3.5) +
  geom_text(aes(label = letter,
                x = xi + ifelse(diet == "highProtein", -0.17, 0.17)),
            size = pt(20), family = bb852_font, fontface = "bold") +
  annotate("text", x = 2.25, y = 19, label = "high protein", hjust = 0,
           colour = bb852$slate, size = pt(16), family = bb852_font,
           fontface = "bold") +
  annotate("text", x = 2.25, y = 15.6, label = "low protein", hjust = 0,
           colour = bb852$coral, size = pt(16), family = bb852_font,
           fontface = "bold") +
  scale_colour_manual(values = diet_cols) +
  scale_x_continuous(breaks = 1:2, labels = c("gt1", "gt2"),
                     expand = expansion(add = c(0.35, 1.05))) +
  labs(x = "Genotype", y = "Mean length (mm) ± SE") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Slide 19: the parallel-lines model (mod_B) ----------------------------------------

x_arrow <- 16.8
y_f <- cb[["(Intercept)"]] + cb[["HandLength"]] * x_arrow
y_m <- y_f + cb[["SexMale"]]

parallel <- ggplot(morph, aes(HandLength, Height, colour = Sex)) +
  geom_point(alpha = 0.45, size = 2.4) +
  geom_abline(data = cover_lines,
              aes(intercept = intercept, slope = slope, colour = Sex),
              linewidth = 1.2) +
  annotate("segment", x = x_arrow, xend = x_arrow, y = y_f, yend = y_m,
           colour = bb852$text, linewidth = 1.1,
           arrow = arrow(length = unit(0.1, "in"), ends = "both", type = "closed")) +
  annotate("text", x = x_arrow - 0.1, y = y_m + 8, hjust = 1,
           size = pt(16), family = bb852_font, fontface = "bold",
           colour = bb852$text, lineheight = 0.9,
           label = sprintf("SexMale\n+%.1f cm", cb[["SexMale"]])) +
  annotate("text", x = hand_range[2], y = 147, hjust = 1, size = pt(16),
           family = bb852_font, colour = bb852$text,
           label = sprintf("Common slope %.2f cm per cm", cb[["HandLength"]])) +
  annotate("text", x = 15, y = 186, hjust = 0, size = pt(16),
           family = bb852_font, fontface = "bold", colour = bb852$coral,
           label = "Male") +
  annotate("text", x = 15, y = 181, hjust = 0, size = pt(16),
           family = bb852_font, fontface = "bold", colour = bb852$slate,
           label = "Female") +
  scale_colour_manual(values = sex_cols) +
  coord_cartesian(xlim = c(15, hand_range[2]), ylim = c(144, 190)) +
  labs(x = hand_lab, y = height_lab)

plots$slide_19_parallel <- parallel + theme_bb852()


# Slide 21: why SexMale is "non-significant" in the interaction model ------------
# In mod_A, SexMale is the male-female difference at HandLength = 0, far
# outside the data, so its standard error is huge. Ribbons are 95% confidence
# intervals for the fitted lines; they widen as the lines are extrapolated.

ca <- coef(mod_A)
extrap <- expand.grid(HandLength = seq(0, 25, by = 0.25), Sex = c("Female", "Male"))
extrap <- cbind(extrap, predict(mod_A, newdata = extrap, interval = "confidence"))
names(extrap)[names(extrap) == "fit"] <- "Height"

plots$slide_21_extrapolation <- ggplot(morph, aes(HandLength, Height, colour = Sex)) +
  annotate("rect", xmin = hand_range[1], xmax = hand_range[2],
           ymin = -Inf, ymax = Inf, fill = bb852$panel) +
  geom_ribbon(data = extrap, aes(ymin = lwr, ymax = upr, fill = Sex),
              colour = NA, alpha = 0.2) +
  geom_line(data = extrap, linewidth = 1.2, linetype = "22") +
  geom_point(alpha = 0.45, size = 2) +
  annotate("text", x = mean(hand_range), y = 95, size = pt(16),
           family = bb852_font, colour = bb852$grey, lineheight = 0.9,
           label = "range of\nthe data") +
  annotate("text", x = 0.3, y = 45, hjust = 0,
           size = pt(16), family = bb852_font, fontface = "bold",
           colour = bb852$text, lineheight = 0.9,
           label = sprintf("SexMale = %.1f (SE %.1f)\nmeasured at hand length 0",
                           ca[["SexMale"]], summary(mod_A)$coef["SexMale", 2])) +
  scale_colour_manual(values = sex_cols) +
  scale_fill_manual(values = sex_cols) +
  scale_x_continuous(limits = c(0, 25), expand = expansion(add = c(0, 0.5))) +
  scale_y_continuous(limits = c(30, 200)) +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()


# Slide 23: hand and foot length are strongly correlated --------------------------

r_hf <- cor(morph$HandLength, morph$FootLength)
plots$slide_23_hand_foot <- ggplot(morph, aes(HandLength, FootLength)) +
  geom_point(colour = bb852$slate, alpha = 0.55, size = 2.6) +
  annotate("text", x = hand_range[1], y = max(morph$FootLength), hjust = 0,
           size = pt(18), family = bb852_font, colour = bb852$coral,
           fontface = "bold", label = sprintf("r = %.2f", r_hf)) +
  labs(x = hand_lab, y = "Foot length (cm)") +
  theme_bb852()


# Save every figure ------------------------------------------------------------------

takeaway_slides <- c("slide_04_by_sex", "slide_13_coefficient_steps",
                     "slide_19_parallel")

for (name in names(plots)) {
  if (name == "slide_01_cover") {
    save_bb852(plots[[name]], name, layout = "cover", bg = bb852$cover)
    next
  }
  save_bb852(plots[[name]], name, layout = "full")
  half_plot <- if (name %in% names(half_plots)) half_plots[[name]] else plots[[name]]
  save_bb852(half_plot, name, layout = "half")
  if (name %in% takeaway_slides) {
    save_bb852(plots[[name]], name, layout = "takeaway")
  }
}
