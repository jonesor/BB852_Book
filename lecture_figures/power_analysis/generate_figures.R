# BB852 power analysis by simulation lecture: figures and slide output
# --------------------------------------------------------------------
# Every statistical figure in the lecture, in the BB852 theme shared with the
# other lecture_figures generators (see lecture_figures/t-tests).
#
# Course book chapter:
# https://jonesor.github.io/BB852_Book/power-analysis-by-simulation.html
#
# Data: none. Everything here is simulated or calculated, which is the point
# of the lecture. Values follow the course book chapter:
#   insect diet example: control mean 12 mm, treatment 14.4 mm (+20%),
#     SD 3 mm, n = 10 per group (seeds 42 and 142 for the single experiment,
#     seed 123 for the 1000-replicate simulation, as in the book)
#   binomial GLM: survival 0.5 vs 0.7, n = 40 per group (seed 123)
#   regression: slope 2, intercept 20, residual SD 12, n = 30,
#     x from 11 to 19 (narrow) or 5 to 25 (wide) (seed 123)
# Dice figure (slides 3-4): exact binomial probabilities, fair die
#   P(six) = 1/6 vs loaded die P(six) = 1/3. Not simulated.
#
# Run from the repository root:
# source("lecture_figures/power_analysis/generate_figures.R")
#
# PNG files are written to lecture_figures/power_analysis/output/<layout>/.
# Every content figure is written at full and half width; figures placed on a
# "Title, Bullets & Photo" slide are also written at takeaway size. The R
# output shown on the slides is written to output/text/.
# Object and file names use the slide numbers of the rebuilt deck
# (BB852_18_Power_Analysis_v2).
#
# Labels use \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)
library(patchwork)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/t-tests and lecture_figures/ancova.

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

output_root <- file.path("lecture_figures", "power_analysis", "output")
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
save_text <- function(lines, filename) {
  writeLines(lines, file.path(text_directory, paste0(filename, ".txt")))
}

# Text size helper: ggplot text size is in mm; 14 pt is the minimum.
pt <- function(points) points / .pt

plots <- list()        # written at full and half width
takeaway <- list()     # also written at takeaway size

group_cols <- c(Control = bb852$slate, `High protein` = bb852$coral)


# Insect diet example: values from the course book --------------------------

controlMean <- 12
treatmentMean <- 14.4
sdValue <- 3
sampleSize <- 10

# Power for one sample size, using the book's recipe.
power_t <- function(n, reps = 1000) {
  sum(replicate(
    reps,
    t.test(
      rnorm(n, controlMean, sdValue),
      rnorm(n, treatmentMean, sdValue)
    )$p.value
  ) < 0.05) / reps
}


# Slide 1: title-slide chart ------------------------------------------------
# The power curve (computed for slide 20) on the cover colour.

set.seed(123)
power_curve <- data.frame(sampleSize = 5:40)
power_curve$Power <- vapply(power_curve$sampleSize, power_t, numeric(1))
power_curve$analytic <- vapply(
  power_curve$sampleSize,
  function(n) power.t.test(n = n, delta = treatmentMean - controlMean,
                           sd = sdValue)$power,
  numeric(1)
)

plots_cover <- ggplot(power_curve, aes(sampleSize, Power)) +
  geom_hline(yintercept = 0.8, colour = bb852$coral, linewidth = 0.8,
             linetype = "dashed") +
  geom_line(colour = bb852$light, linewidth = 1) +
  geom_point(colour = bb852$bg, size = 2.4) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Sample size per group", y = "Power") +
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
save_bb852(plots_cover, "slide_01_cover", "cover", bg = bb852$cover)


# Slide 4: how many rolls to catch a loaded die? -------------------------------
# Exact binomial probabilities of the proportion of sixes, fair (1/6) vs
# loaded (1/3), for 6 rolls and for 60 rolls.

dice <- do.call(rbind, lapply(c(6, 60), function(n) {
  k <- 0:n
  rbind(
    data.frame(rolls = n, prop = k / n, prob = dbinom(k, n, 1 / 6),
               die = "Fair"),
    data.frame(rolls = n, prop = k / n, prob = dbinom(k, n, 1 / 3),
               die = "Loaded")
  )
}))
dice$panel <- factor(paste(dice$rolls, "rolls"),
                     levels = c("6 rolls", "60 rolls"))
dice <- dice[dice$prop <= 0.75, ]

dice_labels <- data.frame(
  panel = factor(c("6 rolls", "6 rolls", "60 rolls", "60 rolls"),
                 levels = c("6 rolls", "60 rolls")),
  prop = c(0.06, 0.42, 0.10, 0.40),
  prob = c(0.44, 0.38, 0.155, 0.13),
  die = c("Fair", "Loaded", "Fair", "Loaded"),
  label = c("Fair die", "Loaded die", "Fair", "Loaded")
)

plots$slide_04_dice <- ggplot(dice, aes(prop, prob, fill = die)) +
  geom_col(data = dice[dice$rolls == 6, ], width = 0.06,
           position = "identity", alpha = 0.75) +
  geom_col(data = dice[dice$rolls == 60, ], width = 0.012,
           position = "identity", alpha = 0.75) +
  geom_text(data = dice_labels, aes(label = label, colour = die),
            hjust = 0, size = pt(16), family = bb852_font,
            fontface = "bold") +
  facet_wrap(~panel, scales = "free_y") +
  scale_fill_manual(values = c(Fair = bb852$slate, Loaded = bb852$coral)) +
  scale_colour_manual(values = c(Fair = bb852$slate, Loaded = bb852$coral)) +
  scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"),
                     breaks = seq(0, 0.75, 0.25)) +
  labs(x = "Proportion of rolls that were sixes", y = "Probability") +
  theme_bb852() +
  theme(axis.text.y = element_blank(), panel.grid.major.x = element_blank(),
        panel.spacing = unit(2, "lines"))
takeaway$slide_04_dice <- plots$slide_04_dice


# Slide 8: the two kinds of error --------------------------------------------------
# A 2 x 2 decision table drawn as tiles.

errors <- data.frame(
  truth = factor(rep(c("No real effect", "Real effect"), each = 2),
                 levels = c("No real effect", "Real effect")),
  decision = factor(rep(c("Not significant", "Significant (p < 0.05)"), 2),
                    levels = c("Significant (p < 0.05)", "Not significant")),
  label = c("Correct", "Type I error\nfalse positive\nrate = \u03b1",
            "Type II error\nfalse negative\nrate = \u03b2",
            "Detection\npower = 1 \u2212 \u03b2"),
  kind = c("ok", "error", "error", "power")
)

plots$slide_08_error_table <- ggplot(errors, aes(truth, decision)) +
  geom_tile(aes(fill = kind), colour = bb852$bg, linewidth = 3) +
  geom_text(aes(label = label,
                colour = ifelse(kind == "ok", "dark", "light")),
            size = pt(17), family = bb852_font, lineheight = 0.95) +
  scale_fill_manual(values = c(ok = bb852$panel, error = bb852$light,
                               power = bb852$coral)) +
  scale_colour_manual(values = c(dark = bb852$text, light = bb852$text)) +
  scale_x_discrete(position = "top") +
  labs(x = "The truth", y = "Your test says") +
  theme_bb852() +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(colour = bb852$text, size = 16),
        axis.title.x.top = element_text(colour = bb852$muted, size = 16),
        axis.title.y = element_text(colour = bb852$muted, size = 16))
takeaway$slide_08_error_table <- plots$slide_08_error_table


# Slide 12: same signal, different noise -------------------------------------------
# Two pairs of normal distributions, means 12 and 14.4, SD 1 vs SD 3.

curves <- do.call(rbind, lapply(c(1, 3), function(s) {
  x <- seq(2, 24, length.out = 400)
  rbind(
    data.frame(sd = s, x = x, y = dnorm(x, controlMean, s), group = "Control"),
    data.frame(sd = s, x = x, y = dnorm(x, treatmentMean, s),
               group = "High protein")
  )
}))
curves$panel <- factor(ifelse(curves$sd == 1, "Little natural variation (SD = 1 mm)",
                              "More natural variation (SD = 3 mm)"),
                       levels = c("Little natural variation (SD = 1 mm)",
                                  "More natural variation (SD = 3 mm)"))

plots$slide_12_signal_noise <- ggplot(curves, aes(x, y, colour = group)) +
  geom_area(aes(fill = group), position = "identity", alpha = 0.2,
            linewidth = 0) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = c(controlMean, treatmentMean),
             colour = bb852$grey, linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = group_cols) +
  scale_fill_manual(values = group_cols) +
  labs(x = "Body length (mm)", y = NULL) +
  theme_bb852() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())


# Slide 16: one simulated experiment -----------------------------------------------
# Same seeds as the course book, so the numbers match the chapter.

set.seed(42)
control <- rnorm(n = 10, mean = 12, sd = 3)
set.seed(142)
treatment <- rnorm(n = 10, mean = 14.4, sd = 3)
one_p <- t.test(control, treatment)$p.value

one_exp <- data.frame(
  group = factor(rep(c("Control", "High protein"), each = 10),
                 levels = c("Control", "High protein")),
  length = c(control, treatment)
)
set.seed(1)
plots$slide_16_one_experiment <- ggplot(one_exp, aes(group, length)) +
  geom_boxplot(aes(colour = group), fill = NA, width = 0.5,
               outlier.shape = NA, linewidth = 0.7) +
  geom_jitter(aes(colour = group), width = 0.08, height = 0, size = 3,
              alpha = 0.8) +
  scale_colour_manual(values = group_cols) +
  labs(x = NULL, y = "Body length (mm)") +
  theme_bb852() +
  theme(axis.text.x = element_text(colour = bb852$text, size = 18))

save_text(c(
  "control <- rnorm(n = 10, mean = 12, sd = 3)",
  "treatment <- rnorm(n = 10, mean = 14.4, sd = 3)",
  "t.test(control, treatment)$p.value",
  capture.output(print(one_p))
), "slide_16_one_experiment")


# Slide 17: twenty experiments, same truth ------------------------------------------
# Each point is the estimated difference (high protein minus control) from one
# simulated experiment with n = 10 per group, with its 95% CI (Welch t-test).

set.seed(2026)
twenty <- do.call(rbind, lapply(1:20, function(i) {
  a <- rnorm(sampleSize, controlMean, sdValue)
  b <- rnorm(sampleSize, treatmentMean, sdValue)
  tt <- t.test(b, a)
  data.frame(experiment = i, diff = mean(b) - mean(a),
             lower = tt$conf.int[1], upper = tt$conf.int[2],
             p = tt$p.value)
}))
not_sig_label <- "p \u2265 0.05"
twenty$sig <- ifelse(twenty$p < 0.05, "p < 0.05", not_sig_label)
n_sig_twenty <- sum(twenty$p < 0.05)

plots$slide_17_twenty_experiments <- ggplot(twenty, aes(experiment, diff)) +
  annotate("segment", x = 0.5, xend = 20.5, y = 0, yend = 0,
           colour = bb852$grey, linewidth = 0.5) +
  annotate("segment", x = 0.5, xend = 20.5,
           y = treatmentMean - controlMean, yend = treatmentMean - controlMean,
           colour = bb852$teal, linetype = "dashed", linewidth = 0.7) +
  geom_linerange(aes(ymin = lower, ymax = upper, colour = sig),
                 linewidth = 1) +
  geom_point(aes(colour = sig), size = 3) +
  annotate("text", x = 20.8, y = treatmentMean - controlMean,
           label = "True difference\n(2.4 mm)", hjust = 0, lineheight = 0.9,
           colour = bb852$teal, size = pt(15), family = bb852_font) +
  annotate("text", x = 20.8, y = 0, label = "No difference", hjust = 0,
           colour = bb852$grey, size = pt(15), family = bb852_font) +
  scale_colour_manual(values = setNames(c(bb852$coral, bb852$grey),
                                        c("p < 0.05", not_sig_label))) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(0.5, 25.5),
                     expand = expansion(0)) +
  labs(x = "Simulated experiment", y = "Difference (mm)") +
  theme_bb852() +
  theme(legend.position = "top", legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, colour = bb852$text),
        panel.grid.major.x = element_blank())

save_text(c(
  paste("Significant experiments out of 20:", n_sig_twenty),
  capture.output(print(transform(twenty, diff = round(diff, 3),
    lower = round(lower, 3), upper = round(upper, 3), p = round(p, 4))))
), "slide_17_twenty_experiments")


# Slide 18: the full simulation (code exactly as on the slide) --------------------

set.seed(123)
powerResults <- replicate(1000,
  t.test(rnorm(sampleSize, controlMean, sdValue),
         rnorm(sampleSize, treatmentMean, sdValue))$p.value)
full_power <- (sum(powerResults < 0.05) / 1000) * 100

save_text(c(
  "controlMean <- 12",
  "treatmentMean <- 14.4",
  "sdValue <- 3",
  "sampleSize <- 10",
  "",
  "powerResults <- replicate(1000,",
  "  t.test(rnorm(sampleSize, controlMean, sdValue),",
  "         rnorm(sampleSize, treatmentMean, sdValue))$p.value)",
  "",
  "(sum(powerResults < 0.05) / 1000) * 100",
  capture.output(print(full_power)),
  "",
  paste("Analytic check, power.t.test:",
        round(power.t.test(n = 10, delta = 2.4, sd = 3)$power, 3))
), "slide_18_full_simulation")


# Slide 19: Type I error check, no real difference ----------------------------------

set.seed(123)
nullResults <- replicate(1000,
  t.test(rnorm(sampleSize, controlMean, sdValue),
         rnorm(sampleSize, controlMean, sdValue))$p.value)
null_rate <- (sum(nullResults < 0.05) / 1000) * 100
save_text(c(
  "# treatmentMean set equal to controlMean (12)",
  "(sum(nullResults < 0.05) / 1000) * 100",
  capture.output(print(null_rate))
), "slide_19_type_I_check")


# Slide 20: power against sample size --------------------------------------------------

n80 <- power_curve$sampleSize[which(power_curve$Power >= 0.8)[1]]
n80_analytic <- ceiling(power.t.test(power = 0.8, delta = 2.4, sd = 3)$n)

plots$slide_20_power_curve <- ggplot(power_curve, aes(sampleSize, Power)) +
  geom_hline(yintercept = 0.8, colour = bb852$coral, linetype = "dashed",
             linewidth = 0.8) +
  geom_vline(xintercept = n80_analytic, colour = bb852$grey,
             linetype = "dotted", linewidth = 0.6) +
  geom_line(colour = bb852$slate, linewidth = 0.8) +
  geom_point(colour = bb852$slate, size = 2.2) +
  annotate("point", x = 10, y = power_curve$Power[power_curve$sampleSize == 10],
           colour = bb852$coral, size = 4.5) +
  annotate("text", x = 11, y = power_curve$Power[power_curve$sampleSize == 10] - 0.04,
           label = "n = 10", hjust = 0, colour = bb852$coral,
           size = pt(16), family = bb852_font, fontface = "bold") +
  annotate("text", x = n80_analytic + 0.5, y = 0.1,
           label = paste(n80_analytic, "(formula)"), hjust = 0,
           colour = bb852$grey, size = pt(15), family = bb852_font) +
  annotate("text", x = 5, y = 0.86, label = "80% power", hjust = 0,
           colour = bb852$coral, size = pt(16), family = bb852_font) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(round(100 * x), "%")) +
  labs(x = "Sample size per group", y = "Power") +
  theme_bb852()
takeaway$slide_20_power_curve <- plots$slide_20_power_curve

save_text(c(
  paste("First n with simulated power >= 0.8:", n80),
  paste("Analytic n for power 0.8 (power.t.test, rounded up):", n80_analytic),
  capture.output(print(round(power_curve, 3)))
), "slide_20_power_curve")


# Slide 21: smallest detectable difference with unequal groups ----------------------
# Fix the sample sizes (15 control, 6 high protein) and vary the true difference.

nControl <- 15
nTreatment <- 6
diffs <- seq(0.5, 7, by = 0.25)
set.seed(123)
detect <- data.frame(diff = diffs)
detect$Power <- vapply(diffs, function(d) {
  sum(replicate(1000,
    t.test(rnorm(nControl, controlMean, sdValue),
           rnorm(nTreatment, controlMean + d, sdValue))$p.value) < 0.05) / 1000
}, numeric(1))
power_unequal <- function(d) {
  se <- sdValue * sqrt(1 / nControl + 1 / nTreatment)
  df <- nControl + nTreatment - 2
  tc <- qt(0.975, df)
  1 - stats::pt(tc, df, d / se) + stats::pt(-tc, df, d / se)
}
detect_analytic <- uniroot(function(d) power_unequal(d) - 0.8, c(0.1, 20))$root
detect_sim <- detect$diff[which(detect$Power >= 0.8)[1]]

plots$slide_21_detectable_difference <- ggplot(detect, aes(diff, Power)) +
  geom_hline(yintercept = 0.8, colour = bb852$coral, linetype = "dashed",
             linewidth = 0.8) +
  geom_vline(xintercept = treatmentMean - controlMean, colour = bb852$grey,
             linetype = "dotted", linewidth = 0.6) +
  annotate("text", x = treatmentMean - controlMean + 0.15, y = 0.08,
           label = "Expected 2.4 mm", hjust = 0, colour = bb852$grey,
           size = pt(15), family = bb852_font) +
  geom_line(colour = bb852$slate, linewidth = 0.8) +
  geom_point(colour = bb852$slate, size = 2) +
  annotate("text", x = 0.5, y = 0.86, label = "80% power", hjust = 0,
           colour = bb852$coral, size = pt(16), family = bb852_font) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(round(100 * x), "%")) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "True difference (mm)", y = "Power") +
  theme_bb852()

save_text(c(
  "t.test(rnorm(15, controlMean, sdValue),",
  "       rnorm(6, controlMean + difference, sdValue))$p.value",
  "",
  paste("First difference with simulated power >= 0.8:", detect_sim),
  paste("Pooled-variance formula (t.test default is Welch, slightly less powerful):",
        round(detect_analytic, 2)),
  capture.output(print(detect))
), "slide_21_detectable_difference")


# Slide 24: binomial GLM (course book code; variables renamed so they do not
# overwrite sampleSize and powerResults from the insect example) ---------------------------

set.seed(123)
controlProb <- 0.5
treatmentProb <- 0.7
sampleSizeBin <- 40
binResults <- replicate(1000, {
  survival <- c(
    rbinom(sampleSizeBin, size = 1, prob = controlProb),
    rbinom(sampleSizeBin, size = 1, prob = treatmentProb)
  )
  treatment <- rep(c("control", "treatment"), each = sampleSizeBin)
  mod <- glm(survival ~ treatment, family = binomial)
  anova(mod, test = "Chi")[["Pr(>Chi)"]][2]
})
bin_power <- (sum(binResults < 0.05) / 1000) * 100
save_text(c(
  "(sum(binResults < 0.05) / 1000) * 100",
  capture.output(print(bin_power)),
  "",
  paste("Analytic n per group for 80% power (power.prop.test):",
        ceiling(power.prop.test(p1 = 0.5, p2 = 0.7, power = 0.8)$n))
), "slide_24_binomial_glm")


# Slide 25: regression power depends on the spread of x ------------------------------------

slope <- 2
intercept <- 20
residualSD <- 12
sampleSizeReg <- 30

reg_power <- function(x) {
  set.seed(123)
  res <- replicate(1000, {
    y <- intercept + slope * x + rnorm(sampleSizeReg, 0, residualSD)
    summary(lm(y ~ x))$coefficients["x", "Pr(>|t|)"]
  })
  (sum(res < 0.05) / 1000) * 100
}
x_narrow <- seq(11, 19, length.out = sampleSizeReg)
x_wide <- seq(5, 25, length.out = sampleSizeReg)
power_narrow <- reg_power(x_narrow)
power_wide <- reg_power(x_wide)

set.seed(31) # a draw whose noise averages close to zero in both panels
noise <- rnorm(sampleSizeReg, 0, residualSD)
reg_data <- rbind(
  data.frame(x = x_narrow, y = intercept + slope * x_narrow + noise,
             design = paste0("x from 11 to 19: power ", power_narrow, "%")),
  data.frame(x = x_wide, y = intercept + slope * x_wide + noise,
             design = paste0("x from 5 to 25: power ", power_wide, "%"))
)
reg_data$design <- factor(reg_data$design, levels = unique(reg_data$design))

plots$slide_25_regression_range <- ggplot(reg_data, aes(x, y)) +
  geom_abline(intercept = intercept, slope = slope, colour = bb852$teal,
              linetype = "dashed", linewidth = 0.7) +
  geom_point(colour = bb852$slate, size = 2.4, alpha = 0.8) +
  facet_wrap(~design, ncol = 1) +
  scale_x_continuous(limits = c(4, 26)) +
  labs(x = "Predictor (x)", y = "Response (y)") +
  theme_bb852()

save_text(c(
  paste("Narrow x (11 to 19) power:", power_narrow),
  paste("Wide x (5 to 25) power:", power_wide)
), "slide_25_regression_range")


# Write figures ----------------------------------------------------------------

for (name in names(plots)) {
  save_bb852(plots[[name]], name, "full")
  save_bb852(plots[[name]], name, "half")
}
for (name in names(takeaway)) {
  save_bb852(takeaway[[name]], name, "takeaway")
}

message("Power analysis figures written to ", output_root)
