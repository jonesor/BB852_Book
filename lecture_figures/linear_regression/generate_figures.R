# BB852 linear regression lecture: R code for every figure and R output
# ----------------------------------------------------------------------
# Recreates the lecture's statistical figures in ggplot2 using the BB852
# theme shared with the other lecture_figures generators, and writes the R
# output shown on the slides (anova, summary, predict) to text files, so the
# slide output cannot drift from the code.
#
# Data source (same as the course book chapter):
# https://jonesor.github.io/BB852_Book/linear-regression-models-with-a-single-continuous-explanatory-variable.html
# CourseData/morphometry.csv: Height and HandLength recorded in mm, converted
# to cm below exactly as in the book (n = 155 people).
#
# Figures marked SIMULATED use invented data, with seeds set, purely to
# illustrate a concept (significance cases, residual patterns, R-squared).
# All other figures use the morphometry teaching data, which are themselves
# simulated (see scripts/SimulateData.R).
#
# Run from the repository root:
# source("lecture_figures/linear_regression/generate_figures.R")
#
# Each figure is written as PNG (300 dpi) and PDF at the exact size of its
# BB852 slide placeholder, into lecture_figures/linear_regression/output/<layout>/.
# R output text is written to lecture_figures/linear_regression/output/text/.
#
# Labels use plotmath (e.g. R^2, beta[0]) rather than typed Unicode symbols,
# so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(patchwork)
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
  grey     = "#6B6E75",
  light    = "#9FB0C4",
  panel    = "#EDEFF1",
  cover    = "#2C4D70"
)

available_fonts <- unique(systemfonts::system_fonts()$family)
# Avenir Next on the Mac used for teaching; fallbacks only for other machines.
bb852_font <- c("Avenir Next", "Helvetica", "Inter")[
  c("Avenir Next", "Helvetica", "Inter") %in% available_fonts][1]
if (is.na(bb852_font)) bb852_font <- "sans"

theme_bb852 <- function(base_size = 18, base_family = bb852_font) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background   = element_rect(fill = bb852$bg, colour = NA),
      panel.background  = element_rect(fill = bb852$bg, colour = NA),
      panel.grid.major  = element_line(colour = bb852$grid, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(colour = bb852$text, size = base_size),
      axis.text         = element_text(colour = bb852$muted, size = base_size - 2),
      axis.ticks        = element_blank(),
      legend.position   = "none",
      strip.text        = element_text(colour = bb852$text, size = base_size - 2,
                                       face = "bold", hjust = 0),
      plot.title        = element_blank(),
      plot.subtitle     = element_blank(),
      plot.caption      = element_blank(),
      plot.margin       = margin(12, 16, 12, 12)
    )
}

# Panel headings inside patchwork figures use a bold "tag"-style title.
# These are panel labels, not plot titles: the slide title carries the message.
panel_label <- function() {
  theme(plot.title = element_text(colour = bb852$text, size = 16, face = "bold",
                                  hjust = 0, margin = margin(b = 6)))
}

# Sizes (inches) match the figure placeholders in BB852_Theme.pptx.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  third    = c(3.64, 5.30),
  slide    = c(13.33, 7.50),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "linear_regression", "output")

save_bb852 <- function(plot, filename, layout = "full", bg = bb852$bg) {
  size <- bb852_sizes[[layout]]
  if (is.null(size)) stop("layout must be one of: ", paste(names(bb852_sizes), collapse = ", "))
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(output_directory, paste0(filename, ".png")), plot,
         width = size[1], height = size[2], units = "in", dpi = 300,
         device = ragg::agg_png, bg = bg)
  ggsave(file.path(output_directory, paste0(filename, ".pdf")), plot,
         width = size[1], height = size[2], units = "in",
         device = cairo_pdf, bg = bg)
  invisible(filename)
}

write_output <- function(expr_text, filename) {
  dir.create(file.path(output_root, "text"), recursive = TRUE, showWarnings = FALSE)
  out <- capture.output(print(eval(parse(text = expr_text), envir = globalenv())))
  writeLines(out, file.path(output_root, "text", paste0(filename, ".txt")))
  invisible(out)
}

plots <- list()


# Data and model ----------------------------------------------------------
# Same code as the book chapter.

morph <- read.csv("CourseData/morphometry.csv")
morph$Height <- morph$Height / 10
morph$HandLength <- morph$HandLength / 10

mod_A <- lm(Height ~ HandLength, data = morph)

write_output("anova(mod_A)", "anova_mod_A")
write_output("summary(mod_A)", "summary_mod_A")

# Prediction for a 20 cm hand print (book's closing question).
new_hand <- data.frame(HandLength = 20)
write_output("predict(mod_A, newdata = data.frame(HandLength = 20), interval = 'confidence')",
             "predict_20cm_confidence")
write_output("predict(mod_A, newdata = data.frame(HandLength = 20), interval = 'prediction')",
             "predict_20cm_prediction")

# Sums of squares used on the slides.
ss_total    <- sum((morph$Height - mean(morph$Height))^2)
ss_residual <- sum(residuals(mod_A)^2)
ss_explained <- ss_total - ss_residual
n_obs <- nrow(morph)
f_value <- (ss_explained / 1) / (ss_residual / (n_obs - 2))
writeLines(
  c(sprintf("n = %d", n_obs),
    sprintf("SS total = %.1f", ss_total),
    sprintf("SS explained = %.1f", ss_explained),
    sprintf("SS residual = %.1f", ss_residual),
    sprintf("mean SS explained = %.1f (df = 1)", ss_explained),
    sprintf("mean SS residual = %.2f (df = %d)", ss_residual / (n_obs - 2), n_obs - 2),
    sprintf("F = %.1f", f_value),
    sprintf("R2 = SS explained / SS total = %.4f", ss_explained / ss_total),
    sprintf("r = %.4f; r^2 = %.4f", cor(morph$HandLength, morph$Height),
            cor(morph$HandLength, morph$Height)^2)),
  file.path(output_root, "text", "sums_of_squares.txt"))

b0 <- unname(coef(mod_A)[1])
b1 <- unname(coef(mod_A)[2])

hand_lab   <- "Hand length (cm)"
height_lab <- "Height (cm)"
x_scale <- scale_x_continuous(breaks = seq(14, 26, 4))
y_scale <- scale_y_continuous(limits = c(140, 200), breaks = seq(140, 200, 20))


# 01 Cover: hand length vs height on the dark title panel ------------------

plots$cover <- ggplot(morph, aes(HandLength, Height)) +
  geom_point(colour = bb852$light, size = 2.4, alpha = 0.8) +
  geom_abline(intercept = b0, slope = b1, colour = bb852$coral, linewidth = 1.4) +
  scale_x_continuous(breaks = seq(14, 26, 4)) +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852() +
  theme(
    plot.background  = element_rect(fill = bb852$cover, colour = NA),
    panel.background = element_rect(fill = bb852$cover, colour = NA),
    panel.grid.major = element_blank(),
    axis.line        = element_line(colour = bb852$light, linewidth = 0.4),
    axis.title       = element_text(colour = bb852$light),
    axis.text        = element_text(colour = bb852$light),
    plot.margin      = margin(150, 30, 130, 20)
  )
save_bb852(plots$cover, "01_cover", "cover", bg = bb852$cover)


# 02 The data and a fitted straight line -----------------------------------

plots$hand_height_line <- ggplot(morph, aes(HandLength, Height)) +
  geom_point(colour = bb852$slate, size = 2.6, alpha = 0.55) +
  geom_abline(intercept = b0, slope = b1, colour = bb852$coral, linewidth = 1.2) +
  x_scale + y_scale +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()
save_bb852(plots$hand_height_line, "02_hand_height_line", "takeaway")


# 03 The equation of a straight line (redrawn from the book) ---------------

line_panel <- function(a, b, label, label_y) {
  x0 <- 3; x1 <- 4
  y0 <- a * x0 + b; y1 <- a * x1 + b
  ggplot() +
    geom_hline(yintercept = 0, colour = bb852$axis, linewidth = 0.6) +
    geom_vline(xintercept = 0, colour = bb852$axis, linewidth = 0.6) +
    annotate("segment", x = 0, xend = 7, y = b, yend = a * 7 + b,
             colour = bb852$slate, linewidth = 1.2) +
    # slope triangle: run of 1, rise of a
    annotate("segment", x = x0, xend = x1, y = y0, yend = y0,
             colour = bb852$coral, linewidth = 0.9, linetype = "dashed") +
    annotate("segment", x = x1, xend = x1, y = y0, yend = y1,
             colour = bb852$coral, linewidth = 0.9, linetype = "dashed") +
    annotate("text", x = (x0 + x1) / 2, y = y0 - sign(a) * 0.9, label = "1",
             colour = bb852$coral, size = 5.6, family = bb852_font) +
    annotate("text", x = x1 + 0.3, y = (y0 + y1) / 2 - sign(a) * 1.2, hjust = 0,
             label = paste0("slope a = ", a), colour = bb852$coral,
             size = 5.6, family = bb852_font) +
    # intercept marker
    annotate("point", x = 0, y = b, colour = bb852$teal, size = 4) +
    annotate("text", x = 0.3, y = if (a > 0) b - 2.2 else b + 1.3, hjust = 0,
             label = paste0("intercept b = ", b), colour = bb852$teal,
             size = 5.6, family = bb852_font) +
    annotate("text", x = 7.4, y = label_y, hjust = 1, label = label,
             colour = bb852$text, size = 6.2, family = bb852_font, fontface = "bold") +
    scale_x_continuous(limits = c(-0.3, 7.6), breaks = 0:7) +
    scale_y_continuous(limits = c(-6, 15.5), breaks = seq(-5, 15, 5)) +
    labs(x = "x", y = "y") +
    theme_bb852()
}
plots$straight_lines <- line_panel(2, 1, "y = 2x + 1", -4) +
  line_panel(-2.3, 11, "y = -2.3x + 11", 14)
save_bb852(plots$straight_lines, "03_straight_line_equations", "full")


# 04 Least squares: which line fits best? -----------------------------------
# Four candidate lines; the fourth is the least-squares fit. The first version
# hides the residual sums of squares (for a prediction); the second shows them.

candidates <- data.frame(
  panel = c("A", "B", "C", "D"),
  slope = c(0, 4.50, -2.00, round(b1, 2)),
  intercept = c(round(mean(morph$Height)), 78, 205, round(b0, 1))
)
candidates$rss <- sapply(seq_len(nrow(candidates)), function(i) {
  sum((morph$Height - (candidates$intercept[i] + candidates$slope[i] * morph$HandLength))^2)
})
writeLines(capture.output(print(candidates)),
           file.path(output_root, "text", "least_squares_candidates.txt"))

ls_panel <- function(i, show_rss) {
  cand <- candidates[i, ]
  best <- cand$rss == min(candidates$rss)
  line_col <- if (show_rss && best) bb852$coral else bb852$slate
  dat <- morph
  dat$pred <- cand$intercept + cand$slope * dat$HandLength
  title <- if (show_rss) {
    sprintf("%s   RSS = %s", cand$panel, format(round(cand$rss), big.mark = ","))
  } else {
    cand$panel
  }
  ggplot(dat, aes(HandLength, Height)) +
    geom_segment(aes(xend = HandLength, yend = pred), colour = bb852$light,
                 linewidth = 0.35) +
    geom_point(colour = bb852$slate, size = 1.4, alpha = 0.6) +
    geom_abline(intercept = cand$intercept, slope = cand$slope,
                colour = line_col, linewidth = 1.2) +
    scale_x_continuous(breaks = c(15, 20, 25)) +
    coord_cartesian(ylim = c(140, 200)) +
    labs(x = hand_lab, y = if (i == 1) height_lab else NULL, title = title) +
    theme_bb852(base_size = 16) + panel_label() +
    theme(axis.title.x = element_text(size = 15))
}
plots$least_squares_question <- wrap_plots(lapply(1:4, ls_panel, show_rss = FALSE), nrow = 1)
plots$least_squares_answer   <- wrap_plots(lapply(1:4, ls_panel, show_rss = TRUE), nrow = 1)
save_bb852(plots$least_squares_question, "04a_least_squares_question", "full")
save_bb852(plots$least_squares_answer, "04b_least_squares_answer", "full")


# 05 Partitioning variation: total = explained + residual -------------------

part <- morph
part$fitted <- fitted(mod_A)
part$mean <- mean(morph$Height)

ss_panel <- function(yend_var, ystart_var, title, line = c("mean", "fit")) {
  line <- match.arg(line)
  p <- ggplot(part, aes(HandLength, .data[[ystart_var]])) +
    geom_segment(aes(xend = HandLength, yend = .data[[yend_var]]),
                 colour = bb852$coral, linewidth = 0.35, alpha = 0.8) +
    geom_point(aes(y = Height), colour = bb852$slate, size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = mean(morph$Height), colour = bb852$grey,
               linewidth = 0.6, linetype = "dashed")
  if (line == "fit") {
    p <- p + geom_abline(intercept = b0, slope = b1, colour = bb852$slate, linewidth = 1.2)
  }
  p + scale_x_continuous(breaks = c(15, 20, 25)) +
    coord_cartesian(ylim = c(140, 200)) +
    labs(x = hand_lab, y = NULL, title = title) +
    theme_bb852(base_size = 16) + panel_label() +
    theme(axis.title.x = element_text(size = 15))
}
plots$ss_partition <-
  (ss_panel("mean", "Height", sprintf("Total SS = %s", format(round(ss_total), big.mark = ",")), "mean") +
     labs(y = height_lab)) +
  ss_panel("mean", "fitted", sprintf("Explained SS = %s", format(round(ss_explained), big.mark = ",")), "fit") +
  ss_panel("fitted", "Height", sprintf("Residual SS = %s", format(round(ss_residual), big.mark = ",")), "fit") +
  plot_layout(nrow = 1)
save_bb852(plots$ss_partition, "05_ss_partition", "full")


# 06 What does "significant" mean for slope and intercept? (SIMULATED) -----
# Four scenarios crossing a zero/non-zero slope with a zero/non-zero intercept.
# n = 20, x uniform on 0-10, residual SD = 1.5. For each scenario we search
# seeds (maximum 500 tries) until the test outcome matches the scenario at
# alpha = 0.05, so the panels show the intended pattern cleanly.

sig_scenarios <- data.frame(
  panel = c("A", "B", "C", "D"),
  slope = c(0, 0.8, 0, 0.8),
  intercept = c(0, 0, 4, 4),
  title = c("Slope n.s.\nIntercept n.s.", "Slope significant\nIntercept n.s.",
            "Slope n.s.\nIntercept significant", "Slope significant\nIntercept significant")
)
simulate_case <- function(i, max_tries = 500) {
  sc <- sig_scenarios[i, ]
  want_slope <- sc$slope != 0
  want_int <- sc$intercept != 0
  for (seed in seq_len(max_tries)) {
    set.seed(1000 * i + seed)
    x <- runif(20, 0, 10)
    y <- sc$intercept + sc$slope * x + rnorm(20, 0, 1.5)
    pv <- summary(lm(y ~ x))$coefficients[, 4]
    if ((pv[2] < 0.05) == want_slope && (pv[1] < 0.05) == want_int) {
      return(data.frame(x = x, y = y, panel = sc$panel, title = sc$title, seed = 1000 * i + seed,
                        p_int = unname(pv[1]), p_slope = unname(pv[2])))
    }
  }
  stop("No seed found for scenario ", i)
}
sig_data <- do.call(rbind, lapply(1:4, simulate_case))
writeLines(capture.output(print(unique(sig_data[, c("panel", "seed", "p_int", "p_slope")]))),
           file.path(output_root, "text", "significance_cases_seeds.txt"))

sig_panel <- function(pan) {
  d <- sig_data[sig_data$panel == pan, ]
  ggplot(d, aes(x, y)) +
    geom_hline(yintercept = 0, colour = bb852$grey, linewidth = 0.6, linetype = "dashed") +
    geom_smooth(method = "lm", formula = y ~ x, colour = bb852$slate, fill = bb852$slate,
                alpha = 0.2, linewidth = 1.2, fullrange = TRUE) +
    geom_point(colour = bb852$slate, size = 2, alpha = 0.7) +
    scale_x_continuous(limits = c(0, 10), breaks = c(0, 5, 10), expand = expansion(mult = 0.02)) +
    coord_cartesian(ylim = c(-4, 14)) +
    labs(x = "x", y = if (pan == "A") "y" else NULL, title = unique(d$title)) +
    theme_bb852(base_size = 16) + panel_label()
}
plots$significance_cases <- wrap_plots(lapply(c("A", "B", "C", "D"), sig_panel), nrow = 1)
save_bb852(plots$significance_cases, "06_significance_cases", "full")


# 07 Diagnostic plots for the hand model ------------------------------------
# Drawn directly from the model so they match ggfortify::autoplot(mod_A)
# panel for panel, but in the course theme.

diag <- data.frame(
  fitted = fitted(mod_A),
  resid = residuals(mod_A),
  std_resid = rstandard(mod_A),
  leverage = hatvalues(mod_A)
)
qq <- qqnorm(diag$std_resid, plot.it = FALSE)
diag$theoretical <- qq$x
diag$sqrt_abs <- sqrt(abs(diag$std_resid))
diag_points <- geom_point(colour = bb852$slate, alpha = 0.6, size = 1.8)
diag_colours <- NULL

diag_theme <- theme_bb852(base_size = 15) + panel_label() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14))
smooth_layer <- geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                            colour = bb852$coral, linewidth = 1)

d1 <- ggplot(diag, aes(fitted, resid)) +
  geom_hline(yintercept = 0, colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  diag_points + diag_colours + smooth_layer +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs fitted") + diag_theme
d2 <- ggplot(diag, aes(theoretical, std_resid)) +
  geom_abline(intercept = 0, slope = 1, colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  diag_points + diag_colours +
  labs(x = "Theoretical quantiles", y = "Std. residuals", title = "Normal Q-Q") + diag_theme
d3 <- ggplot(diag, aes(fitted, sqrt_abs)) +
  diag_points + diag_colours + smooth_layer +
  labs(x = "Fitted values", y = expression(sqrt("|Std. residuals|")),
       title = "Scale-location") + diag_theme
d4 <- ggplot(diag, aes(leverage, std_resid)) +
  geom_hline(yintercept = 0, colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  diag_points + diag_colours +
  labs(x = "Leverage", y = "Std. residuals", title = "Leverage") + diag_theme
plots$diagnostics <- d1 + d2 + d3 + d4 + plot_layout(nrow = 1)
save_bb852(plots$diagnostics, "07_diagnostics_hand_model", "full")


# 07b The question that matters: how steep is the slope? -------------------
# Sampling distribution of the slope estimate (t distribution with n - 2 d.f.,
# centred on the estimate, scaled by its SE) with the 95% confidence interval,
# compared with the null value of 0.

slope_ci <- confint(mod_A)["HandLength", ]
write_output("confint(mod_A)", "confint_mod_A")
se1 <- summary(mod_A)$coefficients["HandLength", "Std. Error"]
df_res <- df.residual(mod_A)
curve_df <- data.frame(slope = seq(b1 - 5 * se1, b1 + 5 * se1, length.out = 300))
curve_df$density <- dt((curve_df$slope - b1) / se1, df_res) / se1
peak <- max(curve_df$density)
plots$slope_ci <- ggplot(curve_df, aes(slope, density)) +
  geom_vline(xintercept = 0, colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = 0.2, y = peak * 0.95, hjust = 0, vjust = 1, size = 5.6,
           family = bb852_font, colour = bb852$grey,
           label = "Null hypothesis:\nslope = 0") +
  geom_area(fill = bb852$light, alpha = 0.5) +
  geom_line(colour = bb852$slate, linewidth = 1.2) +
  annotate("errorbarh", y = peak * 0.08, xmin = slope_ci[1], xmax = slope_ci[2],
           height = peak * 0.06, colour = bb852$coral, linewidth = 1.2) +
  annotate("point", x = b1, y = peak * 0.08, colour = bb852$coral, size = 4) +
  annotate("text", x = b1, y = peak * 1.08, size = 5.6, family = bb852_font,
           colour = bb852$coral,
           label = sprintf("%.2f (95%% CI %.2f to %.2f)", b1, slope_ci[1], slope_ci[2])) +
  scale_x_continuous(limits = c(-0.3, 8), breaks = 0:8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Slope (cm of height per cm of hand length)", y = NULL) +
  theme_bb852() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_bb852(plots$slope_ci, "07b_slope_estimate", "takeaway")


# 08 What problems look like in a residual plot (SIMULATED) ----------------
# n = 100, seed 852. Panel A satisfies the assumptions; B has a curved
# relationship fitted with a straight line; C has variance increasing with x.

set.seed(852)
xs <- runif(100, 0, 10)
sim_good   <- data.frame(x = xs, y = 2 + 1.5 * xs + rnorm(100, 0, 2))
sim_curve  <- data.frame(x = xs, y = 2 + 0.35 * (xs - 5)^2 + 1.5 * xs + rnorm(100, 0, 1))
sim_funnel <- data.frame(x = xs, y = 2 + 1.5 * xs + rnorm(100, 0, 0.25 + 0.5 * xs))

resid_panel <- function(dat, title, highlight = FALSE) {
  m <- lm(y ~ x, data = dat)
  d <- data.frame(fitted = fitted(m), resid = residuals(m))
  ggplot(d, aes(fitted, resid)) +
    geom_hline(yintercept = 0, colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
    geom_point(colour = if (highlight) bb852$coral else bb852$slate, alpha = 0.6, size = 1.8) +
    smooth_layer +
    labs(x = "Fitted values", y = "Residuals", title = title) +
    theme_bb852(base_size = 16) + panel_label() +
    theme(axis.text = element_blank(), panel.grid.major = element_blank(),
          axis.line = element_line(colour = bb852$axis, linewidth = 0.4))
}
plots$residual_problems <-
  resid_panel(sim_good, "No pattern: assumptions OK") +
  resid_panel(sim_curve, "Curve: relationship not linear", TRUE) +
  resid_panel(sim_funnel, "Funnel: variance not constant", TRUE) +
  plot_layout(nrow = 1)
save_bb852(plots$residual_problems, "08_residual_problems", "full")


# 09 Plot the data first (for the lm() code slide) --------------------------

plots$plot_first <- ggplot(morph, aes(HandLength, Height)) +
  geom_point(colour = bb852$slate, size = 2.4, alpha = 0.55) +
  x_scale + y_scale +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()
save_bb852(plots$plot_first, "09_plot_data_first", "half")


# 10 R-squared: proportion of variation explained (SIMULATED) --------------
# Same true line (y = 1.5x) with increasing residual SD; n = 30, seed 7645
# as in the book chapter.

set.seed(7645)
r2_sds <- c(0.05, 1, 3, 8)
r2_data <- do.call(rbind, lapply(r2_sds, function(s) {
  x <- runif(30, 1, 10)
  y <- 1.5 * x + rnorm(30, 0, s)
  data.frame(x = x, y = y, sd = s, r2 = summary(lm(y ~ x))$r.squared)
}))
r2_panel <- function(s) {
  d <- r2_data[r2_data$sd == s, ]
  ggplot(d, aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = bb852$coral,
                linewidth = 1.2) +
    geom_point(colour = bb852$slate, size = 2, alpha = 0.7) +
    scale_x_continuous(breaks = c(2, 6, 10)) +
    labs(x = "x", y = if (s == r2_sds[1]) "y" else NULL,
         title = bquote(bold(R^2 == .(formatC(unique(d$r2), digits = 2, format = "f"))))) +
    theme_bb852(base_size = 16) + panel_label() +
    theme(axis.text.y = element_blank())
}
plots$r_squared <- wrap_plots(lapply(r2_sds, r2_panel), nrow = 1)
save_bb852(plots$r_squared, "10_r_squared", "full")


# 11 Confidence band vs prediction interval --------------------------------
# geom_smooth(method = "lm") draws the 95% confidence interval for the fitted
# mean line. The 95% prediction interval for a new individual is much wider.

grid_x <- data.frame(HandLength = seq(min(morph$HandLength), max(morph$HandLength), length.out = 200))
ci <- cbind(grid_x, predict(mod_A, grid_x, interval = "confidence"))
ci_obs <- predict(mod_A, interval = "confidence")
pi_obs <- suppressWarnings(predict(mod_A, interval = "prediction"))
writeLines(c(
  sprintf("inside 95%% CI band: %.0f%%",
          100 * mean(morph$Height >= ci_obs[, "lwr"] & morph$Height <= ci_obs[, "upr"])),
  sprintf("inside 95%% prediction interval: %.0f%%",
          100 * mean(morph$Height >= pi_obs[, "lwr"] & morph$Height <= pi_obs[, "upr"]))),
  file.path(output_root, "text", "coverage.txt"))

pi <- cbind(grid_x, predict(mod_A, grid_x, interval = "prediction"))

label_x <- 24.6
plots$ci_vs_pi <- ggplot(morph, aes(HandLength, Height)) +
  geom_ribbon(data = pi, aes(x = HandLength, ymin = lwr, ymax = upr), inherit.aes = FALSE,
              fill = bb852$light, alpha = 0.25) +
  geom_line(data = pi, aes(y = lwr), colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  geom_line(data = pi, aes(y = upr), colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  geom_point(colour = bb852$slate, size = 2.2, alpha = 0.45) +
  geom_ribbon(data = ci, aes(x = HandLength, ymin = lwr, ymax = upr), inherit.aes = FALSE,
              fill = bb852$coral, alpha = 0.35) +
  geom_line(data = ci, aes(y = fit), colour = bb852$coral, linewidth = 1.2) +
  annotate("text", x = 13.4, y = 197, hjust = 0, vjust = 1, size = 5.6, family = bb852_font,
           colour = bb852$grey, label = "95% prediction interval:\nwhere a new person is likely to fall") +
  annotate("text", x = 25.9, y = 147, hjust = 1, size = 5.6, family = bb852_font,
           colour = bb852$coral, label = "95% confidence interval\nfor the mean line") +
  x_scale + scale_y_continuous(breaks = seq(140, 200, 20)) +
  coord_cartesian(ylim = c(140, 200)) +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()
save_bb852(plots$ci_vs_pi, "11_ci_vs_prediction_interval", "takeaway")


# 12 Answer: predicted height for a 20 cm hand print -----------------------

p20_ci <- predict(mod_A, new_hand, interval = "confidence")
p20_pi <- predict(mod_A, new_hand, interval = "prediction")
plots$prediction_20cm <- ggplot(morph, aes(HandLength, Height)) +
  geom_point(colour = bb852$light, size = 2.2, alpha = 0.6) +
  geom_abline(intercept = b0, slope = b1, colour = bb852$slate, linewidth = 1.2) +
  annotate("segment", x = 20, xend = 20, y = 140, yend = p20_ci[1, "fit"],
           colour = bb852$grey, linetype = "dashed", linewidth = 0.6) +
  annotate("errorbar", x = 20, ymin = p20_pi[1, "lwr"], ymax = p20_pi[1, "upr"],
           width = 0.5, colour = bb852$coral, linewidth = 1.2) +
  annotate("point", x = 20, y = p20_ci[1, "fit"], colour = bb852$coral, size = 4.5) +
  annotate("text", x = 20.5, y = p20_ci[1, "fit"] - 1, hjust = 0, vjust = 1, size = 5.6,
           family = bb852_font, colour = bb852$coral,
           label = sprintf("%.0f cm\n(95%% PI %.0f to %.0f cm)",
                           p20_ci[1, "fit"], p20_pi[1, "lwr"], p20_pi[1, "upr"])) +
  x_scale + y_scale +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()
save_bb852(plots$prediction_20cm, "12_prediction_20cm", "takeaway")


# 13 Next time: add a second explanatory variable (sex) --------------------

sex_cols <- c(Female = bb852$coral, Male = bb852$slate)
sex_labels <- data.frame(
  Sex = c("Female", "Male"),
  HandLength = c(18.9, 19.4),
  Height = c(146, 183)
)
plots$next_time_sex <- ggplot(morph, aes(HandLength, Height, colour = Sex)) +
  geom_point(size = 2.4, alpha = 0.6) +
  geom_text(data = sex_labels, aes(label = Sex), size = 6.2, fontface = "bold",
            family = bb852_font) +
  scale_colour_manual(values = sex_cols) +
  x_scale + y_scale +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()
save_bb852(plots$next_time_sex, "13_next_time_sex", "takeaway")
