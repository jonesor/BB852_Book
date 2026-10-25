# BB852 Evaluating and comparing models lecture: figures
# ------------------------------------------------------
# Every statistical figure in the lecture, in the BB852 theme shared with the
# other lecture_figures generators (see lecture_figures/ancova).
#
# Course book chapter (source: 3_07-EvaluatingModels.Rmd):
# https://jonesor.github.io/BB852_Book/evaluating-linear-models.html
#
# Data:
#   CourseData/morphometry.csv  n = 155 (simulated in scripts/SimulateData.R);
#                               converted from mm to cm as in the book. The
#                               models and the random variable (seed 42) are
#                               exactly those in the book chapter.
# Simulated here (seeds stated):
#   slide 6  residual patterns (no pattern, curvature, funnel)
#   slide 8  four data sets with the same true slope and different scatter
#
# Run from the repository root:
# source("lecture_figures/evaluating_models/generate_figures.R")
#
# PNG files are written to lecture_figures/evaluating_models/output/<layout>/.
# Every content figure is written at full and half width; figures placed on a
# "Title, Bullets & Photo" slide are also written at takeaway size. The R
# output shown on the slides is written to output/text/.
# Object and file names use the slide numbers of the rebuilt deck
# (BB852_15_Evaluating_Models_v3).
#
# Labels use \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)
library(patchwork)


# Define the BB852 theme and export functions -----------------------------
# Palette, theme and sizes match lecture_figures/ancova.

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

# Panel titles are allowed inside multi-panel figures (the slide title
# describes the whole figure; the panel title names each panel).
panel_theme <- function(base_size = 16) {
  theme_bb852(base_size) +
    theme(
      plot.title = element_text(size = base_size, colour = bb852$text,
                                family = bb852_font, face = "bold",
                                hjust = 0, margin = margin(b = 4)),
      plot.margin = margin(6, 10, 6, 6)
    )
}

# Sizes (inches) match the figure placeholders in the BB852 slide theme.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "evaluating_models", "output")
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

# Text size helper: ggplot text size is in mm. Nothing below 14 pt at final
# size (BB852_figure_instructions.md); theme axis text is base_size - 2, so
# every theme here uses base_size >= 16.
pt <- function(points) points / .pt

sex_cols <- c(Female = bb852$slate, Male = bb852$coral)

plots <- list()
half_plots <- list()
takeaway_plots <- list()


# Data and models (as in the course book chapter) ---------------------------

morph <- read.csv(file.path("CourseData", "morphometry.csv"))
morph$Height <- morph$Height / 10          # mm -> cm, as in the book
morph$HandLength <- morph$HandLength / 10

mod1 <- lm(Height ~ HandLength + Sex, data = morph)

set.seed(42)                               # as in the book
morph$randomVariable <- rnorm(nrow(morph))
mod1_random <- lm(Height ~ HandLength + Sex + randomVariable, data = morph)

mod2 <- lm(Height ~ HandLength * Sex, data = morph)
mod3 <- lm(Height ~ HandLength, data = morph)
mod4 <- lm(Height ~ Sex, data = morph)
mod5 <- lm(Height ~ 1, data = morph)
mod6 <- lm(Height ~ HandLength + randomVariable, data = morph)

mod1_sex_first <- lm(Height ~ Sex + HandLength, data = morph)

AICtable <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
AICtable <- AICtable[order(AICtable$AIC), ]

hand_lab <- "Hand length (cm)"
height_lab <- "Height (cm)"


# R output shown on slides ------------------------------------------------------

save_output("summary(mod1)", "slide_03_summary_mod1")
save_output("anova(mod1)", "slide_09_anova_mod1")
save_output("summary(mod1)$r.squared", "slide_09_r2_mod1")
save_output(
  "c(multiple_mod1 = summary(mod1)$r.squared, multiple_random = summary(mod1_random)$r.squared, adjusted_mod1 = summary(mod1)$adj.r.squared, adjusted_random = summary(mod1_random)$adj.r.squared)",
  "slide_12_r2_random"
)
save_output("confint(mod1)", "slide_03_confint_mod1")
save_output("AICtable", "slide_17_aic_table")
save_output("anova(mod1, mod2)", "slide_17_nested_f_test")
save_output("anova(mod1_sex_first)", "slide_23_anova_sex_first")


# Numbers used on the slides (checked here so slide text cannot drift) ---------

ss1 <- anova(mod1)[["Sum Sq"]]             # HandLength, Sex, Residuals
ss2 <- anova(mod1_sex_first)[["Sum Sq"]]   # Sex, HandLength, Residuals
r2_from_table <- sum(ss1[1:2]) / sum(ss1)
stopifnot(isTRUE(all.equal(r2_from_table, summary(mod1)$r.squared)))
explained <- sum(ss1[1:2])

# Unique contribution of a term = its SS when entered last. Whatever is left
# of the explained SS is shared and cannot be credited to either term.
unique_hand <- ss2[2]                      # HandLength entered after Sex
unique_sex <- ss1[2]                       # Sex entered after HandLength
shared <- explained - unique_hand - unique_sex

shares <- data.frame(
  order = c("HandLength first", "HandLength first",
            "Sex first", "Sex first",
            "Unique and shared", "Unique and shared", "Unique and shared"),
  term = c("Hand length", "Sex", "Hand length", "Sex",
           "Hand length", "Shared", "Sex"),
  ss = c(ss1[1], ss1[2], ss2[2], ss2[1], unique_hand, shared, unique_sex)
)
shares$share <- shares$ss / explained

# Order-independent split (speaker notes only): the lmg metric of relaimpo,
# which for two predictors is the average of the two orders.
lmg_manual <- c(HandLength = (ss1[1] + ss2[2]) / 2,
                Sex = (ss1[2] + ss2[1]) / 2) / explained
if (requireNamespace("relaimpo", quietly = TRUE)) {
  lmg <- relaimpo::calc.relimp(mod1, type = "lmg", rela = TRUE)@lmg
  stopifnot(isTRUE(all.equal(unname(lmg[c("HandLength", "Sex")]),
                             unname(lmg_manual), tolerance = 1e-6)))
}

# Slide 12: how often does adjusted R-squared fall when pure noise is added?
# 2,000 random noise variables, seed stated.
set.seed(2000)
adj_falls <- replicate(2000, {
  z <- rnorm(nrow(morph))
  summary(lm(Height ~ HandLength + Sex + z, data = morph))$adj.r.squared <
    summary(mod1)$adj.r.squared
})

writeLines(
  c(sprintf("R2 from anova table = %.4f", r2_from_table),
    sprintf("%-24s %-12s SS = %8.1f  share of explained = %.3f",
            shares$order, shares$term, shares$ss, shares$share),
    sprintf("Share of TOTAL: HandLength %.3f, Sex %.3f, Residual %.3f",
            ss1[1] / sum(ss1), ss1[2] / sum(ss1), ss1[3] / sum(ss1)),
    sprintf("Unique HandLength %.1f, unique Sex %.1f, shared %.1f (%.1f%% of explained)",
            unique_hand, unique_sex, shared, 100 * shared / explained),
    sprintf("lmg (average of both orders): HandLength %.3f, Sex %.3f",
            lmg_manual[1], lmg_manual[2]),
    sprintf("Adjusted R2 fell in %.1f%% of 2000 noise variables (seed 2000)",
            100 * mean(adj_falls)),
    sprintf("delta AIC: %s", paste(rownames(AICtable),
                                   round(AICtable$AIC - min(AICtable$AIC), 1),
                                   collapse = ", "))),
  file.path(text_directory, "numbers_used_on_slides.txt")
)


# Slide 1: title-slide chart ----------------------------------------------------
# Residuals vs fitted for mod1, on the cover colour.

cover_df <- data.frame(fitted = fitted(mod1), resid = resid(mod1), Sex = morph$Sex)
plots$slide_01_cover <- ggplot(cover_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, colour = bb852$bg, linewidth = 1) +
  geom_point(aes(colour = Sex), size = 2.6, alpha = 0.8) +
  scale_colour_manual(values = c(Female = bb852$light, Male = bb852$coral)) +
  labs(x = "Fitted values", y = "Residuals") +
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


# Slide 3: the running example (mod1, parallel lines) ---------------------------

cb <- coef(mod1)
lines_df <- data.frame(
  Sex = c("Female", "Male"),
  intercept = c(cb[["(Intercept)"]], cb[["(Intercept)"]] + cb[["SexMale"]]),
  slope = cb[["HandLength"]]
)
# Draw each fitted line only over that sex's range of hand length.
ranges <- aggregate(HandLength ~ Sex, data = morph, FUN = range)
lines_df$x_min <- ranges$HandLength[match(lines_df$Sex, ranges$Sex), 1]
lines_df$x_max <- ranges$HandLength[match(lines_df$Sex, ranges$Sex), 2]
lines_df$y_min <- lines_df$intercept + lines_df$slope * lines_df$x_min
lines_df$y_max <- lines_df$intercept + lines_df$slope * lines_df$x_max
lines_df$x <- lines_df$x_max
lines_df$y <- lines_df$y_max

plots$slide_03_running_example <- ggplot(morph, aes(HandLength, Height, colour = Sex)) +
  geom_point(alpha = 0.5, size = 2.4) +
  geom_segment(data = lines_df,
               aes(x = x_min, xend = x_max, y = y_min, yend = y_max, colour = Sex),
               linewidth = 1.2) +
  geom_text(data = lines_df, aes(x = x + 0.15, y = y, label = Sex),
            hjust = 0, size = pt(16), family = bb852_font, fontface = "bold") +
  scale_colour_manual(values = sex_cols) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.22))) +
  labs(x = hand_lab, y = height_lab) +
  theme_bb852()


# Slide 5: the four diagnostic plots for mod1 -----------------------------------
# The same four panels that ggfortify::autoplot(mod1) draws, in the theme.

diag_df <- data.frame(
  fitted = fitted(mod1),
  resid = resid(mod1),
  std_resid = rstandard(mod1),
  leverage = hatvalues(mod1)
)
diag_df$sqrt_abs <- sqrt(abs(diag_df$std_resid))

smooth_layer <- function() {
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              colour = bb852$coral, linewidth = 1.1)
}
diag_point <- function() geom_point(colour = bb852$slate, alpha = 0.55, size = 1.8)

d_rvf <- ggplot(diag_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, colour = bb852$grey, linetype = "22") +
  diag_point() + smooth_layer() +
  labs(title = "Residuals vs fitted", x = "Fitted values", y = "Residuals") +
  panel_theme()

d_qq <- ggplot(diag_df, aes(sample = std_resid)) +
  geom_qq_line(colour = bb852$grey, linetype = "22") +
  geom_qq(colour = bb852$slate, alpha = 0.55, size = 1.8) +
  labs(title = "Normal Q-Q", x = "Theoretical quantiles",
       y = "Std. residuals") +
  panel_theme()

d_sl <- ggplot(diag_df, aes(fitted, sqrt_abs)) +
  diag_point() + smooth_layer() +
  labs(title = "Scale-location", x = "Fitted values",
       y = "\u221A|Std. residuals|") +
  panel_theme()

d_lev <- ggplot(diag_df, aes(leverage, std_resid)) +
  geom_hline(yintercept = 0, colour = bb852$grey, linetype = "22") +
  diag_point() +                        # no smoother: it swings on 2 points
  labs(title = "Leverage", x = "Leverage",
       y = "Std. residuals") +
  panel_theme()

plots$slide_05_diagnostics <- d_rvf + d_qq + d_sl + d_lev + plot_layout(nrow = 1)
half_plots$slide_05_diagnostics <- (d_rvf + d_qq + d_sl + d_lev +
                                      plot_layout(ncol = 2)) &
  panel_theme(16) & theme(plot.margin = margin(3, 8, 3, 4))


# Slide 6: what problems look like (simulated) ------------------------------------
# Residuals vs fitted for three simulated regressions (n = 150 each):
# a correct model, a straight line fitted to a curved relationship, and a
# straight line fitted to data whose spread grows with x.

set.seed(1507)
n_sim <- 150
x_sim <- runif(n_sim, 0, 10)
sim_fit <- function(y, label) {
  m <- lm(y ~ x_sim)
  data.frame(fitted = fitted(m), resid = resid(m) / sd(resid(m)), label = label)
}
patterns <- rbind(
  sim_fit(2 + 1.5 * x_sim + rnorm(n_sim, 0, 1.5), "No pattern: fine"),
  sim_fit(2 + 0.25 * (x_sim - 5)^2 + 1.0 * x_sim + rnorm(n_sim, 0, 0.7),
          "Curve: non-linearity"),
  sim_fit(2 + 1.5 * x_sim + rnorm(n_sim, 0, 0.25 + 0.45 * x_sim),
          "Funnel: unequal variance")
)
patterns$label <- factor(patterns$label, levels = unique(patterns$label))

pattern_panel <- function(lab, colour) {
  ggplot(subset(patterns, label == lab), aes(fitted, resid)) +
    geom_hline(yintercept = 0, colour = bb852$grey, linetype = "22") +
    geom_point(colour = colour, alpha = 0.55, size = 1.8) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = bb852$text, linewidth = 1) +
    coord_cartesian(ylim = c(-3.6, 3.6)) +
    labs(title = lab, x = "Fitted values", y = "Residuals (scaled)") +
    panel_theme(16) +
    theme(axis.text = element_blank())
}
p_fine <- pattern_panel("No pattern: fine", bb852$teal)
p_curve <- pattern_panel("Curve: non-linearity", bb852$coral)
p_funnel <- pattern_panel("Funnel: unequal variance", bb852$coral)

plots$slide_06_residual_patterns <- p_fine + p_curve + p_funnel + plot_layout(nrow = 1)
half_plots$slide_06_residual_patterns <- (p_fine / p_curve / p_funnel) &
  panel_theme(14) & theme(axis.text = element_blank(),
                          plot.margin = margin(2, 8, 2, 4))


# Slide 8: same slope, different scatter, different R-squared (simulated) ------
# Four data sets of n = 40 with the SAME true line (y = 5 + 2x). Only the
# residual SD differs. Each panel reports the fitted slope and R-squared.

set.seed(852)
n_r2 <- 40
x_r2 <- seq(0, 10, length.out = n_r2)
z_r2 <- as.numeric(scale(rnorm(n_r2)))     # one standardised noise vector
sds <- c(0.8, 3.5, 7, 14)
r2_data <- do.call(rbind, lapply(sds, function(s) {
  y <- 5 + 2 * x_r2 + s * z_r2
  m <- lm(y ~ x_r2)
  data.frame(x = x_r2, y = y, sd = s,
             label = sprintf("R\u00B2 = %.2f   slope = %.1f",
                             summary(m)$r.squared, coef(m)[2]))
}))
r2_data$label <- factor(r2_data$label, levels = unique(r2_data$label))

r2_plot <- function(base) {
  # Fitted line (coral) first, true line (dark, dashed) on top so it shows.
  ggplot(r2_data, aes(x, y)) +
    geom_point(colour = bb852$light, alpha = 0.9, size = 1.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = bb852$coral, linewidth = 1.6) +
    geom_abline(intercept = 5, slope = 2, colour = bb852$text,
                linetype = "22", linewidth = 0.9) +
    facet_wrap(~label, nrow = if (base == "row") 1 else 2) +
    labs(x = NULL, y = NULL) +
    theme_bb852(16) +
    theme(axis.text = element_blank(),
          panel.spacing = unit(1, "lines"))
}
plots$slide_08_r2_panels <- r2_plot("row")
half_plots$slide_08_r2_panels <- r2_plot("grid")
takeaway_plots$slide_08_r2_panels <- r2_plot("grid")


# Slide 17: delta AIC for the six candidate models ---------------------------------

aic_df <- data.frame(model = rownames(AICtable),
                     delta = AICtable$AIC - min(AICtable$AIC))
formula_lab <- c(
  mod1 = "HandLength + Sex",
  mod2 = "HandLength * Sex",
  mod3 = "HandLength",
  mod4 = "Sex",
  mod5 = "1 (mean only)",
  mod6 = "HandLength + random"
)
aic_df$label <- sprintf("%s  %s", aic_df$model, formula_lab[aic_df$model])
aic_df$label <- factor(aic_df$label, levels = rev(aic_df$label))
x_cap <- 85
aic_df$off_scale <- aic_df$delta > x_cap
aic_df$plot_delta <- pmin(aic_df$delta, x_cap)
aic_df$text <- ifelse(aic_df$off_scale,
                      sprintf("%.0f (off scale)", aic_df$delta),
                      sprintf("%.1f", aic_df$delta))
aic_df$best <- aic_df$delta < 2
n_models <- nrow(aic_df)

# Off-scale models (mod5, delta 250) are drawn as an arrow to the edge of the
# axis rather than as a point, so no point sits at a misleading position.
aic_plot <- function(base) {
  ggplot(aic_df, aes(plot_delta, label)) +
    annotate("rect", xmin = -Inf, xmax = 2, ymin = -Inf, ymax = Inf,
             fill = bb852$panel) +
    annotate("text", x = 12, y = n_models + 0.85, hjust = 0, vjust = 0.5,
             size = pt(base - 2), family = bb852_font, colour = bb852$grey,
             label = "\u2190 shaded: \u0394AIC < 2") +
    geom_segment(data = subset(aic_df, !off_scale),
                 aes(x = 0, xend = plot_delta, yend = label),
                 colour = bb852$light, linewidth = 1) +
    geom_segment(data = subset(aic_df, off_scale),
                 aes(x = 0, xend = x_cap + 4, yend = label),
                 colour = bb852$slate, linewidth = 1,
                 arrow = arrow(length = unit(0.12, "in"), type = "closed")) +
    geom_point(data = subset(aic_df, !off_scale), aes(colour = best), size = 4) +
    geom_text(aes(label = text,
                  x = ifelse(off_scale, x_cap + 4, plot_delta),
                  hjust = ifelse(off_scale, 1, 0)),
              nudge_y = 0.38, size = pt(base - 2),
              family = bb852_font, colour = bb852$text) +
    scale_colour_manual(values = c(`TRUE` = bb852$coral, `FALSE` = bb852$slate)) +
    scale_x_continuous(limits = c(0, x_cap + 6), breaks = seq(0, 80, 20),
                       expand = expansion(mult = c(0.03, 0.02))) +
    scale_y_discrete(limits = levels(aic_df$label),
                     expand = expansion(add = c(0.6, 1.1))) +
    labs(x = "\u0394AIC (vs best model)", y = NULL) +
    theme_bb852(base) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(colour = bb852$text, hjust = 0))
}
plots$slide_17_delta_aic <- aic_plot(18)
half_plots$slide_17_delta_aic <- aic_plot(16)


# Slide 21: partitioning the variation in height ------------------------------------
# Top bar: total variation (SS) split into hand length, sex and residual.
# Bottom bar: only the explained part, rescaled to 100%.

part_cols <- c(`Hand length` = bb852$slate, Sex = bb852$coral,
               Residual = bb852$panel)
part_df <- rbind(
  data.frame(bar = "All variation in height",
             term = c("Hand length", "Sex", "Residual"),
             share = ss1 / sum(ss1)),
  data.frame(bar = "Variation explained by the model",
             term = c("Hand length", "Sex"),
             share = ss1[1:2] / explained)
)
part_df$bar <- factor(part_df$bar, levels = rev(unique(part_df$bar)))
part_df$term <- factor(part_df$term, levels = rev(names(part_cols)))
part_df$text_col <- ifelse(part_df$term == "Residual", "dark", "light")
part_df$lab <- sprintf("%s\n%.0f%%", part_df$term, 100 * part_df$share)
part_df$lab[part_df$share < 0.10] <- sprintf("%.0f%%", 100 * part_df$share[part_df$share < 0.10])

stack_plot <- function(df, base, wrap_width = 14,
                       fills = c(part_cols, Shared = bb852$light)) {
  ggplot(df, aes(share, bar, fill = term, group = term)) +
    geom_col(width = 0.62, colour = bb852$bg, linewidth = 0.8) +
    geom_text(aes(label = lab, colour = text_col),
              position = position_stack(vjust = 0.5), size = pt(base - 2),
              family = bb852_font, fontface = "bold", lineheight = 0.9) +
    scale_fill_manual(values = fills) +
    scale_colour_manual(values = c(dark = bb852$text, light = bb852$bg)) +
    scale_x_continuous(labels = function(v) paste0(100 * v, "%"),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(labels = function(v) gsub("(.{1,14})(\\s|$)", "\\1\n", v)) +
    labs(x = NULL, y = NULL) +
    theme_bb852(base) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(colour = bb852$text, hjust = 1,
                                     lineheight = 0.9))
}
plots$slide_21_partition <- stack_plot(part_df, 18)
half_plots$slide_21_partition <- stack_plot(part_df, 16)
takeaway_plots$slide_21_partition <- stack_plot(part_df, 16)


# Slide 23: the split depends on the order of terms ----------------------------------
# Bars 1-2: shares of explained SS under each order. Bar 3: unique
# contribution of each term (its SS when entered last) and the shared part.

shares$bar <- factor(shares$order, levels = rev(unique(shares$order)))
shares$term <- factor(shares$term, levels = c("Sex", "Shared", "Hand length"))
shares$text_col <- ifelse(shares$term == "Shared", "dark", "light")
shares$lab <- sprintf("%s\n%.0f%%", shares$term, 100 * shares$share)
shares$lab[shares$share < 0.20] <- sprintf("%.0f%%", 100 * shares$share[shares$share < 0.20])

order_cols <- c(`Hand length` = bb852$slate, Shared = bb852$light,
                Sex = bb852$coral)
plots$slide_23_order <- stack_plot(shares, 18)
half_plots$slide_23_order <- stack_plot(shares, 16)
takeaway_plots$slide_23_order <- stack_plot(shares, 16)


# Save every figure ------------------------------------------------------------------

for (name in names(plots)) {
  if (name == "slide_01_cover") {
    save_bb852(plots[[name]], name, layout = "cover", bg = bb852$cover)
    next
  }
  save_bb852(plots[[name]], name, layout = "full")
  half_plot <- if (name %in% names(half_plots)) half_plots[[name]] else plots[[name]]
  save_bb852(half_plot, name, layout = "half")
  if (name %in% names(takeaway_plots)) {
    save_bb852(takeaway_plots[[name]], name, layout = "takeaway")
  }
}
