# BB852 lecture figures: Generalised linear models (part 2)
#
# Course book chapters:
# https://jonesor.github.io/BB852_Book/extending-use-cases-of-glm.html
# https://jonesor.github.io/BB852_Book/glm-families-and-use-cases.html
#
# Data (empirical, from CourseData/), analysed exactly as in the course book:
#   NFLfieldgoal.csv      binary binomial GLM; yards -> metres (n = 948)
#   hawksbill.csv         aggregated binomial GLM with cbind() (7 clutches)
# Analytical (no data): mean-variance curves, logit link curves, exact
#   binomial confidence intervals for 1/2, 5/10 and 50/100 survivors.
#
# Run from the repository root:
# source("lecture_figures/glm_part2/generate_figures.R")
#
# PNGs are written to lecture_figures/glm_part2/output/<layout>/ at the size
# of the slide placeholder they fill. R output for the slides goes to
# output/text/. Labels use \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)
library(patchwork)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match the other lecture_figures generators.

bb852 <- list(
  bg    = "#FBFAF6",
  text  = "#1B2230",
  muted = "#6B6E75",
  grid  = "#DDE2E8",
  slate = "#24405F",
  coral = "#D0745A",
  teal  = "#2A7F76",
  grey  = "#6B6E75",
  light = "#9FB0C4",
  panel = "#EDEFF1"
)

available_fonts <- unique(systemfonts::system_fonts()$family)
bb852_font <- if (nzchar(Sys.getenv("BB852_FONT"))) {
  Sys.getenv("BB852_FONT")
} else {
  c("Avenir Next", "Helvetica", "Inter", "sans")[
    c("Avenir Next", "Helvetica", "Inter", "sans") %in%
      c(available_fonts, "sans")][1]
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

bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25)
)

output_root <- file.path("lecture_figures", "glm_part2", "output")
unlink(output_root, recursive = TRUE)

save_bb852 <- function(plot, filename, layout = "half") {
  size <- bb852_sizes[[layout]]
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(output_directory, paste0(filename, ".png")), plot,
         width = size[1], height = size[2], units = "in", dpi = 300,
         device = ragg::agg_png, bg = bb852$bg)
  invisible(filename)
}

# Panel labels for side-by-side comparisons are drawn as facet strips (the
# figure instructions forbid plot titles). Layers whose data lack `lab` are
# simply repeated in the single panel.
with_lab <- function(d, title) {
  d$lab <- title
  d
}
strip_lab <- function() facet_wrap(~lab)

# Every content figure is saved at full and half width (generate_all.R checks
# that both sets match), plus at its slide layout if that is different.
save_all <- function(plot, filename, layout = "half") {
  for (lay in unique(c("full", "half", layout))) {
    save_bb852(plot, filename, lay)
  }
}

text_directory <- file.path(output_root, "text")
dir.create(text_directory, recursive = TRUE, showWarnings = FALSE)
save_output <- function(expr_text, filename) {
  out <- capture.output(eval(parse(text = expr_text), envir = globalenv()))
  writeLines(c(paste(">", expr_text), out),
             file.path(text_directory, paste0(filename, ".txt")))
}

pt <- function(points) points / .pt

# Predictions with a 95% CI on the link scale, back-transformed.
predict_ci <- function(model, newdata) {
  pv <- predict(model, newdata = newdata, se.fit = TRUE)
  inv <- family(model)$linkinv
  newdata$fit <- inv(pv$fit)
  newdata$lwr <- inv(pv$fit - 1.96 * pv$se.fit)
  newdata$upr <- inv(pv$fit + 1.96 * pv$se.fit)
  newdata
}


# Data and models (as in the course book) ---------------------------------

NFL <- read.csv(file.path("CourseData", "NFLfieldgoal.csv"))
NFL$Distance <- NFL$Distance * 0.9144
nflMod <- glm(Success ~ Distance, data = NFL, family = binomial)

hawksbill <- read.csv(file.path("CourseData", "hawksbill.csv"))
hawksbill$propFemale <- hawksbill$Nfemale / (hawksbill$Nmale + hawksbill$Nfemale)
modA <- glm(cbind(Nfemale, Nmale) ~ Temperature, data = hawksbill,
            family = binomial)
# The tempting but wrong version: proportions with no sample sizes.
# (R warns about non-integer successes; suppressed here on purpose.)
modA_prop <- suppressWarnings(
  glm(propFemale ~ Temperature, data = hawksbill, family = binomial)
)

save_output("anova(nflMod, test = \"Chi\")", "nfl_anova")
save_output("summary(nflMod)", "nfl_summary")
save_output("confint.default(nflMod)", "nfl_confint")
save_output("exp(coef(nflMod))", "nfl_odds_ratio")
save_output("-coef(nflMod)[1] / coef(nflMod)[2]", "nfl_p50")
save_output("summary(modA)", "turtle_summary")
save_output("-coef(modA)[1] / coef(modA)[2]", "turtle_pivotal")
save_output("coef(summary(modA_prop))", "turtle_prop_noweights")
save_output("deviance(modA) / df.residual(modA)", "turtle_dispersion")
save_output("predict_ci(nflMod, data.frame(Distance = c(20, 30, 50)))",
            "nfl_predictions")

plots <- list()


# Figure: mean-variance relationships of three families (analytical) --------

mv <- rbind(
  data.frame(family = "Gaussian", mu = seq(0, 10, length.out = 101),
             v = 2),
  data.frame(family = "Poisson", mu = seq(0, 10, length.out = 101),
             v = seq(0, 10, length.out = 101)),
  data.frame(family = "Binomial", mu = seq(0, 1, length.out = 101),
             v = seq(0, 1, length.out = 101) * (1 - seq(0, 1, length.out = 101)))
)
mv$family <- factor(mv$family, levels = c("Gaussian", "Poisson",
                                          "Binomial"))
mv_labels <- data.frame(
  family = factor(levels(mv$family), levels = levels(mv$family)),
  x = c(5, 3, 0.5), y = c(2.35, 8.2, 0.285),
  label = c("constant", "variance = \u03bc", "\u03bc(1 \u2212 \u03bc)")
)
plots$mean_variance <- ggplot(mv, aes(mu, v)) +
  geom_line(colour = bb852$slate, linewidth = 1.2) +
  geom_text(data = mv_labels, aes(x, y, label = label), colour = bb852$coral,
            size = pt(16), family = bb852_font) +
  facet_wrap(~family, scales = "free") +
  expand_limits(y = 0) +
  labs(x = "Predicted mean (\u03bc)", y = "Variance") +
  theme_bb852() +
  theme(axis.text = element_blank(), panel.spacing = unit(14, "pt"))
save_all(plots$mean_variance, "mean_variance", "takeaway")


# Figure: same proportion, different certainty (exact binomial CIs) ---------

surv <- data.frame(group = c("1 of 2", "5 of 10", "50 of 100"),
                   x = c(1, 5, 50), n = c(2, 10, 100))
ci <- t(mapply(function(x, n) binom.test(x, n)$conf.int, surv$x, surv$n))
surv$lwr <- ci[, 1]
surv$upr <- ci[, 2]
surv$group <- factor(surv$group, levels = surv$group)
plots$binomial_ci <- ggplot(surv, aes(group, x / n)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.12, linewidth = 1.1,
                colour = bb852$slate) +
  geom_point(size = 3.5, colour = bb852$coral) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Fish surviving", y = "Proportion surviving") +
  theme_bb852()
save_all(plots$binomial_ci, "binomial_ci", "takeaway")


# Figure: the logit link (analytical) --------------------------------------

logit_dat <- data.frame(x = seq(0, 10, length.out = 200))
logit_dat$eta <- -6 + 1.2 * logit_dat$x
logit_dat$p <- plogis(logit_dat$eta)
plots$logit_link <-
  (ggplot(with_lab(logit_dat, "Natural scale: probability"), aes(x, p)) +
     geom_line(colour = bb852$coral, linewidth = 1.2) +
     scale_y_continuous(breaks = seq(0, 1, 0.25)) +
     strip_lab() +
     labs(x = "x",
          y = "Probability (\u03bc)") +
     theme_bb852()) +
  (ggplot(with_lab(logit_dat, "Link scale: log-odds"), aes(x, eta)) +
     geom_line(colour = bb852$slate, linewidth = 1.2) +
     strip_lab() +
     labs(x = "x",
          y = "logit(\u03bc) = \u03b7") +
     theme_bb852())
save_all(plots$logit_link, "logit_link", "full")


# Figures: NFL field goals (binary data) -----------------------------------

set.seed(12)
nfl_base <- ggplot(NFL, aes(Distance, Success)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.4, colour = bb852$light,
              size = 2) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = "Distance (m)", y = "Success (1) or miss (0)") +
  theme_bb852()
plots$nfl_data <- nfl_base
save_all(plots$nfl_data, "nfl_data", "half")

nfl_pred <- predict_ci(nflMod, data.frame(Distance = seq(14, 52, 0.25)))
p50 <- -coef(nflMod)[1] / coef(nflMod)[2]
set.seed(12)
plots$nfl_fit <- ggplot(NFL, aes(Distance, Success)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.4, colour = bb852$light,
              size = 2) +
  geom_ribbon(data = nfl_pred, aes(x = Distance, ymin = lwr, ymax = upr),
              inherit.aes = FALSE, fill = bb852$coral, alpha = 0.2) +
  geom_line(data = nfl_pred, aes(Distance, fit), colour = bb852$coral,
            linewidth = 1.2) +
  annotate("segment", x = p50, xend = p50, y = -0.08, yend = 0.5,
           colour = bb852$teal, linewidth = 0.6, linetype = "22") +
  annotate("segment", x = 12, xend = p50, y = 0.5, yend = 0.5,
           colour = bb852$teal, linewidth = 0.6, linetype = "22") +
  annotate("label", x = p50 - 0.8, y = 0.38, hjust = 1,
           label = sprintf("p = 0.5 at %.1f m", p50), colour = bb852$teal,
           fill = bb852$bg, label.size = 0, size = pt(16),
           family = bb852_font) +
  coord_cartesian(xlim = c(13, 53), ylim = c(-0.06, 1.06)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = "Distance (m)", y = "Probability of success") +
  theme_bb852()
save_all(plots$nfl_fit, "nfl_fit", "takeaway")


# Figure: hawksbill turtles, cbind() vs proportions without sample size -----

turt_grid <- data.frame(Temperature = seq(27.8, 30.5, 0.02))
pred_cbind <- predict_ci(modA, turt_grid)
pred_prop <- predict_ci(modA_prop, turt_grid)

turtle_panel <- function(pred, colour, title) {
  base <- if (is.null(title)) hawksbill else with_lab(hawksbill, title)
  ggplot(base, aes(Temperature, propFemale)) +
    geom_ribbon(data = pred, aes(x = Temperature, ymin = lwr, ymax = upr),
                inherit.aes = FALSE, fill = colour, alpha = 0.2) +
    geom_line(data = pred, aes(Temperature, fit), colour = colour,
              linewidth = 1.2) +
    geom_point(aes(size = Total), colour = bb852$slate, alpha = 0.85) +
    scale_size_area(max_size = 8) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    (if (is.null(title)) NULL else strip_lab()) +
    labs(x = "Incubation temperature (\u00b0C)",
         y = "Proportion female") +
    theme_bb852()
}
plots$turtle_cbind_vs_prop <-
  turtle_panel(pred_cbind, bb852$coral, "cbind(Nfemale, Nmale)") +
  turtle_panel(pred_prop, bb852$grey, "propFemale, no sample sizes")
save_all(plots$turtle_cbind_vs_prop, "turtle_cbind_vs_prop", "full")

plots$turtle_fit <- turtle_panel(pred_cbind, bb852$coral, NULL) +
  geom_vline(xintercept = -coef(modA)[1] / coef(modA)[2], colour = bb852$teal,
             linetype = "22", linewidth = 0.6)
save_all(plots$turtle_fit, "turtle_fit", "half")


message("GLM part 2 figures written to ", output_root, " (font: ", bb852_font,
        ")")
