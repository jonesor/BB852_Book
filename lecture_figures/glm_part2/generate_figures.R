# BB852 lecture figures: Generalised linear models (part 2)
#
# Course book chapters:
# https://jonesor.github.io/BB852_Book/extending-use-cases-of-glm.html
# https://jonesor.github.io/BB852_Book/glm-families-and-use-cases.html
#
# Data (empirical, from CourseData/), analysed exactly as in the course book:
#   NFLfieldgoal.csv      binary binomial GLM; yards -> metres (n = 948)
#   hawksbill.csv         aggregated binomial GLM with cbind() (7 clutches)
# Simulated residuals: DHARMa::simulateResiduals(nflMod, n = 250, seed = 123)
#   for the autoplot() vs DHARMa Q-Q comparison (needs the DHARMa package).
# Binned (nfl_logit only): observed log-odds of success for 5 m distance bins.
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
library(DHARMa)  # install.packages("DHARMa") if missing


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

nfl_base <- ggplot(NFL, aes(Distance, Success)) +
  geom_point(
    position = position_jitter(width = 0, height = 0.05, seed = 12),
    alpha = 0.4,
    colour = bb852$light,
    size = 2
  ) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = "Distance (m)", y = "Success (1) or miss (0)") +
  theme_bb852()
plots$nfl_data <- nfl_base
save_all(plots$nfl_data, "nfl_data", "half")

nfl_pred <- predict_ci(nflMod, data.frame(Distance = seq(14, 52, 0.25)))
p50 <- -coef(nflMod)[1] / coef(nflMod)[2]
plots$nfl_fit <- ggplot(NFL, aes(Distance, Success)) +
  geom_point(
    position = position_jitter(width = 0, height = 0.05, seed = 12),
    alpha = 0.4,
    colour = bb852$light,
    size = 2
  ) +
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
           fill = bb852$bg, linewidth = 0, size = pt(16),
           family = bb852_font) +
  coord_cartesian(xlim = c(13, 53), ylim = c(-0.06, 1.06)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = "Distance (m)", y = "Probability of success") +
  theme_bb852()
save_all(plots$nfl_fit, "nfl_fit", "takeaway")

# Figure: the same NFL model on the scale of the linear predictor (log-odds).
# Shown just before nfl_fit so the slope and intercept can be read off a
# straight line first. Binary data cannot be drawn on this scale (the logit of
# 0 or 1 is infinite), so the data are shown as observed log-odds for 5 m
# distance bins (empirical logit, log((successes + 0.5) / (misses + 0.5)));
# point area shows the number of kicks. Bins with fewer than 10 kicks dropped.
nfl_bins <- cut(NFL$Distance, breaks = seq(15, 50, by = 5))
nfl_binned <- do.call(rbind, lapply(split(NFL, nfl_bins), function(d) {
  data.frame(Distance = mean(d$Distance), n = nrow(d),
             logit = log((sum(d$Success) + 0.5) /
                           (sum(1 - d$Success) + 0.5)))
}))
nfl_binned <- nfl_binned[nfl_binned$n >= 10, ]

nfl_link <- data.frame(Distance = seq(0, 52, 0.25))
nfl_link_pv <- predict(nflMod, newdata = nfl_link, se.fit = TRUE)
nfl_link$fit <- nfl_link_pv$fit
nfl_link$lwr <- nfl_link_pv$fit - 1.96 * nfl_link_pv$se.fit
nfl_link$upr <- nfl_link_pv$fit + 1.96 * nfl_link_pv$se.fit
nfl_in_data <- nfl_link[nfl_link$Distance >= min(NFL$Distance), ]
nfl_extrap <- nfl_link[nfl_link$Distance <= min(NFL$Distance), ]

b0 <- coef(nflMod)[[1]]
b1 <- coef(nflMod)[[2]]
step_x <- c(17, 33)  # two 10 m steps: the same drop in log-odds everywhere
nfl_steps <- data.frame(x = step_x, xend = step_x + 10,
                        y = b0 + b1 * step_x, yend = b0 + b1 * (step_x + 10))

plots$nfl_logit <- ggplot() +
  geom_hline(yintercept = 0, colour = bb852$teal, linewidth = 0.6,
             linetype = "22") +
  geom_ribbon(data = nfl_in_data, aes(Distance, ymin = lwr, ymax = upr),
              fill = bb852$coral, alpha = 0.2) +
  geom_line(data = nfl_extrap, aes(Distance, fit), colour = bb852$coral,
            linewidth = 1, linetype = "22") +
  geom_line(data = nfl_in_data, aes(Distance, fit), colour = bb852$coral,
            linewidth = 1.2) +
  geom_point(data = nfl_binned, aes(Distance, logit, size = n),
             colour = bb852$slate, alpha = 0.7) +
  scale_size_area(max_size = 6) +
  # Slope: run 10 m, drop 1.31 in log-odds, at 17 m and again at 33 m
  geom_segment(data = nfl_steps, aes(x = x, xend = xend, y = y, yend = y),
               colour = bb852$slate, linewidth = 0.6) +
  geom_segment(data = nfl_steps, aes(x = xend, xend = xend, y = y, yend = yend),
               colour = bb852$slate, linewidth = 0.6) +
  annotate("text", x = step_x + 11, y = (nfl_steps$y + nfl_steps$yend) / 2,
           label = sub("-", "\u2212", sprintf("%.2f", 10 * b1)), hjust = 0, colour = bb852$slate,
           size = pt(14), family = bb852_font) +
  # Intercept: log-odds at 0 m, outside the data
  annotate("point", x = 0, y = b0, colour = bb852$coral, size = 3) +
  annotate("text", x = 1.5, y = b0 + 0.05, hjust = 0, vjust = 0,
           label = sprintf("intercept %.2f (0 m: not a real kick)", b0),
           colour = bb852$coral, size = pt(14), family = bb852_font) +
  # 50:50 point, as on the next slide
  annotate("segment", x = p50, xend = p50, y = -1.6, yend = 0,
           colour = bb852$teal, linewidth = 0.6, linetype = "22") +
  annotate("text", x = p50 - 0.8, y = -0.95, hjust = 1,
           label = sprintf("50:50 at %.1f m", p50), colour = bb852$teal,
           size = pt(14), family = bb852_font) +
  coord_cartesian(xlim = c(-1.5, 53), ylim = c(-1.5, 6.3), expand = FALSE) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  scale_y_continuous(breaks = seq(-1, 6, 1)) +
  labs(x = "Distance (m)", y = "Log-odds of success") +
  theme_bb852()
save_all(plots$nfl_logit, "nfl_logit", "takeaway")
save_output("nfl_binned", "nfl_logit_bins")


# Figure: autoplot() vs DHARMa Q-Q plots for the binary NFL model ------------
# Left: the Q-Q panel autoplot(nflMod) draws (standardised deviance residuals
# against normal quantiles). Right: the Q-Q panel plot(simulateResiduals())
# draws (DHARMa scaled residuals against uniform quantiles). Redrawn in the
# BB852 style so the two can sit side by side on one slide.

dev_res <- sort(rstandard(nflMod, type = "deviance"))
qq_dev <- data.frame(
  lab = "autoplot(): deviance residuals",
  expected = qnorm(ppoints(length(dev_res))),
  observed = dev_res
)

nfl_sim <- simulateResiduals(nflMod, n = 250, seed = 123)
sim_res <- sort(nfl_sim$scaledResiduals)
qq_sim <- data.frame(
  lab = "DHARMa: simulated residuals",
  expected = ppoints(length(sim_res)),
  observed = sim_res
)

save_output("testUniformity(nfl_sim, plot = FALSE)", "nfl_dharma_ks")
save_output("testDispersion(nfl_sim, plot = FALSE)", "nfl_dharma_dispersion")
save_output("testOutliers(nfl_sim, plot = FALSE)", "nfl_dharma_outliers")

qq_panel <- function(d, colour) {
  ggplot(d, aes(expected, observed)) +
    geom_abline(intercept = 0, slope = 1, colour = bb852$grey,
                linewidth = 0.6, linetype = "22") +
    geom_point(colour = colour, alpha = 0.4, size = 2) +
    strip_lab() +
    labs(x = "Expected quantile", y = "Observed residual") +
    theme_bb852()
}
plots$nfl_qq_compare <- qq_panel(qq_dev, bb852$grey) +
  qq_panel(qq_sim, bb852$slate)
save_all(plots$nfl_qq_compare, "nfl_qq_compare", "full")


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
