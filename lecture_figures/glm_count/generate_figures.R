# BB852 GLMs with count data (Poisson GLMs and overdispersion): figures
# ---------------------------------------------------------------------
# Every statistical figure in the lecture, in the BB852 theme shared with the
# other lecture_figures generators (see lecture_figures/ancova).
#
# Course book chapter:
# https://jonesor.github.io/BB852_Book/generalised-linear-models.html
#
# Data:
#   CourseData/fox.csv               number of pups vs mother's weight, n = 100
#   CourseData/elephantPoaching.csv  elephants killed vs scout patrol density,
#                                    n = 20 protected areas
#   CourseData/roundabouts.csv       beetle species on 36 vegetation "islands",
#                                    complex vs simple habitat (simulated in
#                                    scripts/SimulateData.R)
# Analytical (no data): Poisson probability distributions (slide 11).
#
# Run from the repository root:
# source("lecture_figures/glm_count/generate_figures.R")
#
# PNG files are written to lecture_figures/glm_count/output/<layout>/. Every
# content figure is written at full and half width; figures placed on a
# "Title, Bullets & Photo" slide are also written at takeaway size. The R
# output shown on the slides is written to output/text/.
# Object and file names use the slide numbers of the rebuilt deck
# (BB852_16_GLMs_count_data), e.g. plots$slide_19_log_scale.
#
# Labels use \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)
library(patchwork)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/ancova.

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

# Panel titles are used only inside multi-panel figures.
panel_title <- function(size = 16) {
  theme(plot.title = element_text(size = size, colour = bb852$text,
                                  family = bb852_font, face = "bold",
                                  hjust = 0, margin = margin(b = 4)))
}

# Sizes (inches) match the figure placeholders in the BB852 slide theme.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "glm_count", "output")
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

plots <- list()
half_plots <- list()


# Data and models ---------------------------------------------------------------

fox <- read.csv(file.path("CourseData", "fox.csv"))
mod1 <- glm(noffspring ~ weight, data = fox, family = poisson)
mod_lm <- lm(noffspring ~ weight, data = fox)

poaching <- read.csv(file.path("CourseData", "elephantPoaching.csv"))
mod_pois <- glm(nkilled ~ scoutPerKm, data = poaching, family = poisson)
mod_quasi <- glm(nkilled ~ scoutPerKm, data = poaching, family = quasipoisson)
mod_pois_funding <- glm(nkilled ~ scoutPerKm + funding, data = poaching,
                        family = poisson)

rb <- read.csv(file.path("CourseData", "roundabouts.csv"))
rb$habitatType <- factor(rb$habitatType, levels = c("complex", "simple"))
mod_rb <- glm(nSpecies ~ habitatType, data = rb, family = poisson)

weight_lab <- "Mother's weight (kg)"
pups_lab <- "Number of pups"

cf <- coef(mod1)
se <- sqrt(diag(vcov(mod1)))
lm_zero <- -coef(mod_lm)[[1]] / coef(mod_lm)[[2]]   # lm prediction = 0 here

# Numbers quoted on the slides, checked here so the deck cannot drift.
stopifnot(
  abs(cf[["(Intercept)"]] - (-0.75)) < 0.005,
  abs(cf[["weight"]] - 0.63) < 0.005,
  abs(exp(cf[["weight"]]) - 1.88) < 0.005,
  abs(lm_zero - 1.46) < 0.01,
  min(fox$weight) > lm_zero
)


# R output shown on slides ------------------------------------------------------

save_output("anova(mod1, test = \"Chi\")", "slide_16_anova_fox")
save_output("summary(mod1)", "slide_17_summary_fox")
save_output("exp(coef(mod1))", "slide_21_exp_coef")
save_output("exp(confint(mod1))", "slide_21_exp_ci")
save_output("predict(mod1, data.frame(weight = c(2, 3, 4)), type = \"response\")",
            "slide_20_predictions_by_hand")
save_output("summary(mod_rb)", "slide_24_summary_groups")
save_output("exp(coef(mod_rb))", "slide_24_exp_coef_groups")
save_output("deviance(mod_rb) / df.residual(mod_rb)", "slide_24_dispersion_groups")
save_output("deviance(mod1) / df.residual(mod1)", "slide_26_dispersion_fox")
save_output("summary(mod_pois)", "slide_27_summary_poisson")
save_output("summary(mod_quasi)", "slide_27_summary_quasi")
save_output("anova(mod_quasi, test = \"F\")", "slide_27_anova_quasi")
save_output("deviance(mod_pois) / df.residual(mod_pois)", "slide_27_dispersion_poisson")
save_output("deviance(mod_pois_funding) / df.residual(mod_pois_funding)",
            "slide_27_dispersion_with_funding")


# Prediction grid on the link and response scales ------------------------------
# Book method: predict on the link scale, add +/- 1.96 SE there, then apply
# the inverse link. The interval is therefore asymmetric on the count scale.

newData <- data.frame(weight = seq(1.7, 4.4, 0.01))
predVals <- predict(mod1, newData, se.fit = TRUE)
inverseFunction <- family(mod1)$linkinv
newData$fit_LP <- predVals$fit
newData$lowerCI_LP <- predVals$fit - 1.96 * predVals$se.fit
newData$upperCI_LP <- predVals$fit + 1.96 * predVals$se.fit
newData$fit <- inverseFunction(newData$fit_LP)
newData$lowerCI <- inverseFunction(newData$lowerCI_LP)
newData$upperCI <- inverseFunction(newData$upperCI_LP)


# Slide 1: title-slide chart -----------------------------------------------------
# Fox data with the Poisson fit, on the cover colour.

plots$slide_01_cover <- ggplot(fox, aes(weight, noffspring)) +
  geom_ribbon(data = newData, aes(x = weight, ymin = lowerCI, ymax = upperCI),
              inherit.aes = FALSE, fill = bb852$light, alpha = 0.25) +
  geom_point(colour = bb852$light, size = 2.6, alpha = 0.8) +
  geom_line(data = newData, aes(weight, fit), colour = bb852$coral,
            linewidth = 1.4) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  labs(x = weight_lab, y = pups_lab) +
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


# Slide 5: the fox data -----------------------------------------------------------
# Plain scatter, as students first plot it in the book.

plots$slide_05_fox_data <- ggplot(fox, aes(weight, noffspring)) +
  geom_point(colour = bb852$slate, size = 2.6, alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  labs(x = weight_lab, y = pups_lab) +
  theme_bb852()


# Slide 6: why a straight line struggles ----------------------------------------
# Left: lm line extended to lighter mothers crosses zero at about 1.46 kg.
# Right: spread of counts within weight quartiles grows with the mean
# (box plot with points, a plot type students know).

lm_line <- data.frame(weight = c(1, 4.6))
lm_line$fit <- predict(mod_lm, lm_line)

lm_panel <- ggplot(fox, aes(weight, noffspring)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = bb852$panel) +
  geom_hline(yintercept = 0, colour = bb852$grey, linewidth = 0.5) +
  geom_point(colour = bb852$slate, size = 2.2, alpha = 0.55) +
  geom_line(data = lm_line, aes(weight, fit), colour = bb852$coral,
            linewidth = 1.3) +
  annotate("text", x = 1.05, y = -2.8, hjust = 0, size = pt(14),
           family = bb852_font, fontface = "bold", colour = bb852$coral,
           lineheight = 0.9,
           label = sprintf("lm() predicts < 0 pups\nbelow %.1f kg", lm_zero)) +
  scale_y_continuous(breaks = seq(0, 12, 4)) +
  coord_cartesian(xlim = c(1, 4.6), ylim = c(-4, 11.5)) +
  labs(x = weight_lab, y = pups_lab, title = "A straight line") +
  theme_bb852(16) + panel_title()

fox$quartile <- cut(fox$weight, quantile(fox$weight, 0:4 / 4),
                    include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))
q_var <- aggregate(noffspring ~ quartile, data = fox, FUN = var)
q_max <- aggregate(noffspring ~ quartile, data = fox, FUN = max)
q_var$y <- q_max$noffspring + 1.2

spread_panel <- ggplot(fox, aes(quartile, noffspring)) +
  geom_boxplot(colour = bb852$slate, fill = bb852$panel, outlier.shape = NA,
               width = 0.55) +
  geom_point(colour = bb852$slate, alpha = 0.45, size = 1.8,
             position = position_jitter(width = 0.12, height = 0, seed = 852)) +
  geom_text(data = q_var, aes(y = y, label = sprintf("var %.1f", noffspring)),
            size = pt(14), family = bb852_font, colour = bb852$coral,
            fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 12, 4), limits = c(0, 13)) +
  labs(x = "Weight quartile", y = pups_lab,
       title = "Spread grows with the mean") +
  theme_bb852(16) + panel_title() +
  theme(panel.grid.major.x = element_blank())

plots$slide_06_lm_problems <- lm_panel + spread_panel
half_plots$slide_06_lm_problems <- (lm_panel / spread_panel) &
  theme_bb852(14) & panel_title(14) & theme(plot.margin = margin(4, 10, 4, 6))


# Shared grid for the log-scale panel (slide 19) -------------------------------

link_x <- data.frame(weight = seq(0, 4.4, 0.01))
link_x$eta <- cf[["(Intercept)"]] + cf[["weight"]] * link_x$weight


# Slide 11: Poisson distributions (analytical) --------------------------------------
# Probability of each count for means 1, 3 and 6: bounded at 0, skewed when the
# mean is small, and wider as the mean grows (variance = mean).

pois_d <- expand.grid(count = 0:14, mean = c(1, 3, 6))
pois_d$p <- dpois(pois_d$count, pois_d$mean)
pois_d$label <- factor(sprintf("mean = variance = %d", pois_d$mean),
                       levels = sprintf("mean = variance = %d", c(1, 3, 6)))

plots$slide_11_poisson <- ggplot(pois_d, aes(count, p)) +
  geom_col(fill = bb852$slate, width = 0.75) +
  facet_wrap(~label, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 14, 2)) +
  labs(x = "Count", y = "Probability") +
  theme_bb852(16) +
  theme(panel.grid.major.x = element_blank())
half_plots$slide_11_poisson <- plots$slide_11_poisson +
  facet_wrap(~label, ncol = 1) + theme_bb852(14) +
  theme(panel.grid.major.x = element_blank())


# Slide 15: diagnostic plots -------------------------------------------------------
# Recreate the four panels students get from ggfortify::autoplot(mod1) without
# relying on ggfortify's deprecated ggplot2 methods.

diag_data <- data.frame(
  fitted = predict(mod1, type = "link"),
  deviance_resid = residuals(mod1, type = "deviance"),
  std_deviance_resid = rstandard(mod1, type = "deviance"),
  std_pearson_resid = rstandard(mod1, type = "pearson"),
  leverage = hatvalues(mod1)
)
diag_data$sqrt_abs_std_deviance <- sqrt(abs(diag_data$std_deviance_resid))

diag_theme <- theme_bb852(14) + panel_title(14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.margin = margin(4, 8, 4, 4)
  )
diag_points <- geom_point(colour = bb852$slate, alpha = 0.5, size = 1.8)
diag_smooth <- geom_smooth(
  method = "loess",
  formula = y ~ x,
  se = FALSE,
  colour = bb852$coral,
  linewidth = 1
)

diag_list <- list(
  ggplot(diag_data, aes(fitted, deviance_resid)) +
    geom_hline(yintercept = 0, colour = bb852$grey, linetype = "22",
               linewidth = 0.5) +
    diag_points + diag_smooth +
    labs(title = "Residuals vs fitted", x = "Predicted values", y = "Residuals") +
    diag_theme,
  ggplot(diag_data, aes(sample = std_deviance_resid)) +
    geom_qq_line(colour = bb852$grey, linetype = "22", linewidth = 0.5) +
    geom_qq(colour = bb852$slate, alpha = 0.5, size = 1.8) +
    labs(title = "Normal Q-Q", x = "Theoretical quantiles",
         y = "Std. deviance resid.") +
    diag_theme,
  ggplot(diag_data, aes(fitted, sqrt_abs_std_deviance)) +
    diag_points + diag_smooth +
    labs(title = "Scale-location", x = "Predicted values",
         y = expression(sqrt("|Std. deviance resid.|"))) +
    diag_theme,
  ggplot(diag_data, aes(leverage, std_pearson_resid)) +
    geom_hline(yintercept = 0, colour = bb852$grey, linetype = "22",
               linewidth = 0.5) +
    diag_points + diag_smooth +
    scale_x_continuous(breaks = c(0, 0.04, 0.08, 0.12)) +
    labs(title = "Residuals vs leverage", x = "Leverage",
         y = "Std. Pearson resid.") +
    diag_theme
)
plots$slide_15_diagnostics <- wrap_plots(diag_list, nrow = 1)
half_plots$slide_15_diagnostics <- wrap_plots(diag_list, ncol = 2)


# Slide 19: coefficients live on the log scale -------------------------------------
# Intercept marked at weight 0, far outside the data.

log_panel <- ggplot(link_x, aes(weight, eta)) +
  annotate("rect", xmin = min(fox$weight), xmax = max(fox$weight),
           ymin = -Inf, ymax = Inf, fill = bb852$panel) +
  geom_line(colour = bb852$slate, linewidth = 1.4) +
  geom_point(data = data.frame(weight = 0, eta = cf[["(Intercept)"]]),
             colour = bb852$coral, size = 3.5) +
  annotate("text", x = 0.15, y = cf[["(Intercept)"]] - 0.35, hjust = 0,
           size = pt(15), family = bb852_font, fontface = "bold",
           colour = bb852$coral, label = "intercept \u22120.75") +
  annotate("segment", x = 2, xend = 3, y = cf[1] + cf[2] * 2,
           yend = cf[1] + cf[2] * 2, colour = bb852$text, linewidth = 0.8) +
  annotate("segment", x = 3, xend = 3, y = cf[1] + cf[2] * 2,
           yend = cf[1] + cf[2] * 3, colour = bb852$text, linewidth = 0.8) +
  annotate("text", x = 3.08, y = cf[1] + cf[2] * 2.5, hjust = 0,
           size = pt(15), family = bb852_font, fontface = "bold",
           colour = bb852$text, label = "+0.63") +
  annotate("text", x = mean(range(fox$weight)), y = 2.4, size = pt(14),
           family = bb852_font, colour = bb852$grey, label = "range of the data") +
  scale_y_continuous(limits = c(-1.3, 2.6)) +
  labs(x = weight_lab, y = "log(expected pups)",
       title = "Log scale (summary)") +
  theme_bb852(16) + panel_title()

count_panel <- ggplot(newData, aes(weight, fit)) +
  geom_point(data = fox, aes(weight, noffspring), colour = bb852$light,
             size = 1.8, alpha = 0.6) +
  geom_line(colour = bb852$coral, linewidth = 1.4) +
  annotate("text", x = 1.75, y = 10.8, hjust = 0, size = pt(15),
           family = bb852_font, fontface = "bold", colour = bb852$coral,
           label = "each kg: \u00d7 1.88") +
  scale_y_continuous(limits = c(0, 11.5), breaks = seq(0, 12, 4)) +
  scale_x_continuous(limits = c(1.65, 4.45)) +
  labs(x = weight_lab, y = "Number of pups",
       title = "Count scale") +
  theme_bb852(16) + panel_title()

plots$slide_19_log_scale <- log_panel + count_panel
half_plots$slide_19_log_scale <- (log_panel / count_panel) &
  theme_bb852(14) & panel_title(14) & theme(plot.margin = margin(4, 10, 4, 6))


# Slide 22: predictions with a 95% confidence band ---------------------------------

plots$slide_22_predictions <- ggplot(fox, aes(weight, noffspring)) +
  geom_ribbon(data = newData, aes(x = weight, ymin = lowerCI, ymax = upperCI),
              inherit.aes = FALSE, fill = bb852$coral, alpha = 0.2) +
  geom_line(data = newData, aes(weight, fit), colour = bb852$coral,
            linewidth = 1.4) +
  geom_point(colour = bb852$slate, size = 2.4, alpha = 0.55) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  labs(x = weight_lab, y = pups_lab) +
  theme_bb852()


# Slide 27: overdispersion in the elephant poaching data ---------------------------
# Same fitted line under poisson and quasipoisson; only the uncertainty differs.

grid_p <- data.frame(scoutPerKm = seq(min(poaching$scoutPerKm),
                                      max(poaching$scoutPerKm), length.out = 200))
band <- function(model, label) {
  pr <- predict(model, grid_p, se.fit = TRUE)
  inv <- family(model)$linkinv
  data.frame(grid_p, fit = inv(pr$fit),
             lwr = inv(pr$fit - 1.96 * pr$se.fit),
             upr = inv(pr$fit + 1.96 * pr$se.fit), model = label)
}
bands <- rbind(band(mod_pois, "poisson"), band(mod_quasi, "quasipoisson"))
bands$model <- factor(bands$model, levels = c("poisson", "quasipoisson"))
stopifnot(isTRUE(all.equal(coef(mod_pois), coef(mod_quasi))))

disp_index <- deviance(mod_pois) / df.residual(mod_pois)
# Effect per 10 extra patrols per 1000 km (= 0.01 per km), as a multiplier with
# a Wald 95% CI on the log scale, under each family.
per10 <- function(model) {
  est <- coef(model)[["scoutPerKm"]]
  se_b <- sqrt(vcov(model)["scoutPerKm", "scoutPerKm"])
  exp(0.01 * (est + c(0, -1.96, 1.96) * se_b))
}
eff_pois <- per10(mod_pois)
eff_quasi <- per10(mod_quasi)
band_labels <- data.frame(
  model = factor(c("poisson", "quasipoisson"),
                 levels = c("poisson", "quasipoisson")),
  text = c(sprintf("\u00d7%.2f (95%% CI %.2f to %.2f)", eff_pois[1], eff_pois[2], eff_pois[3]),
           sprintf("\u00d7%.2f (95%% CI %.2f to %.2f)", eff_quasi[1], eff_quasi[2], eff_quasi[3]))
)
stopifnot(
  abs(eff_pois[1] - 0.68) < 0.005,
  abs(eff_pois[2] - 0.57) < 0.005, abs(eff_pois[3] - 0.81) < 0.005,
  abs(eff_quasi[2] - 0.47) < 0.005, abs(eff_quasi[3] - 0.98) < 0.005
)
stopifnot(abs(disp_index - 4.28) < 0.01)

plots$slide_27_overdispersion <- ggplot(poaching, aes(scoutPerKm * 1000, nkilled)) +
  geom_ribbon(data = bands, aes(x = scoutPerKm * 1000, ymin = lwr, ymax = upr),
              inherit.aes = FALSE, fill = bb852$coral, alpha = 0.2) +
  geom_line(data = bands, aes(scoutPerKm * 1000, fit), colour = bb852$coral,
            linewidth = 1.3) +
  geom_point(colour = bb852$slate, size = 2.4, alpha = 0.7) +
  geom_text(data = band_labels, aes(x = 29, y = 33, label = text),
            inherit.aes = FALSE, hjust = 1, size = pt(14),
            family = bb852_font, fontface = "bold", colour = bb852$text) +
  facet_wrap(~model, nrow = 1) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(x = "Scout patrols per 1000 km", y = "Elephants killed") +
  theme_bb852(16)
half_plots$slide_27_overdispersion <- plots$slide_27_overdispersion +
  facet_wrap(~model, ncol = 1) + theme_bb852(14)


# Slide 24: a factor explanatory variable (t-test-like Poisson GLM) ----------------
# Box plot with points (taught plot type), plus model means +/- 95% CI from
# predict() on the link scale, back-transformed.

rb_new <- data.frame(habitatType = factor(c("complex", "simple"),
                                         levels = c("complex", "simple")))
rb_pred <- predict(mod_rb, rb_new, se.fit = TRUE)
rb_new$fit <- exp(rb_pred$fit)
rb_new$lwr <- exp(rb_pred$fit - 1.96 * rb_pred$se.fit)
rb_new$upr <- exp(rb_pred$fit + 1.96 * rb_pred$se.fit)
cf_rb <- coef(mod_rb)
stopifnot(
  abs(cf_rb[["(Intercept)"]] - 1.59) < 0.005,
  abs(cf_rb[["habitatTypesimple"]] - (-0.64)) < 0.005,
  abs(exp(cf_rb[["habitatTypesimple"]]) - 0.53) < 0.005,
  isTRUE(all.equal(rb_new$fit,
                   as.numeric(tapply(rb$nSpecies, rb$habitatType, mean))))
)

plots$slide_24_groups <- ggplot(rb, aes(habitatType, nSpecies)) +
  geom_boxplot(colour = bb852$grey, fill = bb852$panel, outlier.shape = NA,
               width = 0.45) +
  geom_point(colour = bb852$slate, alpha = 0.5, size = 2.4,
             position = position_jitter(width = 0.1, height = 0, seed = 852)) +
  geom_errorbar(data = rb_new, aes(x = as.numeric(habitatType) + 0.35,
                                   ymin = lwr, ymax = upr),
                inherit.aes = FALSE, width = 0.08, linewidth = 1.1,
                colour = bb852$coral) +
  geom_point(data = rb_new, aes(x = as.numeric(habitatType) + 0.35, y = fit),
             inherit.aes = FALSE, colour = bb852$coral, size = 3.5) +
  geom_text(data = rb_new, aes(x = as.numeric(habitatType) + 0.45, y = fit,
                               label = sprintf("%.1f", fit)),
            inherit.aes = FALSE, hjust = 0, size = pt(15), family = bb852_font,
            fontface = "bold", colour = bb852$coral) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
  labs(x = "Habitat type", y = "Number of beetle species") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Save every figure ------------------------------------------------------------------

takeaway_slides <- c("slide_06_lm_problems", "slide_19_log_scale")

for (name in names(plots)) {
  if (name == "slide_01_cover") {
    save_bb852(plots[[name]], name, layout = "cover", bg = bb852$cover)
    next
  }
  save_bb852(plots[[name]], name, layout = "full")
  half_plot <- if (name %in% names(half_plots)) half_plots[[name]] else plots[[name]]
  save_bb852(half_plot, name, layout = "half")
  if (name %in% takeaway_slides) {
    save_bb852(plots[[name]] & theme_bb852(14) & panel_title(14) &
                 theme(plot.margin = margin(6, 10, 6, 6)),
               name, layout = "takeaway")
  }
}
