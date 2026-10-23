# BB852 ANOVA lecture: R code for every statistical figure
# ---------------------------------------------------------
# Figures for the one-way ANOVA lecture, in the BB852 theme shared with the
# other lecture_figures generators (see lecture_figures/t-tests).
#
# Data source:
# https://jonesor.github.io/BB852_Book/anova-linear-models-with-a-single-categorical-explanatory-variable.html
# Espresso foam index (%) for three brewing methods, n = 9 per method:
# BM = bar machine, HIP = Hyper-Espresso method, IES = I-Espresso system.
# Parenti et al. (2014) Journal of Food Engineering 121: 112-117.
#
# Run from the repository root:
# source("lecture_figures/anova/generate_figures.R")
#
# PNG files are written to lecture_figures/anova/output/<layout>/.
# Each ggplot object is also retained in the named list `plots`.
# Object and file names use the slide numbers of the rebuilt deck
# (BB852_12_ANOVA_v2), e.g. plots$slide_09_ss_partition.
#
# Simulated data are used ONLY for slide 11 (signal versus noise) and slide 26
# (Q-Q plot examples); seeds are set and stated. Every other figure uses the
# espresso data. Statistics are computed here, never typed in.
#
# Labels use plotmath and \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/t-tests.

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

# Avenir Next on the Mac; Helvetica otherwise. BB852_FONT overrides both
# (useful on machines without either font).
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

output_root <- file.path("lecture_figures", "anova", "output")
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

# One colour per brewing method (three data colours, as the rules allow).
method_cols <- c(BM = bb852$slate, HIP = bb852$coral, IES = bb852$teal)


# Data and model --------------------------------------------------------------

espresso <- read.csv(file.path("CourseData", "espresso.csv"))
espresso$method <- factor(espresso$method, levels = c("BM", "HIP", "IES"))

foam_mod <- lm(foamIndx ~ method, data = espresso)
foam_anova <- anova(foam_mod)
group_means <- tapply(espresso$foamIndx, espresso$method, mean)
grand_mean <- mean(espresso$foamIndx)

ss_total <- sum((espresso$foamIndx - grand_mean)^2)
ss_error <- sum(residuals(foam_mod)^2)
ss_treat <- ss_total - ss_error
stopifnot(isTRUE(all.equal(ss_treat, foam_anova$`Sum Sq`[1])))

f_obs <- foam_anova$`F value`[1]
df_treat <- foam_anova$Df[1]
df_error <- foam_anova$Df[2]

# Deterministic horizontal offsets (no random jitter), centred per group.
espresso$offset <- ave(seq_len(nrow(espresso)), espresso$method,
                       FUN = function(i) ((seq_along(i) - 1) %% 5 - 2) * 0.07)
espresso$x <- as.numeric(espresso$method) + espresso$offset

# Observation order for the sums-of-squares "index" plots.
espresso <- espresso[order(espresso$method), ]
espresso$index <- seq_len(nrow(espresso))
espresso$group_mean <- unname(group_means[as.character(espresso$method)])

plots <- list()
half_plots <- list()
half_facet_theme <- theme(
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 14)
)


# Slide 1: title-slide chart --------------------------------------------------

plots$slide_01_cover <- ggplot(espresso, aes(x, foamIndx)) +
  annotate("segment", x = 1:3 - 0.28, xend = 1:3 + 0.28,
           y = group_means, yend = group_means,
           colour = bb852$coral, linewidth = 1.2) +
  geom_point(colour = bb852$light, size = 2.6, alpha = 0.9) +
  scale_x_continuous(breaks = 1:3, labels = levels(espresso$method),
                     limits = c(0.5, 3.5), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(15, 80), breaks = seq(20, 80, 20)) +
  labs(x = NULL, y = "Foam index (%)") +
  theme_bb852() +
  theme(
    plot.background = element_rect(fill = bb852$cover, colour = NA),
    panel.background = element_rect(fill = bb852$cover, colour = NA),
    panel.grid.major = element_blank(),
    axis.line.y = element_line(colour = bb852$light, linewidth = 0.4),
    axis.title = element_text(colour = bb852$light),
    axis.text.y = element_text(colour = bb852$light),
    axis.text.x = element_text(colour = bb852$bg),
    plot.margin = margin(140, 24, 110, 12)
  )


# Slide 8: the espresso data --------------------------------------------------

plots$slide_08_espresso_data <- ggplot(espresso, aes(x, foamIndx)) +
  annotate("segment", x = 1:3 - 0.3, xend = 1:3 + 0.3,
           y = group_means, yend = group_means,
           colour = bb852$coral, linewidth = 1.2) +
  geom_point(colour = bb852$slate, size = 2.8, alpha = 0.75) +
  scale_x_continuous(breaks = 1:3, labels = levels(espresso$method),
                     limits = c(0.5, 3.5), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(15, 80), breaks = seq(20, 80, 20)) +
  labs(x = "Brewing method", y = "Foam index (%)") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Slide 9: partitioning the sums of squares ------------------------------------
# x = observation number (sorted by method). Segments show the deviations that
# are squared and summed in each panel.

ss_label <- function(name, ss) sprintf("%s\nSS = %.1f", name, ss)
panel_names <- c(ss_label("Total", ss_total),
                 ss_label("Within groups (error)", ss_error),
                 ss_label("Between groups (treatment)", ss_treat))

ss_long <- rbind(
  transform(espresso, panel = panel_names[1], from = grand_mean, to = foamIndx),
  transform(espresso, panel = panel_names[2], from = group_mean, to = foamIndx),
  transform(espresso, panel = panel_names[3], from = grand_mean, to = group_mean)
)
ss_long$panel <- factor(ss_long$panel, levels = panel_names)

# Group-mean lines drawn in the within- and between-group panels.
mean_lines <- do.call(rbind, lapply(levels(espresso$method), function(m) {
  idx <- espresso$index[espresso$method == m]
  data.frame(method = m, x = min(idx) - 0.4, xend = max(idx) + 0.4,
             y = group_means[[m]], panel = panel_names[2:3])
}))
mean_lines$panel <- factor(mean_lines$panel, levels = panel_names)
grand_line <- data.frame(panel = factor(panel_names[c(1, 3)], levels = panel_names),
                         y = grand_mean)

# In the between-groups panel the points sit at the group means.
ss_points <- ss_long
ss_points$y <- ifelse(ss_points$panel == panel_names[3], ss_points$group_mean,
                      ss_points$foamIndx)

plots$slide_09_ss_partition <- ggplot(ss_long) +
  geom_hline(data = grand_line, aes(yintercept = y),
             colour = bb852$grey, linewidth = 0.6, linetype = "dashed") +
  geom_segment(aes(x = index, xend = index, y = from, yend = to,
                   colour = method), linewidth = 0.6, alpha = 0.7) +
  geom_segment(data = mean_lines, aes(x = x, xend = xend, y = y, yend = y,
                                      colour = method), linewidth = 1.2) +
  geom_point(data = ss_points, aes(index, y, colour = method), size = 2) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(values = method_cols) +
  scale_x_continuous(breaks = c(5, 14, 23), labels = levels(espresso$method),
                     expand = expansion(add = 1)) +
  scale_y_continuous(limits = c(15, 80), breaks = seq(20, 80, 20)) +
  labs(x = NULL, y = "Foam index (%)") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing = grid::unit(0.35, "in"),
        strip.text = element_text(face = "plain", colour = bb852$text, size = 16))
ss_half_labels <- setNames(
  c(
    sprintf("Total\nSS = %.1f", ss_total),
    sprintf("Within groups\nSS = %.1f", ss_error),
    sprintf("Between groups\nSS = %.1f", ss_treat)
  ),
  panel_names
)
half_plots$slide_09_ss_partition <- plots$slide_09_ss_partition +
  facet_wrap(~panel, ncol = 2, labeller = as_labeller(ss_half_labels)) +
  half_facet_theme


# Slide 11: signal and noise ----------------------------------------------------
# SIMULATED data, n = 9 per group, seed 852. Panels A and B use the same true
# group means (signal) and the same standard-normal draws, scaled by SD 6 and
# SD 20 (noise); panel C has equal true means (SD 6). F is computed from each
# simulated data set.

set.seed(852)
z_shared <- rnorm(27)
z_null <- rnorm(27)
sim_group <- function(means, sd, z, scenario) {
  data.frame(scenario = scenario,
             method = rep(c("BM", "HIP", "IES"), each = 9),
             y = rep(means, each = 9) + z * sd)
}
sim <- rbind(
  sim_group(c(35, 60, 40), 6,  z_shared, "A"),
  sim_group(c(35, 60, 40), 20, z_shared, "B"),
  sim_group(c(45, 45, 45), 6,  z_null,   "C")
)
sim_F <- sapply(split(sim, sim$scenario), function(d)
  anova(lm(y ~ method, data = d))$`F value`[1])
scenario_labels <- c(
  A = sprintf("Means differ, little noise\nF = %.1f", sim_F[["A"]]),
  B = sprintf("Same true means, more noise\nF = %.1f", sim_F[["B"]]),
  C = sprintf("No difference in means\nF = %.1f", sim_F[["C"]])
)
sim$panel <- factor(scenario_labels[sim$scenario], levels = scenario_labels)
sim$x <- match(sim$method, c("BM", "HIP", "IES")) +
  ave(seq_len(nrow(sim)), sim$scenario, sim$method,
      FUN = function(i) ((seq_along(i) - 1) %% 5 - 2) * 0.07)
sim_means <- aggregate(y ~ panel + method, data = sim, FUN = mean)
sim_means$x <- match(sim_means$method, c("BM", "HIP", "IES"))

plots$slide_11_signal_noise <- ggplot(sim, aes(x, y)) +
  geom_segment(data = sim_means, aes(x = x - 0.3, xend = x + 0.3, y = y, yend = y),
               colour = bb852$coral, linewidth = 1.2) +
  geom_point(colour = bb852$slate, size = 2.2, alpha = 0.7) +
  facet_wrap(~panel, nrow = 1) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3"),
                     limits = c(0.5, 3.5), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(-20, 100), breaks = seq(0, 100, 25)) +
  labs(x = "Group", y = "Response") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing = grid::unit(0.35, "in"),
        strip.text = element_text(face = "plain", colour = bb852$text, size = 16))
signal_half_labels <- setNames(
  c(
    sprintf("Different means\nlittle noise\nF = %.1f", sim_F[["A"]]),
    sprintf("Same means\nmore noise\nF = %.1f", sim_F[["B"]]),
    sprintf("No difference\nin means\nF = %.1f", sim_F[["C"]])
  ),
  scenario_labels
)
half_plots$slide_11_signal_noise <- plots$slide_11_signal_noise +
  facet_wrap(~panel, ncol = 2, labeller = as_labeller(signal_half_labels)) +
  half_facet_theme


# Slide 12: the F-distribution under the null ----------------------------------
# F(2, 24): the coral tail is the top 5% (critical value); the observed F lies
# far beyond it, so its tail area is invisible at this scale.

f_crit <- qf(0.95, df_treat, df_error)
f_grid <- seq(0, 32, by = 0.01)
f_curve <- data.frame(F = f_grid, density = df(f_grid, df_treat, df_error))
plots$slide_12_f_distribution <- ggplot(f_curve, aes(F, density)) +
  geom_area(fill = bb852$light, colour = NA) +
  geom_area(data = subset(f_curve, F >= f_crit), fill = bb852$coral, colour = NA) +
  annotate("segment", x = f_obs, xend = f_obs, y = 0, yend = 0.55,
           colour = bb852$slate, linewidth = 1.2) +
  annotate("text", x = f_obs - 0.5, y = 0.62, hjust = 1, size = 5.5,
           family = bb852_font, colour = bb852$slate,
           label = sprintf("Espresso: F = %.1f", f_obs)) +
  annotate("text", x = f_crit + 0.8, y = 0.2, hjust = 0, size = 5.5,
           family = bb852_font, colour = bb852$coral,
           label = sprintf("Top 5%%: F > %.2f", f_crit)) +
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 30, 5),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, 1.02), expand = expansion(mult = 0)) +
  labs(x = expression(italic(F) * "-statistic under " * H[0] * "  (2 and 24 d.f.)"),
       y = NULL) +
  theme_bb852() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())


# Slide 17: treatment contrasts as arrows from the reference mean ---------------

coef_mod <- coef(foam_mod)
arrow_df <- data.frame(x = 2:3, y = unname(coef_mod[1]), yend = unname(coef_mod[1] + coef_mod[2:3]),
                       label = sprintf("%+.1f", coef_mod[2:3]))
plots$slide_17_coefficients <- ggplot(espresso, aes(x, foamIndx)) +
  geom_hline(yintercept = coef_mod[1], colour = bb852$grey,
             linewidth = 0.6, linetype = "dashed") +
  geom_point(colour = bb852$light, size = 2.6, alpha = 0.9) +
  annotate("segment", x = 1:3 - 0.3, xend = 1:3 + 0.3,
           y = group_means, yend = group_means, colour = bb852$slate, linewidth = 1.2) +
  geom_segment(data = arrow_df, aes(x = x + 0.36, xend = x + 0.36, y = y, yend = yend),
               inherit.aes = FALSE, colour = bb852$coral, linewidth = 1.2,
               arrow = grid::arrow(length = grid::unit(0.12, "in"), type = "closed")) +
  geom_text(data = arrow_df, aes(x = x + 0.42, y = (y + yend) / 2, label = label),
            inherit.aes = FALSE, hjust = 0, size = 6, family = bb852_font,
            colour = bb852$coral) +
  annotate("text", x = 1.36, y = coef_mod[1] - 6, hjust = 0, size = 6,
           family = bb852_font, colour = bb852$slate,
           label = sprintf("Intercept\n%.1f", coef_mod[1])) +
  scale_x_continuous(breaks = 1:3, labels = levels(espresso$method),
                     limits = c(0.5, 3.9), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(15, 80), breaks = seq(20, 80, 20)) +
  labs(x = "Brewing method", y = "Foam index (%)") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Slide 22: Tukey HSD differences with family-wise 95% CIs ----------------------
# TukeyHSD() uses the same studentised-range method as agricolae::HSD.test().

tukey <- TukeyHSD(aov(foamIndx ~ method, data = espresso))$method
tukey_df <- data.frame(pair = rownames(tukey), diff = tukey[, "diff"],
                       lwr = tukey[, "lwr"], upr = tukey[, "upr"],
                       p = tukey[, "p adj"])
tukey_df$pair <- sub("-", " − ", tukey_df$pair)
tukey_df$pair <- factor(tukey_df$pair, levels = rev(tukey_df$pair))
tukey_df$sig <- ifelse(tukey_df$lwr > 0 | tukey_df$upr < 0, "yes", "no")

plots$slide_22_tukey_ci <- ggplot(tukey_df, aes(diff, pair, colour = sig)) +
  geom_vline(xintercept = 0, colour = bb852$grey, linewidth = 0.6, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = lwr, xmax = upr),
    width = 0.15,
    linewidth = 1.1,
    orientation = "y"
  ) +
  geom_point(size = 3.4) +
  scale_colour_manual(values = c(yes = bb852$slate, no = bb852$grey)) +
  scale_x_continuous(limits = c(-35, 42), breaks = seq(-30, 40, 10)) +
  labs(x = "Difference in foam index (%)", y = NULL) +
  theme_bb852() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = bb852$text, size = 16))


# Slide 23: box plots with Tukey letters ------------------------------------------
# Box plot plus points: the plot type students already make in BB852
# (geom_boxplot + geom_jitter). Offsets are deterministic so the figure is
# reproducible; geom_jitter(width = 0.1) gives the same look in class.
# Letters: groups sharing a letter do not differ (Tukey HSD, alpha = 0.05).
# They are derived from tukey_df rather than typed in.
# Note: the box shows the median; Tukey compares means (close here, n = 9).

stopifnot(tukey_df$sig[tukey_df$pair == "IES − BM"] == "no",
          all(tukey_df$sig[tukey_df$pair != "IES − BM"] == "yes"))
letters_df <- data.frame(x = 1:3, y = 80, label = c("b", "a", "b"))
plots$slide_23_letters <- ggplot(espresso, aes(x = as.numeric(method), y = foamIndx)) +
  geom_boxplot(aes(group = method), width = 0.5, outlier.shape = NA,
               fill = bb852$bg, colour = bb852$grey, linewidth = 0.7) +
  geom_point(aes(x = x), colour = bb852$slate, size = 2.6, alpha = 0.75) +
  geom_text(data = letters_df, aes(x = x, y = y, label = label), inherit.aes = FALSE,
            size = 7, family = bb852_font, fontface = "bold", colour = bb852$coral) +
  scale_x_continuous(breaks = 1:3, labels = levels(espresso$method),
                     limits = c(0.5, 3.5), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(15, 84), breaks = seq(20, 80, 20)) +
  labs(x = "Brewing method", y = "Foam index (%)") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Slide 26: residual diagnostics for foam_mod ----------------------------------------
# Redrawn in the course theme from the first three panels of autoplot(foam_mod).

diag_df <- data.frame(fitted = fitted(foam_mod), resid = residuals(foam_mod),
                      std = rstandard(foam_mod), method = model.frame(foam_mod)$method)
diag_names <- c("Residuals vs fitted", "Normal Q-Q", "Scale-location")
qq <- qqnorm(diag_df$std, plot.it = FALSE)
diag_long <- rbind(
  data.frame(panel = diag_names[1], x = diag_df$fitted, y = diag_df$resid,
             method = diag_df$method),
  data.frame(panel = diag_names[2], x = qq$x, y = qq$y, method = diag_df$method),
  data.frame(panel = diag_names[3], x = diag_df$fitted, y = sqrt(abs(diag_df$std)),
             method = diag_df$method)
)
diag_long$panel <- factor(diag_long$panel, levels = diag_names)
ref_lines <- data.frame(panel = factor(diag_names[1:2], levels = diag_names),
                        intercept = c(0, 0), slope = c(0, 1))
plots$slide_26_diagnostics <- ggplot(diag_long, aes(x, y)) +
  geom_abline(data = ref_lines, aes(intercept = intercept, slope = slope),
              colour = bb852$grey, linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(colour = method), size = 2.4, alpha = 0.8) +
  facet_wrap(~panel, nrow = 1, scales = "free") +
  scale_colour_manual(values = method_cols) +
  labs(x = NULL, y = NULL) +
  theme_bb852() +
  theme(legend.position = "top", legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(colour = bb852$text, size = 16),
        panel.spacing = grid::unit(0.35, "in"),
        strip.text = element_text(face = "plain", colour = bb852$text, size = 16))
half_plots$slide_26_diagnostics <- plots$slide_26_diagnostics +
  facet_wrap(~panel, ncol = 2, scales = "free") +
  half_facet_theme


# Slide 27: what Q-Q plots look like ----------------------------------------------------
# SIMULATED data, seed 27. Three normal samples (two small, one large) and two
# non-normal samples, each standardised so the panels share axes.

set.seed(27)
qq_samples <- list(
  "Normal, n = 20"       = rnorm(20),
  "Normal, n = 20 "      = rnorm(20),
  "Normal, n = 100"      = rnorm(100),
  "Skewed, n = 100"      = rgamma(100, shape = 1.5),
  "Heavy tails, n = 100" = rt(100, df = 3)
)
qq_long <- do.call(rbind, lapply(names(qq_samples), function(nm) {
  v <- as.vector(scale(qq_samples[[nm]]))
  q <- qqnorm(v, plot.it = FALSE)
  data.frame(panel = nm, x = q$x, y = q$y,
             type = ifelse(grepl("^Normal", nm), "normal", "other"))
}))
qq_long$panel <- factor(qq_long$panel, levels = names(qq_samples))
plots$slide_27_qq_examples <- ggplot(qq_long, aes(x, y, colour = type)) +
  geom_abline(intercept = 0, slope = 1, colour = bb852$grey,
              linewidth = 0.6, linetype = "dashed") +
  geom_point(size = 1.8, alpha = 0.75) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(values = c(normal = bb852$slate, other = bb852$coral)) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 5)) +
  scale_x_continuous(breaks = c(-2, 0, 2)) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bb852() +
  theme(panel.spacing = grid::unit(0.2, "in"),
        strip.text = element_text(face = "plain", colour = bb852$text, size = 14))
half_plots$slide_27_qq_examples <- plots$slide_27_qq_examples +
  facet_wrap(~panel, ncol = 2) +
  half_facet_theme


# Export every figure -----------------------------------------------------------------

figure_names <- c(
  "slide_01_cover",
  "slide_08_espresso_data",
  "slide_09_ss_partition",
  "slide_11_signal_noise",
  "slide_12_f_distribution",
  "slide_17_coefficients",
  "slide_22_tukey_ci",
  "slide_23_letters",
  "slide_26_diagnostics",
  "slide_27_qq_examples"
)
stopifnot(setequal(figure_names, names(plots)))

for (name in names(plots)) {
  if (name == "slide_01_cover") {
    save_bb852(plots[[name]], name, layout = "cover", bg = bb852$cover)
  } else {
    save_bb852(plots[[name]], name, layout = "full")
    half_plot <- if (name %in% names(half_plots)) half_plots[[name]] else plots[[name]]
    save_bb852(half_plot, name, layout = "half")
  }
}

# Slides 16 and 23 sit beside a TAKEAWAY box, so also export at that size.
for (name in c("slide_17_coefficients", "slide_23_letters")) {
  save_bb852(plots[[name]], name, layout = "takeaway")
}

message("Saved ", length(plots), " figures to: ", normalizePath(output_root))
