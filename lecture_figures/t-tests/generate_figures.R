# BB852 t-test lecture: R code for every statistical figure
# -----------------------------------------------------------
# Recreates the lecture's statistical figures in ggplot2, using the BB852
# theme shared with the other lecture_figures generators.
#
# Data source:
# https://jonesor.github.io/BB852_Book/t-test-comparing-two-means.html
# Reaction times: classData.csv, Year 2019, recorded Gender Female/Male.
# The data are typed in below so the script needs no external files.
#
# Run from the repository root:
# source("lecture_figures/t-tests/generate_figures.R")
#
# PNG files are written to lecture_figures/t-tests/output/<layout>/.
# Each ggplot object is also retained in the named list `plots`.
# Object and file names use the slide numbers of the current lecture deck
# (BB852_t_tests_Context_Data_Analysis), e.g. plots$slide_23_reaction_times.
#
# Labels use plotmath (e.g. m^3, H[0], mu) and \u escapes (e.g. \u00b5, the
# micro sign) rather than typed Unicode symbols, so the script stays plain ASCII.
# Pollution values are concentrations in air, in micrograms per cubic metre.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/randomisation_tests.

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
bb852_font <- if ("Avenir Next" %in% available_fonts) {
  "Avenir Next"
} else {
  "Helvetica"
}

theme_bb852 <- function(
    base_size = 18,
    base_family = bb852_font
) {
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      plot.background = element_rect(
        fill = bb852$bg, colour = NA
      ),
      panel.background = element_rect(
        fill = bb852$bg, colour = NA
      ),
      panel.grid.major = element_line(
        colour = bb852$grid, linewidth = 0.4
      ),
      panel.grid.minor = element_blank(),
      axis.title = element_text(
        colour = bb852$text, size = base_size
      ),
      axis.text = element_text(
        colour = bb852$muted, size = base_size - 2
      ),
      axis.ticks = element_blank(),
      legend.position = "none",
      strip.text = element_text(
        colour = bb852$text,
        size = base_size - 2,
        face = "bold",
        hjust = 0
      ),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.margin = margin(12, 16, 12, 12)
    )
}

# Horizontal displays without a meaningful y-axis.
no_y_axis <- function() {
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )
}

# Sizes (inches) match the figure placeholders in the BB852 slide theme.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "t-tests", "output")

save_bb852 <- function(plot, filename, layout = "half", bg = bb852$bg) {
  size <- bb852_sizes[[layout]]
  if (is.null(size)) {
    stop(
      "layout must be one of: ",
      paste(names(bb852_sizes), collapse = ", ")
    )
  }
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    file.path(output_directory, paste0(filename, ".png")),
    plot,
    width = size[1],
    height = size[2],
    units = "in",
    dpi = 300,
    device = ragg::agg_png,
    bg = bg
  )
  invisible(filename)
}


# Data ----------------------------------------------------------------------

pollution <- data.frame(ugm3 = c(
  105, 196, 226, 81, 156, 201, 142, 149, 191, 192,
  178, 185, 231, 76, 207, 138, 146, 175, 114, 155
))
# This is a hypothetical teaching threshold, not a regulatory standard.
threshold <- 100

wide <- data.frame(
  subjectID = 1:5,
  bow = c(39, 114, 150, 51, 60),
  machete = c(119, 216, 240, 129, 137)
)
wide$difference <- wide$machete - wide$bow
paired_long <- rbind(
  data.frame(subjectID = wide$subjectID, tool = "Bow", amount = wide$bow),
  data.frame(subjectID = wide$subjectID, tool = "Machete", amount = wide$machete)
)
paired_long$tool <- factor(paired_long$tool, levels = c("Bow", "Machete"))

female <- c(.2828, .3822, .259, .364, .3226, .306, .3, .294, .313,
            .28, .4022, .3968, .5738, .417, .273, .3314, .385, .388)
male <- c(.251, .31, .265, .2938, .258, .235, .2914, .2644,
          .37, .291, .44, .317, .3321, .368, .267)
reaction <- data.frame(
  Gender = factor(rep(c("Female", "Male"), c(length(female), length(male))),
                  levels = c("Female", "Male")),
  Reaction = c(female, male)
)

# Compute results directly rather than typing rounded values into figures.
pollution_test <- t.test(pollution$ugm3, mu = threshold, alternative = "greater")
paired_test <- t.test(wide$machete, wide$bow, paired = TRUE)
reaction_test <- t.test(Reaction ~ Gender, data = reaction, var.equal = FALSE)
mean_difference_ms <- unname(reaction_test$estimate[1] - reaction_test$estimate[2]) * 1000
reaction_ci_ms <- unname(reaction_test$conf.int) * 1000

plots <- list()


# Slide 1: title-slide chart --------------------------------------------------
# The full image fills the right-hand cover placeholder. The large top/bottom
# margins place the chart centrally, as in the PowerPoint.

cover_data <- paired_long
cover_data$highlight <- ifelse(cover_data$subjectID == 3, "highlight", "other")
plots$slide_01_cover <- ggplot(cover_data, aes(tool, amount, group = subjectID,
                                               colour = highlight)) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c(highlight = bb852$coral, other = bb852$light)) +
  scale_y_continuous(limits = c(0, 260), breaks = seq(0, 250, 50),
                     expand = expansion(mult = 0)) +
  scale_x_discrete(expand = expansion(add = 0.1)) +
  labs(x = NULL, y = "Cassava harvested (kg/hour)") +
  theme_bb852() +
  theme(
    plot.background = element_rect(fill = bb852$cover, colour = NA),
    panel.background = element_rect(fill = bb852$cover, colour = NA),
    panel.grid.major = element_blank(),
    axis.line.y = element_line(colour = bb852$light, linewidth = 0.4),
    axis.title = element_text(colour = bb852$light),
    axis.text.y = element_text(colour = bb852$light),
    axis.text.x = element_text(colour = bb852$bg),
    plot.margin = margin(140, 12, 110, 12)
  )


# Slide 6: raw pollution measurements ----------------------------------------
# Deterministic vertical offsets reproduce the presentation without random jitter.

pollution$offset <- .46 + ((seq_len(nrow(pollution)) - 1) %% 5) * .02
plots$slide_06_pollution <-ggplot(pollution, aes(ugm3, offset)) +
  annotate("segment", x = threshold, xend = threshold, y = .15, yend = .85,
           colour = bb852$coral, linewidth = .55) +
  geom_point(colour = bb852$slate, size = 2.4) +
  scale_x_continuous(limits = c(50, 250), breaks = seq(50, 250, 50),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0)) +
  labs(x = expression("Pollution (\u00b5g/m"^3 * ")"), y = NULL) +
  theme_bb852() + no_y_axis()


# Slide 7: spread of the data versus uncertainty in the mean ------------------
# Grey bars: density-scaled histogram of the 20 measurements (variation among
# observations, SD). Blue curve: normal approximation to the sampling
# distribution of the mean, centred on the estimate with width SE. Both layers
# are densities so they share the y-axis. The curve is NOT a fit to the raw
# data and NOT the null distribution used for the test.

pollution_sd <- sd(pollution$ugm3)
pollution_se <- pollution_sd / sqrt(nrow(pollution))
pollution_xbar <- mean(pollution$ugm3)
x_grid <- seq(50, 250, by = .25)
se_curve <- data.frame(
  x = x_grid,
  density = dnorm(x_grid, mean = pollution_xbar, sd = pollution_se)
)
plots$slide_07_uncertainty_in_mean <- ggplot() +
  geom_histogram(
    data = pollution, aes(ugm3, after_stat(density)),
    breaks = seq(50, 250, 25), closed = "right",
    fill = "#C7CCD4", colour = bb852$bg, linewidth = .6
  ) +
  geom_area(data = se_curve, aes(x, density),
            fill = bb852$light, alpha = .45, colour = NA) +
  geom_line(data = se_curve, aes(x, density),
            colour = bb852$slate, linewidth = 1) +
  geom_vline(xintercept = threshold, colour = bb852$coral, linewidth = .8) +
  geom_vline(xintercept = pollution_xbar, colour = bb852$slate,
             linewidth = .55, linetype = "dashed") +
  scale_x_continuous(limits = c(50, 250), breaks = seq(50, 250, 50),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = seq(0, .04, .01),
                     labels = function(z) sprintf("%.2f", z),
                     expand = expansion(mult = c(0, .05))) +
  labs(x = expression("Pollution (\u00b5g/m"^3 * ")"), y = "Density") +
  theme_bb852() +
  theme(panel.grid.major.y = element_blank())


# Slide 9: same estimated mean, different standard errors -------------------
# Hypothetical estimates (not the pollution data): mean 130 in both panels,
# SE 10 (left) and 25 (right). Signal 30, so signal/noise = 3 and 1.2.

same_mean <- 130
se_values <- c(10, 25)
se_labels <- sprintf('"SE = %d \u00b5g/m"^3', se_values)
se_panels <- do.call(rbind, lapply(seq_along(se_values), function(i) {
  data.frame(
    x = x_grid,
    density = dnorm(x_grid, mean = same_mean, sd = se_values[i]),
    panel = se_labels[i]
  )
}))
se_panels$panel <- factor(se_panels$panel, levels = se_labels)
plots$slide_09_same_mean_different_se <- ggplot(se_panels, aes(x, density)) +
  geom_area(fill = bb852$light, alpha = .75, colour = NA) +
  geom_line(colour = bb852$slate, linewidth = .9) +
  geom_vline(xintercept = threshold, colour = bb852$coral, linewidth = .8) +
  geom_vline(xintercept = same_mean, colour = bb852$slate,
             linewidth = .55, linetype = "dashed") +
  facet_wrap(~panel, nrow = 1, labeller = label_parsed) +
  scale_x_continuous(limits = c(50, 250), breaks = seq(50, 250, 50),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(x = expression("Estimated mean = 130 \u00b5g/m"^3 * " in both panels"),
       y = NULL) +
  theme_bb852() + no_y_axis() +
  theme(
    strip.text = element_text(colour = bb852$slate, face = "plain",
                              size = 18, hjust = 0),
    panel.spacing = grid::unit(.35, "in")
  )


# Slides 11 and 12: null t-distributions -------------------------------------
# These figures illustrate t = 2 with 19 d.f., NOT the pollution t = 6.28.
# Using a fine grid gives smooth curves; shading counts the specified tail(s).

t_df <- 19
observed_t <- 2
t_grid <- seq(-4, 4, by = .005)
t_curve <- data.frame(t = t_grid, density = dt(t_grid, df = t_df))
p_one <- pt(observed_t, df = t_df, lower.tail = FALSE)
p_two <- 2 * pt(abs(observed_t), df = t_df, lower.tail = FALSE)

t_axis_label <- expression(italic(t) * "-statistic under " * H[0])

plot_t_tails <- function(two_sided = FALSE) {
  selected <- if (two_sided) abs(t_curve$t) >= observed_t else t_curve$t >= observed_t
  tails <- t_curve[selected, ]
  # Separate the left and right polygons so a two-sided fill does not bridge zero.
  tails$tail <- ifelse(tails$t < 0, "left", "right")
  ggplot(t_curve, aes(t, density)) +
    geom_area(fill = bb852$light, colour = NA) +
    geom_area(data = tails, aes(group = tail), fill = bb852$coral, colour = NA) +
    scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2),
                       expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0, .42), expand = expansion(mult = 0)) +
    labs(x = t_axis_label, y = NULL) +
    theme_bb852() + no_y_axis()
}
plots$slide_11_null_distribution <- plot_t_tails()

# Both panels in a single wide image. Headings are plotmath strings built
# from the computed p-values.
panel_labels <- c(
  sprintf('H[1]*":"~mu > mu[0]~~~~~italic(p) %%~~%% "%.3f"', p_one),
  sprintf('H[1]*":"~mu != mu[0]~~~~~italic(p) %%~~%% "%.3f"', p_two)
)
two_panels <- rbind(
  transform(t_curve, panel = panel_labels[1]),
  transform(t_curve, panel = panel_labels[2])
)
two_panels$panel <- factor(two_panels$panel, levels = panel_labels)
shaded <- two_panels[
  (two_panels$panel == panel_labels[1] & two_panels$t >= observed_t) |
  (two_panels$panel == panel_labels[2] & abs(two_panels$t) >= observed_t), ]
shaded$tail <- ifelse(shaded$t < 0, "left", "right")
plots$slide_12_one_and_two_sided <-ggplot(two_panels, aes(t, density)) +
  geom_area(fill = bb852$light, colour = NA) +
  geom_area(data = shaded, aes(group = tail), fill = bb852$coral, colour = NA) +
  facet_wrap(~panel, nrow = 1, labeller = label_parsed) +
  scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, .42), expand = expansion(mult = 0)) +
  labs(x = t_axis_label, y = NULL) +
  theme_bb852() + no_y_axis() +
  theme(panel.spacing = grid::unit(.35, "in"))


# Slide 14: one-sided versus two-sided 95% confidence intervals ---------------
# The one-sided test (alternative = "greater") gives the interval [L, Inf): an
# arrowhead at the display limit signals that it has no finite upper end. The
# two-sided interval from t.test(..., mu = 100) is shown below it for contrast.

lower_bound <- unname(pollution_test$conf.int[1])
pollution_mean <- unname(pollution_test$estimate)
two_sided_ci <- unname(t.test(pollution$ugm3, mu = threshold)$conf.int)
y_one <- .66
y_two <- .30
label_one <- sprintf('"One-sided 95%%: [%.1f, " * infinity * ")"', lower_bound)
label_two <- sprintf('"Two-sided 95%%: %.1f to %.1f"', two_sided_ci[1], two_sided_ci[2])
plots$slide_14_lower_bound <- ggplot() +
  annotate("segment", x = threshold, xend = threshold, y = .08, yend = .92,
           colour = bb852$coral, linewidth = .55) +
  # One-sided interval: lower bound only, open towards +Inf
  annotate("segment", x = lower_bound, xend = 219, y = y_one, yend = y_one,
           colour = bb852$slate, linewidth = 1,
           arrow = grid::arrow(length = grid::unit(.12, "in"), type = "closed")) +
  annotate("segment", x = lower_bound, xend = lower_bound,
           y = y_one - .06, yend = y_one + .06,
           colour = bb852$slate, linewidth = .8) +
  annotate("point", x = pollution_mean, y = y_one, colour = bb852$coral, size = 2.7) +
  annotate("text", x = lower_bound - 3, y = y_one + .15, label = label_one,
           parse = TRUE, hjust = 0, family = bb852_font, colour = bb852$slate,
           size = 5) +
  # Two-sided interval: both limits
  annotate("segment", x = two_sided_ci[1], xend = two_sided_ci[2],
           y = y_two, yend = y_two, colour = bb852$grey, linewidth = 1) +
  annotate("segment", x = two_sided_ci, xend = two_sided_ci,
           y = y_two - .06, yend = y_two + .06,
           colour = bb852$grey, linewidth = .8) +
  annotate("point", x = pollution_mean, y = y_two, colour = bb852$coral, size = 2.7) +
  annotate("text", x = two_sided_ci[1] - 3, y = y_two + .15, label = label_two,
           parse = TRUE, hjust = 0, family = bb852_font, colour = bb852$grey,
           size = 5) +
  scale_x_continuous(limits = c(80, 220), breaks = seq(80, 220, 20),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0)) +
  labs(x = expression("Population mean pollution (\u00b5g/m"^3 * ")"), y = NULL) +
  theme_bb852() + no_y_axis()


# Slide 17: paired tool-use measurements -------------------------------------

plots$slide_17_paired_measurements <- ggplot(
  paired_long, aes(tool, amount, group = subjectID)
) +
  geom_line(colour = bb852$grey, linewidth = .55) +
  geom_point(colour = bb852$grey, size = 2.5) +
  scale_x_discrete(expand = expansion(add = .1)) +
  scale_y_continuous(limits = c(0, 260), breaks = seq(0, 250, 50),
                     expand = expansion(mult = 0)) +
  labs(x = NULL, y = "Cassava harvested (kg/hour)") +
  theme_bb852()


# Slide 18: five independent within-person differences -----------------------
# Grey line: null value (difference = 0). Coral line: mean difference.

plots$slide_18_paired_differences <- ggplot(wide, aes(subjectID, difference)) +
  geom_hline(yintercept = 0, colour = bb852$grey, linewidth = .55) +
  geom_hline(yintercept = mean(wide$difference), colour = bb852$coral, linewidth = .55) +
  geom_point(colour = bb852$slate, size = 2.4) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5, expand = expansion(add = .3)) +
  # A small lower expansion keeps the zero line fully visible.
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20),
                     expand = expansion(add = c(2, 0))) +
  labs(x = "Participant", y = expression(Machete - "bow (kg/hour)")) +
  theme_bb852()


# Slide 23: independent reaction-time groups ---------------------------------
# Identical horizontal offsets to the slide, calculated separately per group.

reaction$x_position <- c(
  1 + (((seq_along(female) - 1) * 7) %% 11 - 5) * .018,
  2 + (((seq_along(male) - 1) * 7) %% 11 - 5) * .018
)
plots$slide_23_reaction_times <-ggplot(reaction, aes(x_position, Reaction, colour = Gender)) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c(Female = bb852$slate, Male = bb852$grey)) +
  scale_x_continuous(limits = c(.5, 2.5), breaks = c(1, 2), labels = c("Female", "Male"),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(.2, .65), breaks = seq(.2, .6, .1),
                     labels = function(z) sprintf("%.1f", z), expand = expansion(mult = 0)) +
  labs(x = NULL, y = "Reaction time (seconds)") +
  theme_bb852() +
  theme(panel.grid.major.x = element_blank())


# Slide 26: mean difference and two-sided 95% confidence interval --------------
# Difference is Female minus Male, matching the order in t.test(Reaction ~ Gender).

plots$slide_26_effect_and_ci <-ggplot() +
  annotate("segment", x = 0, xend = 0, y = .2, yend = .8,
           colour = bb852$grey, linewidth = .55) +
  annotate("segment", x = reaction_ci_ms[1], xend = reaction_ci_ms[2], y = .5, yend = .5,
           colour = bb852$slate, linewidth = 1) +
  annotate("segment", x = reaction_ci_ms, xend = reaction_ci_ms,
           y = .44, yend = .56, colour = bb852$slate, linewidth = .65) +
  annotate("point", x = mean_difference_ms, y = .5, colour = bb852$coral, size = 2.7) +
  scale_x_continuous(limits = c(-20, 100), breaks = seq(-20, 100, 20),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0)) +
  labs(x = expression(Female - "male reaction time (milliseconds)"), y = NULL) +
  theme_bb852() + no_y_axis()


# Export every figure --------------------------------------------------------

layouts <- c(
  slide_01_cover = "cover",
  slide_06_pollution = "half",
  slide_07_uncertainty_in_mean = "half",
  slide_09_same_mean_different_se = "full",
  slide_11_null_distribution = "half",
  slide_12_one_and_two_sided = "full",
  slide_14_lower_bound = "half",
  slide_17_paired_measurements = "takeaway",
  slide_18_paired_differences = "half",
  slide_23_reaction_times = "half",
  slide_26_effect_and_ci = "half"
)
stopifnot(setequal(names(layouts), names(plots)))

for (name in names(plots)) {
  save_bb852(
    plots[[name]], name,
    layout = layouts[[name]],
    bg = if (layouts[[name]] == "cover") bb852$cover else bb852$bg
  )
}

message("Saved ", length(plots), " figures to: ", normalizePath(output_root))
