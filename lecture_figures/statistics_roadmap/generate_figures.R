# Load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(ragg)
library(patchwork)


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
  panel    = "#EDEFF1"
)

bb852_discrete <- c(
  bb852$slate,
  bb852$coral,
  bb852$teal,
  bb852$grey
)

theme_bb852 <- function(
    base_size = 18,
    base_family = "Avenir Next"
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
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(
        colour = bb852$text, size = base_size - 2
      ),
      legend.background = element_rect(
        fill = bb852$bg, colour = NA
      ),
      legend.key = element_rect(
        fill = bb852$bg, colour = NA
      ),
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

scale_colour_bb852 <- function(...) {
  scale_colour_manual(values = bb852_discrete, ...)
}

scale_fill_bb852 <- function(...) {
  scale_fill_manual(values = bb852_discrete, ...)
}

scale_colour_bb852_seq <- function(...) {
  scale_colour_gradient(
    low = bb852$panel,
    high = bb852$slate,
    ...
  )
}

scale_fill_bb852_seq <- function(...) {
  scale_fill_gradient(
    low = bb852$panel,
    high = bb852$slate,
    ...
  )
}

scale_colour_bb852_div <- function(midpoint = 0, ...) {
  scale_colour_gradient2(
    low = bb852$teal,
    mid = "#F2F2EE",
    high = bb852$coral,
    midpoint = midpoint,
    ...
  )
}

scale_fill_bb852_div <- function(midpoint = 0, ...) {
  scale_fill_gradient2(
    low = bb852$teal,
    mid = "#F2F2EE",
    high = bb852$coral,
    midpoint = midpoint,
    ...
  )
}

bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  third    = c(3.64, 5.30),
  slide    = c(13.33, 7.50)
)

save_bb852 <- function(
    plot,
    filename,
    layout = "full"
) {
  size <- bb852_sizes[[layout]]

  if (is.null(size)) {
    stop(
      "layout must be one of: ",
      paste(names(bb852_sizes), collapse = ", ")
    )
  }

  ggsave(
    paste0(filename, ".png"),
    plot,
    width = size[1],
    height = size[2],
    units = "in",
    dpi = 300,
    device = ragg::agg_png,
    bg = bb852$bg
  )

  invisible(filename)
}

available_fonts <- unique(systemfonts::system_fonts()$family)
bb852_font <- if ("Avenir Next" %in% available_fonts) {
  "Avenir Next"
} else {
  "Helvetica"
}


output_root <- file.path(
  "lecture_figures",
  "statistics_roadmap",
  "output"
)
unlink(output_root, recursive = TRUE)
full_output_directory <- file.path(output_root, "full")
half_output_directory <- file.path(output_root, "half")
dir.create(full_output_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(half_output_directory, recursive = TRUE, showWarnings = FALSE)

save_figure_versions <- function(plot, filename, half_plot = plot) {
  save_bb852(
    plot,
    file.path(full_output_directory, filename),
    layout = "full"
  )
  save_bb852(
    half_plot,
    file.path(half_output_directory, filename),
    layout = "half"
  )
}


# Helper for normal-curve concept figures ---------------------------------

# Each row of `curves` defines one normal distribution: a panel label, a
# group, a mean and an SD. Returns density values on a regular grid.
normal_curve_data <- function(curves, x_range, n = 401) {
  x_grid <- seq(x_range[1], x_range[2], length.out = n)

  bind_rows(lapply(seq_len(nrow(curves)), function(i) {
    data.frame(
      panel = curves$panel[i],
      group = curves$group[i],
      x = x_grid,
      density = dnorm(x_grid, curves$mean[i], curves$sd[i])
    )
  }))
}

# Panel strips ("A", "B") are used instead of plot titles, so the panels
# can be placed side by side (full) or stacked (half).
concept_theme <- function() {
  theme_bb852(base_family = bb852_font) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none",
      strip.text = element_text(
        colour = bb852$text,
        size = 18,
        face = "bold",
        hjust = 0
      ),
      panel.spacing = grid::unit(18, "pt")
    )
}


# Generate Figure 1: one-sample t-test concept -----------------------------

one_sample_curves <- data.frame(
  panel = c("A", "A", "B", "B"),
  group = c("first", "second", "first", "second"),
  mean = c(150, 200, 170, 170),
  sd = c(40, 40, 40, 80)
)

one_sample_density <- normal_curve_data(one_sample_curves, c(0, 350))

one_sample_arrows <- one_sample_curves %>%
  mutate(
    y = ifelse(group == "first", 0.0020, 0.0030),
    x = 100
  )

one_sample_colours <- c(first = bb852$slate, second = bb852$coral)

one_sample_base <- ggplot(one_sample_density, aes(x = x, y = density)) +
  geom_vline(
    xintercept = 100,
    colour = bb852$text,
    linewidth = 1
  ) +
  geom_line(aes(colour = group), linewidth = 1.3) +
  geom_vline(
    data = one_sample_curves,
    aes(xintercept = mean, colour = group),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_segment(
    data = one_sample_arrows,
    aes(x = x, xend = mean, y = y, yend = y, colour = group),
    linewidth = 0.9,
    arrow = grid::arrow(
      ends = "both",
      type = "closed",
      length = grid::unit(0.07, "inches")
    )
  ) +
  scale_colour_manual(values = one_sample_colours) +
  scale_x_continuous(breaks = seq(0, 300, by = 100)) +
  labs(
    x = expression(Pollution ~ level ~ (mg / m^3)),
    y = "Probability density"
  ) +
  concept_theme()

figure_1 <- one_sample_base + facet_wrap(~panel, nrow = 1)
figure_1_half <- one_sample_base + facet_wrap(~panel, ncol = 1)


# Generate Figure 2: two-sample t-test concept -----------------------------

two_sample_curves <- data.frame(
  panel = c("A", "A", "B", "B"),
  group = c("first", "second", "first", "second"),
  mean = c(40, 60, 40, 80),
  sd = c(10, 10, 10, 10)
)

two_sample_density <- normal_curve_data(two_sample_curves, c(0, 120))

two_sample_arrows <- two_sample_curves %>%
  group_by(panel) %>%
  summarise(x = min(mean), xend = max(mean), .groups = "drop") %>%
  mutate(y = 0.002)

two_sample_base <- ggplot(two_sample_density, aes(x = x, y = density)) +
  geom_line(aes(colour = group), linewidth = 1.3) +
  geom_vline(
    data = two_sample_curves,
    aes(xintercept = mean, colour = group),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_segment(
    data = two_sample_arrows,
    aes(x = x, xend = xend, y = y, yend = y),
    colour = bb852$text,
    linewidth = 0.9,
    arrow = grid::arrow(
      ends = "both",
      type = "closed",
      length = grid::unit(0.07, "inches")
    )
  ) +
  scale_colour_manual(values = one_sample_colours) +
  scale_x_continuous(breaks = seq(0, 125, by = 25)) +
  labs(x = "x", y = "Probability density") +
  concept_theme()

figure_2 <- two_sample_base + facet_wrap(~panel, nrow = 1)
figure_2_half <- two_sample_base + facet_wrap(~panel, ncol = 1)


# Generate Figure 3: paired t-test (cassava tool use) ----------------------

tool_use <- read.csv("CourseData/toolUse.csv", fileEncoding = "UTF-8-BOM")
tool_use$tool <- factor(tool_use$tool, levels = c("bow", "machete"))

stopifnot(
  nrow(tool_use) == 10,
  all(table(tool_use$subjectID) == 2),
  !anyNA(tool_use$amount)
)

figure_3 <- ggplot(
  tool_use,
  aes(x = tool, y = amount, group = subjectID)
) +
  geom_line(
    colour = bb852$grey,
    linewidth = 1.2,
    alpha = 0.45
  ) +
  geom_point(colour = bb852$slate, size = 3.5) +
  scale_x_discrete(labels = c(bow = "Wooden tool (bow)", machete = "Machete")) +
  labs(x = NULL, y = "Cassava harvested (kg/hr)") +
  theme_bb852(base_family = bb852_font) +
  theme(panel.grid.major.x = element_blank())


# Generate Figure 4: one-way ANOVA concept ---------------------------------

anova_curves <- data.frame(
  panel = rep(c("A", "B"), each = 3),
  group = rep(c("first", "second", "third"), times = 2),
  mean = c(40, 45, 50, 40, 45, 80),
  sd = 10
)

anova_density <- normal_curve_data(anova_curves, c(0, 120))

anova_colours <- c(
  first = bb852$slate,
  second = bb852$coral,
  third = bb852$teal
)

anova_base <- ggplot(anova_density, aes(x = x, y = density)) +
  geom_line(aes(colour = group), linewidth = 1.3) +
  geom_vline(
    data = anova_curves,
    aes(xintercept = mean, colour = group),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_colour_manual(values = anova_colours) +
  scale_x_continuous(breaks = seq(0, 125, by = 25)) +
  labs(x = "y", y = "Probability density") +
  concept_theme()

figure_4 <- anova_base + facet_wrap(~panel, nrow = 1)
figure_4_half <- anova_base + facet_wrap(~panel, ncol = 1)


# Generate Figure 5: one-way ANOVA (espresso foam) -------------------------

espresso <- read.csv("CourseData/espresso.csv")
espresso$method <- factor(espresso$method, levels = c("BM", "HIP", "IES"))

stopifnot(
  nrow(espresso) == 27,
  !anyNA(espresso$foamIndx),
  nlevels(espresso$method) == 3
)

set.seed(43)
figure_5 <- ggplot(espresso, aes(x = method, y = foamIndx)) +
  geom_boxplot(
    width = 0.5,
    fill = bb852$panel,
    colour = bb852$muted,
    linewidth = 0.7,
    outlier.shape = NA
  ) +
  geom_jitter(
    colour = bb852$slate,
    size = 3.2,
    width = 0.12,
    height = 0
  ) +
  labs(x = "Method", y = "Foam index") +
  theme_bb852(base_family = bb852_font) +
  theme(panel.grid.major.x = element_blank())


# Simulate data and generate Figure 6: linear regression ------------------

# Illustrative simulated data (no course dataset): y = 0.2 + 1.5x + noise.
set.seed(8521)
regression_data <- data.frame(x = runif(30, 0, 10)) %>%
  mutate(y = 0.2 + 1.5 * x + rnorm(n(), 0, 1.3))

regression_fit <- lm(y ~ x, data = regression_data)
stopifnot(coef(regression_fit)[["x"]] > 1)

figure_6 <- ggplot(regression_data, aes(x = x, y = y)) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = bb852$coral,
    linewidth = 1.3
  ) +
  geom_point(colour = bb852$slate, size = 3.2) +
  labs(x = "x", y = "y") +
  theme_bb852(base_family = bb852_font)


# Simulate data and generate Figure 7: ANCOVA -----------------------------

# Illustrative simulated data: two groups with a common slope and
# different intercepts.
set.seed(8522)
ancova_data <- data.frame(
  group = factor(rep(c("A", "B"), each = 15)),
  x = runif(30, 0, 10)
) %>%
  mutate(y = ifelse(group == "A", 4, 0.5) + 1.4 * x + rnorm(n(), 0, 1.2))

ancova_fit <- lm(y ~ x + group, data = ancova_data)
ancova_lines <- expand.grid(
  x = c(0, 10),
  group = levels(ancova_data$group)
) %>%
  mutate(y = predict(ancova_fit, newdata = .))

figure_7 <- ggplot(ancova_data, aes(x = x, y = y, colour = group)) +
  geom_line(data = ancova_lines, linewidth = 1.3) +
  geom_point(size = 3.2) +
  scale_colour_manual(values = c(A = bb852$coral, B = bb852$slate)) +
  labs(x = "x", y = "y") +
  theme_bb852(base_family = bb852_font)


# Simulate data and generate Figure 8: multiple regression ----------------

# Illustrative simulated data: a response (NTAV) explained by two
# continuous variables (age and weight); lines show fitted values at
# fixed weights.
set.seed(8523)
multiple_data <- data.frame(
  age = runif(120, 35, 85),
  weight = runif(120, 40, 90)
) %>%
  mutate(
    NTAV = -25 + 0.45 * age + 0.9 * weight +
      0.012 * age * weight + rnorm(n(), 0, 14)
  )

multiple_fit <- lm(NTAV ~ age * weight, data = multiple_data)
multiple_lines <- expand.grid(
  age = c(35, 85),
  weight = seq(40, 90, by = 10)
) %>%
  mutate(NTAV = predict(multiple_fit, newdata = .))

figure_8 <- ggplot(
  multiple_data,
  aes(x = age, y = NTAV, colour = weight)
) +
  geom_line(
    data = multiple_lines,
    aes(group = weight),
    linewidth = 1.1
  ) +
  geom_point(size = 2.8, alpha = 0.9) +
  scale_colour_gradient(
    low = bb852$light,
    high = bb852$slate,
    name = "Weight",
    breaks = seq(40, 90, by = 10)
  ) +
  labs(x = "Age", y = "NTAV") +
  theme_bb852(base_family = bb852_font) +
  theme(
    legend.position = "right",
    legend.title = element_text(colour = bb852$text, size = 16),
    legend.key.height = grid::unit(28, "pt")
  )


# Simulate data and generate Figure 9: model comparison -------------------

# Three candidate models fitted to the same kind of data: separate slopes
# and intercepts (interaction), separate intercepts (ANCOVA), and a single
# regression line ignoring group.
set.seed(8524)
comparison_data <- data.frame(
  group = factor(rep(c("A", "B"), each = 15)),
  x = runif(30, 0, 10)
) %>%
  mutate(
    y = ifelse(group == "A", 4.5 + 1.0 * x, 1 + 1.45 * x) +
      rnorm(n(), 0, 1.2)
  )

model_lines <- function(fit, label, by_group = TRUE) {
  grid <- expand.grid(x = c(0, 10), group = levels(comparison_data$group))
  grid$y <- predict(fit, newdata = grid)
  if (!by_group) grid$group <- "All"
  grid$model <- label
  distinct(grid)
}

model_labels <- c(
  "y ~ x * group",
  "y ~ x + group",
  "y ~ x"
)

comparison_lines <- bind_rows(
  model_lines(lm(y ~ x * group, comparison_data), model_labels[1]),
  model_lines(lm(y ~ x + group, comparison_data), model_labels[2]),
  model_lines(lm(y ~ x, comparison_data), model_labels[3], by_group = FALSE)
) %>%
  mutate(model = factor(model, levels = model_labels))

comparison_points <- bind_rows(lapply(model_labels, function(label) {
  mutate(comparison_data, model = label)
})) %>%
  mutate(model = factor(model, levels = model_labels))

comparison_colours <- c(
  A = bb852$coral,
  B = bb852$slate,
  All = bb852$text
)

comparison_base <- ggplot(
  comparison_points,
  aes(x = x, y = y, colour = group)
) +
  geom_line(
    data = comparison_lines,
    aes(group = group),
    linewidth = 1.3
  ) +
  geom_point(size = 2.6) +
  scale_colour_manual(
    values = comparison_colours,
    breaks = c("A", "B")
  ) +
  labs(x = "x", y = "y") +
  theme_bb852(base_family = bb852_font) +
  theme(
    strip.text = element_text(
      colour = bb852$text,
      size = 16,
      face = "bold",
      family = "mono",
      hjust = 0
    ),
    panel.spacing = grid::unit(18, "pt")
  )

figure_9 <- comparison_base + facet_wrap(~model, nrow = 1)
figure_9_half <- comparison_base +
  facet_wrap(~model, ncol = 1) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13, family = "mono", face = "bold",
                              colour = bb852$text, hjust = 0),
    axis.text = element_text(size = 12),
    panel.spacing = grid::unit(6, "pt")
  )


# Simulate data and generate Figure 10: binomial GLM ----------------------

# Illustrative simulated success/failure data with a steep logistic
# relationship to x.
set.seed(8525)
binomial_data <- data.frame(x = runif(60, -90, 90)) %>%
  mutate(y = rbinom(n(), 1, plogis(0.12 * x)))

binomial_fit <- glm(y ~ x, family = binomial, data = binomial_data)
binomial_curve <- data.frame(x = seq(-100, 100, length.out = 400)) %>%
  mutate(y = predict(binomial_fit, newdata = ., type = "response"))

figure_10 <- ggplot(binomial_data, aes(x = x, y = y)) +
  geom_line(
    data = binomial_curve,
    colour = bb852$coral,
    linewidth = 1.3
  ) +
  geom_point(
    colour = bb852$slate,
    size = 3,
    alpha = 0.7,
    position = position_jitter(width = 0, height = 0.015, seed = 1)
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  labs(x = "x", y = "Probability of success") +
  theme_bb852(base_family = bb852_font)


# Simulate data and generate Figure 11: Poisson GLM -----------------------

# Illustrative simulated count data with a log-linear relationship to x.
set.seed(8526)
poisson_data <- data.frame(x = runif(70, 0, 10)) %>%
  mutate(y = rpois(n(), exp(0.05 + 0.25 * x)))

poisson_fit <- glm(y ~ x, family = poisson, data = poisson_data)
poisson_curve <- data.frame(x = seq(0, 10, length.out = 200)) %>%
  mutate(y = predict(poisson_fit, newdata = ., type = "response"))

figure_11 <- ggplot(poisson_data, aes(x = x, y = y)) +
  geom_line(
    data = poisson_curve,
    colour = bb852$coral,
    linewidth = 1.3
  ) +
  geom_point(colour = bb852$slate, size = 3, alpha = 0.85) +
  labs(x = "x", y = "Count (y)") +
  theme_bb852(base_family = bb852_font)


# Save all figures ---------------------------------------------------------

save_figure_versions(figure_1, "01_one_sample_t_test", figure_1_half)
save_figure_versions(figure_2, "02_two_sample_t_test", figure_2_half)
save_figure_versions(figure_3, "03_paired_t_test_tool_use")
save_figure_versions(figure_4, "04_anova_concept", figure_4_half)
save_figure_versions(figure_5, "05_anova_espresso")
save_figure_versions(figure_6, "06_linear_regression")
save_figure_versions(figure_7, "07_ancova")
save_figure_versions(figure_8, "08_multiple_regression")
save_figure_versions(figure_9, "09_model_comparison", figure_9_half)
save_figure_versions(figure_10, "10_glm_binomial")
save_figure_versions(figure_11, "11_glm_poisson")


# Run validation checks ---------------------------------------------------

output_stems <- c(
  "01_one_sample_t_test",
  "02_two_sample_t_test",
  "03_paired_t_test_tool_use",
  "04_anova_concept",
  "05_anova_espresso",
  "06_linear_regression",
  "07_ancova",
  "08_multiple_regression",
  "09_model_comparison",
  "10_glm_binomial",
  "11_glm_poisson"
)
expected_output_files <- c(
  file.path(full_output_directory, paste0(output_stems, ".png")),
  file.path(half_output_directory, paste0(output_stems, ".png"))
)

stopifnot(
  length(expected_output_files) == 22,
  all(file.exists(expected_output_files)),
  all(file.info(expected_output_files)$size > 0)
)

message(
  "Created full- and half-width versions of eleven BB852 statistics ",
  "roadmap figures as PNG files in ", output_root,
  ". All validation checks passed."
)
