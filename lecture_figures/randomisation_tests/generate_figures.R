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
  "randomisation_tests",
  "output"
)
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


# Read and validate the cholesterol data ----------------------------------

ch <- read.csv("CourseData/cholesterol.csv")

ch$Treatment <- factor(
  ch$Treatment,
  levels = c("Control", "Drug")
)

stopifnot(
  nrow(ch) == 12,
  sum(ch$Treatment == "Control") == 6,
  sum(ch$Treatment == "Drug") == 6,
  !anyNA(ch$Treatment),
  !anyNA(ch$Cholesterol)
)


# Calculate the observed group means and difference -----------------------

cholesterol_means <- ch %>%
  group_by(Treatment) %>%
  summarise(mean_cholesterol = mean(Cholesterol), .groups = "drop") %>%
  mutate(group_x = as.numeric(Treatment))

control_mean <- cholesterol_means$mean_cholesterol[
  cholesterol_means$Treatment == "Control"
]
drug_mean <- cholesterol_means$mean_cholesterol[
  cholesterol_means$Treatment == "Drug"
]
observed_difference <- drug_mean - control_mean

stopifnot(
  abs(control_mean - 205.6667) < 0.0001,
  abs(drug_mean - 185.5000) < 0.0001,
  abs(observed_difference - (-20.1667)) < 0.0001
)


# Calculate the exact null distribution ----------------------------------

drug_group_size <- sum(ch$Treatment == "Drug")
drug_allocations <- combn(seq_len(nrow(ch)), drug_group_size)

exact_null_distribution <- data.frame(
  null_difference = apply(
    drug_allocations,
    2,
    function(drug_rows) {
      control_rows <- setdiff(seq_len(nrow(ch)), drug_rows)
      mean(ch$Cholesterol[drug_rows]) -
        mean(ch$Cholesterol[control_rows])
    }
  )
)

number_extreme <- sum(
  exact_null_distribution$null_difference <= observed_difference
)
exact_p_value <- number_extreme / nrow(exact_null_distribution)

stopifnot(
  ncol(drug_allocations) == choose(12, 6),
  nrow(exact_null_distribution) == 924,
  number_extreme == 31,
  abs(exact_p_value - 0.03355) < 0.00001
)


# Generate Figures 1-4 ----------------------------------------------------

# Calculate jitter once so Figures 1 and 2 use identical point positions.
set.seed(852)
cholesterol_plot_data <- ch %>%
  mutate(
    group_x = as.numeric(Treatment),
    jitter_x = group_x + runif(n(), min = -0.075, max = 0.075)
  )

cholesterol_colours <- c(
  Control = bb852$grey,
  Drug = bb852$slate
)

cholesterol_base <- ggplot(
  cholesterol_plot_data,
  aes(x = jitter_x, y = Cholesterol, colour = Treatment)
) +
  geom_point(size = 3.5, alpha = 0.95) +
  scale_colour_manual(values = cholesterol_colours) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Control", "Drug")
  ) +
  scale_y_continuous(breaks = seq(160, 230, by = 10)) +
  coord_cartesian(
    xlim = c(0.55, 2.45),
    ylim = c(160, 235),
    clip = "off"
  ) +
  labs(x = NULL, y = "Cholesterol") +
  guides(colour = "none") +
  theme_bb852(base_family = bb852_font) +
  theme(panel.grid.major.x = element_blank())

figure_1 <- cholesterol_base

figure_2 <- cholesterol_base +
  geom_segment(
    data = cholesterol_means,
    aes(
      x = group_x - 0.20,
      xend = group_x + 0.20,
      y = mean_cholesterol,
      yend = mean_cholesterol,
      colour = Treatment
    ),
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  annotate(
    "segment",
    x = 1.5,
    xend = 1.5,
    y = drug_mean,
    yend = control_mean,
    colour = bb852$coral,
    linewidth = 1.2,
    arrow = grid::arrow(
      ends = "both",
      type = "closed",
      length = grid::unit(0.08, "inches")
    )
  ) +
  annotate(
    "text",
    x = 1,
    y = control_mean + 6,
    label = sprintf("Control mean = %.2f", control_mean),
    colour = bb852$grey,
    family = bb852_font,
    size = 5,
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = 2,
    y = drug_mean - 6,
    label = sprintf("Drug mean = %.2f", drug_mean),
    colour = bb852$slate,
    family = bb852_font,
    size = 5,
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 225,
    label = sprintf(
      "Observed difference\nDrug - Control\n= %.2f",
      observed_difference
    ),
    colour = bb852$coral,
    family = bb852_font,
    size = 5,
    fontface = "bold"
  )

histogram_binwidth <- 2
next_null_difference <- min(
  exact_null_distribution$null_difference[
    exact_null_distribution$null_difference > observed_difference
  ]
)
tail_boundary <- mean(c(observed_difference, next_null_difference))

# The -20 boundary places every value in the exact lower tail in whole bins.
stopifnot(abs(tail_boundary - (-20)) < .Machine$double.eps^0.5)

null_histogram_base <- ggplot(
  exact_null_distribution,
  aes(x = null_difference)
) +
  geom_histogram(
    binwidth = histogram_binwidth,
    boundary = tail_boundary,
    fill = bb852$slate,
    colour = bb852$bg,
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = 0,
    colour = bb852$muted,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  scale_x_continuous(breaks = seq(-30, 30, by = 10)) +
  labs(
    x = "Difference between means\n(Drug - Control)",
    y = "Number of allocations"
  ) +
  theme_bb852(base_family = bb852_font)

histogram_maximum <- max(
  ggplot_build(null_histogram_base)$data[[1]]$count
)
histogram_y_limit <- ceiling(histogram_maximum * 1.28)

null_histogram_base <- null_histogram_base +
  coord_cartesian(
    xlim = c(-31, 31),
    ylim = c(0, histogram_y_limit),
    clip = "off"
  )

figure_3 <- null_histogram_base

figure_4 <- null_histogram_base +
  geom_histogram(
    data = exact_null_distribution %>%
      filter(null_difference <= observed_difference),
    aes(x = null_difference),
    binwidth = histogram_binwidth,
    boundary = tail_boundary,
    fill = bb852$coral,
    colour = bb852$bg,
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_vline(
    xintercept = observed_difference,
    colour = bb852$coral,
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = observed_difference + 0.8,
    y = histogram_maximum * 1.12,
    label = sprintf(
      "Observed difference\nDrug - Control\n= %.2f",
      observed_difference
    ),
    colour = bb852$coral,
    family = bb852_font,
    fontface = "bold",
    size = 5,
    hjust = 0
  ) +
  annotate(
    "text",
    x = -29.5,
    y = histogram_maximum * 0.72,
    label = sprintf(
      "One-sided p-value\n= %d / %d = %.4f",
      number_extreme,
      nrow(exact_null_distribution),
      exact_p_value
    ),
    colour = bb852$coral,
    family = bb852_font,
    size = 5,
    hjust = 0
  )

save_figure_versions(figure_1, "01_cholesterol_data")
save_figure_versions(figure_2, "02_cholesterol_means")
save_figure_versions(figure_3, "03_null_distribution")
save_figure_versions(figure_4, "04_null_distribution_pvalue")


# Simulate data and generate Figure 5 -------------------------------------

# These illustrative data are simulated; each group has SD 10 and the same
# observed difference, while sample size is the only systematic change.
sample_sizes <- c(5, 10, 30)
simulated_effect <- -8
simulated_group_sd <- 10
number_randomisations <- 10000

set.seed(853)
simulated_samples <- lapply(sample_sizes, function(n_per_group) {
  control_residuals <- as.numeric(scale(rnorm(n_per_group))) *
    simulated_group_sd
  drug_residuals <- as.numeric(scale(rnorm(n_per_group))) *
    simulated_group_sd

  data.frame(
    n_per_group = n_per_group,
    Treatment = factor(
      rep(c("Control", "Drug"), each = n_per_group),
      levels = c("Control", "Drug")
    ),
    response = c(
      100 + control_residuals,
      100 + simulated_effect + drug_residuals
    )
  )
})

set.seed(854)
sample_size_null_distributions <- bind_rows(
  lapply(simulated_samples, function(simulated_data) {
    simulated_means <- simulated_data %>%
      group_by(Treatment) %>%
      summarise(group_mean = mean(response), .groups = "drop")

    simulated_observed_difference <-
      simulated_means$group_mean[simulated_means$Treatment == "Drug"] -
      simulated_means$group_mean[simulated_means$Treatment == "Control"]

    randomised_differences <- replicate(
      number_randomisations,
      {
        shuffled_treatment <- sample(simulated_data$Treatment)
        mean(simulated_data$response[shuffled_treatment == "Drug"]) -
          mean(simulated_data$response[shuffled_treatment == "Control"])
      }
    )

    data.frame(
      n_per_group = simulated_data$n_per_group[1],
      null_difference = randomised_differences,
      observed_difference = simulated_observed_difference
    )
  })
)

simulated_observed_differences <- sample_size_null_distributions %>%
  distinct(n_per_group, observed_difference) %>%
  arrange(n_per_group)

null_distribution_spreads <- sample_size_null_distributions %>%
  group_by(n_per_group) %>%
  summarise(
    number_randomisations = n(),
    null_sd = sd(null_difference),
    .groups = "drop"
  ) %>%
  arrange(n_per_group)

stopifnot(
  all(
    abs(
      simulated_observed_differences$observed_difference -
        simulated_effect
    ) < 1e-10
  ),
  all(null_distribution_spreads$number_randomisations == 10000),
  all(diff(null_distribution_spreads$null_sd) < 0)
)

sample_size_x_limit <- ceiling(
  max(abs(sample_size_null_distributions$null_difference))
)
sample_size_x_limits <- c(-sample_size_x_limit, sample_size_x_limit)

density_maxima <- sample_sizes %>%
  vapply(
    function(n_per_group) {
      panel_values <- sample_size_null_distributions$null_difference[
        sample_size_null_distributions$n_per_group == n_per_group
      ]
      max(
        density(
          panel_values,
          from = sample_size_x_limits[1],
          to = sample_size_x_limits[2],
          n = 512
        )$y
      )
    },
    numeric(1)
  )
sample_size_y_limit <- max(density_maxima) * 1.20

sample_size_panels <- lapply(seq_along(sample_sizes), function(panel_index) {
  n_per_group <- sample_sizes[panel_index]
  panel_data <- sample_size_null_distributions %>%
    filter(.data$n_per_group == .env$n_per_group)
  panel_observed_difference <- unique(panel_data$observed_difference)

  ggplot(panel_data, aes(x = null_difference)) +
    geom_density(
      fill = bb852$slate,
      colour = bb852$slate,
      linewidth = 1.2,
      alpha = 0.9,
      n = 512
    ) +
    geom_vline(
      xintercept = panel_observed_difference,
      colour = bb852$coral,
      linewidth = 1.2
    ) +
    annotate(
      "text",
      x = sample_size_x_limits[1] + 0.06 * diff(sample_size_x_limits),
      y = sample_size_y_limit * 0.93,
      label = sprintf("n = %d per group", n_per_group),
      colour = bb852$text,
      family = bb852_font,
      fontface = "bold",
      size = 5,
      hjust = 0
    ) +
    annotate(
      "text",
      x = panel_observed_difference - 0.8,
      y = sample_size_y_limit * 0.34,
      label = sprintf(
        "Observed\n= %.0f",
        panel_observed_difference
      ),
      colour = bb852$coral,
      family = bb852_font,
      size = 4.8,
      hjust = 1,
      vjust = 0.5
    ) +
    scale_x_continuous(
      limits = sample_size_x_limits,
      breaks = scales::breaks_pretty(n = 5)
    ) +
    scale_y_continuous(
      limits = c(0, sample_size_y_limit),
      breaks = scales::breaks_pretty(n = 4),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = "Difference between means",
      y = if (panel_index == 1) "Density" else NULL
    ) +
    theme_bb852(base_family = bb852_font) +
    theme(
      panel.grid.major.x = element_blank(),
      plot.margin = margin(12, 10, 12, 10)
    )
})

figure_5 <- wrap_plots(sample_size_panels, nrow = 1) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bb852$bg, colour = NA)
    )
  )

# The half-width version stacks the three null distributions on a shared
# y-axis, so the narrowing of the distribution with n is directly comparable.
half_sample_size_data <- sample_size_null_distributions %>%
  mutate(
    sample_size_label = factor(
      paste0("n = ", n_per_group, " per group"),
      levels = paste0("n = ", sample_sizes, " per group")
    )
  )

figure_5_half <- ggplot(
  half_sample_size_data,
  aes(x = null_difference)
) +
  geom_density(
    fill = bb852$slate,
    colour = bb852$slate,
    alpha = 0.85,
    linewidth = 1.2,
    n = 512
  ) +
  geom_vline(
    data = distinct(
      half_sample_size_data,
      sample_size_label,
      observed_difference
    ),
    aes(xintercept = observed_difference),
    colour = bb852$coral,
    linewidth = 1.2
  ) +
  facet_wrap(~sample_size_label, ncol = 1) +
  coord_cartesian(xlim = c(-22, 22)) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "Difference between means", y = "Density") +
  theme_bb852(base_family = bb852_font) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(
      colour = bb852$text,
      size = 15,
      face = "bold",
      hjust = 0,
      margin = margin(2, 0, 2, 0)
    ),
    panel.spacing.y = grid::unit(10, "pt"),
    plot.margin = margin(8, 12, 8, 8)
  )

save_figure_versions(
  figure_5,
  "05_sample_size",
  half_plot = figure_5_half
)


# Read and prepare the anorexia data --------------------------------------

anWide <- read.csv("CourseData/anorexiaCBT.csv")

anLong <- anWide %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "time",
    values_to = "weight"
  ) %>%
  mutate(
    time = factor(time, levels = c("Week01", "Week08"))
  ) %>%
  arrange(Subject, time)

paired_data_checks <- anLong %>%
  group_by(Subject) %>%
  summarise(
    number_measurements = n(),
    number_time_points = n_distinct(time),
    has_week_01 = any(time == "Week01"),
    has_week_08 = any(time == "Week08"),
    .groups = "drop"
  )

stopifnot(
  nrow(anLong) == 2 * nrow(anWide),
  all(paired_data_checks$number_measurements == 2),
  all(paired_data_checks$number_time_points == 2),
  all(paired_data_checks$has_week_01),
  all(paired_data_checks$has_week_08),
  !anyNA(anLong$weight)
)


# Generate Figure 6 -------------------------------------------------------

figure_6 <- ggplot(
  anLong,
  aes(x = time, y = weight, group = Subject)
) +
  geom_line(
    colour = bb852$grey,
    linewidth = 1.2,
    alpha = 0.45
  ) +
  geom_point(
    colour = bb852$slate,
    size = 3.2
  ) +
  scale_x_discrete(labels = c(Week01 = "Before", Week08 = "After")) +
  labs(x = "Time", y = "Weight") +
  theme_bb852(base_family = bb852_font) +
  theme(panel.grid.major.x = element_blank())

save_figure_versions(figure_6, "06_paired_observations")


# Run validation checks ---------------------------------------------------

output_stems <- c(
  "01_cholesterol_data",
  "02_cholesterol_means",
  "03_null_distribution",
  "04_null_distribution_pvalue",
  "05_sample_size",
  "06_paired_observations"
)
expected_output_files <- c(
  file.path(full_output_directory, paste0(output_stems, ".png")),
  file.path(half_output_directory, paste0(output_stems, ".png"))
)

stopifnot(
  length(expected_output_files) == 12,
  all(file.exists(expected_output_files)),
  all(file.info(expected_output_files)$size > 0),
  length(list.files(output_root, pattern = "\\.svg$", recursive = TRUE)) == 0
)

message(
  "Created full- and half-width versions of six BB852 randomisation ",
  "figures as PNG files in ", output_root,
  ". All validation checks passed."
)
