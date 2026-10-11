# BB852 — Pimp your plots
# Figure code for the improved lecture deck.


library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)
library(viridis)

plot_dir <- file.path("scripts", "pimp_your_plots_figures")
dir.create(plot_dir, showWarnings = FALSE)

# Load data
anage <- read.csv("CourseData/anage_data.csv") |>
  janitor::clean_names() |>
  rename(BMR = metabolic_rate_w, Class = class) |>
  mutate(Mass = body_mass_g / 1000)

names(anage)

plot_dat <- anage |>
  filter(is.finite(Mass), is.finite(BMR),
         Mass > 0, BMR > 0, !is.na(Class))

# FIGURE 1 — deliberately poor plot
bad_plot <- ggplot(plot_dat, aes(Mass, BMR)) +
  geom_point(size = 4) +
  labs(title = "BMR vs Mass", x = "Mass", y = "BMR") +
  theme_gray(base_size = 16)
bad_plot

# The oversized canvas makes the labels look too small when the image is
# scaled to fit a slide.
ggsave(
  file.path(plot_dir, "01_bad_plot.png"), bad_plot,
  width = 40, height = 28, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 2 — improved plot
good_plot <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.65, size = 2) +
  scale_x_log10(labels = label_number()) +
  scale_y_log10(labels = label_number()) +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Adult body mass (kg)",
       y = "Basal metabolic rate (W)",
       colour = "Class") +
  theme_classic(base_size = 16) +
  theme(legend.position = "top")
good_plot

ggsave(
  file.path(plot_dir, "02_good_plot.png"), good_plot,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 3 — raw versus log scales
p_raw <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Adult body mass (kg)",
       y = "Basal metabolic rate (W)",
       colour = "Class") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")

p_log <- p_raw + scale_x_log10() + scale_y_log10()
scale_comparison <- p_raw + p_log + plot_annotation(tag_levels = "A")
scale_comparison

ggsave(
  file.path(plot_dir, "03_raw_vs_log_scales.png"), scale_comparison,
  width = 28, height = 12, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 4 — variable names versus reader-facing labels
p_names_bad <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Mass", y = "BMR", colour = "Class") +
  theme_classic(base_size = 15)

p_names_good <- p_names_bad +
  labs(x = "Adult body mass (kg)",
       y = "Basal metabolic rate (W)",
       colour = "Class")

label_comparison <- p_names_bad + p_names_good
label_comparison

ggsave(
  file.path(plot_dir, "04_axis_label_comparison.png"), label_comparison,
  width = 28, height = 12, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 5 — purposeful aesthetics
p_aesthetics <- ggplot(plot_dat,
                       aes(Mass, BMR, colour = Class, shape = Class)) +
  geom_point(alpha = 0.65, size = 2.2) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Adult body mass (kg)",
       y = "Basal metabolic rate (W)",
       colour = "Class", shape = "Class") +
  theme_classic(base_size = 16) +
  theme(legend.position = "top")
p_aesthetics

ggsave(
  file.path(plot_dir, "05_purposeful_aesthetics.png"), p_aesthetics,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 6 — categorical, sequential and diverging colour
set.seed(852)

cat_dat <- tibble(
  x = rep(1:20, 3),
  y = c(rnorm(20, 2), rnorm(20, 4), rnorm(20, 6)),
  group = rep(c("A", "B", "C"), each = 20)
)
seq_dat <- tibble(x = runif(80), y = runif(80),
                  value = runif(80, 0, 100))
div_dat <- tibble(x = runif(80), y = runif(80),
                  effect = rnorm(80))

p_cat <- ggplot(cat_dat, aes(x, y, colour = group)) +
  geom_point(size = 2) +
  scale_colour_viridis_d() +
  labs(title = "Categorical", colour = NULL) +
  theme_classic(base_size = 13)

p_seq <- ggplot(seq_dat, aes(x, y, colour = value)) +
  geom_point(size = 2) +
  scale_colour_viridis_c() +
  labs(title = "Sequential", colour = "Value") +
  theme_classic(base_size = 13)

p_div <- ggplot(div_dat, aes(x, y, colour = effect)) +
  geom_point(size = 2) +
  scale_colour_gradient2(midpoint = 0) +
  labs(title = "Diverging", colour = "Effect") +
  theme_classic(base_size = 13)

colour_scale_comparison <- p_cat + p_seq + p_div
colour_scale_comparison

ggsave(
  file.path(plot_dir, "06_colour_scale_types.png"), colour_scale_comparison,
  width = 32, height = 11, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 6B — colour scales matched to the meaning of the variable
categorical_scale <- tibble(
  habitat = factor(
    c("Forest", "Grassland", "Wetland", "Urban", "Cropland"),
    levels = c("Forest", "Grassland", "Wetland", "Urban", "Cropland")
  ),
  y = 1
)

sequential_scale <- tibble(
  value = seq(0, 100, length.out = 101),
  y = 1
)

diverging_scale <- tibble(
  difference = seq(-100, 100, length.out = 201),
  y = 1
)

p_cat_b <- ggplot(categorical_scale, aes(habitat, y, fill = habitat)) +
  geom_tile(width = 0.9, height = 0.7) +
  scale_fill_manual(values = c(
    Forest = "#009E73",
    Grassland = "#E69F00",
    Wetland = "#56B4E9",
    Urban = "#CC79A7",
    Cropland = "#D55E00"
  )) +
  scale_y_continuous(limits = c(0.5, 1.5), breaks = NULL) +
  labs(
    title = "Categorical",
    subtitle = "Distinct groups; no ranking",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    axis.text.x = element_text(
      size = 10, colour = "grey20", margin = margin(t = 6)
    ),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    plot.margin = margin(5, 18, 5, 8)
  )

p_seq_b <- ggplot(sequential_scale, aes(value, y, fill = value)) +
  geom_tile(width = 1.05, height = 0.7) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  scale_x_continuous(
    breaks = c(0, 50, 100),
    labels = c("Low", "Middle", "High"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(limits = c(0.5, 1.5), breaks = NULL) +
  labs(
    title = "Sequential",
    subtitle = "Ordered magnitude: low to high",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    axis.text.x = element_text(
      size = 10, colour = "grey20", margin = margin(t = 6)
    ),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    plot.margin = margin(5, 18, 5, 8)
  )

p_div_b <- ggplot(diverging_scale, aes(difference, y, fill = difference)) +
  geom_tile(width = 1.05, height = 0.7) +
  geom_segment(
    x = 0, xend = 0, y = 0.58, yend = 1.42,
    colour = "grey20", linewidth = 0.35,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient2(
    low = "#0072B2", mid = "#F7F7F7", high = "#D55E00",
    midpoint = 0, guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(-100, 0, 100),
    labels = c("Below", "Reference", "Above"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(limits = c(0.5, 1.5), breaks = NULL) +
  labs(
    title = "Diverging",
    subtitle = "Departure from a meaningful reference",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    axis.text.x = element_text(
      size = 10, colour = "grey20", margin = margin(t = 6)
    ),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    plot.margin = margin(5, 8, 5, 8)
  )

colour_scale_guide <- p_cat_b + p_seq_b + p_div_b +
  plot_layout(nrow = 1) +
  plot_annotation(
    title = "Match the colour scale to the meaning of the variable",
    subtitle = paste(
      "First ask whether colour represents groups, ordered magnitude,",
      "or departures from a meaningful reference."
    ),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12, colour = "grey35")
    )
  )
colour_scale_guide

ggsave(
  file.path(plot_dir, "06B_colour_scale_types.png"), colour_scale_guide,
  width = 33, height = 12, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 6C — vertically stacked version for presentation slides
colour_scale_guide_vertical <- (p_cat_b / p_seq_b / p_div_b) +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Match the colour scale\nto the meaning of the variable",
    subtitle = paste0(
      "Ask whether colour represents groups, ordered magnitude,\n",
      "or departures from a meaningful reference."
    ),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", lineheight = 1.05),
      plot.subtitle = element_text(size = 12, colour = "grey35")
    )
  )
colour_scale_guide_vertical

ggsave(
  file.path(plot_dir, "06C_colour_scale_types_vertical.png"),
  colour_scale_guide_vertical,
  width = 18, height = 24, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 7 — clutter/theme comparison
base_plot <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.6, size = 1.8) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Adult body mass (kg)",
       y = "Basal metabolic rate (W)",
       colour = "Class")

p_default <- base_plot +
  theme_gray(base_size = 13) +
  labs(title = "Default")

p_minimal <- base_plot +
  theme_classic(base_size = 13) +
  labs(title = "Minimal")

p_guides <- base_plot +
  theme_classic(base_size = 13) +
  theme(panel.grid.major.y = element_line(linewidth = 0.35),
        legend.position = "top") +
  labs(title = "Minimal + useful guides")

theme_comparison <- p_default + p_minimal + p_guides
theme_comparison

ggsave(
  file.path(plot_dir, "07_theme_comparison.png"), theme_comparison,
  width = 32, height = 11, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 8 — theme montage
theme_montage <- wrap_plots(
  base_plot + theme_gray() + ggtitle("Gray"),
  base_plot + theme_bw() + ggtitle("BW"),
  base_plot + theme_light() + ggtitle("Light"),
  base_plot + theme_dark() + ggtitle("Dark"),
  base_plot + theme_classic() + ggtitle("Classic"),
  base_plot + theme_minimal() + ggtitle("Minimal"),
  base_plot + theme_linedraw() + ggtitle("Linedraw"),
  base_plot + theme_void() + ggtitle("Void"),
  base_plot + ggthemes::theme_solarized_2(light = FALSE) +
    ggtitle("Solarized dark") +
    theme(
      text = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.title = element_text(colour = "white"),
      plot.title = element_text(colour = "white")
    ),
  ncol = 3
) &
  theme(
    plot.title = element_text(size = 10),
    legend.position = "none"
  )
theme_montage

ggsave(
  file.path(plot_dir, "08_theme_montage.png"), theme_montage,
  width = 30, height = 24, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 8B — selected themes from the ggthemes package
ggthemes_montage <- wrap_plots(
  base_plot + ggthemes::theme_clean() + ggtitle("Clean"),
  base_plot + ggthemes::theme_few() + ggtitle("Few"),
  base_plot + ggthemes::theme_tufte() + ggtitle("Tufte"),
  base_plot + ggthemes::theme_economist() + ggtitle("Economist"),
  base_plot + ggthemes::theme_fivethirtyeight() +
    ggtitle("FiveThirtyEight"),
  base_plot + ggthemes::theme_excel_new() + ggtitle("Excel new"),
  base_plot + ggthemes::theme_gdocs() + ggtitle("Google Docs"),
  base_plot + ggthemes::theme_wsj() + ggtitle("Wall Street Journal"),
  base_plot + ggthemes::theme_stata() + ggtitle("Stata"),
  ncol = 3
) &
  theme(
    plot.title = element_text(size = 10),
    legend.position = "none"
  )
ggthemes_montage

ggsave(
  file.path(plot_dir, "08B_ggthemes_montage.png"), ggthemes_montage,
  width = 30, height = 24, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 9 — coherent multi-panel figure
p_bmr <- base_plot + theme_classic(base_size = 14)

p_mass_specific <- plot_dat |>
  mutate(BMR_per_kg = BMR / Mass) |>
  ggplot(aes(Mass, BMR_per_kg, colour = Class)) +
  geom_point(alpha = 0.6, size = 1.8) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Adult body mass (kg)",
       y = "Mass-specific BMR (W/kg)",
       colour = "Class") +
  theme_classic(base_size = 14)

multi_panel <- (p_bmr / p_mass_specific) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")
multi_panel

ggsave(
  file.path(plot_dir, "09_multi_panel.png"), multi_panel,
  width = 16, height = 22, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 10 — same figure at two output sizes
publication_plot <- good_plot

ggsave(
  file.path(plot_dir, "10_publication_wide_18cm.png"), publication_plot,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)

ggsave(
  file.path(plot_dir, "10_publication_column_8cm.png"), publication_plot,
  width = 8, height = 6, units = "cm", dpi = 300, bg = "white"
)

# FIGURE 11 — deliberately poor plot for final student challenge
challenge_plot <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(size = 4, alpha = 0.35) +
  labs(
    title = "Relationship between BMR and body mass in the AnAge dataset",
    subtitle = "A plot for students to rescue",
    x = "Mass", y = "BMR",
    colour = "Taxonomic Class"
  ) +
  theme_gray(base_size = 17) +
  theme(
    panel.grid.major = element_line(linewidth = 1.2),
    panel.grid.minor = element_line(linewidth = 0.8),
    legend.position = "right"
  )
challenge_plot

ggsave(
  file.path(plot_dir, "11_student_challenge.png"), challenge_plot,
  width = 18, height = 12, units = "cm", dpi = 300, bg = "white"
)
