# BB852 lecture 08: "Pimp your plots" (from default ggplot to scientific figure)
# -----------------------------------------------------------------------------
# Every figure in the lecture deck, generated from the AnAge data used in the
# course book chapter "Pimping your plots" (2_03-pimpYourPlots.Rmd).
#
# Data: CourseData/anage_data.csv, processed as in the chapter: four classes
# (Amphibia, Aves, Mammalia, Reptilia), mass converted from g to kg, rows with
# missing or non-positive mass or BMR dropped (n = 627 species).
#
# Unlike the other lecture generators, most figures here deliberately show
# ggplot's own themes and palettes, because comparing them is the point of the
# lecture. The BB852 colours are used only for labels, the colour-guide and
# the colour-vision figures. Plots are drawn on white, like a figure in a paper.
#
# Two fixes compared with the earlier version of this script:
#   * Rows are drawn largest class first, so the 171 birds are no longer
#     hidden under the 422 mammals.
#   * Log axes get plain-number tick labels (0.001, 1, 1000). label_number()
#     produced "1 000.000" and "10.000", which reads as ten thousand in Danish.
#
# Run from the repository root:
# source("lecture_figures/pimp_your_plots/generate_figures.R")
#
# Each figure is written as a full- and half-width PNG (300 dpi), and also at
# the size of the slide placeholder it is used in (takeaway, cover), into
# lecture_figures/pimp_your_plots/output/<layout>/.
#
# Packages: ggplot2, dplyr, patchwork, scales, ragg, systemfonts, colorspace,
# ggthemes, png.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(patchwork)
library(ragg)


# BB852 colours, fonts and export -----------------------------------------

bb852 <- list(
  bg    = "#FBFAF6",
  text  = "#1B2230",
  muted = "#6B6E75",
  grid  = "#DDE2E8",
  slate = "#24405F",
  coral = "#D0745A",
  teal  = "#2A7F76",
  ochre = "#C4932F"
)

available_fonts <- unique(systemfonts::system_fonts()$family)
pick_font <- function(candidates, fallback) {
  hit <- candidates[candidates %in% available_fonts][1]
  if (is.na(hit)) fallback else hit
}
# Avenir Next on the teaching Mac; fallbacks only for other machines.
bb852_font <- pick_font(c("Avenir Next", "Helvetica", "Inter"), "sans")
code_font  <- pick_font(c("Menlo", "DejaVu Sans Mono"), "mono")

# Sizes (inches) match the figure placeholders in BB852_Theme.pptx.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "pimp_your_plots", "output")
unlink(output_root, recursive = TRUE)

save_layout <- function(plot, filename, layout, bg = "white") {
  size <- bb852_sizes[[layout]]
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(output_directory, paste0(filename, ".png")), plot,
         width = size[1], height = size[2], units = "in", dpi = 300,
         device = ragg::agg_png, bg = bg)
  invisible(filename)
}

# Every content figure goes to full and half (required by generate_all.R),
# plus the layout actually used on the slide when that is different.
save_bb852 <- function(plot, filename, slide_layout = "full", bg = "white") {
  for (layout in unique(c("full", "half", slide_layout))) {
    save_layout(plot, filename, layout, bg = bg)
  }
}


# Data (same processing as the course book chapter) ------------------------

anage <- read.csv("CourseData/anage_data.csv") |>
  rename(Mass = "Body.mass..g.", BMR = "Metabolic.rate..W.") |>
  filter(Class %in% c("Aves", "Amphibia", "Mammalia", "Reptilia")) |>
  mutate(Mass = Mass / 1000, BMRperKg = BMR / Mass)

# The deliberately poor plot keeps the file's own row order (Amphibia, Aves,
# Mammalia, Reptilia), which is what students get by default: mammals on top.
default_order <- anage |>
  filter(is.finite(Mass), is.finite(BMR), Mass > 0, BMR > 0)

# All other plots draw the largest class first so the smaller ones stay
# visible.
plot_dat <- default_order |>
  mutate(n_class = n(), .by = Class) |>
  arrange(desc(n_class))

stopifnot(nrow(plot_dat) == 627)


# Shared pieces ------------------------------------------------------------

# Okabe-Ito colours (the same set as ggthemes::scale_colour_colorblind(),
# used in the course book), one per class.
class_cols <- c(
  Amphibia = "#CC79A7",
  Aves     = "#E69F00",
  Mammalia = "#0072B2",
  Reptilia = "#009E73"
)
class_shapes <- c(Amphibia = 17, Aves = 15, Mammalia = 16, Reptilia = 18)

# Plain-number tick labels for log axes: 0.001, 0.1, 1, 10, 1000.
plain_number <- function(x) {
  out <- formatC(x, format = "f", digits = 4)
  out <- sub("0+$", "", out)
  sub("[.]$", "", out)
}
bmr_breaks  <- 10^(-4:3)

lab_mass <- "Adult body mass (kg)"
lab_bmr  <- "Basal metabolic rate (W)"

# Mass ticks every decade on wide plots, every second decade on narrow
# panels where 0.001 and 0.01 would collide.
log_axes <- function(mass_step = 1) {
  list(
    scale_x_log10(breaks = 10^seq(-3, 3, by = mass_step),
                  labels = plain_number),
    scale_y_log10(breaks = bmr_breaks, labels = plain_number)
  )
}

# The "good" plot used on several slides.
make_good_plot <- function(base_size = 16, point_size = 2, mass_step = 1,
                           legend_rows = 1) {
  ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
    geom_point(alpha = 0.7, size = point_size) +
    log_axes(mass_step) +
    scale_colour_manual(values = class_cols) +
    guides(colour = guide_legend(nrow = legend_rows)) +
    labs(x = lab_mass, y = lab_bmr, colour = NULL) +
    theme_classic(base_size = base_size, base_family = bb852_font) +
    theme(
      legend.position = "top",
      axis.text = element_text(colour = "grey25")
    )
}

# Small bold panel labels ("Avoid", "Prefer", panel names).
panel_label <- function(colour = bb852$text, family = bb852_font, size = 15) {
  theme(plot.title = element_text(colour = colour, family = family, size = size,
                                  face = "bold", hjust = 0,
                                  margin = margin(b = 6)))
}


# 00 Cover: the improved plot at title-slide size --------------------------

cover_plot <- make_good_plot(base_size = 18, point_size = 2.4, mass_step = 2,
                             legend_rows = 2) +
  theme(plot.margin = margin(30, 24, 30, 18))
save_layout(cover_plot, "00_cover", "cover")


# 01 Deliberately poor plot ------------------------------------------------
# Raw scales (one elephant squashes everything), variable names, a title that
# repeats the axes, default colours that fail for colour-vision deficiency,
# oversized points, and birds hidden under mammals.

bad_plot <- ggplot(default_order, aes(Mass, BMR, colour = Class)) +
  geom_point(size = 4) +
  labs(title = "BMR vs Mass") +
  theme_gray(base_size = 14)

save_bb852(bad_plot, "01_bad_plot", "takeaway")


# 02 Improved plot ---------------------------------------------------------

save_bb852(make_good_plot(), "02_good_plot", "takeaway")


# 03 Raw versus log scales --------------------------------------------------

p_raw <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_colour_manual(values = class_cols) +
  labs(x = lab_mass, y = lab_bmr, title = "Raw scales") +
  theme_classic(base_size = 15, base_family = bb852_font) +
  theme(legend.position = "none") +
  panel_label()

p_log <- p_raw + log_axes(2) + labs(title = "Log scales")

save_bb852(p_raw + p_log, "03_raw_vs_log_scales", "full")


# 04 Variable names versus reader-facing labels ----------------------------

p_names_bad <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = class_cols) +
  labs(title = "Avoid") +
  theme_classic(base_size = 15, base_family = bb852_font) +
  panel_label(colour = bb852$coral)

p_names_good <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.7, size = 2) +
  log_axes(2) +
  scale_colour_manual(values = class_cols) +
  labs(x = lab_mass, y = lab_bmr, colour = NULL, title = "Prefer") +
  theme_classic(base_size = 15, base_family = bb852_font) +
  panel_label(colour = bb852$teal)

save_bb852(p_names_bad + p_names_good, "04_axis_label_comparison", "full")


# 05 Purposeful aesthetics: class shown by colour AND shape ----------------
# Redundant encoding, so the groups survive greyscale printing and
# colour-vision deficiency.

p_aesthetics <- ggplot(plot_dat,
                       aes(Mass, BMR, colour = Class, shape = Class)) +
  geom_point(alpha = 0.75, size = 2.4) +
  log_axes() +
  scale_colour_manual(values = class_cols) +
  scale_shape_manual(values = class_shapes) +
  labs(x = lab_mass, y = lab_bmr, colour = NULL, shape = NULL) +
  theme_classic(base_size = 16, base_family = bb852_font) +
  theme(legend.position = "top")

save_bb852(p_aesthetics, "05_purposeful_aesthetics", "takeaway")


# 06 Three kinds of colour scale -------------------------------------------
# Examples tied to the course data: class (categorical), body mass
# (sequential), residual from the mass-BMR line (diverging).

scale_strip_theme <- function() {
  theme_void(base_size = 15, base_family = bb852_font) +
    theme(
      axis.text.x   = element_text(size = 12, colour = bb852$text,
                                   margin = margin(t = 4)),
      legend.position = "none",
      plot.title    = element_text(face = "bold", colour = bb852$text,
                                   margin = margin(b = 2)),
      plot.subtitle = element_text(size = 12, colour = bb852$muted,
                                   margin = margin(b = 6)),
      plot.margin   = margin(6, 12, 6, 12)
    )
}

cat_strip <- ggplot(tibble(Class = factor(names(class_cols)), y = 1),
                    aes(Class, y, fill = Class)) +
  geom_tile(width = 0.92, height = 0.8) +
  scale_fill_manual(values = class_cols) +
  labs(title = "Categorical", subtitle = "Groups with no order (class)") +
  scale_strip_theme()

seq_strip <- ggplot(tibble(x = seq(0, 1, length.out = 101), y = 1),
                    aes(x, y, fill = x)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = c(0, 1), labels = c("Low", "High")) +
  labs(title = "Sequential", subtitle = "Low to high (body mass)") +
  scale_strip_theme()

div_strip <- ggplot(tibble(x = seq(-1, 1, length.out = 201), y = 1),
                    aes(x, y, fill = x)) +
  geom_tile() +
  scale_fill_gradient2(low = "#0072B2", mid = "#F7F7F7", high = "#D55E00") +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Below", "On the line", "Above")) +
  labs(title = "Diverging",
       subtitle = "Either side of a meaningful midpoint (residual)") +
  scale_strip_theme()

colour_guide <- cat_strip / seq_strip / div_strip

save_bb852(colour_guide, "06_colour_scale_types", "half", bg = bb852$bg)


# 07 Colour-vision deficiency simulation -----------------------------------
# ggplot's default four-colour palette versus Okabe-Ito, as seen with normal
# vision, the three types of dichromacy, and in greyscale (colorspace).

default_cols <- scales::hue_pal()(4)
okabe_cols   <- unname(class_cols)

vision <- list(
  "Normal vision" = function(x) x,
  "Deuteranopia"  = colorspace::deutan,
  "Protanopia"    = colorspace::protan,
  "Tritanopia"    = colorspace::tritan,
  "Greyscale"     = colorspace::desaturate
)

cvd_dat <- bind_rows(lapply(names(vision), function(v) {
  bind_rows(
    tibble(palette = "ggplot default", swatch = 1:4,
           fill = vision[[v]](default_cols)),
    tibble(palette = "Okabe-Ito", swatch = 1:4,
           fill = vision[[v]](okabe_cols))
  ) |> mutate(vision = v)
})) |>
  mutate(
    vision  = factor(vision, levels = names(vision)),
    palette = factor(palette, levels = c("ggplot default", "Okabe-Ito"))
  )

cvd_plot <- ggplot(cvd_dat, aes(swatch, 1, fill = fill)) +
  geom_tile(width = 0.9, height = 0.9) +
  scale_fill_identity() +
  facet_grid(palette ~ vision, switch = "y") +
  coord_equal() +
  theme_void(base_size = 15, base_family = bb852_font) +
  theme(
    strip.text.x = element_text(colour = bb852$text, face = "bold",
                                margin = margin(b = 6)),
    strip.text.y.left = element_text(colour = bb852$text, face = "bold",
                                     angle = 0, hjust = 1,
                                     margin = margin(r = 10)),
    panel.spacing.x = unit(14, "pt"),
    panel.spacing.y = unit(10, "pt"),
    plot.margin = margin(8, 12, 8, 8)
  )

save_bb852(cvd_plot, "07_colour_vision_simulation", "full", bg = bb852$bg)


# 08 Clutter: default, bare, light guides ----------------------------------

base_plot <- ggplot(plot_dat, aes(Mass, BMR, colour = Class)) +
  geom_point(alpha = 0.7, size = 1.8) +
  log_axes(2) +
  scale_colour_manual(values = class_cols) +
  labs(x = lab_mass, y = lab_bmr)

p_default <- base_plot +
  labs(title = "Default grey panel") +
  theme_gray(base_size = 14, base_family = bb852_font)

p_bare <- base_plot +
  labs(title = "Bare") +
  theme_classic(base_size = 14, base_family = bb852_font)

p_guides <- base_plot +
  labs(title = "Bare + light gridlines") +
  theme_classic(base_size = 14, base_family = bb852_font) +
  theme(panel.grid.major = element_line(colour = "grey88", linewidth = 0.4))

clutter <- (p_default + p_bare + p_guides) &
  theme(legend.position = "none") &
  panel_label(size = 14)

save_bb852(clutter, "08_clutter_comparison", "full")


# 09 Theme montage (built-in themes plus two from ggthemes) ----------------

theme_panel <- function(th, name) {
  base_plot + th + ggtitle(name) +
    theme(legend.position = "none",
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 6),
          plot.title = element_text(family = code_font, size = 10,
                                    face = "plain", hjust = 0))
}

theme_montage <- wrap_plots(
  theme_panel(theme_gray(),                "theme_gray()"),
  theme_panel(theme_bw(),                  "theme_bw()"),
  theme_panel(theme_classic(),             "theme_classic()"),
  theme_panel(theme_minimal(),             "theme_minimal()"),
  theme_panel(theme_dark(),                "theme_dark()"),
  theme_panel(theme_void(),                "theme_void()"),
  theme_panel(ggthemes::theme_economist(), "theme_economist()"),
  theme_panel(ggthemes::theme_excel_new(), "theme_excel_new()"),
  ncol = 4
)

save_bb852(theme_montage, "09_theme_montage", "takeaway")


# 10 Consistent multi-panel figure -----------------------------------------

p_bmr <- make_good_plot(base_size = 14, point_size = 1.6, mass_step = 2)

p_per_kg <- ggplot(plot_dat, aes(Mass, BMRperKg, colour = Class)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_x_log10(breaks = 10^seq(-3, 3, by = 2), labels = plain_number) +
  scale_y_log10(breaks = 10^(-2:2), labels = plain_number) +
  scale_colour_manual(values = class_cols) +
  labs(x = lab_mass, y = "BMR per kg (W/kg)", colour = NULL) +
  theme_classic(base_size = 14, base_family = bb852_font)

multi_panel <- (p_bmr + p_per_kg) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold", family = bb852_font))

save_bb852(multi_panel, "10_multi_panel", "takeaway")


# 11 Design for the final destination --------------------------------------
# The same plot shown at the same final width (an 8 cm journal column):
# left, made at 25 cm wide and then shrunk; right, saved at 8 cm directly.
# Both use 11 pt text. At 8 cm the legend needs two rows to fit, which is
# part of the message: check the layout at the final size.

column_plot <- make_good_plot(base_size = 11, point_size = 1.2,
                              mass_step = 2, legend_rows = 2)

render_at <- function(plot, width_cm, height_cm) {
  file <- tempfile(fileext = ".png")
  ggsave(file, plot, width = width_cm, height = height_cm, units = "cm",
         dpi = 300, device = ragg::agg_png, bg = "white")
  png::readPNG(file)
}

shrunk  <- render_at(column_plot, 25, 18.75)
at_size <- render_at(column_plot, 8, 6)

image_panel <- function(img, title, colour) {
  wrap_elements(full = grid::rasterGrob(img, interpolate = TRUE)) +
    ggtitle(title) +
    theme(plot.title = element_text(family = bb852_font, size = 15,
                                    face = "bold", colour = colour,
                                    hjust = 0.5))
}

destination <- image_panel(shrunk, "Made at 25 cm, shrunk to 8 cm",
                           bb852$coral) +
  image_panel(at_size, "Saved at 8 cm", bb852$teal)

save_bb852(destination, "11_final_destination", "full", bg = bb852$bg)


# Checks ------------------------------------------------------------------

full_files <- list.files(file.path(output_root, "full"), pattern = "[.]png$")
half_files <- list.files(file.path(output_root, "half"), pattern = "[.]png$")
stopifnot(length(full_files) == 11, setequal(full_files, half_files))

message("Created 11 pimp-your-plots figures (full, half, takeaway) and a ",
        "cover in ", output_root, ".")
