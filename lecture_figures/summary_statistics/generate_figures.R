# BB852 summary statistics lecture (lecture 07): R code for every figure
# ----------------------------------------------------------------------
# "From biological variation to statistical uncertainty": summary statistics,
# distributions and probability. Accompanies book chapter 11
# (2_02-distributionsSummaryStats.Rmd).
#
# ALL DATA HERE ARE SIMULATED, with seeds set, to illustrate concepts. The
# running example is a hypothetical moth light-trap survey:
#   - forewing length: normal, mean 28 mm, SD 2 mm (illustrative values)
#   - moths per trap-night: negative binomial, mean 4.5, size 1.5 (right-skewed,
#     more variable than Poisson)
#   - larval survival: probability 0.7, 20 larvae
#   - population growth: yearly growth rates 1.6, 0.7, 1.6 from N = 100
# No real survey data are used.
#
# Run from the repository root:
# source("lecture_figures/summary_statistics/generate_figures.R")
#
# Each content figure is written as a full- and half-width PNG (300 dpi) into
# lecture_figures/summary_statistics/output/<layout>/. Figures used on
# "Title, Bullets & Photo" slides are also written at takeaway size, and the
# title-slide graphic is written to output/cover/. The numbers quoted on the
# slides are written to output/text/slide_numbers.txt, so the slides cannot
# drift from the code.
#
# Labels use plotmath (e.g. mu, sigma, lambda) rather than typed Unicode
# symbols, so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(patchwork)
library(ragg)


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
  panel    = "#EDEFF1",
  cover    = "#2C4D70"
)

available_fonts <- unique(systemfonts::system_fonts()$family)
# Avenir Next on the Mac used for teaching; fallbacks only for other machines.
bb852_font <- c("Avenir Next", "Helvetica", "Inter")[
  c("Avenir Next", "Helvetica", "Inter") %in% available_fonts][1]
if (is.na(bb852_font)) bb852_font <- "sans"

# Menlo for code shown inside figures (falls back to a monospace font).
code_font <- c("Menlo", "DejaVu Sans Mono")[
  c("Menlo", "DejaVu Sans Mono") %in% available_fonts][1]
if (is.na(code_font)) code_font <- "mono"

theme_bb852 <- function(base_size = 18, base_family = bb852_font) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background   = element_rect(fill = bb852$bg, colour = NA),
      panel.background  = element_rect(fill = bb852$bg, colour = NA),
      panel.grid.major  = element_line(colour = bb852$grid, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(colour = bb852$text, size = base_size),
      axis.text         = element_text(colour = bb852$muted, size = base_size - 2),
      axis.ticks        = element_blank(),
      legend.position   = "none",
      strip.text        = element_text(colour = bb852$text, size = base_size - 2,
                                       face = "bold", hjust = 0),
      plot.title        = element_blank(),
      plot.subtitle     = element_blank(),
      plot.caption      = element_blank(),
      plot.margin       = margin(12, 16, 12, 12)
    )
}

# Panel headings inside patchwork figures use a bold "tag"-style title.
# These are panel labels, not plot titles: the slide title carries the message.
panel_label <- function() {
  theme(plot.title = element_text(colour = bb852$text, size = 16, face = "bold",
                                  hjust = 0, margin = margin(b = 6)))
}

# Sizes (inches) match the figure placeholders in BB852_Theme.pptx.
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  third    = c(3.64, 5.30),
  slide    = c(13.33, 7.50),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "summary_statistics", "output")
unlink(output_root, recursive = TRUE)

save_bb852 <- function(plot, filename, layout = "full", bg = bb852$bg) {
  size <- bb852_sizes[[layout]]
  if (is.null(size)) stop("layout must be one of: ", paste(names(bb852_sizes), collapse = ", "))
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(output_directory, paste0(filename, ".png")), plot,
         width = size[1], height = size[2], units = "in", dpi = 300,
         device = ragg::agg_png, bg = bg)
  invisible(filename)
}

# Save one content figure at every layout it is needed for. Every content
# figure gets full and half versions (checked by generate_all.R).
save_all <- function(make_plot, filename, extra = character()) {
  for (layout in unique(c("full", "half", extra))) {
    save_bb852(make_plot(layout), filename, layout)
  }
}

slide_numbers <- character()
note <- function(label, value) {
  slide_numbers <<- c(slide_numbers, sprintf("%-55s %s", label, value))
}


# Shared simulation parameters --------------------------------------------

wing_mean <- 28   # mm, illustrative population mean forewing length
wing_sd   <- 2    # mm, illustrative population SD
n_small   <- 16   # sample sizes used throughout section 4
n_large   <- 64


# Cover: repeated samples from one population (title slide) ----------------

set.seed(7)
cover_df <- do.call(rbind, lapply(1:14, function(i) {
  data.frame(sample = i, length = rnorm(n_small, wing_mean, wing_sd))
}))
cover_means <- aggregate(length ~ sample, data = cover_df, FUN = mean)

p_cover <- ggplot(cover_df, aes(x = length, y = sample)) +
  geom_vline(xintercept = wing_mean, colour = "#FBFAF6", linewidth = 0.6,
             linetype = "dashed", alpha = 0.6) +
  geom_point(colour = bb852$light, alpha = 0.55, size = 2.6) +
  geom_point(data = cover_means, colour = bb852$coral, shape = 18, size = 7) +
  coord_cartesian(xlim = c(22, 34)) +
  theme_void() +
  theme(plot.background = element_rect(fill = bb852$cover, colour = NA),
        plot.margin = margin(60, 30, 60, 30))
save_bb852(p_cover, "slide_01_cover", layout = "cover", bg = bb852$cover)


# Slide 4: target population, sampling frame, observed sample --------------

make_frame <- function(layout) {
  boxes <- data.frame(
    x = c(1, 2.3, 3.6),
    head = c("TARGET POPULATION", "SAMPLING FRAME", "OBSERVED SAMPLE"),
    body = c("All adult moths of the\nspecies at the site in June",
             "Moths that a light trap\ncan attract",
             "Moths actually caught\nand measured"),
    fill = c(bb852$panel, bb852$panel, bb852$panel)
  )
  w <- 0.55; s <- if (layout == "half") 9.5 else 17
  ggplot(boxes) +
    geom_rect(aes(xmin = x - w, xmax = x + w, ymin = 0.15, ymax = 1.85),
              fill = bb852$panel, colour = NA) +
    geom_rect(aes(xmin = x - w, xmax = x + w, ymin = 1.6, ymax = 1.85),
              fill = c(bb852$slate, bb852$slate, bb852$coral), colour = NA) +
    geom_text(aes(x = x, y = 1.725, label = head), colour = "white",
              fontface = "bold", size = s / .pt * 0.85, family = bb852_font) +
    geom_text(aes(x = x, y = 0.9, label = body), colour = bb852$text,
              size = s / .pt, family = bb852_font, lineheight = 1.05) +
    annotate("segment", x = c(1, 2.3) + w + 0.06, xend = c(2.3, 3.6) - w - 0.06,
             y = 1, yend = 1, colour = bb852$grey, linewidth = 1,
             arrow = arrow(length = unit(0.12, "in"), type = "closed")) +
    coord_cartesian(xlim = c(0.4, 4.2), ylim = c(0.1, 1.9), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = bb852$bg, colour = NA))
}
save_all(make_frame, "slide_04_sampling_frame")


# Slide 7: a distribution of wing lengths ---------------------------------

set.seed(11)
wings <- data.frame(length = rnorm(100, wing_mean, wing_sd))
note("Slide 7: n moths in histogram", nrow(wings))
note("Slide 7: sample mean (mm)", round(mean(wings$length), 1))
note("Slide 7: sample SD (mm)", round(sd(wings$length), 1))

make_hist <- function(layout) {
  ggplot(wings, aes(x = length)) +
    geom_histogram(binwidth = 1, boundary = 0, fill = bb852$slate,
                   colour = bb852$bg, linewidth = 0.4) +
    geom_rug(colour = bb852$light, alpha = 0.7, length = unit(0.06, "npc")) +
    scale_x_continuous(breaks = seq(20, 36, 2)) +
    scale_y_continuous(expand = expansion(mult = c(0.07, 0.05))) +
    labs(x = "Forewing length (mm)", y = "Number of moths") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank())
}
save_all(make_hist, "slide_07_wing_histogram")


# Slides 8 and 11: skewed counts of moths per trap-night ------------------

set.seed(13)  # chosen (and disclosed): mean near 4.5, median 3, two outliers
counts <- data.frame(moths = rnbinom(60, mu = 4.5, size = 1.5))
count_mean   <- mean(counts$moths)
count_median <- median(counts$moths)
note("Slide 8: trap-nights", nrow(counts))
note("Slide 8: mean moths per trap-night", round(count_mean, 1))
note("Slide 8: median moths per trap-night", count_median)
note("Slide 8: SD of counts", round(sd(counts$moths), 1))
note("Slide 11: summary() min, Q1, median, mean, Q3, max",
     paste(round(summary(counts$moths), 1), collapse = ", "))
note("Slide 11: upper whisker limit Q3 + 1.5 x IQR",
     quantile(counts$moths, 0.75) + 1.5 * IQR(counts$moths))
note("Slide 11: outliers beyond upper whisker",
     paste(sort(counts$moths[counts$moths > quantile(counts$moths, 0.75) +
                               1.5 * IQR(counts$moths)]), collapse = ", "))
note("Slide 11: IQR()", IQR(counts$moths))

make_skew <- function(layout) {
  lab_size <- 16 / .pt
  top <- max(table(counts$moths))
  ggplot(counts, aes(x = moths)) +
    geom_bar(fill = bb852$light, width = 0.85) +
    geom_vline(xintercept = count_median, colour = bb852$slate, linewidth = 1.2) +
    geom_vline(xintercept = count_mean, colour = bb852$coral, linewidth = 1.2) +
    annotate("text", x = 9, y = top * 0.98, hjust = 0,
             label = paste0("median = ", count_median), colour = bb852$slate,
             size = lab_size, family = bb852_font, fontface = "bold") +
    annotate("text", x = 9, y = top * 0.82, hjust = 0,
             label = paste0("mean = ", sprintf("%.1f", count_mean)),
             colour = bb852$coral, size = lab_size, family = bb852_font,
             fontface = "bold") +
    scale_x_continuous(breaks = seq(0, 30, 5)) +
    scale_y_continuous(breaks = seq(0, 20, 2),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Moths per trap-night", y = "Number of trap-nights") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank())
}
save_all(make_skew, "slide_08_skewed_counts")


# Slide 9: geometric mean for population growth ---------------------------

rates  <- c(1.6, 0.7, 1.6)
n0     <- 100
arith  <- mean(rates)
geom_m <- exp(mean(log(rates)))
note("Slide 9: yearly growth rates", paste(rates, collapse = ", "))
note("Slide 9: arithmetic mean rate", round(arith, 2))
note("Slide 9: geometric mean rate", round(geom_m, 3))
note("Slide 9: real N after 3 years", n0 * prod(rates))
note("Slide 9: N predicted by arithmetic mean", round(n0 * arith^3, 1))
note("Slide 9: N predicted by geometric mean", round(n0 * geom_m^3, 1))

growth <- rbind(
  data.frame(year = 0:3, N = n0 * cumprod(c(1, rates)), series = "Observed"),
  data.frame(year = 0:3, N = n0 * arith^(0:3), series = "Arithmetic mean"),
  data.frame(year = 0:3, N = n0 * geom_m^(0:3), series = "Geometric mean")
)

make_growth <- function(layout) {
  ends <- subset(growth, year == 3)
  # Observed and geometric mean end at the same N, so they share one label.
  ends <- data.frame(N = c(n0 * prod(rates), n0 * arith^3),
                     series = c("Observed", "Arithmetic mean"),
                     label = c(paste0("Observed and\ngeometric\nmean: ", round(n0 * prod(rates))),
                               paste0("Arithmetic\nmean: ", round(n0 * arith^3))))
  ggplot(growth, aes(x = year, y = N, colour = series, linetype = series)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_text(data = ends, aes(x = 3.12, y = N, label = label), hjust = 0,
              size = 15 / .pt, family = bb852_font, lineheight = 0.95,
              fontface = "bold", show.legend = FALSE) +
    scale_colour_manual(values = c("Observed" = bb852$slate,
                                   "Arithmetic mean" = bb852$coral,
                                   "Geometric mean" = bb852$teal)) +
    scale_linetype_manual(values = c("Observed" = "solid",
                                     "Arithmetic mean" = "dashed",
                                     "Geometric mean" = "dotted")) +
    scale_x_continuous(breaks = 0:3, limits = c(-0.1, 5.3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(85, 235), breaks = seq(100, 200, 50)) +
    labs(x = "Year", y = "Population size") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank())
}
save_all(make_growth, "slide_09_geometric_mean", extra = "takeaway")


# Slide 10: variance and SD from deviations ------------------------------

set.seed(21)
ten <- data.frame(moth = 1:10, length = round(rnorm(10, wing_mean, wing_sd), 1))
ten_mean <- mean(ten$length)
note("Slide 10: ten wing lengths (mm)", paste(ten$length, collapse = ", "))
note("Slide 10: mean of ten (mm)", round(ten_mean, 2))
note("Slide 10: variance (mm^2)", round(var(ten$length), 2))
note("Slide 10: SD (mm)", round(sd(ten$length), 2))

make_dev <- function(layout) {
  ggplot(ten, aes(x = moth, y = length)) +
    geom_hline(yintercept = ten_mean, colour = bb852$slate, linewidth = 1) +
    geom_segment(aes(xend = moth, yend = ten_mean), colour = bb852$coral,
                 linewidth = 1) +
    geom_point(size = 3.5, colour = bb852$slate) +
    annotate("text", x = 9.5, y = ten_mean + 0.3, label = "mean", hjust = 0.5,
             vjust = 0, colour = bb852$slate, size = 16 / .pt,
             family = bb852_font, fontface = "bold") +
    scale_x_continuous(breaks = 1:10, expand = expansion(add = c(0.5, 0.6))) +
    labs(x = "Moth", y = "Forewing length (mm)") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank())
}
save_all(make_dev, "slide_10_deviations")


# Slide 11: box plot and five-number summary of the counts -----------------

make_box <- function(layout) {
  q <- quantile(counts$moths, c(0.25, 0.5, 0.75))
  labs_df <- data.frame(
    y = q,
    label = c("lower quartile", "median", "upper quartile")
  )
  # The maximum may be an outlier beyond the upper whisker (1.5 x IQR).
  ggplot(counts, aes(x = 0, y = moths)) +
    geom_boxplot(width = 0.5, fill = bb852$panel, colour = bb852$slate,
                 outlier.colour = bb852$coral, outlier.size = 3, linewidth = 0.8,
                 coef = 1.5) +
    geom_text(data = labs_df, aes(x = 0.32, y = y, label = label),
              hjust = 0, colour = bb852$text, size = 15 / .pt,
              family = bb852_font) +
    annotate("text", x = -0.32, y = max(counts$moths), hjust = 1,
             label = "outlier", colour = bb852$coral, size = 15 / .pt,
             family = bb852_font, fontface = "bold") +
    scale_x_continuous(limits = c(-0.9, 1.1)) +
    scale_y_continuous(breaks = seq(0, 30, 5)) +
    labs(x = NULL, y = "Moths per trap-night") +
    theme_bb852() +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
}
save_all(make_box, "slide_11_boxplot")


# Slide 13: probability of survival (binomial) ----------------------------

surv <- data.frame(k = 0:20, p = dbinom(0:20, 20, 0.7))
note("Slide 13: expected survivors of 20 at p = 0.7", 20 * 0.7)
note("Slide 13: P(exactly 14 survive)", round(dbinom(14, 20, 0.7), 2))
note("Slide 13: P(12 to 16 survive)", round(sum(dbinom(12:16, 20, 0.7)), 2))

make_surv <- function(layout) {
  ggplot(surv, aes(x = k, y = p, fill = k == 14)) +
    geom_col(width = 0.8) +
    annotate("text", x = 14, y = dbinom(14, 20, 0.7) + 0.012,
             label = sprintf("P(14) = %.2f", dbinom(14, 20, 0.7)),
             colour = bb852$coral, size = 16 / .pt, family = bb852_font,
             fontface = "bold", vjust = 0) +
    scale_fill_manual(values = c(`FALSE` = bb852$light, `TRUE` = bb852$coral)) +
    scale_x_continuous(breaks = seq(0, 20, 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = "Larvae surviving (out of 20)", y = "Probability") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank())
}
save_all(make_surv, "slide_13_survival_binomial")


# Slide 14: discrete probabilities vs continuous density -------------------

make_density_pair <- function(layout) {
  pois <- data.frame(k = 0:9, p = dpois(0:9, 2))
  p1 <- ggplot(pois, aes(x = k, y = p, fill = k == 2)) +
    geom_col(width = 0.75) +
    annotate("text", x = 2, y = dpois(2, 2) + 0.01, vjust = 0,
             label = sprintf("P(X = 2) = %.2f", dpois(2, 2)),
             colour = bb852$coral, size = 15 / .pt, family = bb852_font,
             fontface = "bold") +
    scale_fill_manual(values = c(`FALSE` = bb852$light, `TRUE` = bb852$coral)) +
    scale_x_continuous(breaks = 0:9) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "Count: bar height = probability",
         x = "Moths per trap-night", y = "Probability") +
    theme_bb852() + panel_label() +
    theme(panel.grid.major.x = element_blank())

  x <- seq(21, 35, by = 0.05)
  dens <- data.frame(x = x, d = dnorm(x, wing_mean, wing_sd))
  shade <- subset(dens, x >= 26 & x <= 30)
  p_area <- pnorm(30, wing_mean, wing_sd) - pnorm(26, wing_mean, wing_sd)
  p2 <- ggplot(dens, aes(x = x, y = d)) +
    geom_area(data = shade, fill = bb852$coral, alpha = 0.35) +
    geom_line(colour = bb852$slate, linewidth = 1.2) +
    annotate("text", x = 28, y = 0.08, label = sprintf("area = %.2f", p_area),
             colour = bb852$coral, size = 15 / .pt, family = bb852_font,
             fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(title = "Measurement: area = probability",
         x = "Forewing length (mm)", y = "Probability density") +
    theme_bb852() + panel_label() +
    theme(panel.grid.major.x = element_blank())

  if (layout == "half") p1 / p2 else p1 | p2
}
note("Slide 14: Poisson(2) P(X = 2)", round(dpois(2, 2), 2))
note("Slide 14: Normal(28, 2) P(26 < length < 30)",
     round(pnorm(30, 28, 2) - pnorm(26, 28, 2), 2))
save_all(make_density_pair, "slide_14_probability_vs_density")


# Slide 15: normal distribution, changing mu and sigma --------------------

make_normal <- function(layout) {
  x <- seq(18, 38, by = 0.05)
  mu_df <- rbind(data.frame(x = x, d = dnorm(x, 26, 2), lab = "mu == 26"),
                 data.frame(x = x, d = dnorm(x, 30, 2), lab = "mu == 30"))
  sd_df <- rbind(data.frame(x = x, d = dnorm(x, 28, 1), lab = "sigma == 1"),
                 data.frame(x = x, d = dnorm(x, 28, 3), lab = "sigma == 3"))
  cols <- c(bb852$slate, bb852$coral)
  p1 <- ggplot(mu_df, aes(x, d, colour = lab)) +
    geom_line(linewidth = 1.2) +
    annotate("text", x = c(23, 33), y = 0.17, hjust = c(1, 0),
             label = c("mu == 26", "mu == 30"), parse = TRUE,
             colour = cols, size = 16 / .pt, family = bb852_font) +
    scale_colour_manual(values = cols) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Change the mean (SD = 2 mm)", x = "Forewing length (mm)",
         y = "Probability density") +
    theme_bb852() + panel_label() + theme(panel.grid.major.x = element_blank())
  p2 <- ggplot(sd_df, aes(x, d, colour = lab)) +
    geom_line(linewidth = 1.2) +
    annotate("text", x = c(29.6, 33.5), y = c(dnorm(0) * 0.9, 0.09),
             label = c("sigma == 1", "sigma == 3"), parse = TRUE, hjust = 0,
             colour = cols, size = 16 / .pt, family = bb852_font) +
    scale_colour_manual(values = cols) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(title = "Change the SD (mean = 28 mm)", x = "Forewing length (mm)",
         y = "Probability density") +
    theme_bb852() + panel_label() + theme(panel.grid.major.x = element_blank())
  if (layout == "half") p1 / p2 else p1 | p2
}
save_all(make_normal, "slide_15_normal_parameters")


# Slide 16: Poisson distribution, changing lambda -------------------------

make_poisson <- function(layout) {
  pd <- rbind(data.frame(k = 0:16, p = dpois(0:16, 1.5), lab = "lambda == 1.5"),
              data.frame(k = 0:16, p = dpois(0:16, 6), lab = "lambda == 6"))
  pd$lab <- factor(pd$lab, levels = c("lambda == 1.5", "lambda == 6"))
  ggplot(pd, aes(x = k, y = p, fill = lab)) +
    geom_col(width = 0.75) +
    facet_wrap(~lab, labeller = label_parsed, nrow = if (layout == "half") 2 else 1) +
    scale_fill_manual(values = c(bb852$slate, bb852$coral)) +
    scale_x_continuous(breaks = seq(0, 16, 2)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Moths per trap-night", y = "Probability") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 18, face = "bold", hjust = 0))
}
save_all(make_poisson, "slide_16_poisson_lambda")


# Slide 19: different samples give different means ------------------------

set.seed(5)
rows_df <- do.call(rbind, lapply(1:8, function(i) {
  data.frame(sample = i, length = rnorm(n_small, wing_mean, wing_sd))
}))
rows_means <- aggregate(length ~ sample, data = rows_df, FUN = mean)
note("Slide 19: sample means of 8 samples of n = 16 (mm)",
     paste(sprintf("%.1f", rows_means$length), collapse = ", "))

make_rows <- function(layout) {
  ggplot(rows_df, aes(x = length, y = factor(sample, levels = 8:1))) +
    geom_vline(xintercept = wing_mean, colour = bb852$grey, linewidth = 0.6,
               linetype = "dashed") +
    geom_point(colour = bb852$light, alpha = 0.7, size = 2.6) +
    geom_point(data = rows_means, colour = bb852$coral, shape = 18, size = 6) +
    annotate("text", x = wing_mean + 0.15, y = 9.05, hjust = 0,
             label = "population mean (28 mm)", colour = bb852$grey,
             size = 14 / .pt, family = bb852_font) +
    scale_y_discrete(labels = paste("Sample", 8:1)) +
    coord_cartesian(ylim = c(0.6, 9.3), clip = "off") +
    labs(x = "Forewing length (mm)", y = NULL) +
    theme_bb852() +
    theme(panel.grid.major.y = element_blank())
}
save_all(make_rows, "slide_19_repeated_samples")


# Slide 20: sampling distributions of the mean for n = 16 and n = 64 -------

set.seed(42)
reps <- 2000
means_small <- replicate(reps, mean(rnorm(n_small, wing_mean, wing_sd)))
means_large <- replicate(reps, mean(rnorm(n_large, wing_mean, wing_sd)))
note("Slide 20/21: simulated replicate samples per n", reps)
note("Slide 20/21: SD of sample means, n = 16 (simulated)", round(sd(means_small), 2))
note("Slide 20/21: SD of sample means, n = 64 (simulated)", round(sd(means_large), 2))
note("Slide 21: SEM = 2 / sqrt(16)", wing_sd / sqrt(n_small))
note("Slide 21: SEM = 2 / sqrt(64)", wing_sd / sqrt(n_large))
note("Slide 20: mean of sample means, n = 16", round(mean(means_small), 2))
note("Slide 20: mean of sample means, n = 64", round(mean(means_large), 2))

make_sampling <- function(layout) {
  sd_df <- rbind(data.frame(m = means_small, n = "n = 16"),
                 data.frame(m = means_large, n = "n = 64"))
  ggplot(sd_df, aes(x = m, fill = n)) +
    geom_histogram(binwidth = 0.1, boundary = 28, position = "identity",
                   alpha = 0.75) +
    geom_vline(xintercept = wing_mean, colour = bb852$grey, linewidth = 0.6,
               linetype = "dashed") +
    annotate("text", x = c(26.35, 28.55), y = c(95, 300), hjust = c(0, 0),
             label = c("n = 16", "n = 64"), colour = c(bb852$light, bb852$slate),
             size = 17 / .pt, family = bb852_font, fontface = "bold") +
    scale_fill_manual(values = c("n = 16" = bb852$light, "n = 64" = bb852$slate)) +
    scale_x_continuous(limits = c(26, 30), breaks = 26:30) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Sample mean forewing length (mm)",
         y = "Number of samples") +
    theme_bb852() +
    theme(panel.grid.major.x = element_blank())
}
save_all(make_sampling, "slide_20_sampling_distributions", extra = "takeaway")


# Slide 22: 95% confidence intervals from repeated samples ----------------
# Seed 1 chosen (and disclosed) because exactly one of the 20 intervals misses
# the population mean, as expected on average; real repetitions vary.

set.seed(1)
ci <- do.call(rbind, lapply(1:20, function(i) {
  x <- rnorm(n_small, wing_mean, wing_sd)
  ci_i <- t.test(x)$conf.int
  data.frame(sample = i, mean = mean(x), lower = ci_i[1], upper = ci_i[2])
}))
ci$covers <- ci$lower <= wing_mean & ci$upper >= wing_mean
note("Slide 22: intervals (of 20) that miss the population mean", sum(!ci$covers))
note("Slide 22: CI width range (mm)",
     paste(sprintf("%.2f", range(ci$upper - ci$lower)), collapse = " to "))
note("Slide 22: t multiplier for n = 16 (df = 15)", round(qt(0.975, 15), 2))

make_ci <- function(layout) {
  ggplot(ci, aes(y = sample, x = mean, colour = covers)) +
    geom_vline(xintercept = wing_mean, colour = bb852$grey, linewidth = 0.6,
               linetype = "dashed") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = 1.1) +
    geom_point(size = 2.8) +
    scale_colour_manual(values = c(`TRUE` = bb852$slate, `FALSE` = bb852$coral)) +
    scale_y_continuous(breaks = c(1, 5, 10, 15, 20)) +
    labs(x = "Mean forewing length and 95% CI (mm)", y = "Repeated sample") +
    theme_bb852() +
    theme(panel.grid.major.y = element_blank())
}
save_all(make_ci, "slide_22_confidence_intervals", extra = "takeaway")


# Write the numbers quoted on the slides ----------------------------------

dir.create(file.path(output_root, "text"), recursive = TRUE, showWarnings = FALSE)
writeLines(c("Numbers quoted on the lecture 07 slides (all from simulated data)",
             "", slide_numbers),
           file.path(output_root, "text", "slide_numbers.txt"))
