# BB852 AI workshop (Using AI assistants for R and statistics): figures
# ----------------------------------------------------------------------
# Title-slide figure for the workshop deck "BB852 19 AI Workshop v2", in the
# BB852 theme shared with the other lecture_figures generators (see
# lecture_figures/glm_count).
#
# The figure shows the espresso foam data from the ANOVA practical (lecture
# 12) with each method's mean and 95% confidence interval. Three workshop
# activities ask students to check AI answers about this model
# (foam_mod <- lm(foamIndx ~ method, data = espresso)), so the cover shows
# the answer they will check against.
#
# Data:
#   CourseData/espresso.csv  foam index for three espresso methods
#                            (BM, HIP, IES), n = 9 per method
#
# Run from the repository root:
# source("lecture_figures/ai_workshop/generate_figures.R")
#
# PNG files are written to lecture_figures/ai_workshop/output/<layout>/.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/glm_count.

bb852 <- list(
  bg       = "#FBFAF6",
  text     = "#1B2230",
  muted    = "#6B6E75",
  grid     = "#DDE2E8",
  slate    = "#24405F",
  coral    = "#D0745A",
  light    = "#9FB0C4",
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
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.margin = margin(12, 16, 12, 12)
    )
}

# Sizes (inches) match the figure placeholders in the BB852 slide theme.
bb852_sizes <- list(cover = c(5.43, 7.50))

output_root <- file.path("lecture_figures", "ai_workshop", "output")
unlink(output_root, recursive = TRUE)

save_bb852 <- function(plot, filename, layout = "cover", bg = bb852$bg) {
  size <- bb852_sizes[[layout]]
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(output_directory, paste0(filename, ".png")), plot,
         width = size[1], height = size[2], units = "in", dpi = 300,
         device = ragg::agg_png, bg = bg)
  invisible(filename)
}


# Data and model ----------------------------------------------------------

espresso <- read.csv(file.path("CourseData", "espresso.csv"))
espresso$method <- factor(espresso$method, levels = c("BM", "HIP", "IES"))

# Group means and 95% CIs from the same model students fit in the practical
# (no intercept, so each coefficient is a group mean).
foam_means <- lm(foamIndx ~ method - 1, data = espresso)
ci <- confint(foam_means)
summary_df <- data.frame(
  method = factor(levels(espresso$method), levels = levels(espresso$method)),
  mean = coef(foam_means),
  lower = ci[, 1],
  upper = ci[, 2]
)
print(summary_df, digits = 3)


# Slide 1: title-slide chart ----------------------------------------------
# Raw data (light, jittered horizontally only) with mean and 95% CI (coral),
# on the cover colour.

set.seed(852)
cover_plot <- ggplot(espresso, aes(method, foamIndx)) +
  geom_jitter(width = 0.12, height = 0, colour = bb852$light,
              size = 2.8, alpha = 0.75) +
  geom_errorbar(data = summary_df,
                aes(x = method, ymin = lower, ymax = upper),
                inherit.aes = FALSE, width = 0.22, colour = bb852$coral,
                linewidth = 1.2) +
  geom_point(data = summary_df, aes(method, mean), colour = bb852$coral,
             size = 4.5) +
  scale_y_continuous(limits = c(15, 80), breaks = seq(20, 80, 10)) +
  labs(x = "Brewing method", y = "Foam index (%)") +
  theme_bb852() +
  theme(
    plot.background = element_rect(fill = bb852$cover, colour = NA),
    panel.background = element_rect(fill = bb852$cover, colour = NA),
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = bb852$light, linewidth = 0.4),
    axis.title = element_text(colour = bb852$light),
    axis.text = element_text(colour = bb852$light),
    plot.margin = margin(140, 30, 110, 18)
  )

save_bb852(cover_plot, "slide_01_cover", layout = "cover", bg = bb852$cover)
