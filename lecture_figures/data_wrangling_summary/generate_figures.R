# BB852 data wrangling summary lecture: R code for every figure
# -----------------------------------------------------------------
# Figures for "Data wrangling: the story so far" (BB852 04b), in the BB852
# theme shared with the other lecture_figures generators. (Lecture 04, "Data
# wrangling", has its own generator in lecture_figures/data_wrangling_summary/.)
#
# Data source: CourseData/AarslevTemperature.csv (daily mean temperature,
# deg C, at the Aarslev weather station near Odense; columns YEAR, MONTH, DAY,
# TEMP). The file runs from 11 April 2012 to 7 May 2023; 50 days are missing.
# All figures use these empirical data; nothing is simulated.
#
# Run from the repository root:
# source("lecture_figures/data_wrangling_summary/generate_figures.R")
#
# PNG files are written to lecture_figures/data_wrangling_summary/output/<layout>/.
# Each ggplot object is also retained in the named list `plots`.
# The R output shown on the slides is written to output/text/ so that slide
# text cannot drift from the code.
#
# Labels use \u escapes so the script stays plain ASCII.


# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ragg)


# Define the BB852 theme and export functions -----------------------------
# Palette and theme match lecture_figures/t-tests and lecture_figures/anova.

bb852 <- list(
  bg    = "#FBFAF6",
  text  = "#1B2230",
  muted = "#6B6E75",
  grid  = "#DDE2E8",
  axis  = "#BCC6D2",
  slate = "#24405F",
  coral = "#D0745A",
  teal  = "#2A7F76",
  grey  = "#6B6E75",
  light = "#9FB0C4",
  panel = "#EDEFF1",
  cover = "#2C4D70"
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
bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "data_wrangling_summary", "output")
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

# Title-slide cover graphics are the only figures not written at full and
# half width (as in the other generators).
# Write every content figure at both full and half width, as the other
# generators do; `layouts` lists the extra layouts a figure is used at.
save_all <- function(plot, filename, layouts = c("full", "half")) {
  for (layout in layouts) save_bb852(plot, filename, layout)
}

save_text <- function(lines, filename) {
  output_directory <- file.path(output_root, "text")
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file.path(output_directory, filename))
}

plots <- list()


# Data ------------------------------------------------------------------------
# This is exactly the pipe shown on the slides (native pipe, as in the
# course book's Tips chapter).

weather <- read.csv("CourseData/AarslevTemperature.csv")

august <- weather |>
  filter(MONTH == 8) |>
  group_by(YEAR) |>
  summarise(meanTemp = mean(TEMP))

# R output shown on the slides (printed tibble, as students will see it).
save_text(capture.output(print(august, n = 20)), "august_means.txt")

# Row counts after each step, for the "rows shrink" diagram.
step_rows <- tibble::tibble(
  step  = c("read.csv()", "filter()", "group_by()", "summarise()"),
  label = c("every day", "August days only", "same rows, 11 groups",
            "one row per year"),
  rows  = c(
    nrow(weather),
    nrow(filter(weather, MONTH == 8)),
    nrow(filter(weather, MONTH == 8)),
    nrow(august)
  )
)
save_text(capture.output(print(step_rows)), "step_rows.txt")

# Answer to the "rewrite as a pipe" activity (July maxima). July 2020 has
# only 29 days in the file, which is worth pointing out.
july <- weather |>
  filter(MONTH == 7) |>
  group_by(YEAR) |>
  summarise(maxTemp = max(TEMP), n = n())
save_text(capture.output(print(july, n = 20)), "july_max.txt")

# Answer key for the optional extension questions (speaker notes).
summers <- weather |>
  filter(MONTH %in% 6:8) |>
  group_by(YEAR) |>
  summarise(meanTemp = mean(TEMP), n = n()) |>
  arrange(desc(meanTemp))
winters <- weather |>
  filter(MONTH %in% c(12, 1, 2)) |>
  mutate(winter = ifelse(MONTH == 12, YEAR + 1, YEAR)) |>
  group_by(winter) |>
  summarise(meanTemp = mean(TEMP), n = n()) |>
  arrange(meanTemp)
dates <- as.Date(sprintf("%d-%02d-%02d", weather$YEAR, weather$MONTH, weather$DAY))
save_text(c(
  capture.output(print(summers, n = 20)),
  capture.output(print(winters, n = 20)),
  paste("Missing days:", as.numeric(max(dates) - min(dates)) + 1 - nrow(weather))
), "extension_answers.txt")


# Slide 7: the raw data, with the August days we want highlighted -------------

daily <- weather |>
  mutate(date = as.Date(sprintf("%d-%02d-%02d", YEAR, MONTH, DAY)),
         is_august = MONTH == 8)

plots$slide_07_daily_temps <- ggplot(daily, aes(date, TEMP)) +
  geom_line(colour = bb852$light, linewidth = 0.3) +
  geom_point(data = filter(daily, is_august), colour = bb852$coral,
             size = 0.9) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = "Daily mean (\u00b0C)") +
  theme_bb852()

save_all(plots$slide_07_daily_temps, "slide_07_daily_temps")


# Slide 11: each verb hands a smaller table to the next ------------------------

step_rows <- step_rows |>
  mutate(position = seq_len(n()),
         step = factor(step, levels = step))

plots$slide_11_rows_shrink <- ggplot(step_rows, aes(position, rows)) +
  geom_col(width = 0.6,
           fill = c(bb852$light, bb852$light, bb852$light, bb852$coral)) +
  geom_text(aes(label = format(rows, big.mark = ",")), vjust = -0.4,
            size = 6.5, family = bb852_font, fontface = "bold",
            colour = bb852$text) +
  geom_text(aes(y = -330, label = step), size = 6, family = "Menlo",
            colour = bb852$slate) +
  geom_text(aes(y = -680, label = label), size = 5, family = bb852_font,
            colour = bb852$muted) +
  scale_y_continuous(limits = c(-800, 4500)) +
  labs(x = NULL, y = NULL) +
  theme_bb852() +
  theme(panel.grid.major = element_blank(), axis.text = element_blank())

save_all(plots$slide_11_rows_shrink, "slide_11_rows_shrink")


# Slide 12: the answer, mean August temperature per year ----------------------

extremes <- august |>
  filter(meanTemp %in% range(meanTemp)) |>
  mutate(lab = sprintf("%d: %.1f \u00b0C", YEAR, meanTemp),
         vjust = ifelse(meanTemp == max(meanTemp), -1, 1.9))

plots$slide_12_august_means <- ggplot(august, aes(YEAR, meanTemp)) +
  geom_line(colour = bb852$slate, linewidth = 1.2) +
  geom_point(colour = bb852$slate, size = 3) +
  geom_point(data = extremes, colour = bb852$coral, size = 4) +
  geom_text(data = extremes, aes(label = lab, vjust = vjust), size = 5.5,
            family = bb852_font, colour = bb852$coral) +
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  scale_y_continuous(limits = c(15, 19.5)) +
  labs(x = "Year", y = "August mean (\u00b0C)") +
  theme_bb852()

save_all(plots$slide_12_august_means, "slide_12_august_means",
         layouts = c("full", "half", "takeaway"))


# Slide 1 cover: monthly mean temperature, one row per complete year ---------
# Months run left to right, years top (2013) to bottom (2022). Colour runs
# from teal (cold) through the slide's light neutral to coral (warm), with the
# midpoint at the mean of all months shown. Purely decorative: no text.

monthly <- weather |>
  filter(YEAR >= 2013, YEAR <= 2022) |>
  group_by(YEAR, MONTH) |>
  summarise(meanTemp = mean(TEMP), .groups = "drop")

plots$slide_01_cover <- ggplot(monthly, aes(MONTH, YEAR, fill = meanTemp)) +
  geom_tile(colour = bb852$cover, linewidth = 1.2) +
  scale_fill_gradient2(low = bb852$teal, mid = "#F2F2EE", high = bb852$coral,
                       midpoint = mean(monthly$meanTemp)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bb852$cover, colour = NA),
        plot.margin = margin(40, 40, 40, 40))

save_bb852(plots$slide_01_cover, "slide_01_cover", layout = "cover",
           bg = bb852$cover)
