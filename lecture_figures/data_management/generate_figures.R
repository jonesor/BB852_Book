# BB852 data management lecture: R code for every figure
# --------------------------------------------------------
# Redraws the spreadsheet examples and diagrams for the data management
# lecture in the BB852 theme, so they are sharp at slide size and editable.
#
# Data sources:
# - The messy and tidy spreadsheets are an invented, illustrative example of
#   nest-box records (in the style of the SDU birds data, sduBirds.csv).
#   They are NOT real records.
# - The wide/long figure uses the first rows of CourseData/heights.csv, the
#   same file used in the "Combining data sets" chapter (pivot_longer).
#
# Run from the repository root:
# source("lecture_figures/data_management/generate_figures.R")
#
# PNG files are written to lecture_figures/data_management/output/<layout>/.
# Each ggplot object is also retained in the named list `plots`.
# File names describe content (not slide numbers), because the slide order
# of this lecture is still being revised.
#
# Labels use \u escapes for non-ASCII symbols so the script stays plain ASCII.
#
# Text sizes: geom_text() size is in mm; 5 mm = 14.2 pt. Every layout used on
# the slides keeps text at 5 mm or more (BB852 rule: nothing below 14 pt).
# The half-width copies of full-width figures exist only because the
# aggregate build expects both layouts; they are not used on slides.


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ragg)


# Define the BB852 theme and export functions -----------------------------
# Palette matches the other lecture_figures generators.

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
  ochre    = "#C4932F",
  cell     = "#FFFFFF",
  header   = "#E4E8ED",
  coral_bg = "#F6DCD3",
  teal_bg  = "#D5EAE7"
)

available_fonts <- unique(systemfonts::system_fonts()$family)
pick_font <- function(candidates) {
  hit <- candidates[candidates %in% available_fonts]
  if (length(hit) == 0) "sans" else hit[1]
}
bb852_font <- pick_font(c("Avenir Next", "Helvetica", "DejaVu Sans"))
mono_font  <- pick_font(c("Menlo", "DejaVu Sans Mono"))

bb852_sizes <- list(
  full     = c(11.73, 3.95),
  takeaway = c(7.40, 4.25),
  half     = c(5.57, 4.25),
  cover    = c(5.43, 7.50)
)

output_root <- file.path("lecture_figures", "data_management", "output")
unlink(output_root, recursive = TRUE)

save_bb852 <- function(plot, filename, layout = "half", bg = bb852$bg) {
  size <- bb852_sizes[[layout]]
  if (is.null(size)) {
    stop("layout must be one of: ", paste(names(bb852_sizes), collapse = ", "))
  }
  output_directory <- file.path(output_root, layout)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    file.path(output_directory, paste0(filename, ".png")),
    plot, width = size[1], height = size[2], units = "in",
    dpi = 300, device = ragg::agg_png, bg = bg
  )
  invisible(filename)
}

# Every content figure is saved in both full and half layouts, as the
# aggregate build (generate_all.R) expects. `size_scale` shrinks text for the
# smaller layout.
save_both <- function(make_plot, filename, half_scale = 0.80) {
  save_bb852(make_plot(1.00), filename, layout = "full")
  save_bb852(make_plot(half_scale), filename, layout = "half")
  invisible(filename)
}

theme_blank_bb852 <- function() {
  theme_void(base_family = bb852_font) +
    theme(
      plot.background = element_rect(fill = bb852$bg, colour = NA),
      panel.background = element_rect(fill = bb852$bg, colour = NA),
      legend.position = "none",
      plot.margin = margin(6, 6, 6, 6)
    )
}


# Spreadsheet drawing helper ----------------------------------------------
# Draws a character matrix as a spreadsheet: column letters, row numbers,
# cells, optional merged ranges, fills and bold cells. Like Excel, values that
# look like numbers are right-aligned and text is left-aligned, which is
# itself a clue to how a value has been stored.

looks_numeric <- function(x) {
  grepl("^-?[0-9]+(\\.[0-9]+)?$", x)
}

sheet_cells <- function(m, widths, fills = NULL, bold = NULL, merges = list()) {
  n_row <- nrow(m)
  n_col <- ncol(m)
  left <- cumsum(c(0, widths))[seq_len(n_col)]
  cells <- expand.grid(row = seq_len(n_row), col = seq_len(n_col))
  cells$xmin <- left[cells$col]
  cells$xmax <- cells$xmin + widths[cells$col]
  cells$ymax <- -(cells$row - 1)
  cells$ymin <- -cells$row
  cells$label <- m[cbind(cells$row, cells$col)]
  cells$fill <- bb852$cell
  if (!is.null(fills)) {
    has_fill <- !is.na(fills[cbind(cells$row, cells$col)])
    cells$fill[has_fill] <- fills[cbind(cells$row, cells$col)][has_fill]
  }
  cells$face <- "plain"
  if (!is.null(bold)) {
    cells$face[bold[cbind(cells$row, cells$col)]] <- "bold"
  }
  cells$keep <- TRUE
  for (mg in merges) {
    inside <- cells$row >= mg[1] & cells$row <= mg[3] &
      cells$col >= mg[2] & cells$col <= mg[4]
    anchor <- cells$row == mg[1] & cells$col == mg[2]
    cells$keep[inside & !anchor] <- FALSE
    cells$xmax[anchor] <- left[mg[4]] + widths[mg[4]]
    cells$ymin[anchor] <- -mg[3]
  }
  cells <- cells[cells$keep, ]
  cells$numeric <- looks_numeric(cells$label)
  cells$x <- ifelse(cells$numeric, cells$xmax - 0.12, cells$xmin + 0.12)
  cells$hjust <- ifelse(cells$numeric, 1, 0)
  cells$y <- (cells$ymin + cells$ymax) / 2
  # Merged cells are vertically centred, as Excel does by default.
  cells
}

sheet_plot <- function(m, widths, fills = NULL, bold = NULL, merges = list(),
                       text_size = 5.2, headers = TRUE) {
  cells <- sheet_cells(m, widths, fills, bold, merges)
  n_row <- nrow(m)
  n_col <- ncol(m)
  left <- cumsum(c(0, widths))[seq_len(n_col)]
  p <- ggplot() +
    geom_rect(
      data = cells,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      colour = bb852$axis, linewidth = 0.3
    ) +
    geom_text(
      data = cells[cells$label != "", ],
      aes(x = x, y = y, label = label, hjust = hjust, fontface = face),
      family = bb852_font, colour = bb852$text, size = text_size
    ) +
    scale_fill_identity()
  if (headers) {
    col_head <- data.frame(
      xmin = left, xmax = left + widths, label = LETTERS[seq_len(n_col)]
    )
    row_head <- data.frame(row = seq_len(n_row))
    p <- p +
      geom_rect(
        data = col_head,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 0.75),
        fill = bb852$header, colour = bb852$axis, linewidth = 0.3
      ) +
      geom_text(
        data = col_head,
        aes(x = (xmin + xmax) / 2, y = 0.375, label = label),
        family = bb852_font, colour = bb852$muted, size = text_size
      ) +
      geom_rect(
        data = row_head,
        aes(xmin = -0.55, xmax = 0, ymin = -row, ymax = -(row - 1)),
        fill = bb852$header, colour = bb852$axis, linewidth = 0.3
      ) +
      geom_text(
        data = row_head,
        aes(x = -0.275, y = -(row - 0.5), label = row),
        family = bb852_font, colour = bb852$muted, size = text_size
      )
  }
  p + theme_blank_bb852()
}

# Numbered markers for the "spot the problems" answer slide.
problem_markers <- function(markers, size_scale = 1) {
  list(
    geom_point(
      data = markers, aes(x = x, y = y),
      shape = 21, size = 8.5 * size_scale, stroke = 0,
      fill = bb852$coral
    ),
    geom_text(
      data = markers, aes(x = x, y = y, label = id),
      family = bb852_font, fontface = "bold", colour = "white",
      size = 5.0 * size_scale
    )
  )
}

plots <- list()


# The messy spreadsheet ---------------------------------------------------
# An invented sheet containing eight common problems (after Broman & Woo
# 2018). Students hunt for them before the answer slide.

messy <- matrix("", nrow = 12, ncol = 9)
messy[1, 1] <- "Great tit nest boxes 2025!!"
messy[2, 1] <- "(Anna + me. Site B notes in field book)"
messy[4, 1:5] <- c("Site", "Box", "Date", "Eggs", "Notes")
messy[4, 8:9] <- c("Observer", "Visits")
messy[5, 8:9] <- c("Anna", "12")
messy[6, 8:9] <- c("Owen", "9")
messy[5:11, 2] <- c("1", "2", "3", "4", "1", "2", "3")
messy[5:11, 3] <- c("03/05/2025", "5 May", "45783", "2025-05-07",
                    "May 8", "08.05.25", "9/5")
messy[5:11, 4] <- c("8", "9 eggs", "-", "0", "11", "", "10")
messy[6, 5] <- "ask Anna?"
messy[8, 5] <- "?"
messy[5, 1] <- "Site A"
messy[9, 1] <- "Site B"
messy[12, 3] <- "Total"
messy[12, 4] <- "38"
messy[12, 5] <- "orange = abandoned"

messy_fill <- matrix(NA_character_, nrow = 12, ncol = 9)
messy_fill[7, 1:5] <- "#F2B38F"
messy_fill[9, 2:5] <- "#F2B38F"
messy_fill[12, 5] <- "#F2B38F"
messy_fill[4, 1:5] <- "#C9D6E6"
messy_fill[4, 8:9] <- "#C9D6E6"

messy_bold <- matrix(FALSE, nrow = 12, ncol = 9)
messy_bold[1, 1] <- TRUE
messy_bold[4, ] <- TRUE
messy_bold[12, 3:4] <- TRUE

messy_widths <- c(1.15, 0.6, 1.35, 0.95, 2.0, 0.35, 0.35, 1.05, 0.8)
messy_merges <- list(c(1, 1, 1, 5), c(5, 1, 8, 1), c(9, 1, 11, 1))

make_messy <- function(size_scale = 1) {
  sheet_plot(messy, messy_widths, messy_fill, messy_bold, messy_merges,
             text_size = 5.0 * size_scale)
}

# Marker positions in sheet coordinates (x from column edges, y = -row).
messy_left <- cumsum(c(0, messy_widths))
marker_xy <- function(row, col, dx = 0) {
  c(x = messy_left[col] + dx, y = -(row - 0.5))
}
messy_markers <- data.frame(
  id = 1:8,
  rbind(
    marker_xy(1, 6, 0.10),   # 1 title and notes above the data
    marker_xy(6.5, 1, 1.0),  # 2 merged cells
    marker_xy(4, 3, 1.12),   # 3 dates in six formats
    marker_xy(7, 4, 0.86),   # 4 missing values coded differently
    marker_xy(6, 4, 0.86),   # 5 units and text in a number column
    marker_xy(9, 6, 0.10),   # 6 colour used as data
    marker_xy(4, 8, -0.05),  # 7 a second table on the same sheet
    marker_xy(12, 3, -0.05)  # 8 calculations in the raw data
  )
)

plots$messy_sheet <- make_messy()
save_both(make_messy, "messy_sheet")

make_messy_annotated <- function(size_scale = 1) {
  make_messy(size_scale) + problem_markers(messy_markers, size_scale)
}
plots$messy_sheet_annotated <- make_messy_annotated()
save_both(make_messy_annotated, "messy_sheet_annotated")


# The tidy version of the same data ---------------------------------------
# How the sheet should have been entered at the time: one rectangle, one
# header row, one value per cell, ISO dates, NA plus a reason for missing
# values, and a column instead of colour. Details such as which observer
# visited which box are illustrative: they cannot be recovered from the messy
# sheet, which is the point made on the answer slide.

excel_serial_date <- as.Date(45783, origin = "1899-12-30")  # 1900 date system
stopifnot(format(excel_serial_date) == "2025-05-06")

tidy <- data.frame(
  site      = c("A", "A", "A", "A", "B", "B", "B"),
  boxNumber = c("1", "2", "3", "4", "1", "2", "3"),
  date      = c("2025-05-03", "2025-05-05", format(excel_serial_date),
                "2025-05-07", "2025-05-08", "2025-05-08", "2025-05-09"),
  nEggs     = c("8", "9", "NA", "0", "11", "NA", "10"),
  abandoned = c("FALSE", "FALSE", "TRUE", "FALSE", "TRUE", "FALSE", "FALSE"),
  observer  = c("Anna", "Anna", "Anna", "Anna", "Owen", "Owen", "Owen"),
  comments  = c("NA", "check with Anna", "not counted", "NA",
                "NA", "not checked", "NA")
)
tidy_m <- rbind(names(tidy), as.matrix(tidy))
tidy_fill <- matrix(NA_character_, nrow(tidy_m), ncol(tidy_m))
tidy_fill[1, ] <- "#C9D6E6"
tidy_bold <- matrix(FALSE, nrow(tidy_m), ncol(tidy_m))
tidy_bold[1, ] <- TRUE
tidy_widths <- c(0.6, 1.25, 1.45, 0.85, 1.3, 1.05, 2.15)

make_tidy <- function(size_scale = 1) {
  sheet_plot(tidy_m, tidy_widths, tidy_fill, tidy_bold,
             text_size = 5.0 * size_scale)
}
plots$tidy_sheet <- make_tidy()
save_both(make_tidy, "tidy_sheet")


# Missing values: one column, five codes ----------------------------------

missing_m <- rbind(
  c("Entered", "What goes wrong"),
  c("8", "fine"),
  c("(blank)", "OK only if used everywhere"),
  c("-", "text: whole column becomes text"),
  c("0", "looks like a real zero"),
  c("-999", "a real number in every mean"),
  c("?", "text: whole column becomes text"),
  c("NA", "R reads this as missing")
)
missing_fill <- matrix(NA_character_, nrow(missing_m), 2)
missing_fill[1, ] <- "#C9D6E6"
missing_fill[4:7, 2] <- bb852$coral_bg
missing_fill[8, ] <- bb852$teal_bg
missing_bold <- matrix(FALSE, nrow(missing_m), 2)
missing_bold[1, ] <- TRUE
missing_bold[8, ] <- TRUE

make_missing <- function(size_scale = 1) {
  sheet_plot(missing_m, c(1.25, 3.55), missing_fill, missing_bold,
             text_size = 5.6 * size_scale, headers = FALSE)
}
plots$missing_codes <- make_missing()
save_both(make_missing, "missing_codes", half_scale = 0.9)


# Dates: six ways to type a date ------------------------------------------

dates_m <- rbind(
  c("Typed", "Could mean"),
  c("03/05/2025", "3 May (DK, UK) or 5 March (US)"),
  c("5 May", "which year?"),
  c("45783", "1900-system serial: 2025-05-06"),
  c("08.05.25", "date or text, by locale"),
  c("9/5", "9 May or 5 Sept? Which year?"),
  c("2025-05-07", "one meaning; sorts in order")
)
dates_fill <- matrix(NA_character_, nrow(dates_m), 2)
dates_fill[1, ] <- "#C9D6E6"
dates_fill[2:6, 2] <- bb852$coral_bg
dates_fill[7, ] <- bb852$teal_bg
dates_bold <- matrix(FALSE, nrow(dates_m), 2)
dates_bold[1, ] <- TRUE
dates_bold[7, ] <- TRUE

make_dates <- function(size_scale = 1) {
  sheet_plot(dates_m, c(1.6, 3.8), dates_fill, dates_bold,
             text_size = 5.4 * size_scale, headers = FALSE)
}
plots$date_formats <- make_dates()
save_both(make_dates, "date_formats", half_scale = 0.93)


# Wide versus long: the heights data --------------------------------------
# Real data from CourseData/heights.csv (first four rows), the file used with
# pivot_longer() in the "Combining data sets" chapter.

heights <- read.csv(file.path("CourseData", "heights.csv"))
h4 <- head(heights, 4)
wide_m <- rbind(names(h4), as.matrix(format(h4, trim = TRUE)))

long <- data.frame(
  place  = rep(h4$place, each = 2),
  Sex    = rep(c("Male", "Female"), times = nrow(h4)),
  Height = as.vector(t(as.matrix(h4[, c("heightMale", "heightFemale")])))
)
long_m <- rbind(names(long), as.matrix(format(long, trim = TRUE)))

make_wide_long <- function(size_scale = 1) {
  ts <- 5.2 * size_scale
  wide_fill <- matrix(NA_character_, nrow(wide_m), ncol(wide_m))
  wide_fill[1, ] <- "#C9D6E6"
  wide_fill[1, 2:3] <- bb852$coral_bg   # the variable hidden in the headers
  long_fill <- matrix(NA_character_, nrow(long_m), ncol(long_m))
  long_fill[1, ] <- "#C9D6E6"
  long_fill[-1, 2] <- bb852$teal_bg
  bold_w <- matrix(FALSE, nrow(wide_m), ncol(wide_m)); bold_w[1, ] <- TRUE
  bold_l <- matrix(FALSE, nrow(long_m), ncol(long_m)); bold_l[1, ] <- TRUE

  w_cells <- sheet_cells(wide_m, c(1.1, 1.55, 1.75), wide_fill, bold_w)
  l_cells <- sheet_cells(long_m, c(1.1, 1.05, 1.0), long_fill, bold_l)
  shift <- 6.9
  l_cells[, c("xmin", "xmax", "x")] <- l_cells[, c("xmin", "xmax", "x")] + shift
  # Vertically centre the short wide table against the long one.
  w_offset <- -(nrow(long_m) - nrow(wide_m)) / 2
  w_cells[, c("ymin", "ymax", "y")] <- w_cells[, c("ymin", "ymax", "y")] + w_offset
  cells <- rbind(w_cells, l_cells)

  labels <- data.frame(
    x = c(0, shift),
    y = c(w_offset + 0.55, 0.55),
    label = c("Wide: sex is hidden in the column names",
              "Long (tidy): one row per person")
  )
  arrow_y <- w_offset - nrow(wide_m) / 2
  ggplot() +
    geom_rect(
      data = cells,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      colour = bb852$axis, linewidth = 0.3
    ) +
    geom_text(
      data = cells,
      aes(x = x, y = y, label = label, hjust = hjust, fontface = face),
      family = bb852_font, colour = bb852$text, size = ts
    ) +
    geom_text(
      data = labels, aes(x = x, y = y, label = label),
      hjust = 0, vjust = 0, family = bb852_font, fontface = "bold",
      colour = bb852$slate, size = ts
    ) +
    annotate(
      "segment", x = 4.75, xend = 6.55, y = arrow_y, yend = arrow_y,
      colour = bb852$coral, linewidth = 1.1,
      arrow = arrow(length = unit(0.14, "in"), type = "closed")
    ) +
    annotate(
      "text", x = 5.65, y = arrow_y + 0.55, label = "pivot_longer()",
      family = mono_font, colour = bb852$coral, size = ts
    ) +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    theme_blank_bb852() +
    theme(plot.margin = margin(22, 8, 6, 8))
}
plots$wide_long_heights <- make_wide_long()
save_both(make_wide_long, "wide_long_heights")


# Workflow: raw data stay raw ---------------------------------------------

make_workflow <- function(size_scale = 1) {
  ts <- 5.0 * size_scale
  boxes <- data.frame(
    x = c(1.2, 3.95, 6.7, 9.45),
    title = c("Raw data", "R script", "Clean data", "Results"),
    sub = c("read-only, never edited", "records every change",
            "re-made at any time", "plots, models, tables"),
    fill = c(bb852$slate, bb852$coral, bb852$teal, bb852$light),
    text = c("white", "white", "white", bb852$text)
  )
  files <- data.frame(
    x = boxes$x,
    label = c("sduBirds.csv", "01_clean.R", "birds_clean.csv",
              "fig_eggs.png")
  )
  ggplot(boxes) +
    geom_tile(aes(x = x, y = 0, fill = fill), width = 2.5, height = 1.25) +
    geom_text(aes(x = x, y = 0.2, label = title, colour = text),
              family = bb852_font, fontface = "bold", size = ts * 1.15) +
    geom_text(aes(x = x, y = -0.22, label = sub, colour = text),
              family = bb852_font, size = ts) +
    geom_text(data = files, aes(x = x, y = -0.98, label = label),
              family = mono_font, colour = bb852$muted, size = ts) +
    annotate("segment", x = boxes$x[-4] + 1.27, xend = boxes$x[-1] - 1.27,
             y = 0, yend = 0, colour = bb852$text, linewidth = 0.8,
             arrow = arrow(length = unit(0.12, "in"), type = "closed")) +
    annotate("curve", x = 9.45, xend = 3.95, y = 0.68, yend = 0.68,
             curvature = 0.22, colour = bb852$grey, linewidth = 0.6,
             linetype = "22",
             arrow = arrow(length = unit(0.1, "in"), type = "closed")) +
    annotate("text", x = 6.7, y = 1.42, family = bb852_font,
             label = "Found a problem? Fix the script and re-run, not the raw file",
             colour = bb852$grey, size = ts) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_cartesian(xlim = c(-0.15, 10.8), ylim = c(-1.25, 1.65)) +
    theme_blank_bb852()
}
plots$workflow_raw_to_results <- make_workflow()
save_both(make_workflow, "workflow_raw_to_results")


# Linked tables: joining by a key -----------------------------------------
# Invented teaching tables. Problem 7 (a second table on the same sheet)
# becomes its own file, joined back by the key column "observer", as with
# left_join(mammal, redlist, by = "species") in the Combining data sets
# chapter.

nests_m <- rbind(
  c("boxNumber", "nEggs", "observer"),
  c("1", "8", "Anna"),
  c("2", "9", "Anna"),
  c("3", "11", "Owen"),
  c("4", "10", "Owen")
)
observers_m <- rbind(
  c("observer", "seasons"),
  c("Anna", "1"),
  c("Owen", "12")
)
joined_m <- rbind(
  c("boxNumber", "nEggs", "observer", "seasons"),
  c("1", "8", "Anna", "1"),
  c("2", "9", "Anna", "1"),
  c("3", "11", "Owen", "12"),
  c("4", "10", "Owen", "12")
)

make_join <- function(size_scale = 1) {
  ts <- 5.0 * size_scale
  key_fill <- function(m, key_col) {
    f <- matrix(NA_character_, nrow(m), ncol(m))
    f[1, ] <- "#C9D6E6"
    f[, key_col] <- bb852$teal_bg
    f[1, key_col] <- bb852$teal_bg
    f
  }
  head_bold <- function(m) {
    b <- matrix(FALSE, nrow(m), ncol(m)); b[1, ] <- TRUE; b
  }
  n_cells <- sheet_cells(nests_m, c(1.5, 0.95, 1.2), key_fill(nests_m, 3),
                         head_bold(nests_m))
  o_cells <- sheet_cells(observers_m, c(1.2, 1.1), key_fill(observers_m, 1),
                         head_bold(observers_m))
  j_cells <- sheet_cells(joined_m, c(1.5, 0.95, 1.2, 1.1),
                         key_fill(joined_m, 3), head_bold(joined_m))
  o_cells[, c("ymin", "ymax", "y")] <- o_cells[, c("ymin", "ymax", "y")] - 6.2
  shift <- 6.55
  j_cells[, c("xmin", "xmax", "x")] <- j_cells[, c("xmin", "xmax", "x")] + shift
  j_cells[, c("ymin", "ymax", "y")] <- j_cells[, c("ymin", "ymax", "y")] - 2.0
  cells <- rbind(n_cells, o_cells, j_cells)
  labels <- data.frame(
    x = c(0, 0, shift),
    y = c(0.15, -6.05, -1.85),
    label = c("nests.csv", "observers.csv", "Result: one row per nest")
  )
  ggplot() +
    geom_rect(data = cells,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = fill), colour = bb852$axis, linewidth = 0.3) +
    geom_text(data = cells,
              aes(x = x, y = y, label = label, hjust = hjust,
                  fontface = face),
              family = bb852_font, colour = bb852$text, size = ts) +
    geom_text(data = labels, aes(x = x, y = y, label = label),
              hjust = 0, vjust = 0, family = bb852_font, fontface = "bold",
              colour = bb852$slate, size = ts) +
    annotate("segment", x = 4.0, xend = 6.3, y = -3.4, yend = -3.4,
             colour = bb852$coral, linewidth = 1.1,
             arrow = arrow(length = unit(0.14, "in"), type = "closed")) +
    annotate("text", x = 4.05, y = -3.75, family = mono_font,
             colour = bb852$coral, size = ts, hjust = 0, vjust = 1,
             label = 'left_join(nests,\n  observers,\n  by = "observer")',
             lineheight = 0.95) +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    theme_blank_bb852() +
    theme(plot.margin = margin(22, 8, 6, 8))
}
plots$join_by_key <- make_join()
save_both(make_join, "join_by_key")


# Folder structure ----------------------------------------------------------
# Matches the "Suggested folder structure" in the Paths and projects chapter,
# with a README and data dictionary added.

make_folders <- function(size_scale = 1) {
  ts <- 5.6 * size_scale
  tree <- data.frame(
    depth = c(0, 1, 1, 2, 2, 1, 2, 1, 1, 1),
    label = c("BB852/", "BB852.Rproj", "CourseData/", "sduBirds.csv",
              "sduBirds_dict.csv", "scripts/", "01_clean.R",
              "plots/", "writing/", "README.txt"),
    note = c("one per project", "sets working dir", "data files",
             "raw, never edited", "column meanings", "code",
             "cleaning steps", "figures", "notes, reports",
             "who, what, when, how")
  )
  tree$y <- -seq_len(nrow(tree))
  tree$x <- tree$depth * 0.55
  tree$is_dir <- grepl("/$", tree$label)
  ggplot(tree) +
    geom_segment(
      data = tree[tree$depth > 0, ],
      aes(x = x - 0.4, xend = x - 0.1, y = y, yend = y),
      colour = bb852$axis, linewidth = 0.5
    ) +
    geom_text(aes(x = x, y = y, label = label,
                  colour = ifelse(is_dir, bb852$slate, bb852$text),
                  fontface = ifelse(is_dir, "bold", "plain")),
              hjust = 0, family = mono_font, size = ts) +
    geom_text(aes(x = 4.3, y = y, label = note), hjust = 0,
              family = bb852_font, colour = bb852$muted, size = ts) +
    scale_colour_identity() +
    coord_cartesian(xlim = c(-0.1, 7.3), clip = "off") +
    theme_blank_bb852()
}
plots$folder_structure <- make_folders()
save_both(make_folders, "folder_structure", half_scale = 0.9)


# Data dictionary -----------------------------------------------------------
# Column names follow CourseData/sduBirds.csv; descriptions are written for
# teaching and should be checked against the project's own protocol. Species
# codes point to a separate code list: the course book defines BT, GT and NH,
# but not MT or WR, so their meanings are not invented here.

dict_m <- rbind(
  c("variable", "description", "units / allowed values"),
  c("Year", "year of the visit", "2013 to 2019"),
  c("Day", "day of the year", "1 to 366"),
  c("boxNumber", "nest-box identifier", "integer"),
  c("species", "species code", "species_codes.csv; NA = none"),
  c("nEggs", "eggs counted", "count; NA = not counted"),
  c("observerID", "who recorded it", "initials; NA = unknown")
)
dict_fill <- matrix(NA_character_, nrow(dict_m), 3)
dict_fill[1, ] <- "#C9D6E6"
dict_bold <- matrix(FALSE, nrow(dict_m), 3)
dict_bold[1, ] <- TRUE

make_dictionary <- function(size_scale = 1) {
  sheet_plot(dict_m, c(1.45, 2.3, 3.15), dict_fill, dict_bold,
             text_size = 5.2 * size_scale, headers = FALSE)
}
plots$data_dictionary <- make_dictionary()
save_both(make_dictionary, "data_dictionary", half_scale = 0.58)

message("Data management figures written to ", output_root)


# Title-slide cover ---------------------------------------------------------
# A purpose-built cover: the tidy sheet drawn light-on-dark, cropped so that it
# reads as a texture behind the title. Saved only in output/cover/.

make_cover <- function() {
  cover_m <- rbind(tidy_m, tidy_m[-1, ], tidy_m[-1, ])
  cells <- sheet_cells(cover_m, tidy_widths)
  cells$fill <- ifelse(cells$row == 1, "#3B5F86", "#2C4D70")
  ggplot(cells) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = fill), colour = "#4A6B90", linewidth = 0.35) +
    geom_text(aes(x = x, y = y, label = label, hjust = hjust),
              family = mono_font, colour = "#C9D6E6", size = 4.2) +
    scale_fill_identity() +
    coord_cartesian(xlim = c(0.3, 5.2), ylim = c(-21.5, -0.2), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#2C4D70", colour = NA),
          plot.margin = margin(0, 0, 0, 0))
}
plots$cover <- make_cover()
save_bb852(plots$cover, "cover", layout = "cover", bg = "#2C4D70")


# Nest record card ------------------------------------------------------------
# An original card drawn for the lecture, in the general style of paper nest
# record cards (e.g. the BTO Nest Record Scheme), so no third-party image is
# needed. The visits are invented teaching values for one great tit nest.

make_card <- function(size_scale = 1) {
  ts <- 5.0 * size_scale
  fields <- data.frame(
    x = c(0.35, 2.35, 4.05),
    label = c("Species  GT", "Box  17", "Year  2025")
  )
  visits <- rbind(
    c("Date", "Eggs", "Young", "Code"),
    c("2025-04-18", "0", "0", "B"),
    c("2025-04-24", "5", "0", "E"),
    c("2025-05-02", "9", "0", "E"),
    c("2025-05-16", "0", "9", "Y"),
    c("2025-06-01", "0", "0", "F")
  )
  v_fill <- matrix(NA_character_, nrow(visits), ncol(visits))
  v_fill[1, ] <- "#C9D6E6"
  v_bold <- matrix(FALSE, nrow(visits), ncol(visits))
  v_bold[1, ] <- TRUE
  v_cells <- sheet_cells(visits, c(1.85, 1.05, 1.15, 1.0), v_fill, v_bold)
  v_cells[, c("xmin", "xmax", "x")] <- v_cells[, c("xmin", "xmax", "x")] + 0.35
  v_cells[, c("ymin", "ymax", "y")] <- v_cells[, c("ymin", "ymax", "y")] - 2.0
  ggplot() +
    annotate("rect", xmin = 0.08, xmax = 5.72, ymin = -9.95, ymax = 0.42,
             fill = "#00000014", colour = NA) +
    annotate("rect", xmin = 0, xmax = 5.65, ymin = -9.85, ymax = 0.5,
             fill = bb852$cell, colour = bb852$axis, linewidth = 0.5) +
    annotate("rect", xmin = 0, xmax = 5.65, ymin = -0.45, ymax = 0.5,
             fill = bb852$slate, colour = NA) +
    annotate("text", x = 0.35, y = 0.02, label = "NEST RECORD CARD",
             hjust = 0, family = bb852_font, fontface = "bold",
             colour = "white", size = ts) +
    geom_text(data = fields, aes(x = x, y = -1.15, label = label),
              hjust = 0, family = bb852_font, colour = bb852$text,
              size = ts) +
    geom_rect(data = v_cells,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = fill), colour = bb852$axis, linewidth = 0.3) +
    geom_text(data = v_cells,
              aes(x = x, y = y, label = label, hjust = hjust,
                  fontface = face),
              family = bb852_font, colour = bb852$text, size = ts) +
    annotate("text", x = 0.35, y = -8.75, hjust = 0, vjust = 0.5,
             family = bb852_font, colour = bb852$muted, size = ts,
             lineheight = 0.95,
             label = "Codes: B building, E eggs, Y young,\nF fledged, A abandoned") +
    scale_fill_identity() +
    coord_cartesian(xlim = c(-0.15, 5.85), ylim = c(-10.1, 0.65),
                    expand = FALSE) +
    theme_blank_bb852()
}
plots$nest_record_card <- make_card()
save_both(make_card, "nest_record_card", half_scale = 1)


# BTO nest record card (third-party image) --------------------------------
# Completed card from the BTO Nest Record Scheme, used with permission for
# educational use. Source: Crick, H., Dudley, C. & Glue, D. (2003, revised by
# P. Beaven & D. Leech) The Nest Record Scheme Handbook. British Trust for
# Ornithology, Thetford. (c) BTO. The source file is small (541 x 369 px), so
# a higher-resolution scan would sharpen it. The image is placed on the slide
# background with a source line underneath (Figure & Bullets has no caption).

card_file <- file.path("lecture_figures", "data_management", "source_images",
                       "bto_nest_record_card.jpg")
if (!requireNamespace("jpeg", quietly = TRUE)) {
  stop("The BTO card figure needs the jpeg package: install.packages(\"jpeg\")")
}
if (!file.exists(card_file)) stop("Missing source image: ", card_file)
{
  card_img <- jpeg::readJPEG(card_file)
  card_aspect <- dim(card_img)[1] / dim(card_img)[2]   # height / width
  make_bto_card <- function(size_scale = 1) {
    w <- 4.7
    h <- w * card_aspect
    top <- 4.15
    ggplot() +
      annotate("rect", xmin = 0.24, xmax = 0.24 + w, ymin = top - h - 0.04,
               ymax = top - 0.04, fill = "#00000018", colour = NA) +
      annotation_raster(card_img, xmin = 0.18, xmax = 0.18 + w,
                        ymin = top - h, ymax = top, interpolate = TRUE) +
      annotate("rect", xmin = 0.18, xmax = 0.18 + w, ymin = top - h,
               ymax = top, fill = NA, colour = bb852$axis, linewidth = 0.4) +
      annotate("text", x = 0.18, y = top - h - 0.32, hjust = 0, vjust = 1,
               family = bb852_font, colour = bb852$muted,
               size = 5.0 * size_scale, lineheight = 0.95,
               label = "Source: BTO Nest Record Scheme Handbook (2003).\n© British Trust for Ornithology. Used with permission.") +
      coord_fixed(xlim = c(0, 5.57), ylim = c(0, 4.25), expand = FALSE) +
      theme_blank_bb852() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  plots$bto_nest_record_card <- make_bto_card()
  save_both(make_bto_card, "bto_nest_record_card", half_scale = 1)
}
