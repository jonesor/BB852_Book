## READ ME

### Build workflow

- Prepare datasets and validate references: run `Rscript -e "source('scripts/PrepareCourseData.R')"` (this syncs `CourseData/` from `DataSetLibrary/`, writes `course_data_files.csv`, and fails if a referenced dataset is missing).
- Build the book: run `_build.sh` (which now runs the preparation step, then renders the gitbook).

### Build/release notes

- The published site is served from `docs/` (GitHub Pages).
- `bookdown::gitbook` output is written to `docs/` per `_bookdown.yml`.
- Use `_clean.sh` to remove caches and generated artifacts before a clean rebuild.
- Hand-authored images can live in `assets/`; generated plots should go in `generated/` (ignored by git).

### QA

- Style check: `scripts/style.R`
- Lint: `scripts/lint.R`
- Spell check: `scripts/SpellCheckScript.R`

### Data provenance

- `DataSetLibrary/` is the source-of-truth library (e.g., synced from Dropbox).
- `CourseData/` is the curated subset used by the book, populated by `scripts/PrepareCourseData.R`.
- `course_data_files.csv` and `datasets_missing.csv` are generated during prep to track usage and missing files.

### Contributor checklist

- Run `_clean.sh` before a release build if the output looks stale.
- Run `_build.sh` and confirm `docs/` updates as expected.
- Verify `CourseData/` contents match current references in `.Rmd` files.
- Check annual items in the “Things to do each year” section below.

### Things to do each year:

- Check schedule times are OK. Edit the line: `savingsTimeSwitch <- with_tz(lubridate::as_datetime("2021-10-31 03:00:00"),"Europe/Copenhagen")`, which is in the `Index.Rmd` and `personalCalendar.R` files.
- Confirm that the times are correct by cross referencing the outputs on the website with the official calendar.
- Edit the Instructors part of `Index.Rmd` to add/remove instructors as appropriate.
- Edit the Excel schedule `BB852_Schedule.xlsx` to put the instructors in the correct place.
- Remove/edit the old exam(s) and place in `historicExams` folder
- Check/add/remove items in `ExtraReading.Rmd`
- Style check: `scripts/style.R`
- Spell check: use `scripts/SpellCheckScript.R`
- Rebuild GitHub site. (`bookdown::render_book("index.Rmd", "bookdown::pdf_book")` and `bookdown::render_book("index.Rmd", "bookdown::gitbook")`)

### Note:

- New data sets (e.g. for the exam) should be added to the `DataSetLibrary` folder (OJ's personal dropbox). It will then be automatically transferred into the `CourseData` folder.
- New simulated data should be created in the `scripts/SimulateData.R` script.
