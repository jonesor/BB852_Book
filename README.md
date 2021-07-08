## READ ME

### Things to do each year:

- Check schedule times are OK. Edit the line: `savingsTimeSwitch <- with_tz(lubridate::as_datetime("2021-10-31 03:00:00"),"Europe/Copenhagen")`, which is in the `Index.Rmd` and `personalCalendar.R` files.
- Confirm that the times are correct by cross referencing the outputs on the website with the official calendar.
- Edit the Instructors part of `Index.Rmd` to add/remove instructors as appropriate.
- Edit the Excel schedule `BB852_Schedule.xlsx` to put the instructors in the correct place.
- Remove/edit the old exam(s).
- Check/add/remove items in `ExtraReading.Rmd`
- Style check: Run `styler::style_dir(filetype = "Rmd")`
- Spell check: use `SpellCheckScript.R`
- Rebuild GitHub site. (`Build Book` button in RStudio)

### Note:

- New data sets (e.g. for the exam) should be added to the `PotentialCourseData` folder (OJ's personal dropbox). It will then be automatically transferred into the `CourseData` folder.
- New simulated data should be created in the `SimulateData.R` script.
