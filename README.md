## READ ME

Things to do each year

1) Check schedule times are OK. Edit the line:

`savingsTimeSwitch <- with_tz(lubridate::as_datetime("2021-10-31 03:00:00"),"Europe/Copenhagen")`

Which is in the `Index.Rmd` and `personalCalendar.R` files.

Confim that the times are correct by cross referencing the outputs on the website with the official calendar.

2) Edit the Instructors part of `Index.Rmd` to add/remove instructors as appropriate.

3) Edit the Excel schedule `BB852_Schedule.xlsx` to put the instructors in the correct place.

4) Remove/edit the old exam(s).

5) Spell check (use `SpellCheckScript.R`)

6) Check/add/remove items in `ExtraReading.Rmd`

7) Rebuild GitHub site. (`Build Book` button in RStudio)