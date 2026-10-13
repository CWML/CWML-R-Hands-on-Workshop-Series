# Session 7: Plotting Further — Bar Charts, Colors, Epicurves, and Facets
# R Hands-on Workshop Series — STUDENT VERSION
#________________________________________________________________________________
#
# HOW TO USE THIS SCRIPT
#
#   📖 What you need to know  short notes on the idea behind each part
#   👀 Follow along           type the code with the instructor in the empty space
#   ✏️ TRY IT                 do the same thing yourself on the practice data
#   ✅ Check                  how to tell your TRY IT worked
#   ⚠️ Common mistakes        what usually goes wrong
#
#   Pattern: lines show the shape of the code. Replace the words in CAPITALS.
#
#   Run a line:      put your cursor on it, press Cmd + Return (Mac) or Ctrl + Enter (Windows)
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 7 - Plotting Further_SOLUTIONS.R
#
#   REMEMBER: ggplot layers are joined with + (not %>%), at the END of each line.
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Import the finished data, order the categories, and save some colors.
#
# 📖 What you need to know
#    - Saving your colors in a named vector means every plot uses the SAME color
#      for the same category:
#        vacc_colors <- c("Yes" = "#2a78d6", "No" = "#eb6834", "Unknown" = "#9a9a95")
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio; import combined_linelist.rds as linelist
#    and fct_relevel() age_cat and outcome (same as Session 6)


# ✏️ TRY IT (3 min): import measles_final.rds as measles, fct_relevel() age_cat and
#    vaccinated (same as Session 6), and save vacc_colors (above)
#    ✅ Check: vacc_colors prints three named colors



# ── PART 2: Bar charts of counts: geom_bar() ───────────────────────────────
# 🎯 Goal: Count rows in each category, order the bars, and split them by a second variable.
#
# 📖 What you need to know
#    - geom_bar() COUNTS the rows in each category for you. Give it only x (or y).
#    - Long labels? Put the category on y to get horizontal bars: aes(y = COLUMN)
#    - Order the bars by count: fct_infreq(COLUMN) (most common first);
#      wrap it in fct_rev() so the longest bar is at the TOP of a horizontal chart.
#    - Fill by a second variable, and choose how the pieces sit with position =
#        "stack"  stacked counts (the default)
#        "fill"   every bar is 100%: compares PROPORTIONS across groups
#        "dodge"  side by side: compares COUNTS within each group
#    - y = NULL in labs() removes an axis title you don't need.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: geom_bar() with x = hospital_name, then y = hospital_name


# 👀 Follow along: order the bars with fct_rev(fct_infreq(hospital_name)), add a fill color and labs()


# 👀 Follow along: x = age_cat, fill = outcome with position = "stack", "fill", and "dodge"


# ✏️ TRY IT (3 min): a horizontal bar chart of measles cases by district, longest bar on top
#    ✅ Check: North is at the top; Unknown is at the bottom



# ── PART 3: Bar charts of values: geom_col() ───────────────────────────────
# 🎯 Goal: Plot numbers you calculated yourself.
#
# 📖 What you need to know
#    - geom_bar()  counts rows for you (one row per patient): give it x only
#    - geom_col()  draws numbers you already have (one row per bar): give it x AND y
#    - Typical use: summarise() or count() first (Session 5), then geom_col().
#    - fct_reorder(CATEGORY, NUMBER) sorts the bars by the number.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: create death_by_age (percentage who died per age group), then geom_col()


# ✏️ TRY IT (5 min):
#    a) calculate cases per 10,000 people by district (Session 5), leaving out "Unknown"
#    b) plot it with geom_col(), using x = fct_reorder(district, rate_per_10k)
#    ✅ Check: bars from Central (lowest, 5.2) to East (highest, 22.8)



# ── PART 4: Choosing colors ────────────────────────────────────────────────
# 🎯 Goal: Pick colors on purpose, and reuse them.
#
# 📖 What you need to know
#    - Choose colors people with color blindness can tell apart. AVOID red vs
#      green, and two shades of the same color.
#    - Give "Unknown" or "Other" a gray so it doesn't compete with real categories.
#    - scale_fill_manual(values = YOUR_COLORS) for fill; scale_color_manual() for color.
#    - scale_y_continuous(labels = scales::percent) shows 0.25 as 25%.
#    - Long x-axis labels: theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: save outcome_colors, then a 100% bar chart of outcome by age_cat
#    with scale_fill_manual(), a white outline, and a percent y-axis


# ✏️ TRY IT (4 min): a 100% bar chart of vaccinated by age_cat, with vacc_colors,
#    a percent y-axis, labels, and theme_minimal()
#    ✅ Check: every bar reaches 100%; "Yes" is blue in every bar



# ── PART 5: Epidemic curves ────────────────────────────────────────────────
# 🎯 Goal: Plot the number of new cases over time.
#
# 📖 What you need to know
#    - An epidemic curve ("epicurve") is a histogram of a DATE column.
#    - binwidth = 7 makes each bar one week.
#    - scale_x_date(date_breaks = "1 month", date_labels = "%b") controls the axis:
#        %d day    %b short month (Jan)    %B full month (January)    %Y year
#        "\n" inside date_labels starts a new line: "%b\n%Y"
#    - Plots reveal problems that tables hide: look at the whole range first!
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: geom_histogram() of onset_date with binwidth = 7. What's going on before 2014?


# 👀 Follow along: keep onset dates from 2014, add scale_x_date() and labs()


# 👀 Follow along: fill by outcome with outcome_colors


# ✏️ TRY IT (5 min): a weekly epicurve of measles report_date, filled by vaccinated,
#    with vacc_colors and scale_x_date(date_breaks = "1 month", date_labels = "%b").
#    HINT: filter(!is.na(report_date)) first. When did the outbreak peak?
#    ✅ Check: the peak is in the second half of March (about 115 cases in one week)



# ── PART 6: Facets ─────────────────────────────────────────────────────────
# 🎯 Goal: Split one plot into small panels, one per category.
#
# 📖 What you need to know
#    - facet_wrap(~ COLUMN) makes one panel per value, all with the same axes so
#      they are easy to compare.
#    - ncol = 2 sets the number of columns; scales = "free_y" gives each panel its
#      own y-axis (harder to compare, so use with care).
#    - facet_grid(ROWS ~ COLUMNS) makes a grid from two variables.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: the epicurve with facet_wrap(~ hospital_name) (leave out "Other/Unknown")


# 👀 Follow along: an age histogram with facet_wrap(~ gender) (leave out "Unknown")


# ✏️ TRY IT (4 min): facet your measles epicurve by district (leave out "Unknown").
#    Did every district peak at the same time?
#    ✅ Check: five panels; every district peaks in March



# ── PART 7: Saving plots ───────────────────────────────────────────────────
# 🎯 Goal: Save a plot to a file you can put in a report or slides.
#
# 📖 What you need to know
#    - Save the plot as an object first: my_plot <- ggplot(...) + ...
#    - ggsave(here("outputs", "FILE_NAME.png"), plot = my_plot,
#             width = 8, height = 5, dpi = 300)
#    - .png for slides and documents, .pdf for print. width and height are in
#      inches; dpi = 300 is print quality.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: save the epicurve as an object, then ggsave() it to outputs/epicurve.png


# ✏️ TRY IT (3 min): save your measles epicurve to outputs/measles_epicurve.png,
#    8 inches wide, 5 inches high, dpi = 300
#    ✅ Check: open the file from the Explorer (outputs folder). Does it look right?



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. Make a bar chart of the percentage of measles cases who died in each age group
#     (summarise() first, then geom_col()).
#  2. Try scale_fill_viridis_d() instead of scale_fill_manual() on one of your plots.
#  3. Brought your own data? Make a bar chart, a plot over time (if you have dates),
#     and a faceted plot, and save one of them with ggsave().
#________________________________________________________________________________

