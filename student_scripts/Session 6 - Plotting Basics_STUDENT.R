# Session 6: Plotting Basics with ggplot2
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
#   Numbers:  Follow along 5.3 and TRY IT 5 have the same numbers on the course
#             website, so you can always find your place in either one.
#
#   Run a line:      put your cursor on it, press Cmd + Return (Mac) or Ctrl + Enter (Windows)
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 6 - Plotting Basics_SOLUTIONS.R
#
#   REMEMBER: ggplot layers are joined with + (not %>%), at the END of each line.
#   Build every plot one layer at a time, and run it after each +.
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Import the finished data and put the categories in order.
#
# 📖 What you need to know
#    - The factor order from Session 5 isn't saved in the file, so we set it
#      again right after importing. You can do both in one pipeline:
#        DATA <- import(...) %>% mutate(COLUMN = fct_relevel(COLUMN, ...))
#    - The set-up lines for linelist are filled in for you: just run them.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 1.1: load tidyverse, here, rio; import combined_linelist.rds as linelist
#    and fct_relevel() age_cat and outcome (same as Session 5) (already filled in: run it)
library(tidyverse)
library(here)
library(rio)

linelist <- import(here("processed_data", "combined_linelist.rds"), trust = TRUE) %>%
  mutate(
    age_cat = fct_relevel(age_cat, "Under 18", "18-29", "30-39", "40-49",
                          "50-59", "60-69", "70-79", "80+", "Unknown"),
    outcome = fct_relevel(outcome, "Recover", "Death", "Unknown")
  )


# ✏️ TRY IT 1 (3 min): import measles_final.rds as measles and fct_relevel()
#    age_cat ("Under 5", "5-9", "10-14", "15+", "Unknown") and vaccinated ("Yes", "No", "Unknown")
#    ✅ Check: levels(measles$age_cat) starts with "Under 5"



# ── PART 2: The grammar of graphics ────────────────────────────────────────
# 🎯 Goal: Build a plot one piece at a time.
#
# 📖 What you need to know
#    Every ggplot is built from the same parts, joined with +:
#      ggplot(data = DATA)          which data frame?            (an empty canvas)
#      mapping = aes(x = COLUMN)    which columns go where?      (axes appear)
#      + geom_...()                 what shape? bars, points...  (now there's a plot)
#      + labs()                     titles and axis labels
#      + theme_...()                the overall look
#
# ⚠️ Common mistakes
#    - Using %>% between layers instead of +
#    - Putting + at the START of a line instead of the END. R then says:
#      "Cannot use `+` with a single argument. Did you accidentally put `+` on a new line?"
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 2.1: 1. ggplot(data = linelist)   2. add aes(x = age_years)   3. add geom_histogram()


# ✏️ TRY IT 2 (2 min): build a histogram of measles temp_c in the same three steps
#    ✅ Check: step 2 shows an empty plot with temperatures on the x-axis



# ── PART 3: Histograms ─────────────────────────────────────────────────────
# 🎯 Goal: Show the distribution of one number, and choose the bin width and colors.
#
# 📖 What you need to know
#    - A histogram groups values into ranges ("bins") and shows how many fall in each.
#    - binwidth = 5 makes each bar 5 units wide. Try a few to see the shape.
#    - Inside geom_histogram(): fill = inside color, color = outline color.
#      A white outline separates the bars.
#    - Colors can be names ("steelblue") or hex codes ("#2a78d6").
#
# 🔁 Base R: hist(linelist$age_years) draws a histogram in one line, but you can't
#    add layers to it later. Read the 🔁 box in Part 3 on the website (no typing).
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 3.1: age histogram with binwidth = 5 (then try 1 and 10)


# 👀 Follow along 3.2: add fill = "#2a78d6" and color = "white"


# ✏️ TRY IT 3 (3 min): a histogram of measles age_years with binwidth = 1, a fill color,
#    and a white outline. Which age has the most cases?
#    ✅ Check: the tallest bar is at age 2 (about 100 cases). The warning about
#              "removed rows" is the 29 missing ages: that's fine.



# ── PART 4: Mapping vs setting ─────────────────────────────────────────────
# 🎯 Goal: Color by a column (mapping) or use one fixed color (setting).
#
# 📖 What you need to know
#    - MAPPING, inside aes():   aes(x = age_years, fill = outcome)
#      a COLUMN controls the color. Each value gets a color, and you get a legend.
#    - SETTING, outside aes():  geom_histogram(fill = "#2a78d6")
#      ONE fixed color for everything.
#
# ⚠️ Common mistakes
#    - aes(fill = "blue") does NOT make blue bars! ggplot treats "blue" as data
#      and gives it the first default color (pink-red), with a legend saying "blue".
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 4.1: age histogram with fill = outcome inside aes()


# 👀 Follow along 4.2: the same with fill = "#2a78d6" in geom_histogram()


# 👀 Follow along 4.3: what happens with fill = "blue" inside aes()?


# ✏️ TRY IT 4 (3 min): a histogram of measles temp_c with binwidth = 0.2, then map
#    fill = fever inside aes(). Why are there two "humps"?
#    ✅ Check: the small hump near 37 °C is "no" fever; the big one near 39 °C is "yes"



# ── PART 5: Scatter plots ──────────────────────────────────────────────────
# 🎯 Goal: Show the relationship between two numbers.
#
# 📖 What you need to know
#    - geom_point() draws one point per row: aes(x = COLUMN_1, y = COLUMN_2)
#    - Many overlapping points? size = 1 makes them smaller; alpha = 0.3 makes
#      them see-through (0 = invisible, 1 = solid).
#    - geom_smooth(method = "lm", se = FALSE) adds a straight trend line.
#    - You can pipe data INTO ggplot() to filter first:
#        DATA %>% filter(...) %>% ggplot(aes(...)) + geom_point()
#      %>% before ggplot(), + after it. (Reading only on the website; you'll use it in TRY IT 5.)
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 5.1: age_years vs wt_kg with geom_point()


# 👀 Follow along 5.2: add size = 1, alpha = 0.3; then map color = gender


# 👀 Follow along 5.3: add geom_smooth(method = "lm", se = FALSE, color = "#eb6834")


# ✏️ TRY IT 5 (4 min): for hospitalized patients only (filter(!is.na(days_in_hospital))),
#    plot age_years (x) against days_in_hospital (y) with alpha, and add a trend line.
#    Is there a relationship?
#    ✅ Check: 427 hospitalized patients; the trend line is almost flat (no relationship)



# ── PART 6: Box plots ──────────────────────────────────────────────────────
# 🎯 Goal: Compare a number across groups.
#
# 📖 What you need to know
#    - A box plot: thick line = median; box = middle 50% (25th to 75th percentile,
#      the IQR); whiskers = the rest; dots = possible outliers.
#    - aes(x = GROUP, y = NUMBER, fill = GROUP)
#    - theme(legend.position = "none") removes a legend you don't need.
#    - stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white")
#      adds the mean as a white diamond. (Reading only on the website; copy this
#      line in TRY IT 6.)
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 6.1: age_years by gender with geom_boxplot()


# 👀 Follow along 6.2: filter out "Unknown", add fill = gender, and remove the legend


# ✏️ TRY IT 6 (4 min, homework if behind): a box plot of days_in_hospital by vaccinated (hospitalized
#    patients only), with the mean and no legend
#    ✅ Check: three boxes; the medians are 4–5 days for every group



# ── PART 7: Labels and themes ──────────────────────────────────────────────
# 🎯 Goal: Finish a plot with a title, axis labels, and a theme.
#
# 📖 What you need to know
#    - labs(title = , subtitle = , x = , y = , fill = , caption = )
#    - Complete themes: theme_minimal(), theme_classic(), theme_bw(), theme_light()
#      theme_minimal(base_size = 14) makes all the text bigger (good for slides).
#    - Put a complete theme BEFORE theme(...) changes, or it overwrites them.
#    - Save a plot as an object with <- and add layers to it later:
#        my_plot <- ggplot(...) + geom_...()
#        my_plot + labs(...) + theme_minimal()
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 7.1: add labs() to the box plot


# 👀 Follow along 7.2: try theme_minimal(), theme_classic(), theme_bw() on the age histogram


# 👀 Follow along 7.3: save a plot as age_plot, then add labs() and a theme to it


# ✏️ TRY IT 7 (5 min): make one of your measles plots presentation-ready. Save it as
#    my_plot, then go through the checklist:
#      [ ] the title says what the plot shows, or better, what it finds
#      [ ] both axis labels are words with units: "Temperature (°C)", not temp_c
#      [ ] the legend title is a word too (fill = "Fever" in labs()), or the legend is gone
#      [ ] a caption names the data source
#      [ ] the text is big enough to read from the back of the room (base_size)
#      [ ] colors are fixed on purpose, not just the defaults (Session 7 shows how)
#    ✅ Check: typing my_plot in the Console shows your finished plot, and you can
#              tick the first five boxes



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. Make the age histogram with fill = vaccinated. Which ages have the most
#     unvaccinated cases?
#  2. Swap geom_boxplot() for geom_violin() in your box plot. What does it show?
#  3. Brought your own data? Make a histogram, a scatter plot, and a box plot.
#________________________________________________________________________________

