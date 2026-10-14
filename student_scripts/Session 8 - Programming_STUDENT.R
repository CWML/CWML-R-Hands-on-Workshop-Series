# Session 8: Programming — Functions, if/else, Loops, and Iteration
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
#   Run a block:     highlight several lines (a whole function or loop), then Cmd/Ctrl + Enter
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 8 - Programming_SOLUTIONS.R
#
#   Today's goal: "Your manager wants an epidemic curve for every hospital, every
#   week." By the end of the session, one loop does it.
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Load packages and import the data.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio, janitor; import combined_linelist.rds as linelist


# ✏️ TRY IT (1 min): import processed_data/measles_final.rds as measles
#    ✅ Check: in the Variables pane, measles shows 1163 rows and 14 columns



# ── PART 2: Writing your first function ────────────────────────────────────
# 🎯 Goal: Turn a calculation you repeat into a function with a name.
#
# 📖 What you need to know
#    - Rule of thumb: if you copy and paste code more than twice, write a function.
#    - The shape of a function:
#        FUNCTION_NAME <- function(ARGUMENT) {
#          CODE THAT USES ARGUMENT
#        }
#      The ARGUMENT is the input. The function RETURNS the result of its last line.
#    - Save it with <- like any object. Run the whole block to create it; then it
#      appears in the Variables pane.
#    - If the code inside is vectorized (Session 4), the function is too, so it
#      works on a whole column inside mutate().
#    - Default values: function(temp_c, digits = 1). The user can change digits,
#      but doesn't have to.
#
# ⚠️ Common mistakes
#    - Changing the function but not re-running the block: R still uses the old version.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: write c_to_f(temp_c) and test it on 38 and on c(36.8, 38.5, 40.1)


# 👀 Follow along: use c_to_f() inside mutate() to add temp_f to linelist


# 👀 Follow along: add a digits = 1 default argument with round()


# ✏️ TRY IT (4 min): write f_to_c(temp_f) that converts Fahrenheit to Celsius:
#    (temp_f - 32) * 5 / 9, rounded to 1 decimal. Test it on 98.6 and 104.7.
#    ✅ Check: f_to_c(98.6) is 37 and f_to_c(104.7) is 40.4



# ── PART 3: Functions that use data ────────────────────────────────────────
# 🎯 Goal: Write functions for calculations, tables, and plots you make often.
#
# 📖 What you need to know
#    - A function can take a whole data frame as an argument and return a table
#      or a plot.
#    - An argument holding a VALUE (like a hospital's name) can go straight into
#      filter(): filter(hospital_name == hospital)
#    - paste("Weekly cases:", hospital) builds text from an argument, e.g. a title.
#
# ⚠️ Common mistakes
#    - Don't give an argument the same name as a column! In
#      function(data, district) { data %>% filter(district == district) }
#      R compares the column with itself, so every row is kept.
#      Name it district_name instead.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: write pct(x, digits = 1) that returns round(mean(x, na.rm = TRUE) * 100, digits)
#    and use it in summarise() by hospital


# 👀 Follow along: write hospital_summary(data, hospital) and try it on two hospitals


# 👀 Follow along: write make_epicurve(data, hospital) that returns a weekly epicurve
#    for one hospital, with the hospital's name in the title


# ✏️ TRY IT (6 min):
#    a) write rate_per_10k(cases, population) that returns cases / population * 10000,
#       rounded to 1 decimal. Use it in mutate() after count(district, population).
#    b) write district_summary(data, district_name) that returns the number of cases and
#       the percentage vaccinated (use your pct() function!) for one district.
#    ✅ Check: a) East is 22.8   b) district_summary(measles, "North") gives n 342, 25.7%



# ── PART 4: if / else ──────────────────────────────────────────────────────
# 🎯 Goal: Run different code depending on a condition.
#
# 📖 What you need to know
#    - The shape:
#        if (CONDITION) {
#          CODE WHEN TRUE
#        } else {
#          CODE WHEN FALSE
#        }
#    - if asks ONE yes/no question, so it works on ONE value. Give it a vector and
#      you get: "Error: the condition has length > 1".
#    - For a whole column, use the vectorized if_else() from Session 2.
#    - if is perfect for checking inputs. stop("message") ends the function with
#      a helpful error:  if (!is.numeric(temp_c)) stop("temp_c must be a number")
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: fever_status(temp) with if / else; test it on 39.2 and 36.9


# 👀 Follow along: what happens with fever_status(c(39.2, 36.9))?


# 👀 Follow along: the vectorized version with if_else(), used in mutate() on linelist


# 👀 Follow along: add an input check with stop() to c_to_f(), and try c_to_f("38")


# ✏️ TRY IT (5 min):
#    a) write a one-value fever_status() with if / else and test it on 38.5
#    b) write the vectorized version with if_else(), use it in mutate() on measles$temp_c,
#       and count() the result
#    ✅ Check: a) "fever"   b) 1015 "fever" and 148 "no fever"



# ── PART 5: for loops ──────────────────────────────────────────────────────
# 🎯 Goal: Repeat a whole task once for each value, like saving one plot per hospital.
#
# 📖 What you need to know
#    - The shape:
#        for (ITEM in VECTOR) {
#          CODE THAT USES ITEM
#        }
#      ITEM takes each value of VECTOR in turn.
#    - Inside a loop, nothing prints automatically: use print().
#    - To keep results, create an empty container BEFORE the loop (results <- c())
#      and fill it inside the loop (results[ITEM] <- ...).
#    - make_clean_names() (janitor) turns "St. Mark's Maternity Hospital" into
#      st_marks_maternity_hospital: safe for a file name.
#    - paste0() glues text with no spaces: paste0("epicurve_", NAME, ".png")
#    - In R, try mutate(), group_by() + summarise(), and across() first. They
#      already repeat work for you. Use a loop to repeat a WHOLE task.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: a loop that prints two hospital names


# 👀 Follow along: make the hospitals vector (no "Other/Unknown"), then loop and print
#    the number of cases at each


# 👀 Follow along: save the counts in case_counts


# 👀 Follow along: THE PAYOFF: loop over hospitals and ggsave() one epicurve each to outputs/


# ✏️ TRY IT (6 min): write make_measles_epicurve(data, district_name) for weekly cases by
#    report_date in one district. Then loop over the five districts (not "Unknown") and
#    save each plot to outputs/ as measles_central.png, measles_east.png, ...
#    ✅ Check: five new .png files in the outputs folder



# ── PART 6: Iteration the tidyverse way: map() and walk() ──────────────────
# 🎯 Goal: Do what a loop does, in one line.
#
# 📖 What you need to know (purrr, part of the tidyverse)
#    map(X, FUNCTION)       run FUNCTION on each element of X; return a LIST of results
#    map_dbl(X, FUNCTION)   the same, but return a vector of NUMBERS
#    walk(X, FUNCTION)      run FUNCTION on each element for its side effect
#                           (like saving a file); return nothing
#    - ~ mean(.x, na.rm = TRUE) is a short way to write a function: .x is the
#      current element (the same ~ .x you used with across() in Session 3).
#    - A data frame is a list of columns, so map_dbl(DATA, ...) runs once per column.
#    - Loop or map()? Both are fine. Use the one you can read!
#
# 🔁 Base R: lapply(X, FUNCTION) is like map() (always returns a list).
#    sapply(X, FUNCTION) is like map_dbl(), but it GUESSES the shape of the
#    result (a vector, a matrix, or a list). Extra arguments go after the
#    function: sapply(DATA, mean, na.rm = TRUE)
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: map_dbl() for the mean of age_years, bmi, and temp


# 👀 Follow along: map() to make a list of epicurves, one per hospital; look at the first


# 👀 Follow along: walk() to save them all (the same as the Part 5 loop)


# 🔁 Follow along: the base R way. sapply() and lapply() on age_years, bmi, and temp
#    with mean and with range. Which one returns a matrix?


# ✏️ TRY IT (5 min):
#    a) use map_dbl() to find the mean of age_years, temp_c, and days_in_hospital in measles
#    b) replace your district loop with walk()
#    ✅ Check: a) 8.75, 38.82, 4.94   b) the same five .png files



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. Add a fill argument to make_epicurve(), with a default of "#2a78d6", so you can
#     choose the bar color.
#  2. Add an input check to rate_per_10k(): stop() if population is 0 or missing.
#  3. Write a function that makes the whole measles Table 1 from Session 5, and run it
#     for each district with map().
#  4. Brought your own data? Find a piece of code you copied and pasted, and turn it
#     into a function.
#________________________________________________________________________________

