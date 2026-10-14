# Session 4: Data Structures — Vectors, Subsetting, and Reshaping
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
#   Insert a pipe:   Shift + Cmd + M (Mac) or Shift + Ctrl + M (Windows)
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 4 - Data Structures_SOLUTIONS.R
#
#   Sessions 1–3 used tidyverse functions that hide how R stores data.
#   Today we look under the hood: this explains WHY those functions work.
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Load packages and import the finished data from Session 3.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio; import processed_data/combined_linelist.rds
#    (trust = TRUE) and call it linelist


# ✏️ TRY IT (2 min): import processed_data/measles_final.rds (trust = TRUE) and call it measles
#    ✅ Check: in the Variables pane, measles shows 1163 rows and 14 columns



# ── PART 2: Vectors and types ──────────────────────────────────────────────
# 🎯 Goal: See that every column is a vector, and every vector has one type.
#
# 📖 What you need to know
#    - A vector is a set of values of the SAME type. Every column of a data
#      frame is a vector. DATA$COLUMN pulls one out.
#    - length() tells you how many values a vector has.
#    - class() tells you the type:
#        "numeric"    numbers                38.5, 2
#        "character"  text                   "male", "Recover"
#        "logical"    TRUE / FALSE
#        "Date"       dates                  as.Date("2014-05-13")
#        "factor"     categories with a set order (Session 5)
#    - Mix types in one vector and R converts everything to the most flexible
#      type, usually text: c(1, 2, "unknown") is all text. That's why one
#      "unknown" made the whole age column text in Session 1!
#    - Any calculation with NA gives NA, unless you add na.rm = TRUE.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: pull out linelist$age_years as ages; head() and length() it


# 👀 Follow along: class() of age_years, gender, and onset_date


# 👀 Follow along: what happens with c(1, 2, "unknown") and c(1, TRUE, FALSE)?


# 👀 Follow along: mean(c(38, 39, NA)) with and without na.rm = TRUE


# ✏️ TRY IT (3 min): class() of measles$age_years, measles$vaccinated, and
#    measles$report_date, and length() of measles$temp_c
#    ✅ Check: "numeric", "character", "Date", and 1163



# ── PART 3: Vectorization ──────────────────────────────────────────────────
# 🎯 Goal: Do a calculation on every value at once, and count with TRUE/FALSE.
#
# 📖 What you need to know
#    - Most R calculations work on EVERY element of a vector at once:
#        c(36.8, 38.5, 40.1) * 9 / 5 + 32   converts all three temperatures
#      This is called vectorization. It's why mutate() works on a whole column
#      without a loop.
#    - A comparison gives one TRUE/FALSE per element:  temps > 38
#    - TRUE counts as 1 and FALSE as 0, so:
#        sum(CONDITION)    COUNTS the TRUEs            sum(temp > 38, na.rm = TRUE)
#        mean(CONDITION)   gives the PROPORTION TRUE   mean(fever == "yes")
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: make temps_c <- c(36.8, 38.5, 40.1) and convert it to Fahrenheit


# 👀 Follow along: head() of every patient's weight in pounds: linelist$wt_kg * 2.2


# 👀 Follow along: temps_c > 38, then sum() and mean() of linelist$temp > 38,
#    and mean(linelist$fever == "yes")


# ✏️ TRY IT (4 min):
#    a) convert measles$temp_c to Fahrenheit and look at the first values with head()
#    b) count the cases with a temperature of 39 or more: sum(... >= 39)
#    c) find the proportion of cases who were vaccinated: mean(... == "Yes")
#    ✅ Check: a) starts 100.94, 101.12   b) 565   c) 0.238



# ── PART 4: Subsetting with base R ─────────────────────────────────────────
# 🎯 Goal: Pull out parts of vectors and data frames with [ ], $, and [[ ]].
#
# 📖 What you need to know
#    Vectors: x[ ]
#      x[1]          the first element
#      x[c(1, 3)]    the first and third
#      x[-1]         everything EXCEPT the first
#      x[x > 38]     the elements that meet a condition
#
#    Data frames: df[ROWS, COLUMNS]   rows first, then a comma, then columns
#      df[1:3, c("a", "b")]   rows 1–3, columns a and b
#      df[1:3, ]              rows 1–3, all columns (leave a side empty to keep all)
#      df$a  or  df[["a"]]    one column as a vector ([[ ]] is handy inside functions)
#
#    Base R vs tidyverse
#      df[df$age > 40, ]       filter(df, age > 40)
#      df[, c("a", "b")]       select(df, a, b)
#      df$a                    pull(df, a)
#
# ⚠️ Common mistakes
#    - Forgetting the comma: df[1:3] picks COLUMNS 1–3, not rows!
#    - Base R adds a row of NAs for every NA in the condition.
#      linelist[linelist$temp > 40, ] gives 244 rows; filter(linelist, temp > 40) gives 93.
#      Fix: add & !is.na(temp), or use filter().
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: temps_c[1], temps_c[c(1, 3)], temps_c[-1], temps_c[temps_c > 38]


# 👀 Follow along: linelist[1:3, c("case_id", "age_years", "outcome")]


# 👀 Follow along: the first three hospital names with $ and with [[ ]]


# 👀 Follow along: the mean temperature of patients who died:
#    mean(linelist$temp[linelist$outcome == "Death"], na.rm = TRUE)


# 👀 Follow along: compare nrow(linelist[linelist$temp > 40, ]) with nrow(filter(linelist, temp > 40))


# ✏️ TRY IT (5 min): with base R,
#    a) show rows 1–5 of the patient_id and temp_c columns of measles
#    b) find the mean temperature of children who died (outcome == "Died")
#    c) count the cases aged 15 or older with nrow(measles[...]), then with filter().
#       Why are the answers different? Fix the base R version.
#    ✅ Check: a) the first ID is MS-0931   b) 38.75   c) 225 vs 196 (29 missing ages)



# ── PART 5: Tidy data and pivot_longer() ───────────────────────────────────
# 🎯 Goal: Reshape wide data into long data.
#
# 📖 What you need to know
#    - Data is TIDY when each variable is a column, each observation is a row,
#      and each value is a cell.
#    - WIDE: one column per symptom (fever, chills, cough, ...)
#      LONG: one row per patient PER symptom, with a "symptom" column and a
#            "present" column. Long is easier to count and plot.
#    - pivot_longer(
#        cols      = COLUMNS_TO_STACK,     e.g. fever:vomit  (every column from fever to vomit)
#        names_to  = "NEW_NAME_COLUMN",    gets the old column names
#        values_to = "NEW_VALUE_COLUMN"    gets the values
#      )
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: look at the symptom columns: linelist %>% select(case_id, fever:vomit)


# 👀 Follow along: pivot_longer() the five symptoms into symptoms_long; check nrow()


# 👀 Follow along: count the patients with each symptom (filter present == "yes", then count)
#    and plot it with ggplot() + geom_col() (a preview of Session 6)


# ✏️ TRY IT (5 min): select patient_id, fever, and rash from measles, then
#    pivot_longer() the two symptom columns. count() symptom and present.
#    Pattern: pivot_longer(cols = c(COLUMN_1, COLUMN_2), names_to = "symptom", values_to = "present")
#    ✅ Check: 2326 rows (1163 × 2); fever yes 1064, rash yes 976



# ── PART 6: pivot_wider() ──────────────────────────────────────────────────
# 🎯 Goal: Reshape long data into a wide table people can read.
#
# 📖 What you need to know
#    - pivot_wider() is the opposite of pivot_longer():
#        pivot_wider(
#          names_from  = COLUMN,   its values become the new column NAMES
#          values_from = COLUMN    its values fill the cells
#        )
#    - count(A, B) gives one row per combination (long). pivot_wider() turns it
#      into a cross-table (wide).
#    - values_fill = 0 puts 0 instead of NA where a combination doesn't exist.
#    - Rule of thumb: keep data LONG for analysis and plots; make it WIDE at the
#      end for a table someone will read.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: count(hospital_name, outcome), then pivot_wider() it


# ✏️ TRY IT (4 min): count() district and vaccinated in measles, then pivot_wider()
#    so each vaccination status is a column
#    ✅ Check: 6 rows (5 districts + Unknown); North has No 205, Unknown 49, Yes 88



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. Turn the district × vaccinated table into percentages: before pivot_wider(),
#     group_by(district) and add mutate(pct = round(n / sum(n) * 100, 1)),
#     then use values_from = pct.
#  2. pivot_longer() the fever and rash columns, keep the "yes" rows, and plot
#     the count of each symptom by vaccinated with geom_bar(position = "dodge").
#  3. Brought your own data? Is it wide or long? Reshape it the other way.
#________________________________________________________________________________

