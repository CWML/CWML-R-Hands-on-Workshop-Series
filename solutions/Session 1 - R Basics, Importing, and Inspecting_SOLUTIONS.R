# Session 1: R Basics, Importing, and Inspecting Data
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: R basics ───────────────────────────────────────────────────────
# 🎯 Goal: Create objects and use functions.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 1.1: store a number in an object called my_age, then add 5 to it
my_age <- 35
my_age
my_age + 5

# 👀 Follow along 1.2: make a vector of ages with c(), then find the mean()
ages <- c(2, 18, 56, 61)
mean(ages)

# 👀 Follow along 1.3: round(3.14159) with and without digits = 2
round(3.14159)
round(3.14159, digits = 2)

# ✏️ TRY IT 1: store three temperatures, 38.5, 39.1, and 37.9, in an object
#    called my_temps. Then find their mean().
my_temps <- c(38.5, 39.1, 37.9)
mean(my_temps)   # 38.5


# ── PART 2: Installing and loading packages ────────────────────────────────
# 🎯 Goal: Install the packages for the whole workshop (ONCE per computer),
#    then load the ones we need today (EVERY time you open R).
# ────────────────────────────────────────────────────────────────────────────

# Install (run once; skip this if you did the pre-workshop setup)
install.packages(c("tidyverse", "here", "rio", "janitor", "skimr", "gtsummary", "flextable", "scales"))

# 👀 Follow along 2.1: load today's packages, one library() line each
library(tidyverse)
library(here)
library(rio)
library(skimr)


# ── PART 3: Project folders and here() ─────────────────────────────────────
# 🎯 Goal: Open the workshop folder in Positron and see how here() builds file paths.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 3.1: find the top of the project folder
here::here()

# 👀 Follow along 3.2: build the path to raw_data/linelist_raw.xlsx
here::here("raw_data", "linelist_raw.xlsx")


# ── PART 4: Importing data ─────────────────────────────────────────────────
# 🎯 Goal: Read data files into R with import() + here().
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 4.1: import raw_data/linelist_raw.xlsx and call it linelist_raw
linelist_raw <- import(here("raw_data", "linelist_raw.xlsx"))

# ✏️ TRY IT 2: import the practice data raw_data/measles_raw.csv and call it measles_raw
measles_raw <- import(here("raw_data", "measles_raw.csv"))


# ── PART 5: Inspecting data ────────────────────────────────────────────────
# 🎯 Goal: Look at the data before changing anything, and find its problems.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 5.1: the size and structure of linelist_raw with dim() and glimpse()
dim(linelist_raw)        # 6604 rows, 12 columns
glimpse(linelist_raw)

# 👀 Follow along 5.2: skim(linelist_raw). Look at n_missing, and the min (p0) and
#    max (p100) of wt_kg and ht_cm.
skim(linelist_raw)       # wt_kg goes down to -11; ht_cm up to 295; Case ID has 130 missing

# ✏️ TRY IT 3: use dim(), glimpse(), and skim() on measles_raw
dim(measles_raw)         # 1215 rows, 11 columns
glimpse(measles_raw)
skim(measles_raw)
# a) `Age (years)` is <chr> (text) but should be a number ("unknown" and "999")
# b) Backticks needed: `Patient ID`, `Report Date`, `Age (years)`, `Vaccinated?`,
#    `Temp (C)`, `Days in Hospital`
# c) The largest Temp (C) is 105 (104.7): about 40 °C written in Fahrenheit
# d) 37 rows have no Patient ID

# 👀 Follow along 5.3: count() the hospital, Gender, OUTCOME, and age_unit columns
count(linelist_raw, hospital)
count(linelist_raw, Gender)
count(linelist_raw, OUTCOME)
count(linelist_raw, age_unit)

# ✏️ TRY IT 4: count() District, SEX, and `Vaccinated?` in measles_raw
count(measles_raw, District)        # North: "North", "north", "Nroth", "North District" = 4 ways
count(measles_raw, SEX)             # Male / male / M and Female / female / F
count(measles_raw, `Vaccinated?`)   # No: "No", "no", "N", "NO" = 4 ways

# ⭐ table() drops the 34 cases with no district
table(measles_raw$District)
table(measles_raw$District, useNA = "ifany")

# ✏️ TRY IT 5: fix the broken code
# Count(measles_raw, District)
#   Error: could not find function "Count". R is case-sensitive: count()
count(measles_raw, District)
# count(measles_raw, district)
#   Error: Column `district` is not found. The column is District, capital D
count(measles_raw, District)
# measles_raw <- import(here("raw_data", "measles_raw.cvs"))
#   Error: No such file. The ending should be .csv
measles_raw <- import(here("raw_data", "measles_raw.csv"))
mean(measles_raw$`Age (years)`)
#   Warning: argument is not numeric or logical: returning NA
#   No typo here: Age (years) is stored as text. It's on the problem list,
#   and you fix it in Session 2.


# ── PART 6: The problem list ───────────────────────────────────────────────
# 🎯 Goal: Write down every problem you found. This list is the to-do list for Session 2.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 6.1: the problem list for linelist_raw (fill it in with the class)
#    Problem                                                          Fixed in
# 1. Column names have spaces, dots, and capitals                     Session 2: clean_names()
# 2. Date columns are different types (date.onset is text)            Session 2: as.Date()
# 3. age is text, and 37 ages are in months, not years                Session 2: as.numeric(), if_else()
# 4. Hospital names are misspelled ("Hopital") or inconsistent
#    ("Regional MC"), and many are missing                            Session 2: case_when()
# 5. Gender and OUTCOME have missing values                           Session 3
# 6. Impossible weights (negative or 0) and heights (4 cm, 295 cm)    Session 2: if_else()
# 7. 130 rows have no Case ID                                         Session 3
# 8. Duplicate rows? Not checked yet                                  Session 3

# ✏️ TRY IT 6: write a problem list for measles_raw
# 1. Column names have spaces, capitals, and symbols (? and parentheses)
# 2. `Age (years)` is stored as text because of "unknown" (27 rows) and "999" (4 rows)
# 3. District is misspelled ("Nroth", "Cetnral"), has lower-case versions, and some say "... District"
# 4. SEX is written 6 different ways (Male / male / M, Female / female / F)
# 5. `Vaccinated?` is written 8 different ways (Yes / yes / Y, No / no / N / NO, Unknown)
# 6. Outcome has inconsistent capitalization and 68 missing values
# 7. Some temperatures were entered in Fahrenheit (values around 97–105)
# 8. 37 rows have no Patient ID
# 9. `Report Date`, District, and SEX have some missing values
# 10. Duplicate rows: not checked yet (Session 3)
