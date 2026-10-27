# Session 1: Setup and a First Look at Your Data
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: R basics ───────────────────────────────────────────────────────
# 🎯 Goal: Create objects, use functions, and build a small data frame.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: store a number in an object called my_age, then add 5 to it
my_age <- 35
my_age
my_age + 5

# 👀 Follow along: make a vector of ages with c(), then find the mean()
ages <- c(2, 18, 56, 61)
mean(ages)

# 👀 Follow along: make a vector of hospital names and find its length()
hospitals <- c("Central Hospital", "Military Hospital", "Other")
length(hospitals)

# 👀 Follow along: round(3.14159) with and without digits = 2
round(3.14159)
round(3.14159, digits = 2)

# 👀 Follow along: make a small data frame with data.frame(), then pull out one column with $
patients <- data.frame(
  name = c("Ana", "Ben", "Cho"),
  age  = c(34, 51, 27)
)
patients
patients$age

# ✏️ TRY IT: store three temperatures, 38.5, 39.1, and 37.9, in an object
#    called my_temps. Then find their mean().
my_temps <- c(38.5, 39.1, 37.9)
mean(my_temps)   # 38.5


# ── PART 2: Installing and loading packages ────────────────────────────────
# 🎯 Goal: Install the packages for the whole workshop (ONCE per computer),
#    then load the ones we need today (EVERY time you open R).
# ────────────────────────────────────────────────────────────────────────────

# Install (run once; skip this if you did the pre-workshop setup)
install.packages(c("tidyverse", "here", "rio", "janitor", "skimr", "gtsummary", "flextable", "scales"))

# 👀 Follow along: load today's packages, one library() line each
library(tidyverse)
library(here)
library(rio)
library(skimr)
library(janitor)


# ── PART 3: Project folders and here() ─────────────────────────────────────
# 🎯 Goal: Open the workshop folder in Positron and see how here() builds file paths.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: find the top of the project folder
here::here()

# 👀 Follow along: build the path to raw_data/linelist_raw.xlsx
here::here("raw_data", "linelist_raw.xlsx")

# ✏️ TRY IT: build the path to the practice data, raw_data/measles_raw.csv
here::here("raw_data", "measles_raw.csv")


# ── PART 4: Importing data ─────────────────────────────────────────────────
# 🎯 Goal: Read data files into R with import() + here().
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: import raw_data/linelist_raw.xlsx and call it linelist_raw
linelist_raw <- import(here("raw_data", "linelist_raw.xlsx"))

# 👀 Follow along: import raw_data/case_info.csv (case_info) and raw_data/hosp_info.txt (hosp_info)
case_info <- import(here("raw_data", "case_info.csv"))
hosp_info <- import(here("raw_data", "hosp_info.txt"))

# ✏️ TRY IT: import the practice data raw_data/measles_raw.csv and call it measles_raw
measles_raw <- import(here("raw_data", "measles_raw.csv"))


# ── PART 5: Inspecting data ────────────────────────────────────────────────
# 🎯 Goal: Look at the data before changing anything, and find its problems.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: size and structure of linelist_raw: view(), dim(), names(), head(), glimpse()
view(linelist_raw)
dim(linelist_raw)        # 6604 rows, 12 columns
names(linelist_raw)
head(linelist_raw)
glimpse(linelist_raw)

# ✏️ TRY IT: use dim() and glimpse() on measles_raw
dim(measles_raw)         # 1215 rows, 11 columns
glimpse(measles_raw)
# a) `Age (years)` is <chr> (text) but should be a number
# b) Backticks needed: `Patient ID`, `Report Date`, `Age (years)`, `Vaccinated?`,
#    `Temp (C)`, `Days in Hospital`

# 👀 Follow along: skim(linelist_raw). Look at n_missing, and the min and max of wt_kg and ht_cm.
skim(linelist_raw)

# 👀 Follow along: count() the hospital, Gender, OUTCOME, and age_unit columns
count(linelist_raw, hospital)
count(linelist_raw, Gender)
count(linelist_raw, OUTCOME)
count(linelist_raw, age_unit)

# 🔁 Follow along: the base R way. table() OUTCOME, with and without useNA = "ifany"
table(linelist_raw$OUTCOME)                    # no NA shown
table(linelist_raw$OUTCOME, useNA = "ifany")   # 1493 missing outcomes

# ✏️ TRY IT: a) count() District, SEX, and `Vaccinated?` in measles_raw
count(measles_raw, District)        # North: "North", "north", "Nroth", "North District" = 4 ways
count(measles_raw, SEX)             # Male / male / M and Female / female / F
count(measles_raw, `Vaccinated?`)   # No: "No", "no", "N", "NO" = 4 ways

# b) table() drops the 34 cases with no district
table(measles_raw$District)
table(measles_raw$District, useNA = "ifany")

# 👀 Follow along: use filter(), arrange(), and head() to find
#    - how many patients have ages in months, and the first 10 of them
#    - the smallest weights (any impossible values?)
#    - the tallest patients
nrow(filter(linelist_raw, age_unit == "months"))     # 37
head(filter(linelist_raw, age_unit == "months"), 10)

head(arrange(linelist_raw, wt_kg), 10)               # negative and zero weights!

head(arrange(linelist_raw, desc(ht_cm)), 10)         # heights up to 295 cm

# ✏️ TRY IT: sort measles_raw by `Temp (C)`, largest first, and show only
#    the top 10 rows. What is the highest temperature? What happened?
head(arrange(measles_raw, desc(`Temp (C)`)), 10)
# 104.7 is about 40 °C written in Fahrenheit. A few temperatures were entered in °F.

# 👀 Follow along: missing values with colSums(is.na()), rows with no Case ID
#    (nrow(filter(...))),
#    and duplicates with sum(duplicated()) and get_dupes()
colSums(is.na(linelist_raw))
nrow(filter(linelist_raw, is.na(`Case ID`)))   # 130
sum(duplicated(linelist_raw))                  # 25
get_dupes(linelist_raw)

# ✏️ TRY IT: in measles_raw,
#    a) how many rows have a missing `Patient ID`?
#    b) how many rows are exact duplicates?
#    c) which column has the most missing values? Is that a mistake, or does it make sense?
nrow(filter(measles_raw, is.na(`Patient ID`)))   # a) 37
sum(duplicated(measles_raw))                     # b) 15
colSums(is.na(measles_raw))
# c) `Days in Hospital` (766). That makes sense: patients who were never
#    hospitalized have no hospital days. Missing is not always an error!


# ── PART 6: The problem list ───────────────────────────────────────────────
# 🎯 Goal: Write down every problem you found. This list is the to-do list for Session 2.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: the problem list for linelist_raw (fill it in with the class)
# 1. Column names have spaces, dots, and capitals
# 2. Date columns are different types (date.onset is text, the others are date-times)
# 3. age is stored as text, and 37 ages are in months, not years
# 4. Hospital names are misspelled ("Hopital") or inconsistent ("Regional MC"), and many are missing
# 5. Gender and OUTCOME have missing values
# 6. Impossible weights (negative or 0) and heights (4 cm, 295 cm)
# 7. 130 rows have no Case ID
# 8. 25 rows are duplicates

# ✏️ TRY IT: write a problem list for measles_raw
# 1. Column names have spaces, capitals, and symbols (? and parentheses)
# 2. `Age (years)` is stored as text because of "unknown" (27 rows) and "999" (4 rows)
# 3. District is misspelled ("Nroth", "Cetnral"), has lower-case versions, and some say "... District"
# 4. SEX is written 6 different ways (Male / male / M, Female / female / F)
# 5. `Vaccinated?` is written 8 different ways (Yes / yes / Y, No / no / N / NO, Unknown)
# 6. Outcome has inconsistent capitalization and 68 missing values
# 7. Some temperatures were entered in Fahrenheit (values around 97–105)
# 8. 37 rows have no Patient ID
# 9. 15 rows are duplicates
# 10. `Report Date`, District, and SEX have some missing values
