# Session 2: Cleaning Your Data
# R Hands-on Workshop Series — STUDENT VERSION
#________________________________________________________________________________
#
# HOW TO USE THIS SCRIPT
#
#   📖 What you need to know  short notes on the idea behind each part
#   👀 Follow along           type the code with the instructor in the empty space
#   ✏️ TRY IT                 do the same thing yourself on the practice data
#                             (raw_data/measles_raw.csv, a measles outbreak)
#   ✅ Check                  how to tell your TRY IT worked
#   ⚠️ Common mistakes        what usually goes wrong
#
#   Pattern: lines show the shape of the code. Replace the words in CAPITALS.
#   Numbers:  Follow along 5.3 and TRY IT 5 have the same numbers on the course
#             website, so you can always find your place in either one.
#
#   Run a line:      put your cursor on it, press Cmd + Return (Mac) or Ctrl + Enter (Windows)
#   Insert a pipe:   Shift + Cmd + M (Mac) or Shift + Ctrl + M (Windows)
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 2 - Cleaning_SOLUTIONS.R
#
#   Today we fix the problems on the problem lists you wrote in Session 1.
#   You will build TWO cleaned datasets side by side:
#     linelist_clean  (with the instructor)    measles_clean  (in the TRY ITs)
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Load packages and import the raw data.
#
# 📖 What you need to know
#    - Every session starts the same way: open the project, load packages, import data.
#      You typed these lines yourself in Session 1. From now on they are filled
#      in for you: just run them (Cmd + Return on a Mac, Ctrl + Enter on Windows).
#    - Quick review from Session 1:
#        here("raw_data", "linelist_raw.xlsx")  builds the path to a file, starting at
#                                               the top of the workshop folder (here package)
#        import(PATH)                            reads the file; it works out the file type
#                                               from the ending: .xlsx, .csv, .txt (rio package)
#        here::here()                            shows where the top of the folder is
#    - NEVER edit the raw data file. Every fix happens in code, so anyone can
#      re-run your script and get the same result:
#        1. leave raw_data/linelist_raw.xlsx alone (no fixing by hand in Excel)
#        2. make a NEW object for the clean version (linelist_clean) and put every
#           fix in it; linelist_raw stays as imported
#        3. save the result as a NEW file in processed_data/ (Part 8)
#    - janitor is the one new package today: it gives you clean_names().
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 1.1: load tidyverse, here, rio, janitor, skimr (already filled in: run it)
library(tidyverse)
library(here)
library(rio)
library(janitor)
library(skimr)

# 👀 Follow along 1.2: import raw_data/linelist_raw.xlsx (linelist_raw) and raw_data/hosp_info.txt (hosp_info)
#    (already filled in: run it)
linelist_raw <- import(here("raw_data", "linelist_raw.xlsx"))
hosp_info    <- import(here("raw_data", "hosp_info.txt"))


# ✏️ TRY IT 1 (1 min): import raw_data/measles_raw.csv and call it measles_raw
#    ✅ Check: in the Variables pane, measles_raw shows 1215 rows and 11 columns



# ── PART 2: The pipe %>%, filter(), and arrange() ──────────────────────────
# 🎯 Goal: Write several steps in order, from left to right, with the pipe.
#
# 📖 What you need to know
#    - The pipe %>% passes the data from one step to the next, IN ORDER.
#      Read it as "and then":
#        linelist_raw %>% arrange(wt_kg) %>% head(3)
#        take linelist_raw, AND THEN sort it by weight, AND THEN show 3 rows.
#    - The pipe fills in the data as the FIRST argument of the next function:
#        count(linelist_raw, age_unit)   is the same as   linelist_raw %>% count(age_unit)
#    - filter(), arrange(), select(), and count() come from the dplyr package (tidyverse).
#    - filter(CONDITION) keeps the ROWS that meet a condition
#      Conditions: == equal, != not equal, > < >= <=, %in% c(...), is.na() missing
#      Use == (two equals signs) to compare. One = is for naming arguments.
#    - arrange(COLUMN) sorts rows; arrange(desc(COLUMN)) sorts largest first
#    - select(COLUMN_1, COLUMN_2) keeps only the COLUMNS you name.
#    - Shortcut for %>%: Shift + Cmd + M (Mac) or Shift + Ctrl + M (Windows)
#    - You may also see |>, the pipe built into R. It does the same thing.
#
# ⚠️ Common mistakes
#    - Put %>% at the END of a line, not at the start of the next one.
#    - Don't end the LAST line with %>%: R waits for more (a + appears in the
#      Console). Press Esc and remove it.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 2.1: count(age_unit) with the pipe, then sort by weight and show 3 rows
#    Pattern: DATA %>% STEP_1() %>% STEP_2()


# 👀 Follow along 2.2: how many patients have ages in months? Then the 3 tallest patients
#    Pattern: DATA %>% filter(CONDITION)


# 👀 Follow along 2.3: a longer pipeline, one step per line:
#    select() Case ID, wt_kg, and ht_cm, then arrange() by wt_kg, then head(5)
#    Look at the weights in the output: what do you notice? (Fixed in Part 5.)


# ✏️ TRY IT 2 (4 min): in measles_raw,
#    a) show `Patient ID` and `Temp (C)`, sorted by temperature, largest first,
#       top 10 rows only
#    b) how many rows have a `Temp (C)` above 45?
#    Pattern: DATA %>% select(COLUMN_1, COLUMN_2) %>% arrange(desc(COLUMN)) %>% head(10)
#    ✅ Check: a) two columns, and the top temperature is 104.7   b) 27



# ── PART 3: Column names ───────────────────────────────────────────────────
# 🎯 Goal: Make every column name lower case with underscores, then rename columns.
#
# 📖 What you need to know
#    - clean_names() (janitor package) fixes every column name at once: lower case,
#      spaces and dots become _, symbols are removed. "Case ID" becomes case_id.
#      After this, you never need backticks again!
#    - rename(new_name = old_name) (dplyr package) changes one name. The NEW name goes on the LEFT.
#    - We build the clean data one step at a time. Each step takes the current
#      version, changes one thing, and saves it back with <- :
#        linelist_clean <- linelist_clean %>% STEP()
#      If something goes wrong, re-run from the first step.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 3.1: create linelist_clean from linelist_raw with clean_names().
#    Compare names() before and after.
#    Pattern: NEW_DATA <- OLD_DATA %>% clean_names()


# 👀 Follow along 3.2: rename infection_date -> date_of_infection, date_onset -> onset_date,
#    hosp_date -> hospital_date
#    Pattern: DATA <- DATA %>% rename(NEW_NAME = OLD_NAME, NEW_NAME = OLD_NAME)


# ✏️ TRY IT 3 (2 min): create measles_clean from measles_raw with clean_names()
#    What did `Temp (C)` and `Vaccinated?` turn into?
#    ✅ Check: names(measles_clean) includes "temp_c" and "vaccinated"



# ── PART 4: Fixing types with mutate() ─────────────────────────────────────
# 🎯 Goal: Turn date columns into dates and text columns into numbers.
#
# 📖 What you need to know
#    - mutate() (dplyr package) creates a new column or overwrites an existing one:
#        mutate(COLUMN = WHAT_TO_PUT_IN_IT)
#      Use the same name to overwrite; a new name to add a column.
#    - as.Date() (base R, no package needed) turns text like "2014-05-13" into a date.
#      Other date formats: dmy("13/05/2014"), mdy("05/13/2014") from lubridate.
#    - as.numeric() (base R) turns text like "56" into the number 56. Text that isn't a
#      number (like "unknown") becomes NA, and R warns you:
#      "NAs introduced by coercion". That warning is fine IF you expected it.
#
# ⚠️ Common mistakes
#    - Forgetting the <- at the start: the result prints but isn't saved.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 4.1: as.Date() on date_of_infection, onset_date, hospital_date, date_of_outcome
#    Pattern: DATA <- DATA %>% mutate(COLUMN = as.Date(COLUMN), COLUMN = as.Date(COLUMN))


# 👀 Follow along 4.2: as.numeric() on age


# ✏️ TRY IT 4 (3 min): in measles_clean,
#    a) make report_date a date with as.Date()
#    b) make age_years a number with as.numeric(). Which values became NA?
#    ✅ Check: summary(measles_clean$age_years) shows a maximum of 999. Suspicious!



# ── PART 5: Fixing values with if_else() ───────────────────────────────────
# 🎯 Goal: Fix values that meet ONE condition.
#
# 📖 What you need to know
#    - if_else(CONDITION, VALUE_IF_TRUE, VALUE_IF_FALSE) (dplyr package) checks every row:
#        if_else(age_unit == "months", age / 12, age)
#      "If the unit is months, divide by 12; otherwise keep the age."
#    - Use NA as the value to blank out impossible values:
#        if_else(wt_kg <= 0, NA, wt_kg)
#      Better than deleting the row: we keep the patient's other information.
#    - Shortcuts: na_if(age_years, 999) turns every 999 into NA.
#      coalesce(onset_date, hospital_date) uses the hospital date when the onset
#      date is missing.
#    - 🔁 Base R: ifelse() does the same as if_else(), but silently turns DATES
#      into numbers. Read the 🔁 box in Part 5 on the website (no typing).
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 5.1: create age_years (ages in months divided by 12)


# 👀 Follow along 5.2: replace weights of 0 or below with NA


# ✏️ TRY IT 5 (4 min): in measles_clean,
#    a) 999 is a code for "unknown": replace age_years of 999 with NA
#    b) temperatures above 45 were entered in Fahrenheit. Convert them:
#       Celsius = (Fahrenheit - 32) * 5 / 9, rounded to 1 decimal with round(..., 1)
#    ✅ Check: summary() shows age_years max 52 and temp_c max 40.9



# ── PART 6: Recoding categories with case_when() ───────────────────────────
# 🎯 Goal: Fix categories with MANY conditions: gender and hospital names.
#
# 📖 What you need to know
#    - case_when() (dplyr package) is if_else() for many conditions. R checks each line in
#      order and uses the FIRST one that is TRUE:
#        case_when(
#          CONDITION_1 ~ NEW_VALUE_1,
#          CONDITION_2 ~ NEW_VALUE_2,
#          .default = VALUE_FOR_EVERYTHING_ELSE
#        )
#      Without .default, rows that match nothing become NA.
#    - %in% checks for any of several values:   sex %in% c("Male", "male", "M")
#    - str_detect(COLUMN, "pattern") (stringr package) is TRUE if the text CONTAINS the pattern,
#      so one line catches several spellings. | means "or": "Military|Mitylira"
#    - Text helpers (stringr package): str_to_title("north") gives "North",
#      str_to_lower(), str_to_upper(), str_trim() removes extra spaces
#
# ⚠️ Common mistakes
#    - ~ (tilde) not = between the condition and the new value
#    - A comma after each line EXCEPT the last
#    - Check every recode with count(OLD_COLUMN, NEW_COLUMN)
#    - Code that runs isn't always right: leave "M" out of the male line and
#      86 patients quietly become NA. No error, no warning. Always check.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 6.1: recode gender "m" -> "male", "f" -> "female". Check with count().


# 👀 Follow along 6.2: look at hosp_info and count(hospital), then create hospital_name
#    with case_when() + str_detect(). Check with count(hospital, hospital_name).


# ✏️ TRY IT 6 (4 min): in measles_clean, recode
#    sex:        Male / male / M -> "male",  Female / female / F -> "female"
#    vaccinated: Yes / yes / Y -> "Yes",  No / no / N / NO -> "No",  Unknown -> "Unknown"
#    Pattern: COLUMN %in% c("VALUE_1", "VALUE_2") ~ "NEW_VALUE"
#    ✅ Check: count(sex) shows female 533, male 642, NA 40
#              count(vaccinated) shows only No, Unknown, Yes



# ✏️ TRY IT 7 (5 min, homework if behind): make district one of North, South, East, West, Central
#    No time in class? Finish it at home, then run your TRY IT 9 line again.
#    Step 1: district = str_to_title(district)   fixes the capitals
#    Step 2: case_when() + str_detect() fixes the misspellings ("Nroth", "Cetnral")
#            and the "... District" versions. Keep NA as NA (no .default).
#    ✅ Check: count(district) shows exactly 5 districts plus NA (34)



# ── PART 7: Creating new variables ─────────────────────────────────────────
# 🎯 Goal: Create age groups.
#
# 📖 What you need to know
#    - mutate() can calculate a new column from other columns. Read the BMI
#      example on the website (no typing; BMI is included in Part 8):
#        mutate(bmi = round(wt_kg / (ht_cm / 100)^2, 1))
#    - Use case_when() with < to make groups. Because case_when() stops at the
#      first TRUE line, age_years < 30 on the second line only catches 18–29.
#    - 🔁 Base R: cut(x, breaks = c(...), labels = c(...), right = FALSE) makes
#      the same groups from break points, and returns them in order (a factor).
#      Read the 🔁 box in Part 7 on the website (no typing).
#
# ⚠️ Common mistakes
#    - Ending with .default = "80+" (or TRUE ~ "80+") puts every patient with a
#      MISSING age into the oldest group. Write the last condition in full:
#        age_years >= 80 ~ "80+"
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 7.1: create age_cat: Under 18, 18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+


# ✏️ TRY IT 8 (4 min): in measles_clean,
#    a) make outcome consistent with str_to_title()
#    b) create age_cat: "Under 5", "5-9", "10-14", "15+"
#    ✅ Check: count(outcome) shows only Died, Recovered, NA
#              count(age_cat) shows 4 groups plus NA (31)



# ── PART 8: One pipeline and saving your work ──────────────────────────────
# 🎯 Goal: Combine every step into ONE pipeline, then save the result.
#
# 📖 What you need to know
#    - In a real project, keep ONE block of code that goes from raw data to
#      clean data. Anyone can re-run it and get exactly the same result.
#    - Inside one mutate(), later lines can use columns made on earlier lines.
#    - export() (rio package) is the opposite of import(). Save clean data in processed_data/,
#      never over the raw file.
#    - .rds is R's own file type. It keeps dates as dates and missing values as NA,
#      so it's the best choice for files you will open in R again.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 8.1: one pipeline from linelist_raw to linelist_clean
#    Already filled in: read it with the instructor, then run it.
#    Pattern: linelist_clean <- linelist_raw %>% clean_names() %>% rename(...) %>% mutate(...)
linelist_clean <- linelist_raw %>%
  clean_names() %>%
  rename(
    date_of_infection = infection_date,
    onset_date        = date_onset,
    hospital_date     = hosp_date
  ) %>%
  mutate(
    # Types
    date_of_infection = as.Date(date_of_infection),
    onset_date        = as.Date(onset_date),
    hospital_date     = as.Date(hospital_date),
    date_of_outcome   = as.Date(date_of_outcome),
    age               = as.numeric(age),
    # Values
    age_years = if_else(age_unit == "months", age / 12, age),
    wt_kg     = if_else(wt_kg <= 0, NA, wt_kg),
    gender    = case_when(
      gender == "m" ~ "male",
      gender == "f" ~ "female"
    ),
    hospital_name = case_when(
      str_detect(hospital, "Central")           ~ "Central Hospital",
      str_detect(hospital, "Military|Mitylira") ~ "Military Hospital",
      str_detect(hospital, "Regional")          ~ "Regional Medical center",
      str_detect(hospital, "Mark")              ~ "St. Mark's Maternity Hospital",
      str_detect(hospital, "HMO")               ~ "HMO Health",
      str_detect(hospital, "Premier")           ~ "Premiere Health",
      .default = "Other/Unknown"
    ),
    # New variables
    bmi = round(wt_kg / (ht_cm / 100)^2, 1),
    bmi = if_else(bmi < 10 | bmi > 60, NA, bmi),
    age_cat = case_when(
      age_years < 18  ~ "Under 18",
      age_years < 30  ~ "18-29",
      age_years < 40  ~ "30-39",
      age_years < 50  ~ "40-49",
      age_years < 60  ~ "50-59",
      age_years < 70  ~ "60-69",
      age_years < 80  ~ "70-79",
      age_years >= 80 ~ "80+"
    )
  )

glimpse(linelist_clean)


# 👀 Follow along 8.2: save linelist_clean as processed_data/linelist_clean.rds
#    Pattern: export(DATA, here("FOLDER", "FILE_NAME.rds"))


# ✏️ TRY IT 9 (2 min): save measles_clean as processed_data/measles_clean.rds
#    ✅ Check: the file appears in the processed_data folder (Explorer, on the left)
#    You need this file in Session 3!



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. Rewrite all of your measles TRY ITs as ONE pipeline that starts from
#     measles_raw (like Part 8).
#  2. Brought your own data? Fix the problems on your own problem list, then
#     export() the clean version to processed_data/.
#________________________________________________________________________________

