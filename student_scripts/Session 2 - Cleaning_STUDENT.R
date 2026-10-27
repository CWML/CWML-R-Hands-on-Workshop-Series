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
#    - NEVER edit the raw data file. Every fix happens in code, so anyone can
#      re-run your script and get the same result.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio, janitor, skimr


# 👀 Follow along: import raw_data/linelist_raw.xlsx (linelist_raw) and raw_data/hosp_info.txt (hosp_info)


# ✏️ TRY IT (1 min): import raw_data/measles_raw.csv and call it measles_raw
#    ✅ Check: in the Variables pane, measles_raw shows 1215 rows and 11 columns



# ── PART 2: The pipe %>% ───────────────────────────────────────────────────
# 🎯 Goal: Write several steps in order, from left to right, with the pipe.
#
# 📖 What you need to know
#    - In Session 1, two steps meant one function inside another:
#        head(arrange(linelist_raw, wt_kg), 3)      read from the INSIDE OUT
#    - The pipe %>% writes the same steps IN ORDER:
#        linelist_raw %>% arrange(wt_kg) %>% head(3)
#      Read it as "and then": take linelist_raw, AND THEN arrange it, AND THEN show 3 rows.
#    - The pipe passes the result on its left in as the FIRST argument of the next
#      function, so you don't write the data inside each function:
#        count(linelist_raw, hospital)   is the same as   linelist_raw %>% count(hospital)
#    - select(COLUMN_1, COLUMN_2) keeps only the columns you name.
#    - Shortcut: Shift + Cmd + M (Mac) or Shift + Ctrl + M (Windows)
#    - You may also see |>, the pipe built into R. It does the same thing.
#
# ⚠️ Common mistakes
#    - Put %>% at the END of a line, not at the start of the next one.
#    - Don't end the LAST line with %>%: R waits for more (a + appears in the
#      Console). Press Esc and remove it.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: head(arrange(linelist_raw, wt_kg), 3), then the same with the pipe
#    Pattern: DATA %>% STEP_1() %>% STEP_2()


# 👀 Follow along: a longer pipeline, one step per line:
#    select() Case ID, wt_kg, and ht_cm, then arrange() by wt_kg, then head(5)


# ✏️ TRY IT (3 min): in Session 1 you wrote
#      head(arrange(measles_raw, desc(`Temp (C)`)), 10)
#    Rewrite it with the pipe, and add a select() step so you only see
#    `Patient ID` and `Temp (C)`.
#    ✅ Check: two columns, and the top temperature is 104.7



# ── PART 3: Column names ───────────────────────────────────────────────────
# 🎯 Goal: Make every column name lower case with underscores, then rename columns.
#
# 📖 What you need to know
#    - clean_names() (janitor) fixes every column name at once: lower case,
#      spaces and dots become _, symbols are removed. "Case ID" becomes case_id.
#      After this, you never need backticks again!
#    - rename(new_name = old_name) changes one name. The NEW name goes on the LEFT.
#    - We build the clean data one step at a time. Each step takes the current
#      version, changes one thing, and saves it back with <- :
#        linelist_clean <- linelist_clean %>% STEP()
#      If something goes wrong, re-run from the first step.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: create linelist_clean from linelist_raw with clean_names().
#    Compare names() before and after.
#    Pattern: NEW_DATA <- OLD_DATA %>% clean_names()


# 👀 Follow along: rename infection_date -> date_of_infection, date_onset -> onset_date,
#    hosp_date -> hospital_date
#    Pattern: DATA <- DATA %>% rename(NEW_NAME = OLD_NAME, NEW_NAME = OLD_NAME)


# ✏️ TRY IT (2 min): create measles_clean from measles_raw with clean_names()
#    What did `Temp (C)` and `Vaccinated?` turn into?
#    ✅ Check: names(measles_clean) includes "temp_c" and "vaccinated"



# ── PART 4: Fixing types with mutate() ─────────────────────────────────────
# 🎯 Goal: Turn date columns into dates and text columns into numbers.
#
# 📖 What you need to know
#    - mutate() creates a new column or overwrites an existing one:
#        mutate(COLUMN = WHAT_TO_PUT_IN_IT)
#      Use the same name to overwrite; a new name to add a column.
#    - as.Date() turns text like "2014-05-13" into a date.
#      Other date formats: dmy("13/05/2014"), mdy("05/13/2014") from lubridate.
#    - as.numeric() turns text like "56" into the number 56. Text that isn't a
#      number (like "unknown") becomes NA, and R warns you:
#      "NAs introduced by coercion". That warning is fine IF you expected it.
#
# ⚠️ Common mistakes
#    - Forgetting the <- at the start: the result prints but isn't saved.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: as.Date() on date_of_infection, onset_date, hospital_date, date_of_outcome
#    Pattern: DATA <- DATA %>% mutate(COLUMN = as.Date(COLUMN), COLUMN = as.Date(COLUMN))


# 👀 Follow along: as.numeric() on age


# ✏️ TRY IT (3 min): in measles_clean,
#    a) make report_date a date with as.Date()
#    b) make age_years a number with as.numeric(). Which values became NA?
#    ✅ Check: summary(measles_clean$age_years) shows a maximum of 999. Suspicious!



# ── PART 5: Fixing values with if_else() ───────────────────────────────────
# 🎯 Goal: Fix values that meet ONE condition.
#
# 📖 What you need to know
#    - if_else(CONDITION, VALUE_IF_TRUE, VALUE_IF_FALSE) checks every row:
#        if_else(age_unit == "months", age / 12, age)
#      "If the unit is months, divide by 12; otherwise keep the age."
#    - Use NA as the value to blank out impossible values:
#        if_else(wt_kg <= 0, NA, wt_kg)
#      Better than deleting the row: we keep the patient's other information.
#    - 🔁 Base R: ifelse() does the same, but silently turns DATES into numbers.
#      if_else() keeps the type (and gives an error instead of a surprise).
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: create age_years (ages in months divided by 12)


# 👀 Follow along: replace weights of 0 or below with NA


# 🔁 Follow along: estimated onset (hospital_date when onset_date is missing)
#    with if_else() and with ifelse(). Compare the first 3 values.


# ✏️ TRY IT (4 min): in measles_clean,
#    a) 999 is a code for "unknown": replace age_years of 999 with NA
#    b) temperatures above 45 were entered in Fahrenheit. Convert them:
#       Celsius = (Fahrenheit - 32) * 5 / 9, rounded to 1 decimal with round(..., 1)
#    ✅ Check: summary() shows age_years max 52 and temp_c max 40.9



# ── PART 6: Recoding categories with case_when() ───────────────────────────
# 🎯 Goal: Fix categories with MANY conditions: gender and hospital names.
#
# 📖 What you need to know
#    - case_when() is if_else() for many conditions. R checks each line in
#      order and uses the FIRST one that is TRUE:
#        case_when(
#          CONDITION_1 ~ NEW_VALUE_1,
#          CONDITION_2 ~ NEW_VALUE_2,
#          .default = VALUE_FOR_EVERYTHING_ELSE
#        )
#      Without .default, rows that match nothing become NA.
#    - %in% checks for any of several values:   sex %in% c("Male", "male", "M")
#    - str_detect(COLUMN, "pattern") is TRUE if the text CONTAINS the pattern,
#      so one line catches several spellings. | means "or": "Military|Mitylira"
#    - Text helpers (stringr): str_to_title("north") gives "North",
#      str_to_lower(), str_to_upper(), str_trim() removes extra spaces
#
# ⚠️ Common mistakes
#    - ~ (tilde) not = between the condition and the new value
#    - A comma after each line EXCEPT the last
#    - Check every recode with count(OLD_COLUMN, NEW_COLUMN)
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: recode gender "m" -> "male", "f" -> "female". Check with count().


# 👀 Follow along: look at hosp_info and count(hospital), then create hospital_name
#    with case_when() + str_detect(). Check with count(hospital, hospital_name).


# ✏️ TRY IT (4 min): in measles_clean, recode
#    sex:        Male / male / M -> "male",  Female / female / F -> "female"
#    vaccinated: Yes / yes / Y -> "Yes",  No / no / N / NO -> "No",  Unknown -> "Unknown"
#    Pattern: COLUMN %in% c("VALUE_1", "VALUE_2") ~ "NEW_VALUE"
#    ✅ Check: count(sex) shows female 533, male 642, NA 40
#              count(vaccinated) shows only No, Unknown, Yes



# ✏️ TRY IT (5 min): make district one of North, South, East, West, Central
#    Step 1: district = str_to_title(district)   fixes the capitals
#    Step 2: case_when() + str_detect() fixes the misspellings ("Nroth", "Cetnral")
#            and the "... District" versions. Keep NA as NA (no .default).
#    ✅ Check: count(district) shows exactly 5 districts plus NA (34)



# ── PART 7: Creating new variables ─────────────────────────────────────────
# 🎯 Goal: Calculate BMI and create age groups.
#
# 📖 What you need to know
#    - mutate() can calculate a new column from other columns:
#        mutate(bmi = round(wt_kg / (ht_cm / 100)^2, 1))
#    - Use case_when() with < to make groups. Because case_when() stops at the
#      first TRUE line, age_years < 30 on the second line only catches 18–29.
#    - 🔁 Base R: cut(x, breaks = c(...), labels = c(...), right = FALSE) makes
#      the same groups from break points, and returns them in order (a factor).
#
# ⚠️ Common mistakes
#    - Ending with .default = "80+" (or TRUE ~ "80+") puts every patient with a
#      MISSING age into the oldest group. Write the last condition in full:
#        age_years >= 80 ~ "80+"
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: calculate bmi, then set BMIs below 10 or above 60 to NA


# 👀 Follow along: create age_cat: Under 18, 18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+


# 🔁 Follow along: the same groups with cut(). Check with table(useNA = "ifany").


# ✏️ TRY IT (4 min): in measles_clean,
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
#    - export() is the opposite of import(). Save clean data in processed_data/,
#      never over the raw file.
#    - .rds is R's own file type. It keeps dates as dates and missing values as NA,
#      so it's the best choice for files you will open in R again.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: one pipeline from linelist_raw to linelist_clean
#    Pattern: linelist_clean <- linelist_raw %>% clean_names() %>% rename(...) %>% mutate(...)


# 👀 Follow along: save linelist_clean as processed_data/linelist_clean.rds
#    Pattern: export(DATA, here("FOLDER", "FILE_NAME.rds"))


# ✏️ TRY IT (2 min): save measles_clean as processed_data/measles_clean.rds
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

