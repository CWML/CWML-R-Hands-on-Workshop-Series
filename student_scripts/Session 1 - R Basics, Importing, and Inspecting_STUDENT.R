# Session 1: R Basics, Importing, and Inspecting Data
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
#   Comments:        anything after # is a note to yourself. R ignores it. Add your own!
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 1 - R Basics, Importing, and Inspecting_SOLUTIONS.R
#________________________________________________________________________________


# ── PART 1: R basics ───────────────────────────────────────────────────────
# 🎯 Goal: Create objects and use functions.
#
# 📖 What you need to know
#    - <- stores a value in an object: my_age <- 35. Read it as "my_age gets 35".
#      The object then appears in the Variables pane.
#      Shortcut: Option + - (Mac) or Alt + - (Windows) types <- for you.
#    - c() combines values into a vector: c(2, 18, 56). Text needs quotes: c("Ana", "Ben")
#    - A function does something to its inputs (arguments): round(3.14159, digits = 2)
#      Type ?round in the Console to see the help page for any function.
#    - A data frame is a table: each column is a vector (one type per column),
#      each row is one observation. data$column pulls out one column.
#      (See the diagram on the course website. You'll build data frames in Session 4.)
#
# ⚠️ Common mistakes
#    - R is case-sensitive: Age and age are different objects.
#    - Names can't start with a number or contain spaces: age2 and my_age work,
#      2age and my age don't. Avoid names R already uses, like c, mean, or data.
#    - Text needs quotes ("Central Hospital"); object names don't (my_age).
#    - Every ( needs a ). Positron highlights the matching bracket.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 1.1: store a number in an object called my_age, then add 5 to it
#    Pattern: OBJECT_NAME <- VALUE


# 👀 Follow along 1.2: make a vector of ages with c(), then find the mean()


# 👀 Follow along 1.3: round(3.14159) with and without digits = 2


# ✏️ TRY IT 1 (2 min): store three temperatures, 38.5, 39.1, and 37.9, in an object
#    called my_temps. Then find their mean().
#    ✅ Check: the mean is 38.5



# ── PART 2: Installing and loading packages ────────────────────────────────
# 🎯 Goal: Install the packages for the whole workshop (ONCE per computer),
#    then load the ones we need today (EVERY time you open R).
#
# 📖 What you need to know
#    - A package is a set of extra functions someone else wrote.
#    - install.packages() downloads it to your computer: once, like buying a book.
#    - library() loads it so you can use it: every session, like taking the book off the shelf.
#    - Today's packages:
#        tidyverse  data wrangling and plotting (includes dplyr and ggplot2)
#        here       file paths that work on any computer
#        rio        import() and export() for almost any file type
#        skimr      skim(), a quick summary of a whole dataset
#
# ⚠️ Common mistakes
#    - Package names need quotes in install.packages() but NOT in library().
#    - Red text is not always an error! There are three kinds:
#        Error     starts with "Error". R stopped. Read the message, fix the code, run again.
#        Warning   starts with "Warning". R finished, but check the result
#                  (did values turn into NA, or did rows disappear?).
#        Message   anything else, like the list after library(tidyverse). Just information.
#    - Asking for help? Give: the line you ran, what you expected, and the exact
#      message R gave you (copy it).
# ────────────────────────────────────────────────────────────────────────────

# Install (run once; skip this if you did the pre-workshop setup)
install.packages(c("tidyverse", "here", "rio", "janitor", "skimr", "gtsummary", "flextable", "scales"))

# 👀 Follow along 2.1: load today's packages, one library() line each
#    Pattern: library(PACKAGE_NAME)


# ✅ Check: run all four lines. You should see no line that starts with "Error".



# ── PART 3: Project folders and here() ─────────────────────────────────────
# 🎯 Goal: Open the workshop folder in Positron and see how here() builds file paths.
#
# 📖 What you need to know
#    - Keep everything for a project in ONE folder, and open that folder in Positron:
#      File -> Open Folder... -> cwml-r-workshop -> Open.
#      The workshop files then appear in the Explorer on the left.
#    - here() builds a file path that starts at the project folder, so the same
#      code works on every computer. here("raw_data", "file.csv") means
#      "the file called file.csv, inside the raw_data folder".
#    - here::here() means "the here() function from the here package". The :: lets
#      you use a function before loading its package with library().
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 3.1: find the top of the project folder
#    Pattern: here::here()


# 👀 Follow along 3.2: build the path to raw_data/linelist_raw.xlsx
#    Pattern: here::here("FOLDER", "FILE_NAME.EXTENSION")


# ── PART 4: Importing data ─────────────────────────────────────────────────
# 🎯 Goal: Read data files into R with import() + here().
#
# 📖 What you need to know
#    - import() reads almost any file type (.csv, .xlsx, .txt, SPSS, Stata, SAS, ...).
#      It works out the type from the file extension.
#    - Put here() inside import() to give it the path:
#        linelist_raw <- import(here("raw_data", "linelist_raw.xlsx"))
#    - Save the result with <- or it only prints to the Console and is gone.
#
# ⚠️ Common mistakes
#    - File names must match exactly, including capitals and the extension.
#    - "Error: No such file" means the folder or file name is misspelled, or the
#      workshop folder isn't open (check the Explorer on the left).
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 4.1: import raw_data/linelist_raw.xlsx and call it linelist_raw
#    Pattern: NEW_NAME <- import(here("FOLDER", "FILE_NAME.EXTENSION"))


# ✏️ TRY IT 2 (2 min): import the practice data raw_data/measles_raw.csv and call it measles_raw
#    ✅ Check: in the Variables pane, measles_raw shows 1215 rows and 11 columns



# ── PART 5: Inspecting data ────────────────────────────────────────────────
# 🎯 Goal: Look at the data before changing anything, and find its problems.
#
# 📖 What you need to know
#    Questions to answer for any dataset:
#      1. How many rows and columns? Is that what you expected?        dim()
#      2. Are the column names easy to use? Are the types right?       glimpse()
#      3. Which columns have missing values? Any impossible values?    skim()
#      4. Are the categories spelled consistently?                      count()
#      5. Are there duplicate rows?                                     (Session 3)
#
#    dim()       number of rows and columns
#    glimpse()   every column, its type, and its first few values
#                <chr> = text, <dbl> = number, <date> = date, <dttm> = date-time
#    skim()      a summary of every column: n_missing, and for numbers the
#                smallest (p0) and largest (p100) value
#    count(DATA, COLUMN)   every distinct value in a column and how often it
#                appears. The fastest way to find misspellings!
#    🔁 Base R:  table(data$COLUMN) counts too, but HIDES missing values
#                (add useNA = "ifany"). See the 🔁 box on the course website.
#
# ⚠️ Common mistakes
#    - Column names with spaces or symbols need backticks: `Case ID`, `Temp (C)`.
#      (The backtick key is at the top left of most keyboards.)
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 5.1: the size and structure of linelist_raw with dim() and glimpse()


# 👀 Follow along 5.2: skim(linelist_raw). Look at n_missing, and the min (p0) and
#    max (p100) of wt_kg and ht_cm.


# ✏️ TRY IT 3 (4 min): use dim(), glimpse(), and skim() on measles_raw
#    a) Which column should be a number but is stored as text (<chr>)?
#    b) Which column names will need backticks?
#    c) In skim(), find `Temp (C)`. What is its largest value (p100)? What happened?
#    d) How many rows have no `Patient ID`?
#    ✅ Check: dim() gives 1215 11; the largest Temp (C) shows as 105;
#              Patient ID has 37 missing values



# 👀 Follow along 5.3: count() the hospital, Gender, OUTCOME, and age_unit columns
#    Pattern: count(DATA, COLUMN)


# ✏️ TRY IT 4 (3 min): count() District, SEX, and `Vaccinated?` in measles_raw
#    How many different ways is "North" written? How many ways is "No" written?
#    ⭐ If you have time: try table(measles_raw$District). What is missing
#       compared with count()?
#    ✅ Check: count(District) shows 18 rows (including NA)



# ✏️ TRY IT 5 (5 min): fix the broken code
#    Run these four lines one at a time. For each one: is it an error or a
#    warning? What does the message say? Then fix the code and run it again.
#    ✅ Check: three errors and one warning. Once fixed, the first two lines
#              show the same 18 rows.
Count(measles_raw, District)
count(measles_raw, district)
measles_raw <- import(here("raw_data", "measles_raw.cvs"))
mean(measles_raw$`Age (years)`)



# ── PART 6: The problem list ───────────────────────────────────────────────
# 🎯 Goal: Write down every problem you found. This list is the to-do list for Session 2.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along 6.1: the problem list for linelist_raw (fill it in with the class)
#    Problem                                     Fixed in
# 1.
# 2.
# 3.
# 4.
# 5.
# 6.
# 7.
# 8.


# ✏️ TRY IT 6 (10 min): write a problem list for measles_raw, using everything you
#    found in TRY ITs 3-5 and the questions at the top of Part 5.
#    Start it in class and finish it at home. Save this file: you will fix these
#    problems in Session 2!
#    ✅ Check: you should find at least 8 problems. Compare with the solutions file.
# 1.
# 2.
# 3.
# 4.
# 5.
# 6.
# 7.
# 8.


#________________________________________________________________________________
#
#  ⭐ BONUS: YOUR OWN DATA
#
#  Brought your own dataset? Put the file in the raw_data/ folder, then:
#    1. import() it
#    2. look at it with dim(), glimpse(), and skim()
#    3. count() its text columns to look for misspellings
#    4. write its problem list, using the questions at the top of Part 5
#________________________________________________________________________________
