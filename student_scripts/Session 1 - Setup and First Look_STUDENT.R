# Session 1: Setup and a First Look at Your Data
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
#   Comments:        anything after # is a note to yourself. R ignores it. Add your own!
#   Stuck?           put up your red sticky note, or check
#                    solutions/Session 1 - Setup and First Look_SOLUTIONS.R
#________________________________________________________________________________


# ── PART 1: R basics ───────────────────────────────────────────────────────
# 🎯 Goal: Create objects, use functions, and build a small data frame.
#
# 📖 What you need to know
#    - <- stores a value in an object: my_age <- 35. Read it as "my_age gets 35".
#      The object then appears in the Variables pane.
#    - c() combines values into a vector: c(2, 18, 56)
#    - A function does something to its inputs (arguments): round(3.14159, digits = 2)
#      Type ?round in the Console to see the help page for any function.
#    - A data frame is a table: each column is a vector, each row is one
#      observation. data$column pulls out one column. data.frame() makes one.
#      (A tibble is the tidyverse version of a data frame: you'll meet them later.)
#
# ⚠️ Common mistakes
#    - R is case-sensitive: Age and age are different objects.
#    - Text needs quotes ("Central Hospital"); object names don't (my_age).
#    - Every ( needs a ). Positron highlights the matching bracket.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: store a number in an object called my_age, then add 5 to it
#    Pattern: OBJECT_NAME <- VALUE


# 👀 Follow along: make a vector of ages with c(), then find the mean()


# 👀 Follow along: make a vector of hospital names and find its length()


# 👀 Follow along: round(3.14159) with and without digits = 2


# 👀 Follow along: make a small data frame with data.frame(), then pull out one column with $
#    Pattern: data.frame(COLUMN_1 = c(...), COLUMN_2 = c(...))


# ✏️ TRY IT (2 min): store three temperatures, 38.5, 39.1, and 37.9, in an object
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
#        janitor    tools for cleaning, like get_dupes() today
#
# ⚠️ Common mistakes
#    - Package names need quotes in install.packages() but NOT in library().
#    - Red text is not always an error! Messages like "Attaching core tidyverse
#      packages" are normal. An error starts with "Error".
# ────────────────────────────────────────────────────────────────────────────

# Install (run once; skip this if you did the pre-workshop setup)
install.packages(c("tidyverse", "here", "rio", "janitor", "skimr", "gtsummary", "flextable", "scales"))

# 👀 Follow along: load today's packages, one library() line each
#    Pattern: library(PACKAGE_NAME)


# ✅ Check: run all five lines. You should see no line that starts with "Error".



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

# 👀 Follow along: find the top of the project folder
#    Pattern: here::here()


# 👀 Follow along: build the path to raw_data/linelist_raw.xlsx
#    Pattern: here::here("FOLDER", "FILE_NAME.EXTENSION")


# ✏️ TRY IT (1 min): build the path to the practice data, raw_data/measles_raw.csv
#    ✅ Check: the output ends in .../raw_data/measles_raw.csv



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

# 👀 Follow along: import raw_data/linelist_raw.xlsx and call it linelist_raw
#    Pattern: NEW_NAME <- import(here("FOLDER", "FILE_NAME.EXTENSION"))


# 👀 Follow along: import raw_data/case_info.csv (case_info) and raw_data/hosp_info.txt (hosp_info)


# ✏️ TRY IT (2 min): import the practice data raw_data/measles_raw.csv and call it measles_raw
#    ✅ Check: in the Variables pane, measles_raw shows 1215 rows and 11 columns



# ── PART 5: Inspecting data ────────────────────────────────────────────────
# 🎯 Goal: Look at the data before changing anything, and find its problems.
#
# 📖 What you need to know
#    Size and structure
#      view()      opens the Data Explorer, a spreadsheet view of the data
#      dim()       number of rows and columns
#      names()     the column names
#      head()      the first 6 rows
#      glimpse()   every column, its type, and its first few values
#                  <chr> = text, <dbl> = number, <date> = date, <dttm> = date-time
#      skim()      a summary of every column, including missing values, min, and max
#
#    Categories
#      count(DATA, COLUMN)   every distinct value in a column and how often it
#                  appears. The fastest way to find misspellings!
#      🔁 Base R:  table(data$COLUMN) counts too, but HIDES missing values.
#                  Add useNA = "ifany" to see them.
#
#    Zooming in (the data always goes first)
#      filter(DATA, CONDITION)   keeps ROWS that meet a condition
#      arrange(DATA, COLUMN)     sorts rows; desc(COLUMN) sorts largest first
#      head(DATA, 10)            shows only the first 10 rows
#      Conditions: == equal, != not equal, > < >= <=, is.na() missing
#
#    Two steps at once: put one function INSIDE another. R runs the inside first.
#      head(arrange(linelist_raw, wt_kg), 10)   sort by weight, THEN show 10 rows
#      linelist_raw is a regular data frame, so R prints ALL 6,604 rows unless you
#      wrap the code in head(..., 10). (In Session 2 the pipe %>% makes this easier.)
#
#    Missing values and duplicates
#      colSums(is.na(data))    the number of missing values in each column
#      sum(duplicated(data))   the number of rows that are exact copies of another row
#      get_dupes(data)         shows the duplicated rows
#
# ⚠️ Common mistakes
#    - Column names with spaces or symbols need backticks: `Case ID`, `Temp (C)`.
#      (The backtick key is at the top left of most keyboards.)
#    - Use == (two equals signs) to compare. One = is for naming arguments.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: size and structure of linelist_raw: view(), dim(), names(), head(), glimpse()


# ✏️ TRY IT (3 min): use dim() and glimpse() on measles_raw
#    a) Which column should be a number but is stored as text (<chr>)?
#    b) Which column names will need backticks?
#    ✅ Check: dim() gives 1215 11



# 👀 Follow along: skim(linelist_raw). Look at n_missing, and the min and max of wt_kg and ht_cm.


# 👀 Follow along: count() the hospital, Gender, OUTCOME, and age_unit columns
#    Pattern: count(DATA, COLUMN)


# 🔁 Follow along: the base R way. table() OUTCOME, with and without useNA = "ifany"


# ✏️ TRY IT (3 min):
#    a) count() District, SEX, and `Vaccinated?` in measles_raw
#       How many different ways is "North" written? How many ways is "No" written?
#    b) Now try table(measles_raw$District). What is missing compared with count()?
#    ✅ Check: a) count(District) shows 18 rows (including NA)
#              b) table() has no NA: 34 cases disappear



# 👀 Follow along: use filter(), arrange(), and head() to find
#    - how many patients have ages in months, and the first 10 of them
#    - the smallest weights (any impossible values?)
#    - the tallest patients
#    Pattern: nrow(filter(DATA, CONDITION))
#    Pattern: head(arrange(DATA, COLUMN), 10)


# ✏️ TRY IT (3 min): sort measles_raw by `Temp (C)`, largest first, and show only
#    the top 10 rows. What is the highest temperature? What happened?
#    Pattern: head(arrange(DATA, desc(COLUMN)), 10)
#    ✅ Check: the top value is 104.7



# 👀 Follow along: missing values with colSums(is.na()), rows with no Case ID
#    (nrow(filter(...))),
#    and duplicates with sum(duplicated()) and get_dupes()


# ✏️ TRY IT (3 min): in measles_raw,
#    a) how many rows have a missing `Patient ID`?
#    b) how many rows are exact duplicates?
#    c) which column has the most missing values? Is that a mistake, or does it make sense?
#    ✅ Check: a) 37   b) 15



# ── PART 6: The problem list ───────────────────────────────────────────────
# 🎯 Goal: Write down every problem you found. This list is the to-do list for Session 2.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: the problem list for linelist_raw (fill it in with the class)
# 1.
# 2.
# 3.
# 4.
# 5.
# 6.
# 7.
# 8.


# ✏️ TRY IT (10 min): write a problem list for measles_raw, using everything you
#    found in the TRY ITs above. Save this file: you will fix these problems in Session 2!
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
#    4. check for missing values and duplicates
#    5. write its problem list
#________________________________________________________________________________

