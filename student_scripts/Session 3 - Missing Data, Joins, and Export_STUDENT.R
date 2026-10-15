# Session 3: Missing Data, Joins, and Export
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
#                    solutions/Session 3 - Missing Data, Joins, and Export_SOLUTIONS.R
#
#   Today we finish both datasets so they are ready for analysis:
#     linelist_clean -> linelist_final -> combined_linelist   (with the instructor)
#     measles_clean  -> measles_final                         (in the TRY ITs)
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Import the data we cleaned in Session 2, and a second dataset to join.
#
# 📖 What you need to know
#    - We saved Session 2's work as .rds files. Import them with trust = TRUE:
#        import(here("processed_data", "linelist_clean.rds"), trust = TRUE)
#      An .rds file can contain R code, so rio asks you to confirm you trust it.
#      We made these files ourselves, so they are safe.
#    - Missed Session 2? Ready-made copies of these files are already in processed_data/.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio, janitor, skimr


# 👀 Follow along: import processed_data/linelist_clean.rds (linelist_clean)
#    and raw_data/case_info.csv (case_info)


# ✏️ TRY IT (2 min): import processed_data/measles_clean.rds (measles_clean)
#    and raw_data/district_info.csv (district_info)
#    ✅ Check: measles_clean has 1215 rows; district_info has 5 rows



# ── PART 2: Where is data missing? ─────────────────────────────────────────
# 🎯 Goal: Count missing values and decide which ones are real problems.
#
# 📖 What you need to know
#    - colSums(is.na(DATA)) counts the NAs in every column.
#    - Not every NA is a mistake! A missing date_of_outcome can just mean the
#      patient hasn't recovered or died yet.
#    - Three ways to handle missing values:
#        1. Leave them as NA. Most functions skip them with na.rm = TRUE:
#           mean(age_years, na.rm = TRUE)
#        2. Remove the rows: drop_na(COLUMN). Careful, you can lose a lot of data.
#        3. Replace them, e.g. with "Unknown" for categories (Part 5).
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: colSums(is.na(linelist_clean))


# 👀 Follow along: how many rows would drop_na(bmi) remove? Compare nrow() before and after.


# ✏️ TRY IT (2 min): count the missing values in each column of measles_clean.
#    Which column has missing values that are NOT a mistake?
#    ✅ Check: days_in_hospital has the most NAs (766)



# ── PART 3: Rows with no ID ────────────────────────────────────────────────
# 🎯 Goal: Set aside rows with no ID, with new IDs, so they aren't lost.
#
# 📖 What you need to know
#    - We join on the ID column in Part 6, so rows without an ID can't be linked.
#      Instead of deleting them, we save them separately with new IDs.
#    - row_number() gives 1, 2, 3, ... for each row.
#    - sprintf("%05d", 1) pads a number with zeros to 5 digits: "00001".
#    - paste0() glues text together: paste0("ID", "00001") gives "ID00001".
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: create missing_ids: filter(is.na(case_id)), then give each row
#    a new case_id with paste0("ID", sprintf("%05d", row_number()))


# ✏️ TRY IT (3 min): create measles_missing_ids: the rows of measles_clean with no
#    patient_id, with new IDs NEW-0001, NEW-0002, ...
#    Pattern: paste0("NEW-", sprintf("%04d", row_number()))
#    ✅ Check: measles_missing_ids has 37 rows



# ── PART 4: Duplicates ─────────────────────────────────────────────────────
# 🎯 Goal: Keep only rows that have an ID, and remove exact duplicate rows.
#
# 📖 What you need to know
#    - ! means "not": filter(!is.na(case_id)) keeps rows that HAVE an ID.
#    - distinct() keeps one copy of each row that appears more than once.
#      It only removes EXACT copies (every column the same).
#    - get_dupes(case_id) shows IDs that appear more than once even if the rest
#      of the row is different. Those need a person to decide which is right.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: create linelist_final: filter(!is.na(case_id)), then distinct().
#    Compare nrow() before and after.


# 👀 Follow along: do any IDs still appear more than once? get_dupes(case_id)


# ✏️ TRY IT (3 min): create measles_final from measles_clean: keep rows that have a
#    patient_id, then remove exact duplicates. How many rows did distinct() remove?
#    ✅ Check: 1178 rows after filter(), 1163 after distinct()



# ── PART 5: Replacing missing values and choosing columns ──────────────────
# 🎯 Goal: Replace missing text with "Unknown", then keep and order the columns we need.
#
# 📖 What you need to know
#    - across() applies the same function to many columns at once:
#        mutate(across(where(is.character), ~ replace_na(.x, "Unknown")))
#      Read it as: "for every text column (.x), replace its NAs with Unknown".
#    - where(is.character) picks only text columns, so number columns keep their NAs.
#    - For one column: mutate(gender = replace_na(gender, "Unknown"))
#    - select() keeps columns in the order you list them.
#      select(-COLUMN) drops a column; select(COLUMN, everything()) moves it first.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: replace NA with "Unknown" in every text column of linelist_final.
#    Check with count(gender) and count(outcome).


# 👀 Follow along: select() case_id, gender, age_years, age_cat, wt_kg, ht_cm, bmi,
#    date_of_infection, onset_date, hospital_date, date_of_outcome, hospital_name, outcome


# ✏️ TRY IT (3 min): replace NA with "Unknown" in every text column of measles_final
#    Why does days_in_hospital still have NAs?
#    ✅ Check: count(district) now shows Unknown (34) instead of NA



# ── PART 6: Joining datasets ───────────────────────────────────────────────
# 🎯 Goal: Combine two datasets by matching rows on a shared ID column (the "key").
#
# 📖 What you need to know
#    left_join(A, B, by = "id")    every row of A, plus matching columns from B
#    inner_join(A, B, by = "id")   only rows whose id is in BOTH
#    full_join(A, B, by = "id")    every row from both
#    anti_join(A, B, by = "id")    rows of A with NO match in B (great for checking!)
#
#    - left_join() is the one you'll use most: start with your main data and
#      add information to it.
#    - Always check the number of rows. A left join should NOT add rows. If it
#      does, the key appears more than once in B.
#    - 🔁 Base R: merge(A, B, by = "id") is an INNER join by default.
#      all.x = TRUE makes it a left join, all = TRUE a full join.
#      A$id %in% B$id gives TRUE/FALSE for each id: is it found in B?
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: a tiny example. Make patients (id 1, 2, 3) and labs (id 2, 3, 4)
#    with tibble(), then try all four joins on them.


# 🔁 Follow along: the base R way. merge() with and without all.x = TRUE,
#    then find patients with no lab result with %in%


# 👀 Follow along: create combined_linelist by left_join()-ing case_info to linelist_final
#    Pattern: NEW_DATA <- MAIN_DATA %>% left_join(OTHER_DATA, by = "KEY_COLUMN")


# 👀 Follow along: check the join with nrow() and anti_join()


# ✏️ TRY IT (4 min): add district_info to measles_final with left_join() (the key is district)
#    Did the number of rows change? Which rows got no population, and why?
#    HINT: anti_join(measles_final, district_info, by = "district") %>% count(district)
#    ✅ Check: still 1163 rows; the 34 "Unknown" district rows have no population



# ── PART 7: Exporting ──────────────────────────────────────────────────────
# 🎯 Goal: Save the finished data: .rds to keep working in R, .csv to share.
#
# 📖 What you need to know
#    .rds   for files YOU will open in R again. Keeps dates, factors, and NAs exactly.
#    .csv   for SHARING: opens in Excel and any software. But column types are lost,
#           and missing text comes back as blank "" instead of NA.
#    .xlsx  for sharing with Excel users. Same caveats as .csv.
#    export() picks the type from the file extension, just like import().
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: save combined_linelist as .rds, and linelist_final, missing_ids,
#    and combined_linelist as .csv, all in processed_data/


# ✏️ TRY IT (2 min): save measles_final as processed_data/measles_final.rds AND
#    measles_final.csv, and measles_missing_ids as processed_data/measles_missing_ids.csv
#    ✅ Check: three new files in processed_data/. You need measles_final.rds in Sessions 4–8!



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. How many rows would inner_join() give instead of left_join() for the
#     measles data? Why?
#  2. Write all of your measles steps (Parts 4–6) as one pipeline.
#  3. Brought your own data? Handle its missing values and duplicates, join a
#     second dataset if you have one, and export the result.
#________________________________________________________________________________

