# Session 5: Summarizing Your Data
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
#                    solutions/Session 5 - Summarizing_SOLUTIONS.R
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────
# 🎯 Goal: Load packages and import the finished data from Session 3.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio, janitor, gtsummary; import
#    processed_data/combined_linelist.rds (trust = TRUE) as linelist


# ✏️ TRY IT (1 min): import processed_data/measles_final.rds (trust = TRUE) as measles
#    ✅ Check: in the Variables pane, measles shows 1163 rows and 14 columns



# ── PART 2: Putting categories in order (factors) ──────────────────────────
# 🎯 Goal: Make categories sort in a sensible order instead of alphabetically.
#
# 📖 What you need to know
#    - R sorts text alphabetically, so "Under 18" comes after "80+".
#    - A factor is a text column with a set order of categories (its "levels").
#    - fct_relevel(COLUMN, "FIRST", "SECOND", ...) sets that order. The order
#      then carries through to every table AND every plot.
#    - Other forcats helpers: fct_infreq() orders by frequency, fct_rev() reverses.
#    - Factor order is not saved in a .csv, and we didn't save it in our .rds,
#      so we set it again after importing.
#
# ⚠️ Common mistakes
#    - Misspelling a level: fct_relevel() warns "Unknown levels" and ignores it.
#
# 🔁 Base R: factor(COLUMN, levels = c("FIRST", "SECOND", ...)) sets the order too,
#    but a misspelled level turns those values into NA, with no warning.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: count(age_cat). What order are the groups in?


# 👀 Follow along: fct_relevel() age_cat: "Under 18", "18-29", "30-39", "40-49", "50-59",
#    "60-69", "70-79", "80+", "Unknown"; then outcome: "Recover", "Death", "Unknown"
#    Pattern: DATA <- DATA %>% mutate(COLUMN = fct_relevel(COLUMN, "LEVEL_1", "LEVEL_2", ...))


# 🔁 Follow along: the base R way. factor() with the right levels, then with "death" misspelled


# ✏️ TRY IT (3 min): in measles, fct_relevel()
#    age_cat:    "Under 5", "5-9", "10-14", "15+", "Unknown"
#    vaccinated: "Yes", "No", "Unknown"
#    outcome:    "Recovered", "Died", "Unknown"
#    ✅ Check: count(age_cat) now starts with "Under 5"



# ── PART 3: Counting and percentages with count() ──────────────────────────
# 🎯 Goal: Count categories, cross-tabulate, and add percentages.
#
# 📖 What you need to know
#    - count(A) counts each value; count(A, B) counts each combination.
#      The counts go in a column called n.
#    - Add a percentage with mutate(): n / sum(n) is the proportion.
#      scales::percent(n / sum(n), accuracy = 0.1) shows it as "23.8%".
#    - group_by(A) makes the NEXT steps happen separately for each group.
#      group_by(age_cat) %>% count(outcome) %>% mutate(...) gives percentages
#      WITHIN each age group (each group adds up to 100%).
#    - End with ungroup() when you're done with the groups.
#
# 🔁 Base R: table(data$A, data$B) makes a cross-table, and
#    prop.table(TABLE, margin = 1) turns it into row proportions.
#    The result is a table, not a data frame (it won't work with mutate() or ggplot).
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: count(outcome), then count(age_cat, outcome)


# 👀 Follow along: count(outcome) with a percent column


# 👀 Follow along: percentages within each age group with group_by()


# 🔁 Follow along: the base R way. table(age_cat, outcome), then prop.table(margin = 1)


# ✏️ TRY IT (5 min):
#    a) what percentage of measles cases were vaccinated?
#    b) within each age group, what percentage were vaccinated?
#       Which age group has the lowest percentage?
#    ✅ Check: a) Yes 23.8%   b) 10-14 is lowest at 21.2%



# ── PART 4: Summary statistics with summarise() ────────────────────────────
# 🎯 Goal: Calculate statistics overall and for each group.
#
# 📖 What you need to know
#    - summarise() collapses many rows into ONE row of results. Name each result
#      on the left of =:
#        summarise(n = n(), mean_age = mean(age_years, na.rm = TRUE))
#    - With group_by() first, you get one row PER GROUP.
#    - Useful functions: mean(), median(), sd(), min(), max(), IQR(), n(),
#      n_distinct(), sum(is.na(x))
#    - na.rm = TRUE skips missing values. Without it, one NA makes the result NA.
#    - across() does the same calculation on many columns:
#        summarise(across(c(age_years, bmi), ~ mean(.x, na.rm = TRUE)))
#    - Remember from Session 4: mean(fever == "yes") is the PROPORTION with fever.
#
# 🔁 Base R, for ONE statistic by group:
#      tapply(data$X, data$GROUP, median, na.rm = TRUE)       a named vector
#      aggregate(X ~ GROUP, data = data, FUN = median)        a data frame
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: summarise() n, mean, sd, and median of age_years for everyone


# 👀 Follow along: the same by outcome, plus mean_bmi and mean_temp


# 👀 Follow along: across(): the mean of age_years, bmi, and temp by outcome


# 👀 Follow along: percentage with fever, cough, and vomiting by outcome:
#    round(mean(fever == "yes") * 100, 1)


# 🔁 Follow along: the base R way. Median age by outcome with tapply() and aggregate()


# ✏️ TRY IT (4 min): for each measles outcome, calculate the number of cases, mean and
#    median age, mean temperature, and mean days in hospital (careful: NAs!)
#    ✅ Check: Recovered n = 1048, median age 7; Died n = 50, median age 5



# ✏️ TRY IT (4 min): cases per 10,000 people by district
#    = number of cases / population * 10000
#    HINT: count(district, population) gives both columns; then mutate() and arrange(desc())
#    ✅ Check: East has the highest rate (22.8); Central the lowest (5.2)



# ── PART 5: Quick tables with tabyl() ──────────────────────────────────────
# 🎯 Goal: Frequency tables and cross-tabs with totals and percentages in one step.
#
# 📖 What you need to know
#    - tabyl(A) or tabyl(A, B) (janitor) makes the table.
#    - The adorn_ functions add to it, IN THIS ORDER:
#        adorn_totals("row")              totals (needs the raw counts, so first)
#        adorn_percentages("row")         turn counts into proportions ("row" or "col")
#        adorn_pct_formatting(digits = 1) show them as percentages
#        adorn_ns()                       add the counts back in brackets
#        adorn_title("combined")          a title, last
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: tabyl(outcome), then tabyl(age_cat, outcome)


# 👀 Follow along: tabyl(age_cat, outcome) with all five adorn_ functions


# ✏️ TRY IT (4 min): a tabyl() of vaccinated by outcome in measles, with row
#    percentages and counts. What do you notice about deaths?
#    ✅ Check: 6.3% of unvaccinated cases died, vs 0.7% of vaccinated cases



# ── PART 6: Table 1 with gtsummary ─────────────────────────────────────────
# 🎯 Goal: Make a publication-ready table of patient characteristics, and save it to Word.
#
# 📖 What you need to know
#    - tbl_summary(by = GROUP) describes every column you select(): counts and %
#      for categories, median (IQR) for numbers.
#    - gtsummary has its own "Missing" row, so turn "Unknown" back into NA first:
#        mutate(across(where(is.character), ~ na_if(.x, "Unknown")))
#    - Customize: label = list(COLUMN ~ "Nice name"), statistic = all_continuous() ~ "{mean} ({sd})",
#      then add_overall(), bold_labels(), modify_caption("**Title**")
#    - Save to Word: TABLE %>% as_flex_table() %>%
#        flextable::save_as_docx(path = here("outputs", "FILE.docx"))
#
# ⚠️ Common mistakes
#    - After filter(outcome != "Unknown"), the empty level is still there.
#      fct_drop(outcome) removes it, or tbl_summary() shows an empty column.
# ────────────────────────────────────────────────────────────────────────────

# 👀 Follow along: a basic tbl_summary(by = outcome)


# 👀 Follow along: the same with na_if() for "Unknown"


# 👀 Follow along: a customized table1 with labels, mean (SD), add_overall(),
#    bold_labels(), and a caption; then save it to outputs/table1.docx


# ✏️ TRY IT (6 min): a tbl_summary() of measles cases by outcome (leave out "Unknown"
#    outcomes) with sex, age_years, age_cat, vaccinated, rash, temp_c, and
#    days_in_hospital. Give the rows nice labels and add an overall column.
#    ✅ Check: the column headers say Overall N = 1,098, Recovered N = 1,048, Died N = 50



#________________________________________________________________________________
#
#  ⭐ BONUS
#
#  1. Save your measles table to Word as outputs/measles_table1.docx
#  2. Use pivot_wider() (Session 4) to turn count(district, outcome) into a table
#  3. Brought your own data? Make a Table 1 of it.
#________________________________________________________________________________

