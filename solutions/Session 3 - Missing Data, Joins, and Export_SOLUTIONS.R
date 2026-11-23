# Session 3: Missing Data, Joins, and Export
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along 1.1: load tidyverse, here, rio, janitor, skimr
library(tidyverse)
library(here)
library(rio)
library(janitor)
library(skimr)

# 👀 Follow along 1.2: import processed_data/linelist_clean.rds (linelist_clean)
#    and raw_data/case_info.csv (case_info)
linelist_clean <- import(here("processed_data", "linelist_clean.rds"), trust = TRUE)
case_info      <- import(here("raw_data", "case_info.csv"))

# ✏️ TRY IT 1: import processed_data/measles_clean.rds and raw_data/district_info.csv
measles_clean <- import(here("processed_data", "measles_clean.rds"), trust = TRUE)
district_info <- import(here("raw_data", "district_info.csv"))

nrow(measles_clean)   # 1215
district_info         # 5 rows


# ── PART 2: Where is data missing? ─────────────────────────────────────────

# 👀 Follow along 2.1: colSums(is.na(linelist_clean))
colSums(is.na(linelist_clean))

# 📖 Reading only (on the website): linelist_clean %>% drop_na(bmi) would keep
#    5222 of 6604 rows, so we would lose 1382 patients.

# ✏️ TRY IT 2: count the missing values in each column of measles_clean
colSums(is.na(measles_clean))
# days_in_hospital (766): patients who were never hospitalized have no hospital days.


# ── PART 3: Rows with no ID ────────────────────────────────────────────────

# 👀 Follow along 3.1: create missing_ids
missing_ids <- linelist_clean %>%
  filter(is.na(case_id)) %>%
  mutate(case_id = paste0("ID", row_number()))

nrow(missing_ids)   # 130: ID1 to ID130

# ✏️ TRY IT 3 (homework if behind): create measles_missing_ids with new IDs NEW-1, NEW-2, ...
measles_missing_ids <- measles_clean %>%
  filter(is.na(patient_id)) %>%
  mutate(patient_id = paste0("NEW-", row_number()))

nrow(measles_missing_ids)   # 37


# ── PART 4: Duplicates ─────────────────────────────────────────────────────

# 👀 Follow along 4.1: create linelist_final: filter(!is.na(case_id)), then distinct()
linelist_final <- linelist_clean %>%
  filter(!is.na(case_id)) %>%
  distinct()

nrow(linelist_clean)   # 6604
nrow(linelist_final)   # 6451

# 👀 Follow along 4.2: do any IDs still appear more than once? get_dupes(case_id)
linelist_final %>% get_dupes(case_id)   # yes: same ID, different details

# ✏️ TRY IT 4: create measles_final: keep rows with a patient_id, then distinct()
measles_final <- measles_clean %>%
  filter(!is.na(patient_id))

nrow(measles_final)   # 1178

measles_final <- measles_final %>%
  distinct()

nrow(measles_final)   # 1163: distinct() removed 15 duplicate rows


# ── PART 5: Replacing missing values and choosing columns ──────────────────

# 👀 Follow along 5.1: replace NA with "Unknown" in every text column of linelist_final
linelist_final <- linelist_final %>%
  mutate(across(where(is.character), ~ replace_na(.x, "Unknown")))

linelist_final %>% count(gender)
linelist_final %>% count(outcome)

# 👀 Follow along 5.2: select() and order the columns
linelist_final <- linelist_final %>%
  select(case_id, gender, age_years, age_cat, wt_kg, ht_cm, bmi,
         date_of_infection, onset_date, hospital_date, date_of_outcome,
         hospital_name, outcome)

# ✏️ TRY IT 5: replace NA with "Unknown" in every text column of measles_final
measles_final <- measles_final %>%
  mutate(across(where(is.character), ~ replace_na(.x, "Unknown")))

measles_final %>% count(district)   # Unknown (34)
# days_in_hospital is a number, not text, so where(is.character) skips it.
# That's what we want: "Unknown" is not a number of days.


# ── PART 6: Joining datasets ───────────────────────────────────────────────

# 👀 Follow along 6.1: a tiny example
patients <- tibble(id = c(1, 2, 3), name = c("Ana", "Ben", "Cho"))
labs     <- tibble(id = c(2, 3, 4), result = c("positive", "negative", "positive"))

left_join(patients, labs, by = "id")    # all 3 patients; Ana has NA result
inner_join(patients, labs, by = "id")   # only ids 2 and 3
full_join(patients, labs, by = "id")    # ids 1–4
anti_join(patients, labs, by = "id")    # Ana: no lab result

# 🔁 Reading only (on the website): base R merge() is an INNER join unless you
#    add all.x = TRUE. patients[!patients$id %in% labs$id, ] is like anti_join().

# 👀 Follow along 6.2: create combined_linelist
combined_linelist <- linelist_final %>%
  left_join(case_info, by = "case_id")

# 👀 Follow along 6.3: check the join
nrow(linelist_final)      # 6451
nrow(combined_linelist)   # 6451: no rows added
anti_join(linelist_final, case_info, by = "case_id") %>% nrow()   # 0: every case matched
sum(!linelist_final$case_id %in% case_info$case_id)               # 0: the same check in base R
case_info %>% get_dupes(case_id) %>% nrow()                       # 0: case_id is unique in case_info

# ✏️ TRY IT 6: add district_info to measles_final
measles_final <- measles_final %>%
  left_join(district_info, by = "district")

nrow(measles_final)   # still 1163
anti_join(measles_final, district_info, by = "district") %>% count(district)
# The 34 "Unknown" district rows have no match, so their population is NA.


# ── PART 7: Exporting ──────────────────────────────────────────────────────

# 👀 Follow along 7.1: .rds for R, .csv for sharing
export(combined_linelist, here("processed_data", "combined_linelist.rds"))
export(combined_linelist, here("processed_data", "combined_linelist.csv"))

# ✏️ TRY IT 7: save the measles files
export(measles_final, here("processed_data", "measles_final.rds"))
export(measles_final, here("processed_data", "measles_final.csv"))


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. inner_join() drops rows with no match (the "Unknown" district): 1129 rows
measles_clean %>%
  filter(!is.na(patient_id)) %>%
  distinct() %>%
  mutate(across(where(is.character), ~ replace_na(.x, "Unknown"))) %>%
  inner_join(district_info, by = "district") %>%
  nrow()

# 2. All of the measles steps as one pipeline (gives the same measles_final)
measles_final <- measles_clean %>%
  filter(!is.na(patient_id)) %>%
  distinct() %>%
  mutate(across(where(is.character), ~ replace_na(.x, "Unknown"))) %>%
  left_join(district_info, by = "district")
