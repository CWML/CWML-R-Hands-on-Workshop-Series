# Session 2: Cleaning Your Data
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio, janitor, skimr
library(tidyverse)
library(here)
library(rio)
library(janitor)
library(skimr)

# 👀 Follow along: import raw_data/linelist_raw.xlsx (linelist_raw) and raw_data/hosp_info.txt (hosp_info)
linelist_raw <- import(here("raw_data", "linelist_raw.xlsx"))
hosp_info    <- import(here("raw_data", "hosp_info.txt"))

# ✏️ TRY IT: import raw_data/measles_raw.csv and call it measles_raw
measles_raw <- import(here("raw_data", "measles_raw.csv"))


# ── PART 2: Column names ───────────────────────────────────────────────────

# 👀 Follow along: create linelist_clean from linelist_raw with clean_names().
#    Compare names() before and after.
names(linelist_raw)

linelist_clean <- linelist_raw %>%
  clean_names()

names(linelist_clean)

# 👀 Follow along: rename infection_date -> date_of_infection, date_onset -> onset_date,
#    hosp_date -> hospital_date
linelist_clean <- linelist_clean %>%
  rename(
    date_of_infection = infection_date,
    onset_date        = date_onset,
    hospital_date     = hosp_date
  )

# ✏️ TRY IT: create measles_clean from measles_raw with clean_names()
measles_clean <- measles_raw %>%
  clean_names()

names(measles_clean)   # `Temp (C)` -> temp_c and `Vaccinated?` -> vaccinated


# ── PART 3: Fixing types with mutate() ─────────────────────────────────────

# 👀 Follow along: as.Date() on date_of_infection, onset_date, hospital_date, date_of_outcome
linelist_clean <- linelist_clean %>%
  mutate(
    date_of_infection = as.Date(date_of_infection),
    onset_date        = as.Date(onset_date),
    hospital_date     = as.Date(hospital_date),
    date_of_outcome   = as.Date(date_of_outcome)
  )

# 👀 Follow along: as.numeric() on age
linelist_clean <- linelist_clean %>%
  mutate(age = as.numeric(age))

# ✏️ TRY IT: in measles_clean, a) report_date as a date, b) age_years as a number
measles_clean <- measles_clean %>%
  mutate(
    report_date = as.Date(report_date),
    age_years   = as.numeric(age_years)   # "unknown" becomes NA (the warning is expected)
  )

summary(measles_clean$age_years)   # max is 999!


# ── PART 4: Fixing values with if_else() ───────────────────────────────────

# 👀 Follow along: create age_years (ages in months divided by 12)
linelist_clean <- linelist_clean %>%
  mutate(age_years = if_else(age_unit == "months", age / 12, age))

# 👀 Follow along: replace weights of 0 or below with NA
linelist_clean <- linelist_clean %>%
  mutate(wt_kg = if_else(wt_kg <= 0, NA, wt_kg))

# 🔁 Follow along: estimated onset with if_else() and ifelse()
if_else(is.na(linelist_clean$onset_date), linelist_clean$hospital_date, linelist_clean$onset_date) %>% head(3)
ifelse(is.na(linelist_clean$onset_date), linelist_clean$hospital_date, linelist_clean$onset_date) %>% head(3)
# ifelse() returns numbers (days since 1970-01-01): the Date type is lost

# ✏️ TRY IT: a) 999 -> NA, b) Fahrenheit -> Celsius
measles_clean <- measles_clean %>%
  mutate(
    age_years = if_else(age_years == 999, NA, age_years),
    temp_c    = if_else(temp_c > 45, round((temp_c - 32) * 5 / 9, 1), temp_c)
  )

summary(measles_clean$age_years)   # max 52
summary(measles_clean$temp_c)      # max 40.9


# ── PART 5: Recoding categories with case_when() ───────────────────────────

# 👀 Follow along: recode gender "m" -> "male", "f" -> "female". Check with count().
linelist_clean <- linelist_clean %>%
  mutate(gender = case_when(
    gender == "m" ~ "male",
    gender == "f" ~ "female"
  ))

linelist_clean %>% count(gender)

# 👀 Follow along: look at hosp_info and count(hospital), then create hospital_name
#    with case_when() + str_detect(). Check with count(hospital, hospital_name).
hosp_info
linelist_clean %>% count(hospital)

linelist_clean <- linelist_clean %>%
  mutate(hospital_name = case_when(
    str_detect(hospital, "Central")           ~ "Central Hospital",
    str_detect(hospital, "Military|Mitylira") ~ "Military Hospital",
    str_detect(hospital, "Regional")          ~ "Regional Medical center",
    str_detect(hospital, "Mark")              ~ "St. Mark's Maternity Hospital",
    str_detect(hospital, "HMO")               ~ "HMO Health",
    str_detect(hospital, "Premier")           ~ "Premiere Health",
    .default = "Other/Unknown"
  ))

linelist_clean %>% count(hospital, hospital_name)

# ✏️ TRY IT: recode sex and vaccinated
measles_clean <- measles_clean %>%
  mutate(
    sex = case_when(
      sex %in% c("Male", "male", "M")     ~ "male",
      sex %in% c("Female", "female", "F") ~ "female"
    ),
    vaccinated = case_when(
      vaccinated %in% c("Yes", "yes", "Y")     ~ "Yes",
      vaccinated %in% c("No", "no", "N", "NO") ~ "No",
      vaccinated == "Unknown"                  ~ "Unknown"
    )
  )

measles_clean %>% count(sex)          # female 533, male 642, NA 40
measles_clean %>% count(vaccinated)

# ✏️ TRY IT: make district one of North, South, East, West, Central
measles_clean <- measles_clean %>%
  mutate(
    district = str_to_title(district),
    district = case_when(
      str_detect(district, "North|Nroth")     ~ "North",
      str_detect(district, "South")           ~ "South",
      str_detect(district, "East")            ~ "East",
      str_detect(district, "West")            ~ "West",
      str_detect(district, "Central|Cetnral") ~ "Central"
    )
  )

measles_clean %>% count(district)     # 5 districts + NA (34)


# ── PART 6: Creating new variables ─────────────────────────────────────────

# 👀 Follow along: calculate bmi, then set BMIs below 10 or above 60 to NA
linelist_clean <- linelist_clean %>%
  mutate(
    bmi = round(wt_kg / (ht_cm / 100)^2, 1),
    bmi = if_else(bmi < 10 | bmi > 60, NA, bmi)
  )

summary(linelist_clean$bmi)

# 👀 Follow along: create age_cat: Under 18, 18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+
linelist_clean <- linelist_clean %>%
  mutate(age_cat = case_when(
    age_years < 18  ~ "Under 18",
    age_years < 30  ~ "18-29",
    age_years < 40  ~ "30-39",
    age_years < 50  ~ "40-49",
    age_years < 60  ~ "50-59",
    age_years < 70  ~ "60-69",
    age_years < 80  ~ "70-79",
    age_years >= 80 ~ "80+"
  ))

linelist_clean %>% count(age_cat)

# 🔁 Follow along: the same groups with cut()
cut(linelist_clean$age_years,
    breaks = c(0, 18, 30, 40, 50, 60, 70, 80, Inf),
    labels = c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
    right  = FALSE) %>%
  table(useNA = "ifany")   # same counts, already in order

# ✏️ TRY IT: a) outcome with str_to_title(), b) age_cat for measles
measles_clean <- measles_clean %>%
  mutate(
    outcome = str_to_title(outcome),
    age_cat = case_when(
      age_years < 5   ~ "Under 5",
      age_years < 10  ~ "5-9",
      age_years < 15  ~ "10-14",
      age_years >= 15 ~ "15+"
    )
  )

measles_clean %>% count(outcome)      # Died, Recovered, NA
measles_clean %>% count(age_cat)      # 4 groups + NA (31)


# ── PART 7: One pipeline and saving your work ──────────────────────────────

# 👀 Follow along: one pipeline from linelist_raw to linelist_clean
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

# 👀 Follow along: save linelist_clean as processed_data/linelist_clean.rds
export(linelist_clean, here("processed_data", "linelist_clean.rds"))

# ✏️ TRY IT: save measles_clean as processed_data/measles_clean.rds
export(measles_clean, here("processed_data", "measles_clean.rds"))


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. All of the measles TRY ITs as one pipeline (gives the same result)
measles_clean <- measles_raw %>%
  clean_names() %>%
  mutate(
    report_date = as.Date(report_date),
    age_years   = as.numeric(age_years),
    age_years   = if_else(age_years == 999, NA, age_years),
    temp_c      = if_else(temp_c > 45, round((temp_c - 32) * 5 / 9, 1), temp_c),
    sex = case_when(
      sex %in% c("Male", "male", "M")     ~ "male",
      sex %in% c("Female", "female", "F") ~ "female"
    ),
    vaccinated = case_when(
      vaccinated %in% c("Yes", "yes", "Y")     ~ "Yes",
      vaccinated %in% c("No", "no", "N", "NO") ~ "No",
      vaccinated == "Unknown"                  ~ "Unknown"
    ),
    district = str_to_title(district),
    district = case_when(
      str_detect(district, "North|Nroth")     ~ "North",
      str_detect(district, "South")           ~ "South",
      str_detect(district, "East")            ~ "East",
      str_detect(district, "West")            ~ "West",
      str_detect(district, "Central|Cetnral") ~ "Central"
    ),
    outcome = str_to_title(outcome),
    age_cat = case_when(
      age_years < 5   ~ "Under 5",
      age_years < 10  ~ "5-9",
      age_years < 15  ~ "10-14",
      age_years >= 15 ~ "15+"
    )
  )
