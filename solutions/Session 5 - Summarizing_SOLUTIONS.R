# Session 5: Summarizing Your Data
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along: load packages; import combined_linelist.rds as linelist
library(tidyverse)
library(here)
library(rio)
library(janitor)
library(gtsummary)

linelist <- import(here("processed_data", "combined_linelist.rds"), trust = TRUE)

# ✏️ TRY IT: import measles_final.rds as measles
measles <- import(here("processed_data", "measles_final.rds"), trust = TRUE)


# ── PART 2: Putting categories in order (factors) ──────────────────────────

# 👀 Follow along: count(age_cat). What order are the groups in?
linelist %>% count(age_cat)   # alphabetical: "Under 18" ends up near the bottom

# 👀 Follow along: fct_relevel() age_cat and outcome
linelist <- linelist %>%
  mutate(
    age_cat = fct_relevel(age_cat, "Under 18", "18-29", "30-39", "40-49",
                          "50-59", "60-69", "70-79", "80+", "Unknown"),
    outcome = fct_relevel(outcome, "Recover", "Death", "Unknown")
  )

linelist %>% count(age_cat)

# 🔁 Follow along: the base R way
factor(c("Recover", "Death", "Death"), levels = c("Recover", "Death"))
factor(c("Recover", "Death", "Death"), levels = c("Recover", "death"))   # NA, no warning!

# ✏️ TRY IT: fct_relevel() age_cat, vaccinated, and outcome in measles
measles <- measles %>%
  mutate(
    age_cat    = fct_relevel(age_cat, "Under 5", "5-9", "10-14", "15+", "Unknown"),
    vaccinated = fct_relevel(vaccinated, "Yes", "No", "Unknown"),
    outcome    = fct_relevel(outcome, "Recovered", "Died", "Unknown")
  )

measles %>% count(age_cat)


# ── PART 3: Counting and percentages with count() ──────────────────────────

# 👀 Follow along: count(outcome), then count(age_cat, outcome)
linelist %>% count(outcome)
linelist %>% count(age_cat, outcome)

# 👀 Follow along: count(outcome) with a percent column
linelist %>%
  count(outcome) %>%
  mutate(percent = scales::percent(n / sum(n), accuracy = 0.1))

# 👀 Follow along: percentages within each age group with group_by()
linelist %>%
  group_by(age_cat) %>%
  count(outcome) %>%
  mutate(percent = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup()

# 🔁 Follow along: the base R way
age_outcome <- table(linelist$age_cat, linelist$outcome)
age_outcome
round(prop.table(age_outcome, margin = 1) * 100, 1)   # each row adds up to 100

# ✏️ TRY IT:
# a)
measles %>%
  count(vaccinated) %>%
  mutate(percent = scales::percent(n / sum(n), accuracy = 0.1))   # Yes 23.8%

# b)
measles %>%
  group_by(age_cat) %>%
  count(vaccinated) %>%
  mutate(percent = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  ungroup()   # 10-14 is lowest: 21.2% vaccinated


# ── PART 4: Summary statistics with summarise() ────────────────────────────

# 👀 Follow along: summarise() for everyone
linelist %>%
  summarise(
    n          = n(),
    mean_age   = mean(age_years, na.rm = TRUE),
    sd_age     = sd(age_years, na.rm = TRUE),
    median_age = median(age_years, na.rm = TRUE)
  )

# 👀 Follow along: by outcome
linelist %>%
  group_by(outcome) %>%
  summarise(
    n          = n(),
    mean_age   = mean(age_years, na.rm = TRUE),
    median_age = median(age_years, na.rm = TRUE),
    mean_bmi   = mean(bmi, na.rm = TRUE),
    mean_temp  = mean(temp, na.rm = TRUE)
  )

# 👀 Follow along: across()
linelist %>%
  group_by(outcome) %>%
  summarise(across(c(age_years, bmi, temp), ~ mean(.x, na.rm = TRUE)))

# 👀 Follow along: percentage with each symptom by outcome
linelist %>%
  group_by(outcome) %>%
  summarise(
    n_cases   = n(),
    n_fever   = sum(fever == "yes"),
    pct_fever = round(mean(fever == "yes") * 100, 1),
    pct_cough = round(mean(cough == "yes") * 100, 1),
    pct_vomit = round(mean(vomit == "yes") * 100, 1)
  )

# 🔁 Follow along: the base R way
tapply(linelist$age_years, linelist$outcome, median, na.rm = TRUE)   # 13, 13, 14
aggregate(age_years ~ outcome, data = linelist, FUN = median)

# ✏️ TRY IT: summary statistics by measles outcome
measles %>%
  group_by(outcome) %>%
  summarise(
    n          = n(),
    mean_age   = mean(age_years, na.rm = TRUE),
    median_age = median(age_years, na.rm = TRUE),
    mean_temp  = mean(temp_c),
    mean_days  = mean(days_in_hospital, na.rm = TRUE)
  )
# Recovered n = 1048 (median age 7); Died n = 50 (median age 5): children who died were younger

# ✏️ TRY IT: cases per 10,000 people by district
measles %>%
  count(district, population) %>%
  mutate(rate_per_10k = round(n / population * 10000, 1)) %>%
  arrange(desc(rate_per_10k))
# East is highest (22.8). Central has the most people but the lowest rate (5.2).
# The "Unknown" district has no population, so no rate.


# ── PART 5: Quick tables with tabyl() ──────────────────────────────────────

# 👀 Follow along: tabyl(outcome), then tabyl(age_cat, outcome)
linelist %>% tabyl(outcome)
linelist %>% tabyl(age_cat, outcome)

# 👀 Follow along: with all five adorn_ functions
linelist %>%
  tabyl(age_cat, outcome) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>%
  adorn_title("combined")

# ✏️ TRY IT: vaccinated by outcome
measles %>%
  tabyl(vaccinated, outcome) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()
# 6.3% of unvaccinated cases died, compared with 0.7% of vaccinated cases.


# ── PART 6: Table 1 with gtsummary ─────────────────────────────────────────

# 👀 Follow along: a basic tbl_summary(by = outcome)
linelist %>%
  filter(outcome != "Unknown") %>%
  mutate(outcome = fct_drop(outcome)) %>%
  select(outcome, gender, age_years, age_cat, bmi, hospital_name, fever, cough, temp) %>%
  tbl_summary(by = outcome)

# 👀 Follow along: the same with na_if() for "Unknown"
linelist %>%
  filter(outcome != "Unknown") %>%
  mutate(outcome = fct_drop(outcome)) %>%
  select(outcome, gender, age_years, age_cat, bmi, hospital_name, fever, cough, temp) %>%
  mutate(across(where(is.character), ~ na_if(.x, "Unknown"))) %>%
  tbl_summary(by = outcome)

# 👀 Follow along: a customized table1, saved to Word
table1 <- linelist %>%
  filter(outcome != "Unknown") %>%
  mutate(outcome = fct_drop(outcome)) %>%
  select(outcome, gender, age_years, bmi, hospital_name, fever, cough, temp) %>%
  mutate(across(where(is.character), ~ na_if(.x, "Unknown"))) %>%
  tbl_summary(
    by = outcome,
    label = list(
      gender        ~ "Gender",
      age_years     ~ "Age (years)",
      bmi           ~ "BMI",
      hospital_name ~ "Hospital",
      fever         ~ "Fever",
      cough         ~ "Cough",
      temp          ~ "Temperature (°C)"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 1. Patient characteristics by outcome**")

table1

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = here("outputs", "table1.docx"))

# ✏️ TRY IT: a Table 1 for the measles cases
measles_table1 <- measles %>%
  filter(outcome != "Unknown") %>%
  mutate(outcome = fct_drop(outcome)) %>%
  select(outcome, sex, age_years, age_cat, vaccinated, rash, temp_c, days_in_hospital) %>%
  mutate(across(where(is.character), ~ na_if(.x, "Unknown"))) %>%
  tbl_summary(
    by = outcome,
    label = list(
      sex              ~ "Sex",
      age_years        ~ "Age (years)",
      age_cat          ~ "Age group",
      vaccinated       ~ "Vaccinated",
      rash             ~ "Rash",
      temp_c           ~ "Temperature (°C)",
      days_in_hospital ~ "Days in hospital"
    ),
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  bold_labels()

measles_table1
# "Missing" for days in hospital = patients who were never hospitalized


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. Save the measles table to Word
measles_table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = here("outputs", "measles_table1.docx"))

# 2. A district × outcome table with pivot_wider()
measles %>%
  count(district, outcome) %>%
  pivot_wider(names_from = outcome, values_from = n, values_fill = 0)
