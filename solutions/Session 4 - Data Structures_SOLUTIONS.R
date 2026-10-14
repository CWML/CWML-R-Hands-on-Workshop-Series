# Session 4: Data Structures — Vectors, Subsetting, and Reshaping
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along: load tidyverse, here, rio; import combined_linelist.rds as linelist
library(tidyverse)
library(here)
library(rio)

linelist <- import(here("processed_data", "combined_linelist.rds"), trust = TRUE)

# ✏️ TRY IT: import processed_data/measles_final.rds and call it measles
measles <- import(here("processed_data", "measles_final.rds"), trust = TRUE)


# ── PART 2: Vectors and types ──────────────────────────────────────────────

# 👀 Follow along: pull out linelist$age_years as ages; head() and length() it
ages <- linelist$age_years
head(ages)
length(ages)   # 6451, one per row

# 👀 Follow along: class() of age_years, gender, and onset_date
class(linelist$age_years)    # "numeric"
class(linelist$gender)       # "character"
class(linelist$onset_date)   # "Date"

# 👀 Follow along: what happens with c(1, 2, "unknown") and c(1, TRUE, FALSE)?
c(1, 2, "unknown")   # all text: "1" "2" "unknown"
c(1, TRUE, FALSE)    # all numbers: 1 1 0

# 👀 Follow along: mean(c(38, 39, NA)) with and without na.rm = TRUE
mean(c(38, 39, NA))                # NA
mean(c(38, 39, NA), na.rm = TRUE)  # 38.5

# ✏️ TRY IT: class() and length()
class(measles$age_years)     # "numeric"
class(measles$vaccinated)    # "character"
class(measles$report_date)   # "Date"
length(measles$temp_c)       # 1163


# ── PART 3: Vectorization ──────────────────────────────────────────────────

# 👀 Follow along: make temps_c and convert it to Fahrenheit
temps_c <- c(36.8, 38.5, 40.1)
temps_c * 9 / 5 + 32

# 👀 Follow along: head() of every patient's weight in pounds
head(linelist$wt_kg * 2.2)

# 👀 Follow along: comparisons, sum(), and mean()
temps_c > 38
sum(linelist$temp > 38, na.rm = TRUE)    # 4943 patients
mean(linelist$temp > 38, na.rm = TRUE)   # 0.785 (78.5%)
mean(linelist$fever == "yes")            # 0.787 (78.7%)

# ✏️ TRY IT:
head(measles$temp_c * 9 / 5 + 32)    # a) 100.94 101.12 102.74 ...
sum(measles$temp_c >= 39)            # b) 565
mean(measles$vaccinated == "Yes")    # c) 0.238 (23.8%)


# ── PART 4: Subsetting with base R ─────────────────────────────────────────

# 👀 Follow along: subsetting a vector
temps_c[1]
temps_c[c(1, 3)]
temps_c[-1]
temps_c[temps_c > 38]

# 👀 Follow along: rows and columns of a data frame
linelist[1:3, c("case_id", "age_years", "outcome")]

# 👀 Follow along: the first three hospital names with $ and with [[ ]]
linelist$hospital_name[1:3]
linelist[["hospital_name"]][1:3]

# 👀 Follow along: the mean temperature of patients who died
mean(linelist$temp[linelist$outcome == "Death"], na.rm = TRUE)

# 👀 Follow along: base R vs filter() with missing values
nrow(linelist[linelist$temp > 40, ])   # 244: includes 151 rows of NAs!
nrow(filter(linelist, temp > 40))      # 93
nrow(linelist[linelist$temp > 40 & !is.na(linelist$temp), ])   # 93: fixed

# ✏️ TRY IT:
# a)
measles[1:5, c("patient_id", "temp_c")]

# b)
mean(measles$temp_c[measles$outcome == "Died"])   # 38.75

# c)
nrow(measles[measles$age_years >= 15, ])          # 225: includes 29 NA rows (missing ages)
nrow(filter(measles, age_years >= 15))            # 196
nrow(measles[measles$age_years >= 15 & !is.na(measles$age_years), ])   # 196: fixed


# ── PART 5: Lists ──────────────────────────────────────────────────────────

# 👀 Follow along: a list
patient <- list(
  id       = "5fe599",
  age      = 2,
  symptoms = c("fever", "cough"),
  visits   = as.Date(c("2014-05-13", "2014-05-15"))
)
patient
patient$symptoms     # one element
patient[["age"]]     # one element
patient["age"]       # a smaller list
length(patient)      # 4
str(patient)

# 👀 Follow along: a data frame is a list
is.list(linelist)    # TRUE
length(linelist)     # 19 columns

# 👀 Follow along: a test result is a list
age_test <- t.test(age_years ~ gender, data = filter(linelist, gender != "Unknown"))
age_test
names(age_test)
age_test$p.value
age_test$estimate

# ✏️ TRY IT: a test result is a list
temp_test <- t.test(temp_c ~ vaccinated, data = filter(measles, vaccinated != "Unknown"))   # a)
temp_test$p.value    # b) 0.565
temp_test$estimate   #    38.80 (No) and 38.84 (Yes): almost no difference


# ── PART 6: Tidy data and pivot_longer() ───────────────────────────────────

# 👀 Follow along: look at the symptom columns
linelist %>% select(case_id, fever:vomit) %>% head(3)

# 👀 Follow along: pivot_longer() the five symptoms
symptoms_long <- linelist %>%
  select(case_id, fever:vomit) %>%
  pivot_longer(
    cols      = fever:vomit,
    names_to  = "symptom",
    values_to = "present"
  )

head(symptoms_long)
nrow(symptoms_long)   # 32255 = 6451 patients × 5 symptoms

# 👀 Follow along: count the patients with each symptom, and plot it
symptom_counts <- symptoms_long %>%
  filter(present == "yes") %>%
  count(symptom)

symptom_counts

ggplot(symptom_counts, aes(x = n, y = fct_reorder(symptom, n))) +
  geom_col(fill = "#2a78d6") +
  labs(title = "Patients reporting each symptom", x = "Number of patients", y = NULL) +
  theme_minimal()

# ✏️ TRY IT: measles fever and rash, long
measles_symptoms <- measles %>%
  select(patient_id, fever, rash) %>%
  pivot_longer(
    cols      = c(fever, rash),
    names_to  = "symptom",
    values_to = "present"
  )

nrow(measles_symptoms)                    # 2326 = 1163 × 2
measles_symptoms %>% count(symptom, present)   # fever yes 1064, rash yes 976


# ── PART 7: pivot_wider() ──────────────────────────────────────────────────

# 👀 Follow along: count(hospital_name, outcome), then pivot_wider()
linelist %>% count(hospital_name, outcome)

linelist %>%
  count(hospital_name, outcome) %>%
  pivot_wider(
    names_from  = outcome,
    values_from = n
  )

# ✏️ TRY IT: a district × vaccinated table
measles %>%
  count(district, vaccinated) %>%
  pivot_wider(
    names_from  = vaccinated,
    values_from = n
  )
# North: No 205, Unknown 49, Yes 88


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. Percentages instead of counts
measles %>%
  count(district, vaccinated) %>%
  group_by(district) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = vaccinated, values_from = pct)

# 2. Symptoms by vaccination status
measles %>%
  select(patient_id, vaccinated, fever, rash) %>%
  pivot_longer(cols = c(fever, rash), names_to = "symptom", values_to = "present") %>%
  filter(present == "yes") %>%
  ggplot(aes(x = symptom, fill = vaccinated)) +
  geom_bar(position = "dodge") +
  labs(x = "Symptom", y = "Number of cases", fill = "Vaccinated") +
  theme_minimal()
