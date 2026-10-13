# Session 8: Programming — Functions, if/else, Loops, and Iteration
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#
# NOTE: two lines below end in an ERROR on purpose (to show what goes wrong).
# They are wrapped in try() so the rest of the script keeps running.
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along: load packages; import linelist
library(tidyverse)
library(here)
library(rio)
library(janitor)

linelist <- import(here("processed_data", "combined_linelist.rds"), trust = TRUE)

# ✏️ TRY IT: import measles
measles <- import(here("processed_data", "measles_final.rds"), trust = TRUE)


# ── PART 2: Writing your first function ────────────────────────────────────

# 👀 Follow along: write c_to_f(temp_c) and test it
c_to_f <- function(temp_c) {
  temp_c * 9 / 5 + 32
}

c_to_f(38)
c_to_f(c(36.8, 38.5, 40.1))   # vectorized

# 👀 Follow along: use c_to_f() inside mutate()
linelist %>%
  mutate(temp_f = c_to_f(temp)) %>%
  select(case_id, temp, temp_f) %>%
  head(3)

# 👀 Follow along: a digits = 1 default argument
c_to_f <- function(temp_c, digits = 1) {
  round(temp_c * 9 / 5 + 32, digits)
}

c_to_f(38.37)
c_to_f(38.37, digits = 0)

# ✏️ TRY IT: f_to_c()
f_to_c <- function(temp_f) {
  round((temp_f - 32) * 5 / 9, 1)
}

f_to_c(98.6)    # 37
f_to_c(104.7)   # 40.4: the Fahrenheit temperature from Session 1!


# ── PART 3: Functions that use data ────────────────────────────────────────

# 👀 Follow along: pct()
pct <- function(x, digits = 1) {
  round(mean(x, na.rm = TRUE) * 100, digits)
}

linelist %>%
  group_by(hospital_name) %>%
  summarise(
    n         = n(),
    pct_death = pct(outcome == "Death"),
    pct_fever = pct(fever == "yes")
  )

# 👀 Follow along: hospital_summary()
hospital_summary <- function(data, hospital) {
  data %>%
    filter(hospital_name == hospital) %>%
    summarise(
      n         = n(),
      pct_death = pct(outcome == "Death")
    )
}

hospital_summary(linelist, "Military Hospital")
hospital_summary(linelist, "Central Hospital")

# 👀 Follow along: make_epicurve()
make_epicurve <- function(data, hospital) {
  data %>%
    filter(hospital_name == hospital, onset_date >= as.Date("2014-01-01")) %>%
    ggplot(aes(x = onset_date)) +
    geom_histogram(binwidth = 7, fill = "#2a78d6", color = "white") +
    labs(
      title = paste("Weekly cases:", hospital),
      x     = "Week of onset",
      y     = "Number of cases"
    ) +
    theme_minimal()
}

make_epicurve(linelist, "Military Hospital")

# ✏️ TRY IT:
# a)
rate_per_10k <- function(cases, population) {
  round(cases / population * 10000, 1)
}

measles %>%
  count(district, population) %>%
  mutate(rate = rate_per_10k(n, population)) %>%
  arrange(desc(rate))   # East 22.8

# b)
district_summary <- function(data, district_name) {
  data %>%
    filter(district == district_name) %>%
    summarise(
      n              = n(),
      pct_vaccinated = pct(vaccinated == "Yes")
    )
}

district_summary(measles, "North")   # n 342, 25.7%


# ── PART 4: if / else ──────────────────────────────────────────────────────

# 👀 Follow along: fever_status() with if / else
fever_status <- function(temp) {
  if (temp >= 38) {
    "fever"
  } else {
    "no fever"
  }
}

fever_status(39.2)
fever_status(36.9)

# 👀 Follow along: what happens with a vector? An error (on purpose):
try(fever_status(c(39.2, 36.9)))   # Error: the condition has length > 1

# 👀 Follow along: the vectorized version with if_else()
fever_status <- function(temp) {
  if_else(temp >= 38, "fever", "no fever")
}

fever_status(c(39.2, 36.9))

linelist %>%
  mutate(fever_measured = fever_status(temp)) %>%
  count(fever_measured)

# 👀 Follow along: an input check with stop()
c_to_f <- function(temp_c, digits = 1) {
  if (!is.numeric(temp_c)) {
    stop("temp_c must be a number, not ", class(temp_c))
  }
  round(temp_c * 9 / 5 + 32, digits)
}

try(c_to_f("38"))   # Error: temp_c must be a number, not character (on purpose)

# ✏️ TRY IT:
# a)
fever_status_one <- function(temp) {
  if (temp >= 38) {
    "fever"
  } else {
    "no fever"
  }
}

fever_status_one(38.5)   # "fever"

# b)
fever_status <- function(temp) {
  if_else(temp >= 38, "fever", "no fever")
}

measles %>%
  mutate(fever_measured = fever_status(temp_c)) %>%
  count(fever_measured)   # fever 1015, no fever 148


# ── PART 5: for loops ──────────────────────────────────────────────────────

# 👀 Follow along: a loop that prints two hospital names
for (h in c("Central Hospital", "Military Hospital")) {
  print(h)
}

# 👀 Follow along: the hospitals vector, and a loop that prints the number of cases
hospitals <- linelist %>%
  filter(hospital_name != "Other/Unknown") %>%
  distinct(hospital_name) %>%
  pull(hospital_name) %>%
  sort()

hospitals

for (h in hospitals) {
  n_cases <- sum(linelist$hospital_name == h)
  print(paste(h, "has", n_cases, "cases"))
}

# 👀 Follow along: save the counts
case_counts <- c()

for (h in hospitals) {
  case_counts[h] <- sum(linelist$hospital_name == h)
}

case_counts

# 👀 Follow along: THE PAYOFF: one epicurve per hospital
for (h in hospitals) {
  p <- make_epicurve(linelist, h)
  file_name <- paste0("epicurve_", make_clean_names(h), ".png")
  ggsave(here("outputs", file_name), plot = p, width = 8, height = 5, dpi = 300)
}

# ✏️ TRY IT: one measles epicurve per district
make_measles_epicurve <- function(data, district_name) {
  data %>%
    filter(district == district_name, !is.na(report_date)) %>%
    ggplot(aes(x = report_date)) +
    geom_histogram(binwidth = 7, fill = "#2a78d6", color = "white") +
    labs(
      title = paste("Weekly measles cases:", district_name),
      x     = "Week of report",
      y     = "Number of cases"
    ) +
    theme_minimal()
}

districts <- c("Central", "East", "North", "South", "West")

for (d in districts) {
  p <- make_measles_epicurve(measles, d)
  file_name <- paste0("measles_", make_clean_names(d), ".png")
  ggsave(here("outputs", file_name), plot = p, width = 8, height = 5, dpi = 300)
}


# ── PART 6: Iteration the tidyverse way: map() and walk() ──────────────────

# 👀 Follow along: map_dbl()
linelist %>%
  select(age_years, bmi, temp) %>%
  map_dbl(~ mean(.x, na.rm = TRUE))

# 👀 Follow along: map() to a list of plots
epicurves <- map(hospitals, ~ make_epicurve(linelist, .x))
length(epicurves)   # 6
epicurves[[1]]

# 👀 Follow along: walk() to save them all
walk(hospitals, ~ ggsave(
  here("outputs", paste0("epicurve_", make_clean_names(.x), ".png")),
  plot = make_epicurve(linelist, .x), width = 8, height = 5, dpi = 300
))

# ✏️ TRY IT:
# a)
measles %>%
  select(age_years, temp_c, days_in_hospital) %>%
  map_dbl(~ mean(.x, na.rm = TRUE))   # 8.75, 38.82, 4.94

# b)
walk(districts, ~ ggsave(
  here("outputs", paste0("measles_", make_clean_names(.x), ".png")),
  plot = make_measles_epicurve(measles, .x), width = 8, height = 5, dpi = 300
))


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. A fill argument with a default
make_epicurve <- function(data, hospital, fill = "#2a78d6") {
  data %>%
    filter(hospital_name == hospital, onset_date >= as.Date("2014-01-01")) %>%
    ggplot(aes(x = onset_date)) +
    geom_histogram(binwidth = 7, fill = fill, color = "white") +
    labs(title = paste("Weekly cases:", hospital), x = "Week of onset", y = "Number of cases") +
    theme_minimal()
}

make_epicurve(linelist, "Central Hospital", fill = "#eb6834")

# 2. An input check
rate_per_10k <- function(cases, population) {
  if (any(is.na(population) | population == 0)) {
    stop("population must not be 0 or missing")
  }
  round(cases / population * 10000, 1)
}

try(rate_per_10k(10, 0))   # Error (on purpose)

# 3. A Table 1 function, run for each district
library(gtsummary)

measles_table1 <- function(data) {
  data %>%
    filter(outcome != "Unknown") %>%
    select(outcome, sex, age_years, vaccinated, rash, temp_c) %>%
    mutate(across(where(is.character), ~ na_if(.x, "Unknown"))) %>%
    tbl_summary(by = outcome, missing_text = "Missing")
}

district_tables <- map(districts, ~ measles_table1(filter(measles, district == .x)))
district_tables[[1]]   # Central
