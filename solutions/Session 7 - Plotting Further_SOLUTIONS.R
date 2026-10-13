# Session 7: Plotting Further — Bar Charts, Colors, Epicurves, and Facets
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along: load packages; import linelist and order the categories
library(tidyverse)
library(here)
library(rio)

linelist <- import(here("processed_data", "combined_linelist.rds"), trust = TRUE) %>%
  mutate(
    age_cat = fct_relevel(age_cat, "Under 18", "18-29", "30-39", "40-49",
                          "50-59", "60-69", "70-79", "80+", "Unknown"),
    outcome = fct_relevel(outcome, "Recover", "Death", "Unknown")
  )

# ✏️ TRY IT: import measles, order the categories, and save vacc_colors
measles <- import(here("processed_data", "measles_final.rds"), trust = TRUE) %>%
  mutate(
    age_cat    = fct_relevel(age_cat, "Under 5", "5-9", "10-14", "15+", "Unknown"),
    vaccinated = fct_relevel(vaccinated, "Yes", "No", "Unknown")
  )

vacc_colors <- c("Yes" = "#2a78d6", "No" = "#eb6834", "Unknown" = "#9a9a95")
vacc_colors


# ── PART 2: Bar charts of counts: geom_bar() ───────────────────────────────

# 👀 Follow along: x, then y
ggplot(data = linelist, mapping = aes(x = hospital_name)) +
  geom_bar()

ggplot(data = linelist, mapping = aes(y = hospital_name)) +
  geom_bar()

# 👀 Follow along: ordered bars
ggplot(data = linelist, mapping = aes(y = fct_rev(fct_infreq(hospital_name)))) +
  geom_bar(fill = "#2a78d6") +
  labs(title = "Cases by hospital", x = "Number of cases", y = NULL) +
  theme_minimal()

# 👀 Follow along: stack, fill, dodge
ggplot(data = linelist, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "stack")

ggplot(data = linelist, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill")

ggplot(data = linelist, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "dodge")

# ✏️ TRY IT: measles cases by district, longest bar on top
ggplot(data = measles, mapping = aes(y = fct_rev(fct_infreq(district)))) +
  geom_bar(fill = "#2a78d6") +
  labs(title = "Measles cases by district", x = "Number of cases", y = NULL) +
  theme_minimal()


# ── PART 3: Bar charts of values: geom_col() ───────────────────────────────

# 👀 Follow along: percentage who died per age group
death_by_age <- linelist %>%
  filter(outcome != "Unknown", age_cat != "Unknown") %>%
  group_by(age_cat) %>%
  summarise(
    n         = n(),
    pct_death = mean(outcome == "Death") * 100
  )

death_by_age

ggplot(data = death_by_age, mapping = aes(x = age_cat, y = pct_death)) +
  geom_col(fill = "#2a78d6") +
  labs(title = "Case fatality by age group", x = "Age group", y = "Deaths (% of cases)") +
  theme_minimal()

# ✏️ TRY IT: case rates by district
# a)
district_rates <- measles %>%
  filter(district != "Unknown") %>%
  count(district, population) %>%
  mutate(rate_per_10k = round(n / population * 10000, 1))

district_rates

# b)
ggplot(data = district_rates, mapping = aes(x = fct_reorder(district, rate_per_10k), y = rate_per_10k)) +
  geom_col(fill = "#2a78d6") +
  labs(
    title = "Measles cases per 10,000 people by district",
    x     = "District",
    y     = "Cases per 10,000 people"
  ) +
  theme_minimal()


# ── PART 4: Choosing colors ────────────────────────────────────────────────

# 👀 Follow along: outcome by age group with chosen colors
outcome_colors <- c("Recover" = "#2a78d6", "Death" = "#eb6834", "Unknown" = "#9a9a95")

ggplot(data = linelist, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = outcome_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Outcome by age group",
    x     = "Age group",
    y     = "Percent of cases",
    fill  = "Outcome"
  ) +
  theme_minimal()

# ✏️ TRY IT: vaccination by age group
ggplot(data = measles, mapping = aes(x = age_cat, fill = vaccinated)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = vacc_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Vaccination status of measles cases by age group",
    x     = "Age group",
    y     = "Percent of cases",
    fill  = "Vaccinated"
  ) +
  theme_minimal()


# ── PART 5: Epidemic curves ────────────────────────────────────────────────

# 👀 Follow along: all onset dates. What's going on before 2014?
ggplot(data = linelist, mapping = aes(x = onset_date)) +
  geom_histogram(binwidth = 7, fill = "#2a78d6", color = "white")
# 538 onset dates fall in 2012–2013. Every one belongs to a case ID that appears
# more than once: almost certainly data-entry errors in the year.

# 👀 Follow along: from 2014, with a date axis
linelist %>%
  filter(onset_date >= as.Date("2014-01-01")) %>%
  ggplot(mapping = aes(x = onset_date)) +
  geom_histogram(binwidth = 7, fill = "#2a78d6", color = "white") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Epidemic curve: weekly cases by date of symptom onset",
    subtitle = "Onset dates from 2014 onward",
    x        = "Week of onset",
    y        = "Number of cases"
  ) +
  theme_minimal()

# 👀 Follow along: fill by outcome
linelist %>%
  filter(onset_date >= as.Date("2014-01-01")) %>%
  ggplot(mapping = aes(x = onset_date, fill = outcome)) +
  geom_histogram(binwidth = 7, color = "white") +
  scale_fill_manual(values = outcome_colors) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(x = "Week of onset", y = "Number of cases", fill = "Outcome") +
  theme_minimal()

# ✏️ TRY IT: the measles epicurve
measles_epicurve <- measles %>%
  filter(!is.na(report_date)) %>%
  ggplot(mapping = aes(x = report_date, fill = vaccinated)) +
  geom_histogram(binwidth = 7, color = "white") +
  scale_fill_manual(values = vacc_colors) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Measles cases by week of report, 2024",
    x     = "Week of report",
    y     = "Number of cases",
    fill  = "Vaccinated"
  ) +
  theme_minimal()

measles_epicurve
# The outbreak peaked in the second half of March 2024 (about 115 cases in the peak week).


# ── PART 6: Facets ─────────────────────────────────────────────────────────

# 👀 Follow along: epicurve by hospital
linelist %>%
  filter(onset_date >= as.Date("2014-01-01"), hospital_name != "Other/Unknown") %>%
  ggplot(mapping = aes(x = onset_date)) +
  geom_histogram(binwidth = 7, fill = "#2a78d6", color = "white") +
  facet_wrap(~ hospital_name) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b\n%Y") +
  labs(title = "Weekly cases by hospital", x = "Week of onset", y = "Number of cases") +
  theme_minimal()

# 👀 Follow along: age histogram by gender
linelist %>%
  filter(gender != "Unknown") %>%
  ggplot(mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "#2a78d6", color = "white") +
  facet_wrap(~ gender) +
  labs(title = "Age distribution by gender", x = "Age (years)", y = "Number of cases") +
  theme_minimal()

# ✏️ TRY IT: the measles epicurve by district
measles %>%
  filter(!is.na(report_date), district != "Unknown") %>%
  ggplot(mapping = aes(x = report_date, fill = vaccinated)) +
  geom_histogram(binwidth = 7, color = "white") +
  facet_wrap(~ district) +
  scale_fill_manual(values = vacc_colors) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(
    title = "Measles cases by week of report and district, 2024",
    x     = "Week of report",
    y     = "Number of cases",
    fill  = "Vaccinated"
  ) +
  theme_minimal()
# Every district peaked in March (West and South a little earlier than the others).
# North has the most cases.


# ── PART 7: Saving plots ───────────────────────────────────────────────────

# 👀 Follow along: save the epicurve
epicurve <- linelist %>%
  filter(onset_date >= as.Date("2014-01-01")) %>%
  ggplot(mapping = aes(x = onset_date)) +
  geom_histogram(binwidth = 7, fill = "#2a78d6", color = "white") +
  labs(title = "Weekly cases", x = "Week of onset", y = "Number of cases") +
  theme_minimal()

ggsave(here("outputs", "epicurve.png"), plot = epicurve,
       width = 8, height = 5, dpi = 300)

# ✏️ TRY IT: save the measles epicurve
ggsave(here("outputs", "measles_epicurve.png"), plot = measles_epicurve,
       width = 8, height = 5, dpi = 300)


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. Percentage who died by age group (under 5s have the highest percentage)
measles %>%
  filter(outcome != "Unknown", age_cat != "Unknown") %>%
  group_by(age_cat) %>%
  summarise(pct_died = mean(outcome == "Died") * 100) %>%
  ggplot(mapping = aes(x = age_cat, y = pct_died)) +
  geom_col(fill = "#2a78d6") +
  labs(title = "Measles deaths by age group", x = "Age group", y = "Died (% of cases)") +
  theme_minimal()

# 2. A built-in colorblind-friendly palette
ggplot(data = measles, mapping = aes(x = age_cat, fill = vaccinated)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  theme_minimal()
