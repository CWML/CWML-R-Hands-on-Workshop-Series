# Session 6: Plotting Basics with ggplot2
# R Hands-on Workshop Series — SOLUTIONS
#________________________________________________________________________________
#
# This is the student script with all of the code filled in, including every
# ✏️ TRY IT. Use it to check your work, but try each part yourself first!
#________________________________________________________________________________


# ── PART 1: Set up ─────────────────────────────────────────────────────────

# 👀 Follow along 1.1: load packages; import linelist and order the categories
library(tidyverse)
library(here)
library(rio)

linelist <- import(here("processed_data", "combined_linelist.rds"), trust = TRUE) %>%
  mutate(
    age_cat = fct_relevel(age_cat, "Under 18", "18-29", "30-39", "40-49",
                          "50-59", "60-69", "70-79", "80+", "Unknown"),
    outcome = fct_relevel(outcome, "Recover", "Death", "Unknown")
  )

# ✏️ TRY IT 1: import measles and order the categories
measles <- import(here("processed_data", "measles_final.rds"), trust = TRUE) %>%
  mutate(
    age_cat    = fct_relevel(age_cat, "Under 5", "5-9", "10-14", "15+", "Unknown"),
    vaccinated = fct_relevel(vaccinated, "Yes", "No", "Unknown")
  )

levels(measles$age_cat)


# ── PART 2: The grammar of graphics ────────────────────────────────────────

# 👀 Follow along 2.1: three steps
ggplot(data = linelist)

ggplot(data = linelist, mapping = aes(x = age_years))

ggplot(data = linelist, mapping = aes(x = age_years)) +
  geom_histogram()

# ✏️ TRY IT 2: a histogram of measles temp_c in three steps
ggplot(data = measles)

ggplot(data = measles, mapping = aes(x = temp_c))

ggplot(data = measles, mapping = aes(x = temp_c)) +
  geom_histogram()


# ── PART 3: Histograms ─────────────────────────────────────────────────────

# 👀 Follow along 3.1: binwidth = 5
ggplot(data = linelist, mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 5)

# 👀 Follow along 3.2: fill and outline colors
ggplot(data = linelist, mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "#2a78d6", color = "white")

# ✏️ TRY IT 3: measles ages, binwidth = 1
ggplot(data = measles, mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 1, fill = "#2a78d6", color = "white")
# The tallest bar is age 2; most cases are ages 1–5.


# ── PART 4: Mapping vs setting ─────────────────────────────────────────────

# 👀 Follow along 4.1: mapping
ggplot(data = linelist, mapping = aes(x = age_years, fill = outcome)) +
  geom_histogram(binwidth = 5, color = "white")

# 👀 Follow along 4.2: setting
ggplot(data = linelist, mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "#2a78d6", color = "white")

# 👀 Follow along 4.3: fill = "blue" inside aes() — NOT blue!
ggplot(data = linelist, mapping = aes(x = age_years, fill = "blue")) +
  geom_histogram(binwidth = 5)

# ✏️ TRY IT 4: temperatures, then fill = fever
ggplot(data = measles, mapping = aes(x = temp_c)) +
  geom_histogram(binwidth = 0.2, color = "white")

ggplot(data = measles, mapping = aes(x = temp_c, fill = fever)) +
  geom_histogram(binwidth = 0.2, color = "white")
# The small hump near 37 °C is patients without fever; the big hump near 39 °C is fever.


# ── PART 5: Scatter plots ──────────────────────────────────────────────────

# 👀 Follow along 5.1: age vs weight
ggplot(data = linelist, mapping = aes(x = age_years, y = wt_kg)) +
  geom_point()

# 👀 Follow along 5.2: smaller, see-through points; then color = gender
ggplot(data = linelist, mapping = aes(x = age_years, y = wt_kg)) +
  geom_point(size = 1, alpha = 0.3)

ggplot(data = linelist, mapping = aes(x = age_years, y = wt_kg, color = gender)) +
  geom_point(size = 1, alpha = 0.4)

# 👀 Follow along 5.3: a trend line
ggplot(data = linelist, mapping = aes(x = age_years, y = wt_kg)) +
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#eb6834")

# 👀 Follow along 5.4: only patients with fever
linelist %>%
  filter(fever == "yes") %>%
  ggplot(mapping = aes(x = age_years, y = wt_kg)) +
  geom_point(size = 1, alpha = 0.3)

# ✏️ TRY IT 5: age vs days in hospital
measles %>%
  filter(!is.na(days_in_hospital)) %>%
  ggplot(mapping = aes(x = age_years, y = days_in_hospital)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#eb6834")
# The trend line is almost flat: no relationship. "No relationship" is a result too!


# ── PART 6: Box plots ──────────────────────────────────────────────────────

# 👀 Follow along 6.1: age by gender
ggplot(data = linelist, mapping = aes(x = gender, y = age_years)) +
  geom_boxplot()

# 👀 Follow along 6.2: no "Unknown", fill = gender, no legend
linelist %>%
  filter(gender != "Unknown") %>%
  ggplot(mapping = aes(x = gender, y = age_years, fill = gender)) +
  geom_boxplot() +
  theme(legend.position = "none")

# 👀 Follow along 6.3: add the mean
linelist %>%
  filter(gender != "Unknown") %>%
  ggplot(mapping = aes(x = gender, y = age_years, fill = gender)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  theme(legend.position = "none")

# ✏️ TRY IT 6: days in hospital by vaccination status
measles %>%
  filter(!is.na(days_in_hospital)) %>%
  ggplot(mapping = aes(x = vaccinated, y = days_in_hospital, fill = vaccinated)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  theme(legend.position = "none")


# ── PART 7: Labels and themes ──────────────────────────────────────────────

# 👀 Follow along 7.1: labs() on the box plot
linelist %>%
  filter(gender != "Unknown") %>%
  ggplot(mapping = aes(x = gender, y = age_years, fill = gender)) +
  geom_boxplot() +
  labs(
    title    = "Age distribution by gender",
    subtitle = "Cases with known gender",
    x        = "Gender",
    y        = "Age (years)",
    caption  = "Source: workshop outbreak linelist"
  ) +
  theme(legend.position = "none")

# 👀 Follow along 7.2: themes
ggplot(data = linelist, mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "#2a78d6", color = "white") +
  labs(title = "Age distribution of cases", x = "Age (years)", y = "Number of cases") +
  theme_minimal(base_size = 14)

# 👀 Follow along 7.3: a plot saved as an object
age_plot <- ggplot(data = linelist, mapping = aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "#2a78d6", color = "white")

age_plot +
  labs(title = "Age distribution of cases", x = "Age (years)", y = "Number of cases") +
  theme_minimal()

# ✏️ TRY IT 7: finish a plot and save it as my_plot
my_plot <- ggplot(data = measles, mapping = aes(x = temp_c, fill = fever)) +
  geom_histogram(binwidth = 0.2, color = "white") +
  labs(
    title    = "Temperature of measles cases",
    subtitle = "Patients with fever cluster around 39 °C",
    x        = "Temperature (°C)",
    y        = "Number of cases",
    fill     = "Fever",
    caption  = "Source: practice measles linelist"
  ) +
  theme_minimal()

my_plot


# ── ⭐ BONUS ───────────────────────────────────────────────────────────────

# 1. Ages by vaccination status: unvaccinated cases are the largest group at almost every age
ggplot(data = measles, mapping = aes(x = age_years, fill = vaccinated)) +
  geom_histogram(binwidth = 1, color = "white")

# 2. A violin plot shows the full shape of each distribution
measles %>%
  filter(!is.na(days_in_hospital)) %>%
  ggplot(mapping = aes(x = vaccinated, y = days_in_hospital, fill = vaccinated)) +
  geom_violin() +
  theme(legend.position = "none")
