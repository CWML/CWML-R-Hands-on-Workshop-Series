# =============================================================================
# make_epi_data.R
# Builds the data for the proposed Epi track (9E-1 and 9E-2, see proposal/README.md):
# REAL Connecticut town boundaries and populations, plus a FICTIONAL measles
# outbreak. Students never run this script: they get the files it writes to raw_data/.
#
# Files written:
#   raw_data/ct_towns.rds              sf object: Connecticut's 169 towns (US Census, real)
#   raw_data/ct_town_population.csv    town populations, 2024 estimates (US Census, real)
#   raw_data/ct_measles_outbreak.csv   FICTIONAL measles linelist, one row per case
#
# Sources (public domain, no API key needed):
#   - Boundaries: US Census cartographic boundary files via tigris::county_subdivisions()
#   - Population: US Census Bureau, Vintage 2024 subcounty population estimates
#     https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/cities/totals/sub-est2024_9.csv
#
# The outbreak is invented. It starts in New Haven and spreads person to person
# (a propagated epidemic curve in waves about 12 days apart), mostly within towns
# and to neighboring towns. The linelist is a snapshot "as of 18 April 2025",
# taken while the outbreak is still going. Built-in signals:
#   - Most cases are unvaccinated children
#   - Unvaccinated cases and children under 5 are hospitalized more often
#     (for the 2 x 2 table and odds ratios)
#   - Cases with recent onset more often have an "Unknown" outcome (follow-up
#     not finished yet), which changes the case fatality ratio
#   - The last week or two of the epidemic curve are incomplete (reporting delay)
#   - Each case has GPS coordinates inside its town (for point maps)
# =============================================================================

library(tidyverse)
library(here)
library(sf)

set.seed(2025)

# ── 1. Town boundaries (real) ────────────────────────────────────────────────
towns <- tigris::county_subdivisions(state = "CT", cb = TRUE, year = 2023,
                                     progress_bar = FALSE) %>%
  st_transform(4326) %>%
  transmute(
    town      = NAME,
    town_fips = COUSUBFP,
    region    = str_remove(NAMELSADCO, " Planning Region")
  ) %>%
  arrange(town)

stopifnot(nrow(towns) == 169)

# ── 2. Town populations (real) ───────────────────────────────────────────────
pop_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/cities/totals/sub-est2024_9.csv"
population <- read_csv(pop_url, show_col_types = FALSE) %>%
  filter(as.numeric(SUMLEV) == 61) %>%                       # 61 = county subdivision (town)
  transmute(
    town_fips  = sprintf("%05d", as.integer(COUSUB)),
    town       = str_remove(NAME, " town$"),
    population = POPESTIMATE2024
  ) %>%
  arrange(town)

stopifnot(nrow(population) == 169, setequal(population$town_fips, towns$town_fips))

# ── 3. A fictional measles outbreak (propagated, person to person) ───────────
neighbors <- st_touches(towns)                               # which towns share a border
town_pop  <- population$population[match(towns$town_fips, population$town_fips)]

pick_town <- function(from) {
  u <- runif(1)
  if (u < 0.72) return(from)                                 # same town
  nb <- neighbors[[from]]
  if (u < 0.95 && length(nb) > 0) return(nb[sample.int(length(nb), 1)])  # a neighbor
  sample.int(nrow(towns), 1, prob = town_pop)                # anywhere (travel)
}

start  <- as.Date("2025-01-06")
index  <- which(towns$town == "New Haven")
cases  <- tibble(id = 1L, parent = NA_integer_, onset = start, town_i = index)
queue  <- 1L
max_cases <- 420

while (length(queue) > 0 && nrow(cases) < max_cases) {
  i  <- queue[1]; queue <- queue[-1]
  t  <- as.numeric(cases$onset[i] - start)
  # reproduction number falls as vaccination clinics and isolation start (~day 45)
  R  <- 2.6 / (1 + exp((t - 45) / 9)) + 0.35
  n_new <- rpois(1, R)
  for (k in seq_len(n_new)) {
    if (nrow(cases) >= max_cases) break
    new_id <- nrow(cases) + 1L
    cases <- add_row(cases,
      id     = new_id,
      parent = cases$id[i],
      onset  = cases$onset[i] + max(7, round(rnorm(1, 12, 2.5))),  # serial interval
      town_i = pick_town(cases$town_i[i])
    )
    queue <- c(queue, new_id)
  }
}

# The linelist is a snapshot "as of" this date, taken while the outbreak is still going:
# only cases REPORTED by then are included, and recent cases often have no outcome yet.
end_of_data <- as.Date("2025-04-18")
n <- nrow(cases)

linelist <- cases %>%
  arrange(onset, id) %>%
  mutate(
    case_id     = sprintf("CT-%04d", row_number()),
    onset_date  = onset,
    report_date = onset_date + 3 + rpois(n, 2),              # rash appears ~3 days after onset, then reporting delay
    town        = towns$town[town_i],
    age_years   = pmin(round(rgamma(n, shape = 1.4, rate = 0.16)), 70),
    sex         = sample(c("female", "male"), n, replace = TRUE),
    vaccinated  = sample(c("No", "Yes", "Unknown"), n, replace = TRUE, prob = c(0.78, 0.12, 0.10)),
    exposure_setting = sample(
      c("Household", "School or daycare", "Healthcare facility", "Community", "Unknown"),
      n, replace = TRUE, prob = c(0.38, 0.27, 0.06, 0.14, 0.15)
    ),
    # hospitalization: more likely if unvaccinated or under 5
    p_hosp       = plogis(-1.9 + 1.0 * (vaccinated == "No") + 0.9 * (age_years < 5)),
    hospitalized = if_else(runif(n) < p_hosp, "Yes", "No"),
    # follow-up: the more recent the case, the more likely its outcome is not recorded yet
    days_before_end = as.numeric(end_of_data - onset_date),
    p_unknown   = 0.03 + 0.6 * exp(-days_before_end / 18),
    outcome     = case_when(
      runif(n) < p_unknown ~ "Unknown",
      hospitalized == "Yes" & runif(n) < 0.03 ~ "Died",
      TRUE ~ "Recovered"
    )
  ) %>%
  filter(report_date <= end_of_data) %>%                     # not yet reported = not in the linelist
  mutate(case_id = sprintf("CT-%04d", row_number()))
n <- nrow(linelist)

# GPS coordinates: one random point inside each case's town
pts <- map(seq_len(n), function(j) {
  st_coordinates(st_sample(towns$geometry[linelist$town_i[j]], 1, exact = TRUE))
})
coords <- do.call(rbind, pts)

linelist <- linelist %>%
  mutate(
    longitude = round(coords[, "X"], 5),
    latitude  = round(coords[, "Y"], 5)
  ) %>%
  select(case_id, onset_date, report_date, town, age_years, sex, vaccinated,
         exposure_setting, hospitalized, outcome, latitude, longitude)

# ── 4. Write ─────────────────────────────────────────────────────────────────
saveRDS(towns, here("raw_data", "ct_towns.rds"))
write_csv(population, here("raw_data", "ct_town_population.csv"))
write_csv(linelist, here("raw_data", "ct_measles_outbreak.csv"))

cat("Towns:", nrow(towns), "\n")
cat("Cases:", nrow(linelist), " from", format(min(linelist$onset_date)),
    "to", format(max(linelist$onset_date)), "\n")
cat("Towns with cases:", n_distinct(linelist$town), "\n")
print(count(linelist, outcome))
print(count(linelist, vaccinated, hospitalized))
