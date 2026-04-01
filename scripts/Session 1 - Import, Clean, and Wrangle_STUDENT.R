# Important Notes

## Comments - used to add notes or details to code using the pound symbol (#)
## Running code - highlight lines then command/return or control/enter
## Using pipes (%>%) connects different R functions together; can run code between pipes as well
________________________________________________________________________________

# ── STEP 1: R Project Folder ────────────────────────────────────────────
# 🎯 Goal: Set up an R Project folder as the foundation for this workshop.
#    An R Project keeps your files organized, makes your code reproducible,
#    and eliminates file path headaches — you'll use this every time you
#    start a new analysis.
# ────────────────────────────────────────────────────────────────────────

install.packages("renv")
library(renv)

# ── STEP 2a:Load R environment ──────────────────────────────────────────
# 🎯 Goal: Load all the R environment packages.


renv::restore()


# ── STEP 2b: R Packages ──────────────────────────────────────────────────
# 🎯 Goal: Load all the R packages you will need for this session.

library(tidyverse)
library(magrittr)
library(here)
library(readr)
library(janitor)
library(openxlsx)
library(rio)
library(skimr)

# ── STEP 3: Importing data ─────────────────────────────────────────────
# 🎯 Goal: Read the raw dataset (linelist_raw.xlsx) into R and assign it
#    to a data frame. You will use import() from the rio package paired
#    with here() so that your file path works on any machine.
# ────────────────────────────────────────────────────────────────────────

# Use the import() and here() functions and call your new data frame "linelist_raw"

linelist_raw <- import(here("raw_data", "_.xlsx"))

# ── STEP 4: Inspect data ───────────────────────────────────────────────
# 🎯 Goal: Get familiar with the dataset before touching it.
#    You will use several functions to explore its size, structure, and
#    variable types, then check character variables for misspellings,
#    inconsistent values, and other issues that will need to be fixed.
# ────────────────────────────────────────────────────────────────────────

# Run each function one at time 

view(_)

head(_)

glimpse(_)

skim(_)

# It may be a good idea to inspect variables with charter stings as values for misspellings or other possible errors

## Which character values should we check?

linelist_raw %>% count(_)
linelist_raw %>% count(_)
linelist_raw %>% count(_)

## After inspecting, what issues did you find?

# ── STEP 5: Clean data ─────────────────────────────────────────────────
# 🎯 Goal: Fix all the problems you identified in Step 4.
#    This includes standardizing column names, converting date columns to
#    the correct type, renaming variables, correcting hospital name values,
#    fixing the age variable type, and adding a new BMI column — all in
#    a single pipeline.
# ────────────────────────────────────────────────────────────────────────

# Read in hospital name look-up table (hosp_info.txt) using import() and here() and assign it to a new data frame called "hosp_info"

hosp_info <- import(here('raw_data', 'hosp_info.txt'))

# VERY IMPORTANT: un-comment the pipe (%>%) after each line is ran successfully 
# and before new line is added and ran

## Start by creating a new data frame called "linelist_clean" using the <- operator
linelist_clean <- linelist_raw #%>% 
  
## 1. clean column names using clean_names()
ADD_CODE_HERE #%>% 
  
## 2. clean date values - Update variable names!
mutate(
  infection_date = as.Date(_),
  hosp_date = as.Date(_),
  VARIABLE_NAME = as.Date(_),
  date_of_outcome = as.Date(_)
) #%>% 
  
## 3. update date variable names
rename(
  NEW_NAME = hosp_date,
  NEW_NAME = date_onset,
  NEW_NAME = infection_date
) #%>% 
  
## 4. Clean values (hospital names) under hospital variable
### import the hospital_info file which is a look-up table that tells us what the names should be try it in the console
### REMINDER: folder name is 'raw_data' and file name is 'hosp_info.txt'

mutate(
  hospital_name = case_when( #creating new variable called "hospital_name"
    str_detect(hospital, "Central") ~ "NAME_FROM_LOOKUP_TABLE",
    str_detect(hospital, "Military|Mitylira") ~ "NAME_FROM_LOOKUP_TABLE", 
    str_detect(hospital, "Regional") ~ "NAME_FROM_LOOKUP_TABLE",
    str_detect(hospital, "Mark") ~ "NAME_FROM_LOOKUP_TABLE",
    str_detect(hospital, "HMO") ~ "NAME_FROM_LOOKUP_TABLE",
    str_detect(hospital, "Premier") ~ "NAME_FROM_LOOKUP_TABLE",
    is.na(hospital) | hospital == "" ~ "Other/Unknown", # if value is missing or is "NA" then "Other/Unknown"
    TRUE ~ "Other/Unknown" # All other remaining values are "Other/Unknown"
  )
) #%>% 
  
### Verify your changes worked by counting the new hospital_name variable like before; try this in the console: linelist_clean %>% count(hospital_name)

## 5. Fix age_years variable type to numeric value using mutate
mutate(NEW_VARIABLE_NAME = as.numeric(OLD_VARIABLE_NAME)) #%>% 

## 6. Perform a quick calculation using wt_kg and ht_cm to calculate BMI
mutate(NEW_VARIABLE_NAME = round(wt_kg / (ht_cm/100)^2, 2))


# ── STEP 6: Missing data 1 ──────────────────────────────────────────────
# 🎯 Goal: Rescue rows that are missing a case_id instead of just
#    dropping them. You will isolate those rows into a separate data frame
#    and assign them new unique IDs so they are not lost from the analysis.
# ────────────────────────────────────────────────────────────────────────

# 1. Create a new data frame (<-) for the rows with missing case_id

missing_ids <- linelist_clean %>%
  filter(is.na(case_id)) %>%
  mutate(
    case_id = paste0("ID", sprintf("%05d", row_number()))
  )

# ── STEP 7: Missing data 2 ──────────────────────────────────────────────
# 🎯 Goal: Finalize your cleaned dataset.
#    You will filter out the rows with missing case_id (now saved
#    separately), replace any remaining missing character values with
#    "Unknown", and select and reorder the final set of variables you
#    want to keep.
# ────────────────────────────────────────────────────────────────────────

# 1. Create a new data frame (<-) the filters out the rows with missing case_id called "linelist_final"
missing_ids <- linelist_clean %>%
  filter(!is.na(VARIABLE_NAME))  %>%
  mutate(across(where(is.character), ~replace_na(.x, "Unknown"))) #%>% 
  
# 2. Choose which variables to retain and select their order in the data frame using select()
## Order of variables: case_id, gender, age_years, wt_kg, ht_cm, bmi, date_of_infection, onset_date, hospital_date, hospital_name, date_of_outcome, outcome 

FUNCTION_NAME(case_id, gender, age_years, wt_kg, ht_cm, bmi, date_of_infection, onset_date, hospital_date, hospital_name, date_of_outcome, outcome)

# ── STEP 8: Join data ───────────────────────────────────────────────────
# 🎯 Goal: Enrich your cleaned dataset by bringing in additional case
#    information from a second file (case_info.csv). You will join the
#    two data frames on a shared key (case_id) using left_join() and
#    then remove any duplicate rows that result.
# ────────────────────────────────────────────────────────────────────────

# 1. Import case_info.csv; don't forget to create a new object (<-)
case_info <- FUNCTION_NAME(FUNCTION_NAME('raw_data', 'case_info.csv'))

# 2. Join linelist_final and case_info using left_join() on the case_id variable; don't forget to create a new object (<-)
combined_linelist <- PREVIOUS_DF %>%
  left_join(case_info, by = "_") %>% #COMMON VARIABLE IN BOTH DFs
  distinct() # gets rid of duplicates

# ── STEP 9: Export data ──────────────────────────────────────────────────
# 🎯 Goal: Save your work! You will export your final cleaned datasets
#    (combined_linelist and missing_ids) as .csv files into the
#    processed_data folder so they are ready for future analysis or sharing.
# ──────────────────────────────────────────────────────────────────────────

# We will export: combined_linelist and missing_ids, 
# Save these files in the processed_data folder
# save them as .csv files

export(linelist_final, here('processed_data', '_.csv'))

export(combined_linelist, here('processed_data', '_.csv'))

export(missing_ids, here('processed_data', '_.csv'))




