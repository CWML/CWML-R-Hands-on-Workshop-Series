# Important Notes

## Comments - used to add notes or details to code using the pound symbol (#)
## Running code - highlight lines then command/return
## Using pipes (%>%) connects different R functions together; can run code between pipes as well
________________________________________________________________________________

###############################
# Step 1: create an R Project #
###############################

######################################
# Step 2: Load package using Pacmn() #
######################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  magrittr,     
  here,
  readr,
  janitor,
  openxlsx,
  rio,
  skimr
)

###########################################################################################
# Step 3: Import the linelist_raw.xlsx data file from the raw_data folder in your project #
###########################################################################################

# Use the import() and here() functions and call your new data frame "linelist_raw"

my_data <- import(here("FOLDER NAME", "DATA_FILE.xlsx"))

#####################################
# Step 4: Inspect the raw data file #
#####################################

# Run each function one at time 

view(DATA_FRAME_NAME)

head(DATA_FRAME_NAME)

glimpse(DATA_FRAME_NAME)

skim(DATA_FRAME_NAME)

# It may be a good idea to inspect variables with charter stings as values for misspellings or other possible errors
## Which character values should we check?

linelist_raw %>% count(VARIABLE_NAME)
linelist_raw %>% count(VARIABLE_NAME)
linelist_raw %>% count(VARIABLE_NAME)

## After inspecting, what issues did you find?

######################################################################
# Step 5: Clean our data - add your code in front of each pipe (%>%) #
######################################################################

# VERY IMPORTANT: un-comment the pipe (%>%) after each line is ran successfully and befor new line is added and ran

## Start by creating a new data frame called "linelist_clean" using the <- operator
ADD CODE HERE #%>% 
  
## 1. clean column names using clean_names()
ADD CODE HERE #%>% 
  
## 2. clean date values - Update varaible names!
mutate(
  VARIABLE_NAME = as.Date(VARIABLE_NAME),
  VARIABLE_NAME = as.Date(VARIABLE_NAME),
  VARIABLE_NAME = as.Date(VARIABLE_NAME),
  VARIABLE_NAME = as.Date(VARIABLE_NAME)
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
    str_detect(hospital, "Central") ~ "NAME FROM LOOKUP TABLE",
    str_detect(hospital, "Military|Mitylira") ~ "NAME FROM LOOKUP TABLE", 
    str_detect(hospital, "Regional") ~ "NAME FROM LOOKUP TABLE",
    str_detect(hospital, "Mark") ~ "NAME FROM LOOKUP TABLE",
    str_detect(hospital, "HMO") ~ "NAME FROM LOOKUP TABLE",
    str_detect(hospital, "Premier") ~ "NAME FROM LOOKUP TABLE",
    is.na(hospital) | hospital == "" ~ "Other/Unknown", # if value is missing or is "NA" then "Other/Unknown"
    TRUE ~ "Other/Unknown" # All other remaining values are "Other/Unknown"
  )
) #%>% 
  
### Verify your changes worked by counting the new hospital_name variable like before; try this in the console: linelist_clean %>% count(hospital_name)

## 5. Fix age_years variable type to numeric value using mutate
ADD CODE HERE #%>% 

## 6. Perform a quick calculation using wt_kg and ht_cm to calculate BMI
mutate(NEW_VARIABLE_NAME = round(wt_kg / (ht_cm/100)^2, 2))

#################################
# Step 6. Handle missing values #
#################################

# 1. Create a new data frame (<-) for the rows with missing case_id

missing_ids <- linelist_clean %>%
  filter(is.na(case_id)) %>%
  mutate(
    case_id = paste0("ID", sprintf("%05d", row_number()))
  )

#################################
# Step 7: Handle missing values #
#################################

# 1. Create a new data frame (<-) the filters out the rows with missing case_id called "linelist_final"
NEW_DF <- PREVIOUS_DF %>%
  filter(!is.na(VARIABLE_NAME))  %>%
  mutate(across(where(is.character), ~replace_na(.x, "Unknown"))) #%>% 
  
# 2. Choose which variables to retain and select their order in the data frame using select()
## Order of variables: case_id, gender, age_years, bmi, date_of_infection, onset_date, hospital_date, hospital_name, outcome 
ADD CODE HERE 

######################################
# Step 8: Join two cleaned data sets #
######################################

# 1. Import case_info.csv; don't forget to create a new object (<-)
ADD CODE HERE

# 2. Join linelist_final and case_info using left_join() on the case_id variable; don't forget to create a new object (<-)
combined_linelist <- PREVIOUS_DF %>%
  left_join(case_info, by = "COMMON VARIABLE IN BOTH DFs") %>% 
  distinct() # gets rid of duplicates

###############################################################################################
# Step 9: Export the cleaned datasets and save to our project under the processed_data folder #
###############################################################################################

# We will export: combined_linelist and missing_ids, 
# Save these files in the processed_data folder
# save them as .csv files

export(DF_NAME, here('FOLDER', 'combined_linelist.csv'))

export(DF_NAME, here('FOLDER', 'missing_ids.csv'))




