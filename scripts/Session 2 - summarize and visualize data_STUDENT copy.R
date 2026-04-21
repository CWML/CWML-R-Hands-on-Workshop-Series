# Important Notes

## Comments - used to add notes or details to code using the pound/number/hash-tag symbol (#)
## Running code - highlight lines (chunk) then command/return or control/enter
## You need to update the parts of the codes provided that have ALL CAPS or question marks (?)
## To create a new pipe(%>%) use Shift+Command+M (Mac) or Shift+Control+M #________________________________________________________________________________

# ── STEP 1: R Project Folder ────────────────────────────────────────────
# 🎯 Goal: Set up an R Project folder as the foundation for this workshop.
#    An R Project keeps your files organized, makes your code reproducible,
#    and eliminates file path headaches — you'll use this every time you
#    start a new analysis.
# ────────────────────────────────────────────────────────────────────────


# ── STEP 2a:Load R environment ──────
# Load all the R environment packages.
#──────────────────────────────────────
install.packages("renv")
library(renv)

renv::restore()

# ── STEP 2b: R Packages ────────────────────────────────────────────────
# Load all the R packages you will need for this session.
#────────────────────────────────────────────────────────────────────────

library(rio)
library(here)
library(skimr)
library(tidyverse)
library(gtsummary)
library(rstatix)
library(janitor)
library(scales)
library(flextable)
library(officer)
library(psych)
library(stringr)
library(ggforce)
library(forcats)

# install.packages("pacman")  # run once if needed

#pacman::p_load(tidyverse, magrittr, here, readr, janitor, openxlsx, rio, skimr, gtsummary, rstatix, scales, flextable, officer, psych, stringr, ggforce, forcats)


# ── STEP 3: Import Data ───────────────────────────────────────────────────────────────────
# Import the combined_linelist.csv data file from the processed_data folder in your project
#───────────────────────────────────────────────────────────────────────────────────────────

# Use the import() and here() functions and call your new data frame "data"
# HINT 1: folder name is "processed_data" and file name is "combined_linelist.csv"
# HINT 2: use the structure written below

data <- import(here("processed_data", "combined_linelist.csv"), setclass = "tbl")

# ── STEP  4: Inspect the raw data file ───────
# Inspect data using str() and skim() functions
#──────────────────────────────────────────────

# Run each function one at time 

str(DATA_FRAME_NAME)

skim(DATA_FRAME_NAME)

## After inspecting, what issues did you find?

# ── STEP  5: Cleaning and updating  ────────────────────────────────────────────────────────────────────────
# Use dplyr functions to filter and mutate your data frame to create a new data frame called "summary_data"
#────────────────────────────────────────────────────────────────────────────────────────────────────────────

# Us the <- operator to create a new data frame called "summary_data"
# First: we want to FILTER bmi so we are left with those who have a BMI between 15 and 60
# Second: we want to use MUTATE to recode the values under the variable gender 


NEW_DF <- data #%>% 

  mutate(!(bmi <= PARAMETER1 | bmi > PARAMETER2)) #%>%  
  
  FUNCTION(VARIABLE = recode(gender,    
                         "m" = "_",
                         "f" = "_"
  )) #%>% 
  
  mutate(age_cat = case_when(
    age_years < 18 ~ "Under 18",
    age_years >= 18 & age_years < 30 ~ "18-29",
    age_years >= 30 & age_years < 40 ~ "_",
    age_years >= 40 & age_years < 50 ~ "_",
    age_years >= 50 & age_years < 60 ~ "_",
    age_years >= 60 & age_years < 70 ~ "_",
    age_years >= 70 & age_years < 80 ~ "_",
    TRUE ~ "80+"  # This catches all remaining cases
  ))

# Re-inspect your data

str(summary_data)

skim(summary_data)

# ── STEP  6: descriptive tables ────────────────────────────────────────────────
# Use the following functions to create descriptive tables for your data frame.
#────────────────────────────────────────────────────────────────────────────────

##########
# Base R #
##########

# Base R for numeric: summary()

## This code uses a mixture of base R and tidyverse logic
### HINT: we want to SELECT only NUMERIC variables for the SUMMARY() function

summary_data %>% 
  FUNCTION(age_years, wt_kg, ht_cm, bmi, temp) %>% 
  FUNCTION()

# Base R for non-numeric data: table()

## HINT: we want to use the TABLE function to search CHARACTER variables in our data frame
## NOTE: just choose one variable for this example

table(summary_data$VAR)

#────────────────────────────────────────────────────────────────────────────────

####################################################
# Using the psych::describe() package and function #
####################################################

## the double colon (::) is used to specify which package you want the function to come from
## using select_if tells the function to look at only numeric values in you data frame

psych::FUNCTION(summary_data %>% select_if(is.numeric))

#────────────────────────────────────────────────────────────────────────────────

###################################################
# Using the janitor::tabyl() package and function #
###################################################

## run these 2 codes separately
summary_data %>% 
  tabyl(age_cat)

summary_data %>%
  tabyl(age_cat, fever)

## Now using pipes (%>%) and the various adorn functions, add column totals; row total; and change row names to "Age Groups"

#────────────────────────────────────────────────────────────────────────────────

############################################################### 
# Using the rstatix::get_summary_stats() package and function #
###############################################################

## NOTE 1: type = "common" common stats; type = "full" more stats
## NOTE 2: used for numeric variables

summary_data %>%
  FUNCTION(age_years, wt_kg, ht_cm, bmi, temp, 
                    type = "common") 

#────────────────────────────────────────────────────────────────────────────────

#################################################
# Using the dplyr::count() package and function #
#################################################

## Start with this simple code
summary_data %>% 
  count(age_cat)

## To get a cross tabulation, add another non-numeric variable within count() - for example you can use "outcome"
## HINT1: Separate the variables in count() using a comma (,)
summary_data %>% 
  count(age_cat, ADD_VARIABLE_HERE)

# Use mutate() to create a new variable called percent and scales() from the Scales Package to calculate percentages
## Run this code to see what happens
summary_data %>% 
  count(age_cat) %>%          
  mutate(                               
    percent = scales::percent(n / sum(n), accuracy = 0.1)

## Now add a group_by() statement under the line "summary_data %>%" to group by "outcome"
summary_data %>% 
  FUNCTION_NAME(outcome) %>% 
  count(age_cat) %>%          
  mutate(                               
    percent = scales::percent(n / sum(n), 
                              accuracy = 0.1))

#────────────────────────────────────────────────────────────────────────────────

#####################################################
# using the dplyr::summarise() package and function #
#####################################################

## Run this code to see what happens
summary_data %>%
  summarise(
    mean_age = mean(age_years, na.rm = TRUE),
    sd_age = sd(age_years, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    sd_bmi = sd(bmi, na.rm = TRUE),
    n = n()
  )

## Adding in the group_by() here reduces your data to a single row per group
## HINT1: add group_by() just like before and use the "outcome" variable

summary_data %>%
  FUNCTION_NAME(outcome) %>% 
  summarise(
    mean_age = mean(age_years, na.rm = TRUE),
    sd_age = sd(age_years, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    sd_bmi = sd(bmi, na.rm = TRUE),
    n = n()
  )

## Using across() lets you apply the same operations to multiple columns at once 
summary_data %>%
  group_by(outcome) %>%
  summarise(
    across(c(age_years, temp, bmi),         # Which columns to use
           list(mean = ~mean(., na.rm = TRUE),         # First calculation for each column
                sd = ~sd(., na.rm = TRUE)))            # Second calculation for each column
  )


## You can even add calculated columns to your summary
## this example uses sum() then divides by total [n()] to calculate percentages
## NOTE: this code is good for categorical/character variables; it is counting how many "yes" responses there are
summary_data %>%
  group_by(outcome) %>%
  summarise(
    n_cases = n(),
    n_fever = sum(fever == "yes", na.rm = TRUE),
    n_cough = sum(cough == "yes", na.rm = TRUE),
    n_vomit = sum(vomit == "yes", na.rm = TRUE),
    fever_pct = round(sum(fever == "yes", na.rm = TRUE) / n() * 100, 1),
    cough_pct = round(sum(cough == "yes", na.rm = TRUE) / n() * 100, 1),
    vomit_pct = round(sum(vomit == "yes", na.rm = TRUE) / n() * 100, 1)
  )


#──── Step 7: Time to make plots! ────────────────────────────────────────────────────────────────────
# I will give you the basic code and you will build the plots up based on the details in the comments
# Just use copy/paste to add the details below each plot type and see how it changes the plot
# PLEASE READ COMMENTS CAREFULLY
#─────────────────────────────────────────────────────────────────────────────────────────────────────

#############
# Histogram #
#############

## Fill in all the appropriate fields then run (control + enter)
## Note1: The ggplot pipe (+) is added after each function to combine the codes
## Note2: Just like the DPLYR pipe (%>%) code and be ran in between each pipe
## We will use summary_data as our DF

## Start with making a simple histogram
## DF = summary_data and for 'the'x' we will use variable wt_kg (VAR)
ggplot(data = DF_NAME, mapping = aes(x = VAR)) + # set data and axes
  geom_histogram() # display histogram


## Now we will build on our previous code using pipes and adding details inside the geom_histogram() function to make the histogram look nicer
ggplot(data = summary_data, mapping = aes(x = wt_kg)) + # set data and axes
  geom_histogram(binwidth = 5,  # width of bins
                 color = "red", # bin line color
                 fill = "blue", # bin interior color
                 alpha = 0.1    # bin transparency
                 )


## Add some labels and some finishing touches with this code added to the above
## NOTE: A plus sign '+' is added after the geom_histogram() function to pipe the new code to the previous code

ggplot(data = summary_data, mapping = aes(x = wt_kg)) + # set data and axes
  geom_histogram(  # display histogram
    binwidth = 5,  # width of bins
    color = "red", # bin line color
    fill = "blue", # bin interior color
    alpha = 0.1    # bin transparency
  ) +
  labs(
    title = "Distribution of Patient Ages", 
    subtitle = "Histogram with 5-Year Bins"
  ) +
  theme_minimal()


#################
# Scatter plots #
#################

## Now we will create a simple scatter plot 
ggplot(data = DF_NAME, mapping = aes(x = age_years, y = wt_kg)) +
  geom_point()

## Now we will add dplyr's filter() function to select only those with fever ("yes")
## We will also add a comparison of age_years and wt_kg

ggplot(data = summary_data %>%  
         filter(fever == "yes"), 
       mapping = aes(x = age_years, y = wt_kg)) +
  geom_point() 


## Now let's add color details using the BMI of each data point using the following code
## Copy/paste this line in aes() after "y = wt_kg"; don't for get to use a comma after wt_kg

ggplot(data = summary_data %>% 
         filter(fever == "yes"), 
       mapping = aes(x = age_years, y = wt_kg, color = VAR_NAME))  + 
  geom_point() # display data as points

## Next, add some details to the points using size and alpha (transparency) within the geom_point() function
## Once you do this and run it, you can change the numbers and re-run it and see what happens

ggplot(data = summary_data %>% 
         filter(fever == "yes"), 
       mapping = aes(x = age_years, y = wt_kg, color = bmi)) +
  geom_point( 
    size = 1,
    alpha = 0.5)

## Finally, add geom_smooth() to add a line of best fit to the scatter plot

ggplot(data = summary_data %>% 
         filter(fever == "yes"), 
       mapping = aes(x = age_years, y = wt_kg, color = bmi)) +
  geom_point( 
    size = 1,
    alpha = 0.5) +
  geom_smooth( method = "lm", size = 2, se = FALSE)

############
# Box plot #
############

## Run this code to make a simple boxplot
ggplot(data = summary_data, mapping = aes(y = age_years, x = gender, fill = gender)) +
  geom_boxplot() + 
  theme_minimal() 

## what are some weird things you notice?
## Let's address this by:

## Add filter() to remove "Unknown" values for gender
## Add theme(leagend.position = "none") to remove the legend since we don't need it for this plot
ggplot(data = summary_data %>% 
         filter(gender != "Unknown"),
       mapping = aes(y = age_years, x = gender, fill = gender)) +
  geom_boxplot()+
  theme(legend.position = "none")

## Now add labs() to add a title and axis labels; you can make up your own title and labels

ggplot(data = summary_data %>% 
         filter(gender != "Unknown"),
       mapping = aes(y = age_years, x = gender, fill = gender)) +
  geom_boxplot() + 
  labs(
    title = "Age Distribution by Gender",
    y = "Age (Years)",
    x = "Gender"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## Here, we will add stat_summary() to calcuate the mean and add it to the boxplot as a point
## you can change the shape, size, and color of the point as well; run the code to see how it changes the plot

ggplot(data = summary_data %>% filter(gender != "Unknown"),
       mapping = aes(y = age_years, x = gender, fill = gender)) +
  geom_boxplot() + 
  labs(
    title = "Age Distribution by Gender",
    y = "Age (Years)",
    x = "Gender"
  ) + 
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23, # Diamond shape
    size = 3,
    fill = "black"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## Now I will show you a more complicated code for a detailed boxplot
## Let's see what it gives us

box_stats <- summary_data %>% # First create the summary statistics
  group_by(gender) %>%
  summarise(
    Median = median(age_years, na.rm = TRUE),
    Q1 = quantile(age_years, 0.25, na.rm = TRUE),
    Q3 = quantile(age_years, 0.75, na.rm = TRUE)
  )

box_text <- box_stats %>% # Then create the text labels
  mutate(
    Label_Text = paste0(
      "Median: ", round(Median, 1), "\n",
      "Q1: ", round(Q1, 1), "\n",
      "Q3: ", round(Q3, 1)
    ),
    # Set y to a consistent middle value (e.g., the median) for placement
    y_pos = Median 
  )

ggplot(data = summary_data %>% filter(gender != "Unknown"), # Create the plot
       mapping = aes(y = age_years, x = gender, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  
  geom_text(   # Use geom_text to place the entire block of text
    data = box_text, 
    aes(y = y_pos, label = Label_Text), 
    x = as.numeric(factor(box_text$gender)) + 0.1, # Move label to the right of the box
    hjust = 0, # Left-align the text block
    vjust = -1.5,
    size = 3.5, 
    color = "darkslategray"
  ) +
  theme_minimal() +
  labs(title = "Age Distribution with Summary Text Block", y = "Age (Years)", x = "Gender") +
  theme(legend.position = "none")



##############
# Bar chart #
#############

## Run this code for a simple bar chart
## NOTE1: after you run it, change "position = " from "fill" to "stack" then rerun it
## NOTE2: change "x = " to "y = " and rerun it
## NOTE3: change everything back to what they were initially 

ggplot(data = summary_data, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.1)

## You can manually change the bar colors using, scale_fill_manual()
ggplot(data = summary_data, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.1) +
  scale_fill_manual(values = c("Death" = "CHOOSE_A_COLOR", 
                             "Recover" = "CHOOSE_A_COLOR", 
                             "Unknown" = "CHOOSE_A_COLOR"))

## You can add titles and labels using lab() just like the previous plot examples
## Create you own labels and title and add to the above bar chart code

ggplot(data = summary_data, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.1) +
  scale_fill_manual(values = c("Death" = "darkred", 
                               "Recover" = "red", 
                               "Unknown" = "blue")) +
  labs(
    title = "Outcome Proportion Across Age Categories",
    subtitle = "The height of each color shows the percentage of that outcome within the age group.",
    x = "Age Category",
    y = "Proportion of Cases (100%)",
    fill = "Outcome"
  )

## Add theme(axis.text.x = element_text(angle = 45, hjust = 1)) to rotate the x-axis labels so they don't overlap

ggplot(data = summary_data, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.1) +
  scale_fill_manual(values = c("Death" = "darkred", 
                               "Recover" = "red", 
                               "Unknown" = "blue")) +
  labs(
    title = "Outcome Proportion Across Age Categories",
    subtitle = "The height of each color shows the percentage of that outcome within the age group.",
    x = "Age Category",
    y = "Proportion of Cases (100%)",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################
# Facet examples #
##################

# Use facet_wrap to make a bar chart that shows the proportions based on specific variables

## this is an example of a bar chart with a facet_wrap uing variable gender
ggplot(summary_data %>% 
         # Filter out "Other/Unknown" from the hospital_name column
         filter(hospital_name != "Other/Unknown") %>%
         # Filter out "Unknown" from the outcome column
         filter(outcome != "Unknown") %>%
         # --- ADDED FILTER STEP FOR GENDER ---
         filter(gender != "Unknown") %>%
         drop_na(hospital_name, outcome, gender)) +
  
  # 1. Bar Geometry: Use position="fill" for 100% proportional stacking
  geom_bar(aes(y = fct_rev(forcats::fct_infreq(hospital_name)), 
               fill = outcome), 
           width = 0.7, 
           color = "black", 
           linewidth = 0.1,
           position = "fill") + 
  
  # 2. Add Faceting by Gender (now only showing known genders)
  facet_wrap(~ gender) +
  
  # 3. Only define colors for the two remaining outcomes
  scale_fill_manual(values = c("Death" = "firebrick", 
                               "Recover" = "seagreen")) +
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  
  labs(title = "Proportion of Case Outcomes by Hospital, Faceted by Gender (Known Cases)",
       subtitle = "Unknown outcomes, 'Other/Unknown' hospitals, and Unknown genders are excluded.",
       y = "Hospital Name",
       x = "Proportion of Cases (100%)",
       fill = "Outcome")

## Facet for box plot by variable outcome

# 1. UPDATE SUMMARY DATA: Group by BOTH gender AND outcome
box_stats_faceted <- summary_data %>%
  # Filter data first for cleaner summaries and faster calculation
  filter(gender != "Unknown" & outcome != "Unknown") %>%
  group_by(gender, outcome) %>% # <-- NEW: Group by both variables
  summarise(
    Median = median(age_years, na.rm = TRUE),
    Q1 = quantile(age_years, 0.25, na.rm = TRUE),
    Q3 = quantile(age_years, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. UPDATE TEXT LABELS: Create text block for each gender/outcome combination
box_text_faceted <- box_stats_faceted %>%
  mutate(
    Label_Text = paste0(
      "Median: ", round(Median, 1), "\n",
      "Q1: ", round(Q1, 1), "\n",
      "Q3: ", round(Q3, 1)
    ),
    y_pos = Median
  )

# 3. CREATE THE FACETED PLOT
ggplot(data = summary_data %>% filter(gender != "Unknown" & outcome != "Unknown"), # Filter data for plot
       mapping = aes(y = age_years, x = gender, fill = gender)) +
  
  geom_boxplot(alpha = 0.7) +
  
  # Use geom_text to place the entire block of text
  # We must use 'group' or 'x' mapping to position the labels correctly within each facet.
  geom_text(
    data = box_text_faceted,
    aes(y = y_pos, label = Label_Text, group = gender), # Add 'group' to ensure correct mapping
    x = as.numeric(factor(box_text_faceted$gender)) + 0.1,
    hjust = 0,
    vjust = -2,
    size = 3,
    color = "darkslategray"
  ) +
  
  # <-- NEW: Add Facet Layer -->
  facet_wrap(~ outcome) +
  
  theme_minimal() +
  labs(title = "Age Distribution by Gender, Faceted by Outcome", 
       subtitle = "Displaying Median, Q1, and Q3 values (Excluding Unknowns)",
       y = "Age (Years)", 
       x = "Gender") +
  theme(legend.position = "none")

## Facet for histogram by hospital name

# You may want to filter out the 'Other/Unknown' hospital for cleaner facets
ggplot(data = summary_data %>% filter(hospital_name != "Other/Unknown"), 
       mapping = aes(x = wt_kg)) +
  
  # Add Facet Layer
  facet_wrap(~ hospital_name) +
  
  geom_histogram(
    binwidth = 5,
    color = "red",
    fill = "blue",
    alpha = 0.5 # Increased alpha for better visibility in smaller facets
  ) +
  
  labs(
    title = "Distribution of Patient Weight (wt_kg) by Hospital", # Updated Title
    subtitle = "Histogram with 5 kg Bins, Faceted by Hospital Name", # Updated Subtitle
    x = "Weight (kg)",
    y = "Count"
  ) +
  
  theme_minimal()












