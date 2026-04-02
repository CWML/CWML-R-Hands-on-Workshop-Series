# Important Notes

## Comments - used to add notes or details to code using the pound/number/hash-tag symbol (#)
## Running code - highlight lines (chunk) then command/return or control/enter
## You need to update the parts of the codes provided that have ALL CAPS or question marks (?)
## To create a new pipe(%>%) use Shift+Command+M (Mac) or Shift+Control+M (PC)
________________________________________________________________________________

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

# ── STEP 3: Import Data ───────────────────────────────────────────────────────────────────
# Import the combined_linelist.csv data file from the processed_data folder in your project
#───────────────────────────────────────────────────────────────────────────────────────────

# Use the import() and here() functions and call your new data frame "data"
# HINT 1: folder name is "processed_data" and file name is "combined_linelist.csv"
# HINT 2: use the structure written below

data <- import(here("processed_data", "_.csv"), setclass = "tbl")

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

skim(DATA_FRAME_NAME)

# ── STEP  6: descriptive tables ────────────────────────────────────────────────
# Use the following functions to create descriptive tables for your data frame.
#────────────────────────────────────────────────────────────────────────────────

##########
# Base R #
##########

# Base R for numeric: summary()

## This code uses a mixture of base R and tidyverse logic
### HINT: we want to SELECT only NUMERIC variables for the SUMMARY() function

DF_NAME %>% 
  FUNCTION(age_years, wt_kg, ht_cm, bmi, temp) %>% 
  FUNCTION()

# Base R for non-numeric data: table()

## HINT: we want to use the TABLE function to search CHARACTER variables in our data frame
## NOTE: just choose one variable for this example

table(summary_data$VAR)

________________________________________________________________________________________________________________________________________

####################################################
# Using the psych::describe() package and function #
####################################################

## the double colon (::) is used to specify which package you want the function to come from
## using select_if tells the function to look at only numeric values in you data frame

psych::FUNCTION(summary_data %>% select_if(is.numeric))

________________________________________________________________________________________________________________________________________

###################################################
# Using the janitor::tabyl() package and function #
###################################################

## run these 2 codes separately
summary_data %>% 
  tabyl(age_cat)

summary_data %>%
  tabyl(age_cat, fever)

## Now using pipes (%>%) and the various adorn functions, add column totals; row total; and change row names to "Age Groups"

________________________________________________________________________________________________________________________________________

############################################################### 
# Using the rstatix::get_summary_stats() package and function #
###############################################################

## NOTE 1: type = "common" common stats; type = "full" more stats
## NOTE 2: used for numeric variables

summary_data %>%
  FUNCTION(age_years, wt_kg, ht_cm, bmi, temp, 
                    type = "common") 

________________________________________________________________________________________________________________________________________

#################################################
# Using the dplyr::count() package and function #
#################################################

## Start with this simple code
summary_data %>% 
  count(age_cat)

## To get a cross tabulation, add another non-numeric variable within count() - for example you can use "outcome"
## HINT1: Separate the variables in count() using a comma (,)
summary_data %>% 
  count(age_cat, ADD_MISSING)

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

________________________________________________________________________________________________________________________________________

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

## Fill in all the appropriate fields then run!
## REMEBER: use the ggplot pipe (+) after each function combine the codes
## Start with making a simple histogram for the variable wt_kg (VAR)
## We will use summary_data as our DF

ggplot(data = DF, mapping = aes(x = VAR)) + # set data and axes
  geom_histogram() # display histogram

## Let's add some details, copy/paste these lines in between the parentheses for geom_histogram() in the above code
binwidth = 5,  # width of bins
color = "red", # bin line color
fill = "blue", # bin interior color
alpha = 0.1    # bin transparency

## Add some labels and some finishing touches with this code added to the above
## HINT: add a plus sign '+' after the geom_histogram() function and then copy/paste this code below it

labs(
  title = "WRITE A TITLE HERE", 
  subtitle = "WRITE A SUBTITLE HERE"
) +
  theme_minimal()


#################
# Scatter plots #
#################

## Now we will create a simple scatter plot along with dplyr's filter() to select only those with fever ("yes")
## We will also add a comparison of age_years and wt_kg

## Run this first code and see what happens
ggplot(data = DF %>%  filter(fever == "yes"), 
       mapping = aes(x = age_years, y = wt_kg)) +
  geom_point() 


## Now let's add color details using the BMI of each data point using the following code
## Copy/paste this line in aes() after "y = wt_kg"; don't for get to use a comma after wt_kg

color = bmi

## Next, add these details inside geom_point()
## Once you do this and run it, you can change the numbers and re-run it and see what happens

    size = 1,
    alpha = 0.5

## Finally, add this line to the end of the code and we'll see what happens
## REMINDER: add a plus sign '+' on the line above this new line

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
  
### first, adding this line to the above code after, data = summary_data  
### HINT: paste in between summary_data an the comma (,)
  %>% filter(gender != "Unknown")
  
### then, add this line to the above code after theme_minimal() to remove the legend 
### HINT: add a plus sign '+' after theme_minimal() and then copy/paste this line below it
theme(legend.position = "none")


## Now add these labels to the boxplot
## HINT: this goes under geom_boxplot()
## REMEMBER: copy/paste these lines in the code above and use the plus sign
labs(
  title = "Age Distribution by Gender",
  y = "Age (Years)",
  x = "Gender"
)

## Now copy these new lines and paste them under the labels you just created
## NOTE: try changing the numbers under "shape" and "size" and color under "fill" to see how it affects the plot
stat_summary(
  fun = mean,
  geom = "point",
  shape = PICK A NUMBER BETWEEN 0-25,
  size = 3,
  fill = "black"
)

## Now I will show you a demonstration of a more complicated code for a detailed boxplot

##############
# Bar chart #
#############

## Run this code for a simple bar chart
## NOTE1: after you run it, change "position = " from "fill" to "stack" then rerun it
## NOTE2: change "x = " to "y = " and rerun it
## NOTE3: change everything back to what they were initially 

ggplot(data = summary_data, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.1) #+

## You can manually change the bar colors using, scale_fill_manual()
## choose some colors then un-comment the plus sign ABOVE and run the whole code up to ggplot()
scale_fill_manual(values = c("Death" = "CHOOSE A COLOR", 
                             "Recover" = "CHOOSE A COLOR", 
                             "Unknown" = "CHOOSE A COLOR")) #+

## You can add titles and labels using lab() just like the previous plot examples
## Create you own labels and title and add to the above bar chart code
## REMINDER: un-comment the plus sign ABOVE and run the whole code up to ggplot()
labs(
  title = "Outcome Proportion Across Age Categories",
  subtitle = "The height of each color shows the percentage of that outcome within the age group.",
  x = "Age Category",
  y = "Proportion of Cases (100%)",
  fill = "Outcome"
  ) +
theme_minimal() #+

## Then add this line at the very end of your bar chart code to see how it changes
## REMINDER: un-comment the plus sign ABOVE and run the whole code up to ggplot()
theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Now I will demonstrate a couple of other graphs

##################
# Facet examples #
##################

## First run this code to make a bar chart
## NOTE: we begin by filtering out "Unknown" values from the variables we will use to make the facets look cleaner
ggplot(summary_data %>% 
         filter(hospital_name != "Other/Unknown") %>%
         filter(outcome != "Unknown") %>%
         filter(gender != "Unknown") %>%
         drop_na(hospital_name, outcome, gender)) +
  geom_bar(aes(y = fct_rev(forcats::fct_infreq(hospital_name)), 
               fill = outcome), 
           width = 0.7, 
           color = "black", 
           linewidth = 0.1,
           position = "fill") 


## We are going to make facets by gender using this one line
## HINT: add a plus sign to after the geom_bar() function then run the code up from facet_wrap() to ggplot()
facet_wrap(~ VAR) 

## Then you can add the other functions we discussed earlier
scale_fill_manual(values = c("Death" = "firebrick", 
                             "Recover" = "seagreen")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "TITLE",
       subtitle = "SUBTITLE",
       y = "Y-AXIS",
       x = "X-AXIS",
       fill = "Outcome")











