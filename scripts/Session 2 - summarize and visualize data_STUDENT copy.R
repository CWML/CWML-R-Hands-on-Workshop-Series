# Important Notes

## Comments - used to add notes or details to code using the pound/number/hash-tag symbol (#)
## Running code - highlight lines (chunk) then command/return or control/enter
## You need to update the parts of the codes provided that have ALL CAPS or question marks (?)
## To create a new pipe(%>%) use Shift+Command+M (Mac) or Shift+Control+M (PC)
________________________________________________________________________________

###############################
# Step 1: create an R Project #
###############################

######################################
# Step 2: Load package using Pacman() #
######################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,          # File import/export
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management, summary, and visualization
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to nice images using html
  officer,      # helper functions for tables
  psych,        # detailed descriptive statistics including skew and kurtosis
  stringr,      # working with characters
  ggforce,      # ggplot extras
  forcats       # working with factors
) 

#####################################################################################################
# Step 3: Import the combined_linelist.csv data file from the processed_data folder in your project #
#####################################################################################################

# Use the import() and here() functions and call your new data frame "data"
# HINT 1: folder name is "processed_data" and file name is "combined_linelist.csv"
# HINT 2: use the structure written below

my_data <- import(here("FOLDER NAME", "DATA_FILE.xlsx"), setclass = "tbl")

#####################################
# Step 4: Inspect the raw data file #
#####################################

# Run each function one at time 

str(DATA_FRAME_NAME)

skim(DATA_FRAME_NAME)

## After inspecting, what issues did you find?

#########################################
# Step 5: further cleaning and updating #
#########################################

# Us the <- operator to create a new data frame called "summary_data"
# First: we want to FILTER bmi so we are left with those who have a BMI between 15 and 60
# Second: we want to use MUTATE to recode the values under the variable gender 


NEW_DF <- data ?
  FUNCTON(!(VARIABLE <= PARAMETER1 | VARIABLE > PARAMETER2)) ? 
  FUNCTION(VARIABLE = recode(VARIABLE,    
                         "OLD VALUE" = "NEW VALUE",
                         "OLD VALUE" = "NEW VALUE"
  )) ?
  mutate(age_cat = case_when(
    age_years < 18 ~ "Under 18",
    age_years >= 18 & age_years < 30 ~ "NEW VALUE",
    age_years >= 30 & age_years < 40 ~ "NEW VALUE",
    age_years >= 40 & age_years < 50 ~ "NEW VALUE",
    age_years >= 50 & age_years < 60 ~ "NEW VALUE",
    age_years >= 60 & age_years < 70 ~ "NEW VALUE",
    age_years >= 70 & age_years < 80 ~ "NEW VALUE",
    TRUE ~ "80+"  # This catches all remaining cases
  ))

# Re-inspect your data

str(DATA_FRAME_NAME)

skim(DATA_FRAME_NAME)

#################################
# Step 6. descriptive tables    #
#################################

# Base R 1: summary()
## This code uses a mixture of base R and tidyverse logic
### HINT: we want to SELECT only NUMERIC variables for the SUMMARY() function

DF_NAME ?
  FUNCTION(VAR1, VAR2, VAR3, VAR4, VAR5) ?
  FUNCTION()

# Base R 2: table()
## HINT: we want to use the TABLE function any search any CHARACTER variable in our data frame
## NOTE: just choose one variable for this example
FUNCTION(DATA_FRAME$VAR)

# psych::describe()
## the double colon (::) is used to specify which package you want the function to come from
## using select_if tells the function to look at only numeric values in you data frame

PACKAGE::FUNCTION(DATA_FRAME %>% select_if(is.numeric))

# janitor::tabyl()
## run these 2 codes separately
summary_data %>% 
  tabyl(age_cat)

summary_data %>%
  tabyl(age_cat, fever)

## Now using pipes (%>%) and the various adorn functions, add column totals; row total; and change row names to "Age Groups"

# rstatix::get_summary_stats()
## NOTE 1: type = "common" common stats; type = "full" more stats
## NOTE 2: used for numeric variables

DF_NAME %>%
  FUNCTION(age_years, wt_kg, ht_cm, bmi, temp, 
                    type = "?") 

# dplyr::count()
## Start with this simple code
summary_data %>% 
  count(age_cat)

## Add another non-numeric variable within count() - for example you can use "outcome"
## HINT: Separate the variables in count() using a comma (,)
ENTER CODE HERE

## Run this code to see what happens
summary_data %>% 
  count(age_cat) %>%          
  mutate(                               
    percent = scales::percent(n / sum(n), accuracy = 0.1)

## Now add a group_by() statement under the line "summary_data %>%" to group by "outcome" and don't forget to pipe (%>%)
summary_data %>% 
  WRITE THE NEW LINE HERE
  count(age_cat) %>%          
  mutate(                               
    percent = scales::percent(n / sum(n), accuracy = 0.1)

# dplyr::summarise()
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
## HINT2: copy/paste the previous code and then add the new line or just add the new line to you previous code
## HINT3: remember to use the pip (%>%) after every line
ENTER CODE HERE

## Using across() lets you apply the same operations to multiple columns at once 
summary_data %>%
  group_by(VARIABLE) %>%
  summarise(
    across(c(VARIABLE1, VARIABLE2, VARIABLE3),         # Which columns to use
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

#################################
# Step 7: Plots using ggplot    #
#################################

# Histogram
_______________

## Start with making a simple histogram for the variable wt_kg
ggplot(data = DF, mapping = aes(x = VAR)) +
  geom_histogram()

## Let's add some details, copy/paste these lines in between the parentheses for geom_histogram() in the above code
binwidth = 5,  # width of bins
color = "red", # bin line color
fill = "blue", # bin interior color
alpha = 0.1    # bin transparency

## Add some labels and some finishing touches with this code added to the above
## REMEBER: use the ggplot pipe (+) after each function combine the codes
## NOTE: Copy/paste to the above code like before or comment out the 4 lines above then run the whole code
labs(
  title = "WRITE A TITLE HERE", 
  subtitle = "WRITE A SUBTITLE HERE"
) +
  theme_minimal()

#############################################################################################################################################

# Scatter plot
___________________

## Now we will create a simple scatter plot along with dplyr's filter() to select only those with fever
## HINT: We want to graph a comparison of age_years and wt_kg
ggplot(data = DF ? filter(fever == "VALUE"), 
       mapping = aes(x = VAR1, y = VAR2)) +
  geom_point() 

## Once you create your plot, we can add color details using the BMI of each data point using the following code
## Copy/paste this line in aes() after the variable name for the y-axis; don't for get to use a comma
color = bmi

## The add these details inside geom_point()
## Once you do this and run it, change the numbers and re-run it and see what happens
  size = 1,
  alpha = 0.5

## Finally, add this line to the end of the code and we'll see what happens
## HINT: use a plus sign before adding this line
geom_smooth( method = "lm", size = 2, se = FALSE)

#############################################################################################################################################

# Box plot
_______________

## Run this code to make a simple boxplot
ggplot(data = summary_data, mapping = aes(y = age_years, x = gender, fill = gender)) +
  geom_boxplot() + 
  theme_minimal() 

## what are some weird things you notice?
## Let's address this by adding this line to the above code
theme(legend.position = "none")

filter(gender != "VALUE"),

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

#############################################################################################################################################

# Bar chart
_____________

## Run this code for a simple bar chart
## NOTE1: after you run it, change "position = " from "fill" to "stack" then rerun it
## NOTE2: change "x = " to "y = " and rerun it
## NOTE3: change everything back to what they were initially 

ggplot(data = summary_data, mapping = aes(x = age_cat, fill = outcome)) +
  geom_bar(position = "fill", color = "black", linewidth = 0.1)

## You can manually change the bar colors using, scale_fill_manual()
## Add these line to the above code
scale_fill_manual(values = c("Death" = "darkred", 
                             "Recover" = "red", 
                             "Unknown" = "blue")) 

## You can add titles and labels using lab() just like the previous plot examples
## Create you own labels and title and add to the above bar chart code
## REMEBER: use quotations and add a plus sign before the labs() function
labs(
  title = ADD TITLE,
  subtitle = ADD SUBTITLE,
  x = ADD LABLE,
  y = ADD LABLE,
  fill = "Outcome"
  ) +
theme_minimal()

## Then add this line at the very end of your bar chart code to see how it changes
theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Now I will demonstrate a couple of other graphs

#############################################################################################################################################

# Facet examples
__________________

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

## No will demonstrate using facets with histograms and boxplots










