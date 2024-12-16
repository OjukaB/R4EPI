# ===================================================================================================================================
# Conditional operations
# ===================================================================================================================================
library(dplyr)

# Let me try some coding with Melina's current work
schisto.diabetes.study <- tibble(
  Rat.serial = c(5, 7, 4, 1, 2, 10, 12, 18, 19, 24),
  FBG = c(7.8, 4.2, 14.2, 19.9, 3.6, 5.6, 10.5, 7, 9.2, 2.9)
)
# This dataframe contains the values of the glucose levels for Melina's rats on the final week before treatment
# Now let me make a diagnosis using conditional operations:
schisto.diabetes.study %>% 
  mutate(
    Diagnosis = if_else(
      condition = FBG < 5.8,
      true = "Hypoglycemic",
      false = "Hyperglycemic"
    )
  )
# I used the glucose levels of Melina's rats indicated in the FBG variable to give information to the Diagnosis
# column using conditional operations. In the condition argument, I told R to look for glucose levels that are 
# below 5.8 (usually normal to low levels of glucose) and told it to return hypoglycemic if the condition was found to be true

# But check this out, I can use this code too:
schisto.diabetes.study %>% 
  mutate(
    Diagnosis = if_else(c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE), "Hypoglycemic", "Hyperglycemic")
  )
# So, you see the condition argument? Yes, you can directly pass logical values to it (TRUE/FALSE) then the values passed to the
# 'true =' and 'false =' arguments still in the same if_else() function are going to correspond to the data you input in the condition argument
# It can however be tideous to type in every single value for the 'conditions =' argument if the dataset has numerous observations

# --------------------------------------------------------------------------------------------------------------------------------------
# Operands and operators- operands are basically values that are to be tested. They are of two types - varible operands or constant operands (character, numeric or date values)
# Introduction to the modulus arithmetic operator (%%)
2 %% 2
# Returns 0  because when 2 is divided by two, the remainder is 0
5 %% 2
# Returns 1 because when 5 is divided by 2, then the remainder will be 1
# How can the modulus operator actually be used with real case data?:
case.1 <- tibble(
  id = c(001, 002, 003, 004),
  outcome = c(1, 0, 0, 1)
)
# This is a simulated data, so lets assume that I want to add a column with values that interchange after every row:
case.1 %>% 
  mutate(if_else(row_number() %% 2 == 1, "placebo", "pzq"))
# I told R that it should compare the use the modulus operation between the row number and 2, and if the answer is 1 then it should return the value passed to the true argument, and if not, values passed the false argument are returned

# Check this matching operation out:
case.2 <- tibble(
  name.1 = c("Abby", "Ally", "ally", NA),
  name.2 = c("Abby", "Ali", "Ally", "Ally")
)
case.2 %>% 
  mutate(
    match = name.1 == name.2
  )
# It returns an NA in the row containing the missing value. But in real sense it is a mismatch and I want it to return a 'false' value instead of NA
case.2 %>% 
  mutate(
    match = name.1 == name.2,
    match = if_else(is.na(match), FALSE, match)
  )

# Let me now test two conditions in the if_else() function
blood_pressure <- tibble(
  id     = 1:10,
  sysbp  = c(152, 120, 119, 123, 135, 83, 191, 147, 209, 166),
  diasbp = c(78, 60, 88, 76, 85, 54, 116, 95, 100, 106)
)
blood_pressure <- blood_pressure %>% 
  mutate(bp = if_else(sysbp < 120 & diasbp < 80, "Normal", "Abnormal")
  )
blood_pressure <- blood_pressure %>% 
  mutate(
    bp.1 = case_when(
      sysbp < 120 & diasbp < 80 ~ "Normal",
      sysbp >= 120 & sysbp < 130 & diasbp < 80 ~ "Elevated", # Systolic (120-130), Diastolic (less than 80)
      sysbp >= 130 & sysbp < 140 | diasbp >= 80 & diasbp < 90 ~ "Hypertension Stage 1", # Systolic (130-139), Diastolic (80-89)
      sysbp >= 140 | diasbp >= 90 ~ "Hpertension Stage 2"
    )
  )

# Collapsing variables
test.scores <- tibble(
  ID = 01:15,
  Score = c(NA, sample(15:95, 13, FALSE), NA)
)
test.scores <- test.scores %>% 
  mutate(
    score.cat = case_when(
      Score <= 30 ~ 1, # Fail
      Score >= 31 & Score <= 49 ~ 2, # Below average
      Score >= 50 & Score <= 65 ~ 3, # Average
      Score > 65 ~ 4 # Above average
    )
  )
# It is however recommended that you use numerical values if you are collapsing data then create another column factoring the values of the collapsed data references

# Recoding missing values
# It is common to find "don't know" and "refused" as answer options in epidemiological studies alongside the "missing" options
# It is normally coded by "7" and "9" respectively. Andi in datasets that have more than 7 observations it changes to "77" and "99" and so on
# Consider the following options for a race variable:
# 1 = White
# 2 = Black or African American
# 3 = American Indian or Alaskan Native
# 4 = Asian
# 5 = Pacific Islander
# 7 = Don’t know
# 9 = Refused
# And the following option for an ethnicity column:
# 0 = No, not Hispanic
# 1 = Yes, Hispanic
# 7 = Don’t know
# 9 = Refused

set.seed(41)
Ages <- tibble(
  Id = 1:15,
  Age = c(sample(5:40, 13, TRUE), NA, NA)
)

demographics.4 <- Ages %>% 
  mutate(
    race     = c(1, 2, 1, 4, 7, 1, 2, 9, 1, 3, 2, 2, 1, 9, 1),
    hispanic = c(7, 0, 1, 0, 1, 0, 1, 9, 0, 1, 1, 0, 7, 0, 7)
  )
# Now lemme recode the '7' and '9' to NA
demographics.4 <- demographics.4 %>% 
  mutate(
    race.recode = if_else(race == 7 | race == 9, NA, race), # Told R to replace the 7 or 9 with NA
    hispanic.recode = if_else(hispanic == 7 | hispanic == 9, NA, hispanic)
  )
# Normally, the above code should return an error because the value type we have passed to the 'true =' argument is 'NA' a logical type of value that mismatches with the value I passed to the 'false =' argument which is a numerical(double) type of value
# It doesnt though, I am not sure why. but if it does, I just have to specify the type of 'NA' to key in so as to eliminate the mismatch within the same variable:
demographics.4 <- demographics.4 %>% 
  mutate(
    race.recode = if_else(race == 7 | race == 9, NA_real_, race),
    hispanic.recode = if_else(hispanic == 7 | hispanic == 9, NA_real_, hispanic)
  )
# Now lemme combine the race and ethnicity options
demographics.4 <- demographics.4 %>% 
  mutate(
    race.recode = if_else(race == 7 | race == 9, NA_real_, race),
    hispanic.recode = if_else(hispanic == 7 | hispanic == 9, NA_real_, hispanic),
    race.eth = case_when(
      race.recode == 1 & hispanic.recode == 0 ~ 1, # This creates the option of people who are white but not Hispanic
      race.recode == 2 & hispanic.recode == 0 ~ 2, # Black/African American but not Hispanic
      # In this dataset, there are only one occurence of Asian and American Indian/Alaskan and no occurence on Pacific Islander so it is common in epidemiology to group observations that are very few into one labelled 'others':
      race.recode == 3 & hispanic.recode == 0 ~ 3, # American/Alaskan and non- Hispanic
      race.recode == 4 & hispanic.recode == 0 ~ 3, # Asian and non-Hispanic
      race.recode == 5 & hispanic.recode == 0 ~ 3, # Pacific Islander and non-Hispanic
      hispanic.recode == 1 ~ 4 # Any race that's Hispanic
    )
  )
# Now lemme combine the entire operation
demographics.4 %>% 
  mutate(
    # Collapse the ages into categories
    Age.cat = case_when(
      Age <= 12 ~ 1, # Child
      Age >= 13 & Age <= 18 ~ 2, # Adolescent
      Age >= 19 ~ 3 # Adult
    ),
    # Lemme factor it 
    Age.cat.f = factor(Age.cat,
                       labels = c("Child", "Adolescent", "Adult")),
    race.eth.f = factor(race.eth,
                        labels = c("White, Non-Hispanic", "Black, Non-Hispanic", "Others, Non-Hispanic", "Hispanic any race"))
  )
