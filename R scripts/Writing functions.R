# Welcome Billy, to the analysis part of R4EPI. 
# Remember DRY? (Don't Repeat Yourself). Yeah I'll focus a lot on that in the coming scripts.
# ----------------------------------------------------------------------------------------------------------------------------------
# TIDY EVALUATION
# ---------------------------------------------------------------------------------------------------------------------------------------
# ===============================================================================================================================================
# WRITING FUNCTIONS
# ===============================================================================================================================================
# I want you to take a look at the following descriptive analysis on a set of continuous data that I have analysed befor 'town.3':
town.3 <- tibble(
  Age = c(25, 20, 40, 29, 15, 19, 28, 31, 38, 45, 33, 36, 25, 27, 30, 18, 16, 35, 37,
          49, 50, 22, 23, 55, 24, NA, 15, 17, 48, 51, 54, 44, 42, 41, NA, NA, 39, 53, 25, 44),
  Age.group = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                2, 2, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2),
  Sex = c(2, 2, 2, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2,
          2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 2),
  Weight = c(60, 48, 85, 69, 40, 46, 90, 75, 77, 89, 70, 68, 61, 66, 41, 85, 43, 74, 80,
             NA, 99, 63, 90, 48, 67, 70, 44, 49, 87, 97, 60, 86, 79, 83, 55, 80, 69, NA, 101, 96),
  Height = c(5.5, 5.7, 5.8, 5.8, 5.9, 5.5, 5.8, 6.0, 5.9, 6.3, 5.6, 6.1, 5.5, 5.8, 5.5, 5.8, 5.8,
             6.0, 5.8, 6.1, 6.2, 5.6, 5.8, 6.0, 6.0, 5.8, 4.8, 5.8, 6.5, 5.5, 6.5, 6.2, 5.9, 5.8,
             5.2, 4.8, 6.0, 5.7, 5.8, 5.7),
  Health = c(2, 1, 3, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 3, 1, 2, 3, 2, 3, 2, 3, 1, 2, 1, 2, 1, 2, 4, 1, 2,
             2, 3, 1, 2, 2, 1, 4, 4)
) %>% 
  mutate(
    Age.group = factor(Age.group, labels = c("40 and under", "60 and under")),
    Sex = factor(Sex, labels = c("Male", "Female")),
    Health = factor(Health, labels = c("Underweight", "Healthy", "Overweight", "Obese"))
  ) %>% 
  print()
# Let's say that I want to get summary stats on age, weight and height:
town.3 %>% 
  summarise(
    n.miss = sum(is.na(Age)), # Number of missing values
    mean = mean(Age, na.rm = TRUE), # Mean of Ages excluding the missing values
    median =  median(Age, na.rm = TRUE),
    std = sd(Age, na.rm = TRUE), 
    min = min(Age, na.rm = TRUE),
    max = max(Age, na.rm = TRUE)
  )
# Will I have to type this entire code chunk again for weight and height? Now imagine if I had 20 different variables that required similar analysis! Heh!
# But luckily, it is possible to write functions that will help eliminate all that repetitive code typing:
Summary_stats <- function(var) {
  town.3 %>%
    summarise(
    n.miss = sum(is.na({{ var }})),
    mean = mean({{ var }}, na.rm = TRUE),
    median = median({{ var }}, na.rm = TRUE),
    std = sd({{ var }}, na.rm = TRUE),
    min = min({{ var }}, na.rm = TRUE),
    max = max({{ var }}, na.rm = TRUE)
    )
}
# I have created a function called Summary_stats that has condensed all the summary stats that I want to do for my datasets reducing the codechunks needed to 
# return the stats that I want from 9 to 1:
Summary_stats(Age)
# Returns similar stats it had before
Summary_stats(Weight)
Summary_stats(Height)

# Ah, let me get a little bit more complex about function writing
# Consider the following data frames that I would like to merge and compare to see the observations that are matching:
people_1 <- tribble(
  ~id_1, ~name_first_1, ~name_last_1, ~street_1,
  1,     "Easton",      NA,           "Alameda",
  2,     "Elias",       "Salazar",    "Crissy Field",
  3,     "Colton",      "Fox",        "San Bruno",
  4,     "Cameron",     "Warren",     "Nottingham",
  5,     "Carson",      "Mills",      "Jersey",
  6,     "Addison",     "Meyer",      "Tingley",
  7,     "Aubrey",      "Rice",       "Buena Vista",
  8,     "Ellie",       "Schmidt",    "Division",
  9,     "Robert",      "Garza",      "Red Rock",
  10,    "Stella",      "Daniels",    "Holland"
) 
people_2 <- tribble(
  ~id_2, ~name_first_2, ~name_last_2, ~street_2,
  1,     "Easton",      "Stone",      "Alameda",
  2,     "Elas",        "Salazar",    "Field",
  3,     NA,            "Fox",        NA,
  4,     "Cameron",     "Waren",      "Notingham",
  5,     "Carsen",      "Mills",      "Jersey",
  6,     "Adison",      NA,           NA,
  7,     "Aubrey",      "Rice",       "Buena Vista",
  8,     NA,            "Schmidt",    "Division",
  9,     "Bob",         "Garza",      "Red Rock",
  10,    "Stella",      NA,           "Holland"
  )
people <- people_1 %>% 
  bind_cols(people_2)
# Here are the dummy variables that will tell me what observations match:
people %>% 
  mutate(
    name_first.match = name_first_1 == name_first_2,
    name_last.match = name_last_1 == name_last_2,
    street.match = street_1 == street_2
  ) %>% 
  select(id_1, starts_with("name_fir"), starts_with("name_la"), starts_with("s"))
# The base R operator returns some results the way I dont want it to as whenerver the equality operator is analysing anything with missing values then it 
# will always return 'NA' as the result. But I want it to return FALSE instead which is more accurate as the values being compared do not match
# ie: I want 'Colton == NA' to return FALSE instead of NA.
# I will thus create a function to do exactly what I want it to do. You see, functions can also be created for such situations, not just to eliminate repetition

# Lets get on with it! It is important to start writing functions by solving the one issue that you have found and testing it 
# to see if it works before implementing it to the entire dataset that you want to analyse.

# I will start with finding the need for a function:
"colton" == NA
# It returns NA which isnt the result that I want. So that justifies the need for a function as the equality operator is just a base R operator built to function exactly like that

# I will then find a solution to the error:
results <- "Colton" == NA # Value returned by running 'result' will still be NA
results <- if_else(is.na(result), FALSE, result) # This amends the object result to return FALSE if 'result' is NA or to return the value that was initially in the result if it is not NA
# Solution found

# Now lemme make that solution a function by making the codechunk that created the solution the function body of the function I want to create
is_matching <- function() {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_matching() # Running this function returns the value 'FALSE'
# The function is however not ready as there was no argument created for it so if I try to pass any value/vector to the function, it will return an error as there arent any arguments to pass values/vectors to:
is_matching("Ellie")
is_matching(name = name_first_1)
# Lets create an argument to pass values to then:
is_matching <- function(name) {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
# The value 'name' that I have inserted in the brackets after function is what will for the argument(s)
is_matching(name = "Ellie")
# You might think it works because 'Ellie' == NA in the merged dataframe above. but let me try again with another value that actually matches
is_matching(name = "Easton")
# It still returns FALSE which is untrue! and so, we still have to fix the function by making it more general
is_matching <- function(name) {
  result <- name == NA 
  result <- if_else(is.na(result), FALSE, result)
  result
}
# Note that the variable 'name' matches the argument 'name'. THEY SHOULD ALWAYS MATCH!
is_matching(name = "Easton")
# It is still going to returns FALSE because 'Easton  == NA' is correctly analysed by R to return FALSE like I coerced it to 
# The function is still specific to the one issue I was trying to fix initially but it needs to be more general for it to work
# I want to create a function that compares two variables and if the result returned after that comparison is 'NA' then I want it to return FALSE instead
is_matching <- function(name1, name2) {
  result <- name1 == name2 
  result <- if_else(is.na(result), FALSE, result)
  result
}
# I have created two arguments 'name1' and 'name2' and the code chunk is going to compare values passed to name1 and name2 and if the result is NA, then it is going to change it to FALSE

# Let me go ahead and run it with the value "Easton" that matches for both the data frames:
is_matching("Easton", "Easton")
# It actually returns TRUE because they do match. 
is_matching("Ellie", NA)
# The function is finally working!

# Now lemme run it with the entire merged data frame:
people %>% 
  mutate(
    name_first.match = is_matching(name_first_1, name_first_2),
    name_last.match = is_matching(name_last_1, name_last_2),
    street.match = is_matching(street_1, street_2)
  ) %>% 
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
# And that my friend is a problem solved by a function I have just created!

# Let me try creating another function and assigning it default values to arguments
increment5 <- function(x) {
  x + 1
}
# This function's function is to increase a value or a set of values by 1:
increment5(c(5, 6, 7))
# Results are 6, 7, 8
increment5 <- function(x, by) {
  x + by
}
increment5(c(5, 6, 7), 3)
# I created another argument and used it in the summation operation that I have coded for the fuction
# However, to ad a default value to any or all the arguments:
increment5 <- function(x, by = 2) {
  x + by
}
# Thats how it is done, so now if no value is passed to the 'by =' argument, the value(s), passed to the 'x =' argument is automatically increased by the set default value of 'by =' which is 2
increment5(c(1, 2, 3))

# It is important to note that the last line of code in the function determines the results returned by the function:
# ie: in our function, is_matching(), if I dint write a code to print the results in the last line then running the function would not return any result:
is_matching <- function(name1, name2) {
  result <- name1 == name2 
  result <- if_else(is.na(result), FALSE, result)
}
people %>% 
  mutate(
    name_first.match = is_matching(name_first_1, name_first_2)) %>% 
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
# It is therefore considered best practice to write the name assigned to the results of the last line of code as the last line of code in the function body or to use the return() function:
is_matching <- function(name1, name2) {
  results.1 <- name1 == name2 
  results.1 <- if_else(is.na(results.1), FALSE, result)
  return(results.1)
}
people %>% 
  mutate(
    name_first.match = is_matching(name_first_1, name_first_2)) %>% 
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Lexical scoping rules
# These are rules that R uses in the execution of created functions
# The first rule is that, objects assigned values inside the function do not appear in the global environment
# Take for example, there is an object 'results.1' that I created inside the increment5() function right? Let me try running it outside the function:
results.1
# Error is returned as the object 'results.1' does not exist inside the global environment

# The second lexical scoping rule is that if an object does not exist inside the function being created, then R is going to look for that object in the global environment:
# Let me create another function:
addition <- function(x) {
  x + y
}
addition(2)
# It returns an error saying object 'y' not found. Thats because I didnt create it inside the function. However check this out:
y <- 50
addition(2)
# It returns 52 because it didnt find the object y inside the function so it looked for it in the global environment and found it (having the value 50 within it)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Welcome Billy, to Tidy Evaluation. This is basically the usage of tidyverse package functions (like dplyr functions), inside the functions I will be wriring
# R's principle of non-standard evaluation prevents the usage of non-Base R functions in writing functions normally like we have been above!:
# So the usage of these non Base R functions require a little tweak:
# Lets analyse the categorical variables of the town.3 data frame:
town.3 %>% 
  count(Health) %>% 
  mutate(
    percentage = n / sum(n) * 100
  )
# Like the steps followed in the writing of functions above, I will begin with, finding need for a function. 
# I have 3 different categorical variables to analyse, and so I want to reduce repetition.
# Second step is to make the code work for one specific case -  I want to get the percentages of the unique observations of the Health column.
# These two steps I have already done above. Third step is to make the solution a function:
percentage_stats <- function(var) {
  town.3 %>% 
    count(Health) %>% 
    mutate(percentage = n / sum(n) * 100)
}
percentage_stats(Health)
# It returns the correct stats I want but it is specific to the Health variable alone. IF i try analysing any of the other categorical variables, it will not work:
percentage_stats(Sex)
# It will still return the same results it did before for the Health variable
# Hence I have to generalise the function:
percentage_stats <- function(var) {
  town.3 %>% 
    count(var) %>% 
    mutate(percentage = n / sum(n) * 100)
}
percentage_stats(Sex)
# It returns an error saying that the column 'var' is not found and that is because of 
# the non-standard evaluation that doesn't allow that even though our code specifies that the analysis should come from the data frame town.3
# That can be fixed through wrapping the variables in the function I am writing with {}:
percentage_stats <- function(var) {
  town.3 %>% 
    count({{ var }}) %>% 
    mutate(percentage = n / sum(n) * 100)
}
percentage_stats(Sex)
percentage_stats(Age.group)
# And that finally works

# However all the functions I have created so far are only specific to the data frame that I was analysing . What if I wanted to analyse another data frame?:
# Lets say I want to analyze the percentage of the drug and placebo that was used in the schistosoma_analysis.1 data frame. This is how I would go about it:
percentage_stats <- function(data, var) {
  data %>% 
    count({{ var }}) %>% 
    mutate(percentage = n / sum(n) * 100)
}
# I created another argument that allows the introduction of any other data frame then generalised the code chunk from study to match the new argument 'data'
percentage_stats(Schistosoma_analysis.1, Drug)
# Perfect! and I can even use the piping function to carry out that analysis
Schistosoma_analysis.1 %>% 
  percentage_stats(Drug)
# The function is now fully functional to any data frame as its first argument is going to be the data frame to be analyzed followed by the column to be analyzed.
# It is no longer specific to the data frame town.3:
percentage_stats(Health)
# It returns an error. However, this would work:
town.3 %>% 
  percentage_stats(Health)

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# The 'data' variable in the function I have written is not wrapped in{} like the 'var' variable. This is because of a specific aspect of non-standard evaluation
# called data masking. Data masking allows the access of a column of a data frame without the use of base-R operators ie: '$' or '[]' 
# eg: the column Health of town does not exist in the global environment as a standalone object and so will require data masking in order to call it
# However, town.3 exists in the global environment as a standalone object and hence doesn't require data maskng in order to be called in function writing
# However, wrapping the 'data' variable in the function body referring to any data frame does no harm!
percentage_stats <- function(data, var) {
  {{ data }} %>% 
    count({{ var }}) %>% 
    mutate(percentage = n / sum(n) * 100)
}
Schistosoma_analysis.1 %>% 
  percentage_stats(Blood_poo)
