# =====================================================================================================================
# Working with character strings
# =====================================================================================================================

install.packages("stringr")
library(stringr)
library(dplyr)
library(readr)

Health.r <- read_rds("C:/Users/ADMIN/Downloads/ehr.Rds")
# Let me start by cleaning the names column
Health.r %>% 
  group_by(name) %>% 
  mutate(rep = row_number() > 1) %>% # This is meant to show us the observations that were repeated
  arrange(name) %>% 
  select(name, rep, dob, address, city)
# There are clearly rows that have been repeated but I cannot clean it programmatically with normal R fuctions as R
# sees all 15 observations as different because none of them have been written identically
# Let me take a closer look

Health.r %>% 
  arrange(name) %>% 
  pull(name)
# There are definitely same people appearing more than once in the dataframe but R views them as different
# entries because they have been spelled differently in the duplicates

# Introduction to stringr package: changing the case of characters
Health.r %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_lower() # This stringr function turns all characters to lower case
Health.r %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_upper() 
Health.r %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_title() # Keeps all the first characters of a string as uppercase and the rest as lowercase in all the values
Health.r %>% 
  arrange(name) %>% 
  pull(name) %>% 
  str_to_sentence() # Keeps only the first character of a value as uppercase and the rest lower in the same value

# Now let me begin cleaning
Health.r <- Health.r %>% 
  mutate(name = str_to_lower(name))
# Now lemme try and see if I can programmatically find duplicates
Health.r %>% 
  group_by(name) %>% 
  mutate(rep = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, rep, dob, address, city)
# Coercing all the letters of the characters has enabled R to programmatically catch one duplicate No. 2
# Let me now remove any white space in the data values 'basically just space before and/or after a value':
Health.r <- Health.r %>% 
  mutate(name = str_trim(name))
Health.r %>% 
  group_by(name) %>% 
  mutate(rep = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, rep, dob, address, city)
# Yep, found another duplicate - No. 10

# Introduction to Regular Expressions
# Let me remove commas in the name vector using the regex (regular experessions)
str_replace(
  string = "weston fox,", # Value passed to this argument is usually a character sring or a vector of character strings
  pattern = ",", # Value passed to this argument should be what is to be replaced (This is a regex)
  replacement = "" # Value passed to this argument should be what the replacement is (This is a regex)
)
Health.r <- Health.r %>% 
  mutate(name = str_replace(name, ",", ""))
Health.r %>% 
  group_by(name) %>% 
  mutate(rep = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, rep, dob, address, city)
# Yet another addition to the duplicates R finds - No. 14

# Removing middle initials in names:
str_replace(
  string = "tatum s chavez",
  pattern = " \\w ", # This is a reg ex that tells the function that it should look for the first space it sees followed by a word character followed by another space
  replacement = " " # This is also a regex
)
# The '\\w' regex is called a token and it refers to any word, number or underscore character
Health.r <- Health.r %>% 
  mutate(name = str_replace(name, " \\w ", " "))
Health.r %>% 
  group_by(name) %>% 
  mutate(rep = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, rep, dob, address, city)
# Another duplicate recognized - No. 12

# Let me remove the whitespace in the character vector with regex
str_replace(
  string = "Ivy   mccann",
  pattern = "\\s{2,}", # The '\s' is a token referring to any whitespace while the '2' in the curly braces specifies the minimum amount of the character specified before the curly braces
  # The first number is usually followed by another number that usually specifies the max amount of the character specified. 
  # If there isn't a first number then it tells R that it shouldn't consider any minimum and vice versa for the second number
  replacement = " "
)
# Now let me clean the data
Health.r <- Health.r %>% 
  mutate(name = str_replace(name, "\\s{2,}", " "))
Health.r %>% 
  group_by(name) %>% 
  mutate(rep = row_number() > 1) %>% 
  arrange(name) %>% 
  select(name, rep, dob, address, city)
# You guessed it! another duplicate found at No. 6
# If I was dealing with a large dataset that would have made it difficult to count the unique observations/people:
Health.r %>% 
  group_by(name) %>% 
  filter(row_number() == 1) %>% # This code tells R that it should only keep the rows that do not show a count of more than 1, (basically a duplicate)
  ungroup() %>% 
  summarise('Unique Observations' = n())

# Lets see how many of the people lived in each city
Health.r %>% 
  group_by(name) %>% 
  filter(row_number() == 1) %>%
  ungroup()
Health.r %>% 
  group_by(city) %>% 
  summarise(count = n())
# Yep we have cleaning to do for the city data too
# Ill begin by clearing the letter case error:
Health.r <- Health.r %>% 
  mutate(address = str_to_lower(address),
         city = tolower(city)
  )
Health.r %>% 
  group_by(city) %>% 
  summarise(count = n())
# Now, the str_repalce() function doesnt just use regex:
str_replace(
  string = "city of fort worth",
  pattern = "city of ", # Pretty self explanatory - removes this from the string
  replacement = ""
)
Health.r <- Health.r %>% 
  mutate(city = str_replace(city, "city of ", ""))
Health.r %>% 
  group_by(city) %>% 
  summarise(count = n())
# The city column characters are cleaned successfully

# Separation of character values into components using the str_extract() function
str_extract("zariah hernandez", "^\\w+")
# The above code is just the 'string=' and 'pattern =' arguments
# In the regex, '^' is an anchor and it tells R to look for the pattern at the beginning of the character string
# '+' is a quantifier that tells R to match the pattern one or more times. '\w' ofcourse as mentioned before refers to a word character
# The regex as a whole tells the str_extract() function to extracts what it the word characters it finds from the beginning of the string
# The extraction stops after the name 'zariah' because what follows it is a space character that is not part of what '\w' specifies
str_extract("zariah hernandez", "\\w+$")
# In the regex above, '$' tells R to do similar functions as the regex before but only beginning with the character at the end of the string

# Let me separate the names in the dataframe
Health.r <- Health.r %>% 
  mutate(name.first = str_extract(name, "^\\w+"),
         name.last = str_extract(name, "\\w+$")
         )
Health.r %>% 
  select(name, name.first, name.last)

# Checkout the regex tester and debugger at regex101.com 

# Let me check out the symptoms variable
Health.r %>% 
  select(name.first, name.last, symptoms)
Health.r %>% 
  group_by(symptoms) %>% # Let me see if I can be able to see the recorded count of every symptom 
  summarise(count = n())
# Yeah, that doesnt work. And hence, hello regex and dummy variables!:
Health.r <- Health.r %>% 
  mutate(
    pain = str_detect(symptoms, "Pain"),
    headache = str_detect(symptoms, "Headache"),
    nausea = str_detect(symptoms, "Nausea")
  )
# Now to get a count of each recorded symptom:
table(Health.r$pain)

# R however stores FALSE and TRUE values as '0' and '1' respectively:
Health.r %>% 
  select(pain) %>% 
  mutate(pain.1 = as.numeric(pain))
