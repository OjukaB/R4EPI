# ==========================================================================================================
# Working with dates
# ==========================================================================================================

Dates <- readr::read_csv("C:/Users/ADMIN/Downloads/birth_dates.csv")
Dates

# ------------------------------------------------------------------------------------------------------------------
# R stores dates as numbers in respect to the Unix epoch date; so a date in R is stored as the number of days before or after 
# the Unix epoch date: 1970-01-01, 00:00:00 UTC

as.Date("2024-11-02")
# Let me see what is happening under the hood
unclass(as.Date("2024-11-02"))
# Since R uses the Unix epoch date, it will return the results of the above code as '20029', to tell me that the date specified above
# is 20029 days after the Unix epoch date
# The epoch date in R also allows me to work in the opposite way as such:
as.Date(20029)
# The date value returned by the code above is the same date value I inputed above- '2024-11-02'

# It is worth noting though that not all programs and programming languages use the Unix epoch date. ie:
# if I was importing a similar date value "20029" from SAS that does not use the Unix epoch date, then the date R
# would have displayed is not the same as the date that would have been displayed in SAS. 
# However, I can specify to R the origin date "1960-01-01" that is considered the origin date in SAS
Dates.1 <- tibble(
  date = c(20028, 20029, 20030)
)
# Lets assume this simulates dataframe contains the integer representation of dates from SAS
Dates.1 <- Dates.1 %>% 
  mutate(date = as.Date(date, origin = "1960-01-01"))
# This code chunk specifies to R that the values of the dataframe are actually date values and so it will covert it to dates
# But because I know that there is a mismatch in origin dates in R and SAS, I specified the origin date
# The returned date values is the correct values as viewed from SAS that we imported from

# As stated by Prof Brad, it can be diificult to work with date-time values ('POSIXct' or 'dttm' values)
# Hence, coercion of dttm value to date values
print(Dates)

Dates <- Dates %>% 
  mutate(
    dob_actual.converted = as.Date(dob_actual)
  )
# Passing the dttm column to the as.Date function coerces it to date values
Dates %>% 
  select(dob_actual, dob_actual.converted) # This is for a quick side by side display of the dttm column and its coercion column

# Now, coverting dttm to date value can be easy but convertion of chr values to date values requires a bit more:
# I'll have to specify for R what each components of the chr values are

Dates %>% 
  mutate(
    dob_typical.converted = as.Date(dob_typical, format = "%m/%d/%Y")
    # So, there are usually a couple of symbols used by R to give description to character date values
    # Here: '%m' tells R the value described is month 'month number'
    # '%d' tells R the value described is 'day of the month as a number'
    # and '%Y' tells R the value described is 'year number with century'
    # Make sure to state it exactly how the column to be coerced has stated it, so include the '/' in between the values
  ) %>% 
  select(dob_typical, dob_typical.converted)

Dates %>% 
  mutate(dob_long.converted = as.Date(dob_long, format = "%B %d, %Y")) %>% 
  # '%B' tells R the value described is 'Month name in full'
  select(dob_long, dob_long.converted)

# Now, let me do the opposite, convert date values into character values
Dates %>% 
  mutate(dob.abb = format(dob_actual, "%d %b %y")) %>% 
  # '%b' in R represents abbreviated month name and '%y' is the year number without the century
  select(dob_actual, dob.abb)

# -------------------------------------------------------------------------------------------------------
# Useful built in dates
# -------------------------------------------------------------------------------------------------------
Sys.Date()
# Tells today's date
install.packages("lubridate")
lubridate::today()

Sys.time()
# Returns dttm values
lubridate::now() # Same function as above

# The lubridate's now() function can be used to calculate the amount of time it takes R to complete a task:
# Let me create a set of random numbers
set.seed(57)
rand_mill <- rnorm(1000000)
# This generated 1,000,000 random numbers

start  <- lubridate::now()
sum    <- sum(rand_mill)
length <- length(rand_mill)
mean   <- sum / length
mean
stop <- lubridate::now()
# The global environment has presented some information; including the mean, sum, start and stop times
stop - start
# This code returns the time difference between the start and stop of the code times

# Let me compare the running time above to that of the mean() function
start <- lubridate::now()
mean(rand_mill)
stop <- lubridate::now()
stop - start

# Okay, so, it seems the mean() function is actually slower than the previous layed out functions hehe! Weird.

# ----------------------------------------------------------------------------------------------------
# Some useful info
# In case you want R to generate for a sequence of month values to save time:
month.name
# Gives the month full names
month.abb
# Gives the abbreviated values for month names

# Creating a vector containing a sequence of dates can be done as such:
seq.Date(
  from = as.Date("2024-10-21"),
  to = as.Date("2024-11-7"),
  by = "days"
)
# I just created a sequence of dates between 21st October this year 7th November this year

# Calculation of time between dates:
# I want to calculate the age of the participants in the 'Dates' dataframe above up until last year Nov 1st

Age <- Dates %>% 
  select(name_first, dob = dob_default)
# So, if I wanted to calculate their ages today this is the code that I would have used for the new column I want to create
Age %>% 
  mutate(today = Sys.Date())
# But I want to check their dates from last year as mentioned above:
Age <- Age %>% 
  mutate(today = as.Date("2023-11-1"))

# Let me now calculate a time interval of the observations in the Age dataframe
# First lemme show the operations and function s that can do that work
library(lubridate)

Age %>% 
  mutate(
    age.sub = today -dob,
    age.diff = difftime(today, dob), # This is the base R function for calculating time between date intervals
    age.lub = dob %--% today # This is lubridate's function that shows only the intervals between dates
  )
# The first two operations bring back the time between the dob and today as days however that is not commonly used

Age %>% 
  mutate(
    age.sub = as.numeric(today -dob) / 365.25,
    age.diff = as.numeric(difftime(today, dob)) / 365.25,
    age.lub = (dob %--% today) / years(1)
  )
# I used base R's as.numeric() function to convert the date values into integer values in order to be able to divide
# it with 365.25 that is the approximate no. of days in a year
# However, in the column using lubridate's functions, I used the years() function and passed 1 to it to tell R that
# I want it to show me the number of 1 year periods are in each interval(each observation)

# The difference between base R and lubridate's functions of calculating the age is that lubridate is more  specific:
start <- as.Date("2018-03-01")
end <- as.Date("2020-03-01")

as.numeric(difftime(end, start)) / 365.25
# Result returned is 2.001369 as 2020 is a leap year and hence had one extra day
(start %--% end) / years(1)
# Result returned is exactly 2 

# Rounding off years
Age %>% 
  mutate(
    age.years = (dob %--% today) / years(1),
    age.true = trunc(age.years), # This rounds of the age to the age as of the last birthday
    age.near = round(age.years) # This rounds of the age to the nearest year
  )
# I dont think that the round() function is commonly used for this purpose as it is rare to consider someone 29 just because they are 
# 2 or 1 month away from their actual 29th birthday
# Though, here's a shortcut for the result age as of the llast birthday:
Age %>% 
  mutate(age.years = (dob %--% today) %/% years(1))

# Segregating date components (can be useful in linking records across multiple dataframes)

Age <- Age %>% 
  select(!today) # I will not be needing the today column from here

# Lubridate has functions to separate components of a date
Age %>% 
  mutate(
    day = day(dob),
    month = month(dob),
    year = year(dob)
  )
# Lubridate can also add more descriptive segregations
Age %>% 
  mutate(
    wday = wday(dob), # This tells me the day of the week in which the specified date falls in as an integer [available options ofcourse are 1-7]
    day.full = wday(dob, label = TRUE, abbr = FALSE), # This labels the day in full
    day.abb = wday(dob, label = TRUE, abbr = TRUE), # This labels the day but in abbreviation
    week.year = week(dob), # This tells us the week of the year the specified date has fallen into
    week.cdc = epiweek(dob), # This tells us the epidemiological week in which the specified date falls into according to the US CDC calendar whose epidemiological weeks usually start with sunday
    week.ep = isoweek(dob) # This tells us the week the date falls in according to the standard epidemiological calendar that usually begins with Monday.
  )

# Sorting dates in ascending and descending order
Age %>% 
  arrange(dob) # This dplyr's function arranges the dates in ascending order

Age %>% 
  arrange(desc(dob)) # This function arranges the dates in descending order
