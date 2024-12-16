# ============================================================================================================================
# Restructuring data frames
# ============================================================================================================================

install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggplot2)

# It is important to note that most of the longitudinal studies and analyses that can be done for its data require data frames to be structured in the person-period/long format
# --------------------------------------------------------------------------------------------------------------------------------
# Let's simulate some data
babies <- tibble(
  id       = 1001:1008,
  sex      = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3  = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6  = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9  = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19)
)
# Say hello to the pivot_longer() function of the tidyr package
babies %>% 
  pivot_longer(
    cols = starts_with("weight_"), # This argument outlines the columns that are to be pivoted
    names_to = "months",
    names_prefix = "weight_",
    values_to = "Weight"
  )
# And that converts the babies data frame from a person-level format to a person-period format

# Lets break it down:
# Let me begin with the 'names_to =' argument. The argument is meant to give a name for a new column to be created that is going to contain data about the column names of the 
# previous columns that are to be pivoted. Lets take a look at how the new data frame would look like without editing this argument:
babies %>% 
  pivot_longer(
    cols = starts_with("weight_")
  )
# The first new column that is to have information about the variability of interest in the previous column names that are to be pivoted alone, is labelled "names" which is the default setting

# Now the 'names_prefix =' argument: 
# The value passed to this argument should be a regex that tells R the matching text from the begin of the variable names of each variable to be pivoted that will form the values of the new column created above 'month'
# This is just in attempt of separating important data values on the names of the columns to be pivoted. ie: the original columns to be pivoted above are labelled 'weight_3/weight_6' 
# in real sense there are two significant data values that I want to separate. The first being, 'weight_' and the second '3/6'. The first data value tells that the values in the column are weight values
# but the new column will not have any weight values and shows no representation to the variability of the previous column names that I am trying to express
# The second data values ie: '3/6/9' actually contains info about the variability that I want to express and so that is what I want to remain as the values of this new column I have created
babies %>% 
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "month",
    names_prefix = "weight_"
  )
# That makes it more informative now in a succint manner
# But hey, this is meant to be a regex, so lets complicate things a bit:
babies %>% 
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "month",
    names_prefix = "\\w+_" #This is a regex that tells R to remove any word from the start that is followed by an underscore together with the underscore
)

# The 'values_to =' is straightforward and is passed with the name for the other new column that is now meant to contain the actual values of the original columns to be pivoted

# However, notice that the value type of the newly created 'months' column is chr. Let me coerce iit into an integer. It can be done in one of two ways:
babies %>% 
  pivot_longer(
    cols = starts_with("weight_"),
    names_to = "months",
    names_prefix = "weight_",
    values_to = "Weight"
  ) %>% 
  mutate(months = as.integer(months))
# Or, just directly using the pivot_longer() function:
babies %>% 
  pivot_longer(
    cols = starts_with("weight_"), 
    names_to = "months",
    names_prefix = "weight_",
    names_transform = list(months = as.integer), # This argument does the coercion directly through a list of the name(s) of column(s) to be coerced and what it is to be coerced to
    values_to = "Weight"
  )

# So far I have pivoted values only from a single set of related columns. Let me try from multiple sets of columns:
set.seed(15)
babies <- tibble(
  id       = 1001:1008,
  sex      = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3  = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6  = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9  = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19),
  length_3  = c(17, 19, 23, 20, 18, 22, 21, 18),
  length_6  = round(length_3 + rnorm(8, 2, 1)),
  length_9  = round(length_6 + rnorm(8, 2, 1)),
  length_12 = round(length_9 + rnorm(8, 2, 1)),
)
# I have added another layer of complexity into our dataset by adding another length variable that was also measured in time-varying intervals. Lets pivot bith then!

# The 'names_prefix =' and 'values_to =' arguments cannot be used in the pivoting of multiple column sets because they are used to analyse only one column set (either weights or length)
# So both are dropped and say hello to the 'name_sep =' argument and the '.value' special value that is going to correctly separate the two column sets:
babies.1 <- babies %>% 
  pivot_longer(
    cols = c(-id, -sex), # This tells R that the columns to be pivoted are all except these that are mentioned
    names_to = c(".value", "months"),
    names_sep = "_",
    names_transform = list(months = as.integer)
  )
# So lets break it down: 
# 'name_sep =' argument is used to separate values in the column names at the underscore that are to be pivoted ie: weight_3 is separated into 'weight' and '3' and same for 'length_3' etc.
# The '.values' special value is still a bit confusing to me. But what I understand it does is: separates the value description from the name description
# So, as mentioned before, name_sep separates weight_3 into two character strings: 'weight' and '3'. So, what '.value' does is tell pivot_longer() that the character string before the underscore 
# is the value description that contains the values we want to separate into another column.
# But the special value can also tell the opposite when arranged differently in the name_to argument
babies %>% 
  pivot_longer(
    cols = c(-id, -sex), # This tells R that the columns to be pivoted are all except these that are mentioned
    names_to = c("months", ".values"),
    names_sep = "_",
  )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Now, lemme plot this data
babies.1 %>% 
  mutate(months = factor(months, c(3, 6, 9, 12))) %>% 
  ggplot() +
  geom_point(aes(weight, length, color = months)) +
  labs(
    x = "Weight (Pounds)",
    y = "Length (Inches)",
    color = "Age (Months)"
  ) +
  theme_classic()
# ------------------------------------------------------------------------------------------------------------------------------------------['

# Let me now try converting the longer formats back into the wider formats
babies <- babies.1 %>% 
  pivot_wider(
    names_from = "months", # Column passed here should contain values that are meant to be the names of the new wider columns
    values_from = c("weight", "length") # Column passed here should contain the values of that are meant to be the values for the newly created wider columns
  )

# So far we have only explained the need to restructure data frames based on the type of study the datasets are being generated from (ie: longitudinal study = person-period format)
# However there is a more systematic guidline that can be used in decision making of the correct data frame format to use:
# These are the guidlines outlined by Hadley Wickham whose paper stated the guidlines as:
# 1. Each variable (measurement/characteristic of an observation), should have its own column
# 2. Each observation should have its own row
# 3. Each value should have its own cell
# So far, the examples I have used have demonsttrated the first two guidelines (Guideline 1 = wider format; guideline 2 = longer format)
# Lets take a look at the third guideline:
# Let me try and create a dataframe showing the range of weights for the babies over the months recorded:

babies.weight <- tibble(
  ID = 1001:1008,
  Weight_range = c("9-17", "11-20", "17-24", "16-22", "11-18", "17-26", "16-21", "15-19")
) %>% 
  print()
# This kind of data frame would be considered messy asa there are different values in one cell and this led to R assuming the weight_range is a character vector

# Let me fix that: say hello to tidyr's separate() function!:
babies.weight %>%
  separate(
    col = -ID,
    into = c("min_weight", "max_weight"), # This argument gives names to the new columns that are going to be created to hold the separated values
    sep = "-", # This argument specifies to R the character that is separating the joint values to be separated
    convert = TRUE # This argument coerces the values being separated from chr to integers
  )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Pivoting data is not 100% as it can have holes especially in records that incorporate time-variability.
# Lets take a look at a repoprt recording the number of births in a town between 11/10/2024 and 21/10/2024:
Town_birth_records <- tibble(
  ID = 567:574,
  Date = as.Date(c(20007, 20007, 20010, 20010, 20010, 20015, 20015, 20016))
)
# Let me see the number of recorded births within that period:
Town_birth_records %>% 
  count(Date)
# A total of 8 births were recorded within that period, which is true, but what if we want to get the rate of birth in that town?:
Town_birth_records %>% 
  count(Date) %>% 
  summarise(Birth_rate = mean(n))
# It returns '2' which is wrong because it has given the rate from the four days that births were recorded alone leaving out the other 6 days that this study was carried out for
# So let us introduce the data recorded for the days that no births were recorded. Say hello to the complete() function
Town_birth_records %>% 
  count(Date) %>% 
  complete(
    Date = seq.Date(
      from = as.Date(20007),
      to = as.Date(20017),
      by = "days"
    )
  )
# This includes all the days that this study was carried out for.
# However, it shows 'NA' for the days that zero births were recorded, so it I try to find mean again it will not differ from before because all missing data will be excluded
# Let me then correct that!:
Town_birth_records %>% 
  count(Date) %>% 
  complete(
    Date = seq.Date(
      from = as.Date(20007),
      to = as.Date(20017),
      by = "days"
    ),
    fill = list(n = 0) # This argument fills in all the missing values in the column 'n' with '0'
  )
# So now we can correctly find the rate of births within the study's running period
Town_birth_records %>% 
  count(Date) %>% 
  complete(
    Date = seq.Date(
      from = as.Date(20007),
      to = as.Date(20017),
      by = "days"
    ),
    fill = list(n = 0)
  ) %>% 
  summarise(Birth_rate = mean(n))
# And the correct answer is 0.727!