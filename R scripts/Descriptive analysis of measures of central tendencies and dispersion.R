#Measures of central tendency
#Mean
library(tibble)

demographics.2 <- tribble(
  ~ID, ~Age, ~Sex,
  115, 26, 1,
  116, 20, 1,
  117, 18, 4,
  118, 22, 2,
  119, 23, 1,
  120, 22, 2,
  121, 2500, 1,
  122, 18, 1,
  123, 21, 2,
  124, 25, 1,
  125, 28, 5,
  126, 19, 2,
  127, 20, 2,
  128, 27, 3,
  129, 24, 1,
  130, 22, 2
)
mean(demographics.2[["Age"]])
#Could also just use the (demographics.2$Age) way

#Median
median(demographics.2[["Age"]])

#Creation of mode function

mode_val <- function(x) {
  value_counts <- table(x)
  #This code counts the number of occurrences for each value of x
  max_count <- max(value_counts)
  #This gets the maximum number of times any value is observed
  index <- value_counts == max_count
  #This creates an index vector that corresponds with the count values similar to the maximum count value
  unique_values <- names(value_counts)
  result <- unique_values[index]
  #This gets the values observed as many times as the max value count using the indices
  no_mode <- length(value_counts) == length(result)
  #I guess this code is for returning the mode values if you have any?
  if (no_mode) {
    result <- NA
  }
  #This code changes the result to NA if there is no mode in the dataset
  result
}

#Lets take the generated code for a test run then
mode_val(demographics.2[["Age"]])
#I am sorry but I understand close to naught from this code chunk above used to create this mode function. The book says that I should just copy paste it for now and await the chapter that'll teach me exactly what these codes mean and do. So I guess until then!

#Descriptive vomparison of all the measures of central tendencies

demographics.2 %>% 
  summarise(
    min_age = min(Age),
    mean_age = mean(Age),
    median_age = median(Age),
    mode_age = mode_val(Age) %>% as.double(),
    max_age = max(Age)
  )
#Here is where the importance of doing basic descriptive analysis before analysis is important.
#It help find errors in data as seen in the information given above by our code that returned the max age for our dataset to be 2500 and the mean age to be 177. I'm not sure about you but personally I have never met anyone as old as 2500 or even 177
#Furthermore, none of the observations recorded anything over 30 except for that one extreme observation.
#Such kind of information will tell me that the raw data is not right. Plus, the median and mode that are usually not very susceptible to extremes like the mean will give more accurate results relatively. So you can always fall back to those two if the mean does this
#==============================================================================================================================================================================================================================================================================

#Real data will often have missing data. So lets incorporate some in our simulated data

demographics.2 <- demographics.2 %>% 
  mutate(Age = replace(Age, c(2, 12), NA))
demographics.2
mean(demographics.2$Age)
#Trying to do mean on the same variable returns an NA right now because R perceives NA as data exists but just not available and not as missing data translating to non-existent data
#So R will not exclude the NA unless I tell it to

demographics.2 %>% 
  filter(!is.na(Age)) %>% 
  summarise(
    min_age = min(Age),
    mean_age = mean(Age),
    median_age = median(Age),
    mode_age = mode_val(Age) %>% as.double(),
    max_age = max(Age)
  )
#That is method 1

demographics.2 %>% 
  summarise(
    min_age = min(Age, na.rm = TRUE),
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    mode_age = mode_val(Age) %>% as.double(),
    max_age = max(Age, na.rm = TRUE)
  )
#Method 2
#==================================================================================================================================================================================================================================================================================
#Here is another package created by Prof. Brad Canell to give descriptive statistics to measures of central tendencies

install.packages("meantables")
library(meantables)

demographics.2 %>% 
  filter(!is.na(Age)) %>%
  mean_table(Age)

#====================================================================================================================================================================================================================================================================================
#Descriptive analysis of measures of dispersion
library(dplyr)

#Let me first correct the data
demographics.2 <- demographics.2 %>% 
  mutate(Age = replace(Age, c(2, 7, 12), c(26, 25, 22)))

demographics.2 %>% 
  summarise(
    min_age = min(Age, na.rm = TRUE),
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    mode_age = mode_val(Age) %>% as.double(),
    max_age = max(Age, na.rm = TRUE)
  )
demographics.2 %>% 
  summarise(
    min_age = min(Age),
    mean_age = mean(Age),
    max_age = max(Age),
    range_age = max_age - min_age
  )

#Now lemme get the variance of the data
var(demographics.2$Age)

#Comparison of measures of dispersion

install.packages("purrr")
library(purrr)

demographics.3 <- tibble(
  age.group.1 = rep(25, 20),
  #The rep() function's first value is the value you want to create repeatedly and the second value is the number of times you want to repeat it
  age.group.2 = c(rep(21, 10), rep(19, 10)),
  age.group.3 = c(rep(18, 10), rep(28, 10)),
  age.group.4 = c(rep(21, 5), rep(23, 5), rep(27, 5), rep(26, 5)),
  age.group.5 = c(rep(20, 10), rep(22, 10))
)

#Lemme create a dataframe containing summarised data of measures of dispersion

tibble(
  Variable = names(demographics.3),
  Mean = purrr::map_dbl(demographics.3, mean),
  Variance = purrr::map_dbl(demographics.3, var),
  Std.deviation = purrr::map_dbl(demographics.3, sd)
)
