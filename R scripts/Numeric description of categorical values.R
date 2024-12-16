#Numeric description of categorical values
#This can be done by factorization. Let us simulate some data and tests this out

library(dplyr)
demographics <- tibble(
  ID = c("100", "101", "102"),
  Age = c(22, 24, 21),
  Sex = c(2, 1, 1)
)
#With this information it is impossible for one to figure out what the values for the categorical variable 'sex' mean. So we add description to it using factors

demographics <- tibble(
  ID = c("100", "101", "102"),
  Age = c(22, 24, 21),
  Sex = c(2, 1, 1),
  Sex.chr = c("male", "female", "female")
)
#This added a character vector description of the sex variable, however, R takes it as an independent variable with no relation to the sex variable which is not right
#To mitigate this, we can use factors like so:

demographics <- demographics %>% 
  mutate(
#The mutate() function adds a new variable into a dataset
    Sex.f = factor(
      x = Sex,
      levels = 1:5,
      labels = c("female", "male", "non-binary", "lesbian", "gay")
    )
  )
demographics

#Now there is a difference between the two additional variables for description that we have added. One is a factor of an existing variable while another is independent

as.numeric(demographics$Sex.chr)
#This gives an error telling me that the variable 'Sex.chr' is not a numeric variable and it should because it was introduced as a representative of a numeric variable and not as an independent variable

as.numeric(demographics$Sex.f)
#This code shows us that the variable 'Sex.f' is a factor of an existing variable as it should!

#Another difference of the two introduced variables is the arrangement in which R will display results at some point
#R will also not know all the involved categories in this dataset if the factoring 'levels' argument was never included in the code chunk

table(demographics$Sex.chr)
#Shows the frequency of categories that were only observed in this datagroup and excluding the rest as they weren't observed

table(demographics$Sex.f)
#This variable however shows the frequency of all available categories in the study and in the order specified above

#It is also possible to coerce character variables into factors

demographics <- demographics %>% 
  mutate(
    Sex.chr.f = factor(
     x = Sex.chr,
     levels = c("female", "male", "non-binary", "lesbian", "gay")
     )
  )
demographics
#Now R registers the introduced variable 'Sex.chr.f' as a factor to the  existing variable 'Sex.chr'
#==============================================================================================================================================

demographics.1 <- tibble(
  ID = c("103", "104", "105", "106", "107", "108", "109", "110", "111", "112", "113", "114"),
  Age = c(20, 25, 22, 19, 19, 20, 24, 22, 25, 23, 23, 23),
  Sex = c(1, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2),
  Sex.f = factor(Sex, 1:5, c("female", "male", "non-binary", "lesbian", "gay")
#The factor() function can be included directly into the tibble when creating the initial data frame without need for us using the mutate() function later to create a new column
                 )
)
demographics.1

#Methods of estimating frequency of a value in a dataset
#Base R method

table(demographics.1$Sex)

#The gmodel method
install.packages("gmodels")
library(gmodels)

gmodels::CrossTable(demographics.1$Sex)

#The tidyverse method

demographics.1 %>% 
  group_by(Sex.f) %>% 
#The function group_by() splits the larger original dataframe into a smaller one that contains the individual values of a varible separated with information about the no. of rows each value has appeared on
  summarise(count = n())
#The summarise() function condenses the large dataframe information into summary statistics but it doesnt give much information on its own. It needs additional information on what info in the dataframe it is supposed to summarize
#Thats where the n() function comes in. It counts the rows of a specific value has been observed in or just gives the total number of rows in the entire dataframe if no specification has been provided
#I have passed the character vector-count to the n() function as the previous code group_by() would have just named the count of n variable as 'n()'
demographics.1 %>% 
  count(Sex.f)
#This is another shortcut to what I have done above that also creates a dataframe from a dataframe
#=======================================================================================================================================================================================================================================

#Calculation of percentages
#Calculation of the percentages of each gender

demographics.1 %>% 
  count(Sex.f) %>% 
  mutate(Percentages = n/sum(n)*100)
#I have added another variable containing the percentages of the genders and named it 'Percentages' then passed values of n divided by the total number of values of n in the dataframe using sum() function then multiplied it by 100

#Often will often find that there are missing values. Let me add some missing values in our dataframe

demographics.1 <- demographics.1 %>% 
  mutate(Sex.f = replace(Sex, c(3, 4, 10), NA))

#Now I will calculate percentages with the included changes

demographics.1 %>% 
  count(Sex.f) %>% 
  mutate(Percentages = n/sum(n)*100)
#This creates another category for missing values and also includes its percentages along with the rest
#But I can also get R to only calculate values for non-missing data alone as is mostly done in real study analysis like so:

demographics.1 %>% 
  filter(!is.na(Sex.f)) %>% 
  count(Sex.f) %>% 
  mutate(Percentages = n/sum(n)*100)
#The filter() function help eliminate analysis of the missing data. The filter() function itself just tells R to keep specific rows in a dataframe. When the '!' is added in the code, it tells R to keep all the rows except the one specified in this function

#In order to round of the decimal places in the results returned by the percentages, do this
demographics.1 %>% 
  filter(!is.na(Sex.f)) %>% 
  count(Sex.f) %>% 
  mutate(Percentages = n/sum(n)*100 %>% round(1))
#Or a shortcut to that rounding off function is to just add ',2' after the percentage calculation code
#====================================================================================================================================================================================================================================================================

#Prof. Brad Canell created a package with the functions above that can also be integrated into a dplyr pipeline
install.packages("freqtables")
library(freqtables)

demographics.1 %>% 
  filter(!is.na(Sex.f)) %>% 
  freq_table(Sex.f)
#It has additional information as compared to the previous ones
