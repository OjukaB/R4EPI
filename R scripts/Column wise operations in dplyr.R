# ============================================================================================================================
# Column-wise operations in dplyr
# ============================================================================================================================
library(dplyr, warn.conflicts = FALSE)

set.seed(17)
df6 <- tibble(
  ID = 1:10,
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10),
  w = rnorm(10),
  v = rnorm(10)
)
# If i wanted to do the mean of the columns with the knowledge I have so far:
df6 %>% 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    mean_z = mean(z),
    mean_w = mean(w),
    mean_v = mean(v)
  )
# That was a lot of repetition. Now imagine I had 50 column to get means for? 
# Say hello to the across() function of the dplyr:
df6 %>% 
  summarise(
    across(
      .cols = c(x:v),
      .fns = mean,
      .names = "{col}_mean"
    )
  )
# I really dont understand why it is returning an error saying that that values passed to the '.fns =' argument should be a function(s)
# and the same error doesnt show in the book with the same codes!

# Anyway, I shall continue regardless of the errors:
# Normally real case datasets most of the time include missing data:
df6$x[4] <- NA_real_
df6$y[8] <- NA_real_
df6$w[2] <- NA_real_
df6$v[10] <- NA_real_
# Here is how I would tell the across() function to run mean operations on across the columns without the missing values
# because as we all know, running mean with missing values will just return NA
df6 %>% 
  summarise(
    across(
      .cols = c(x:v), # This argument outlines the columns to be operated on
      .fns = mean, # This argument outlines the function(s) to be executed in the columns highlighted above
      na.rm = TRUE, # Now this is the '... =' argument and it is where the arguments of the functions outlined above to be executed are included
      .names = "{col}_mean" # This argument is meant to specify what the names of the new columns with the results of the functions run are going to be labelled
    )
  )
# Notice how the '... =' argument didnt actually get typed out and that the argument of the function highlighted in the '.fns =' argument is what is typed directly. Yeah.
# Lemme talk about a few special keywords:
# In the '.ma,es =' argument above, I used the special keyword '{col}'. Its function is to basically tell R to use the column names of the variables as it was 
# originally in the df6 data frame.
# So '{col}_mean' means that I want the function to assign the original variable name first followed by an underscore then the word "mean" as the new names of the 
# columns to be formed with results of the operations run across the variables.

# Another special keyword that I want to introduce is the '{fn}'. This tells the across() function to use the name assigned to the name-function pair in the '.fns =' argument
# For it to work there needs to be a list of name-function pairs in the '.fns =' argument:
df6 %>% 
  summarise(
    across(
      .cols = c(x:v),
      .fns = list(meanof = mean),
      na.rm = TRUE,
      .names = "{fn}_{col}"
    )
  )
# In the list(meanof = mean) I have passed to the .fns argument, the 'meanof' is the name I am labelling the function as and the following 'mean' that I passed to the name 
# is the actual function to be run across the variables.
# '{fn}_{col}' basically tells the function to name the columns with the results of the function as the name I have assigned to the functions to be run followed by the 
# original names of the variables in the df6 data frame. So if this code wasnt glitching as it is right now, then the resulting columns would have turned out like this:
# 'meanof_x', 'meanof_y', 'meanof_z',.... 

# There is a third syntax to the '.fns =' argument called the purr style lambda:
df6 %>% 
  summarise(
    across(
      .cols = c(x:v),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{col}_mean"
    )
  )
# That is the expected format of writing with this third syntax. The function passes the columns passed in the .cols argument to the '.x' argument in this format
# of the function being run.
# The na.rm of the mean() function has changed to TRUE. I do not know why it has but it seems to work well and passing FALSE to it returns values only for the z column alone
# the rest return NA. I do not understand why as the argument passed with FALSE is meant to exclude missing values.
# However though the other two syntaxes for writing functions in the .fns argument of the across() function werent working but this one does! I do not yet understand why.

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# across() with mutate()
# Remember the conditional operations I ran previously on the data frame demographics.4 where 7/77/777...and 9/99/999... represented missing values:
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
# Lemme some columns:
demographics.4 <- demographics.4 %>% 
  mutate(
    education = c(2, 3, 1, 1, 9, 1, 1, 1, 3, 7, 7, 2, 3, 4, 3),
    social.class = c(2, 3, 1, 1, 1, 9, 9, 1, 2, 7, 3, 3, 7, 2, 1)
  )
# Normally, you would use the case_when() function that is pretty long, or the if_else() function to recode the 7s and 9s to NA:
demographics.4 %>% 
  mutate(
    race = if_else(race == 7 | race == 9, NA_real_, race),
    hispanic = if_else(hispanic == 7 | hispanic == 9, NA_real_, hispanic),
    education = if_else(hispanic == 7 | education == 9, NA_real_, education),
    social.class = if_else(social.class == 7 | social.class == 9, NA_real_, social.class)
)
# That is a lot of repetition and it is definitely retyping the same code more than twice that is one of the justifications to look for a means of reducing repetition
# Repetition also introduces possibilities of typos in the code. ie: the results of the above code still has the 7s of the column education not recoded
# which is because there was a mishap in the typing of the code above.
# Now imagine if the columns to be edited with the same code were 50. Damn!
# The across() function can come to the rescue in both instances:
demographics.4 %>% 
  mutate(
    across(
      .cols = c(-Id, -Age),
      .fns = ~ if_else(.x == 7 | .x == 9, NA_real_, .x) # Basically, if columns passed to the .cols above are 7 or 9 recode it to NA, if not, return the values that were initially there
    )
  )
# Voila! That is a more succint code that is almost completely typo and repetition immune.

# Let me try again with a different function:
set.seed(23)
drug_trial <- tibble(
  id           = 1:10,
  se_headache  = sample(0:1, 10, TRUE),
  se_diarrhea  = sample(0:1, 10, TRUE),
  se_dry_mouth = sample(0:1, 10, TRUE),
  se_nausea    = sample(0:1, 10, TRUE)
)
# To factor the 0s and 1s to NOs and YESes:
drug_trial %>% 
  mutate(
    se_headache_f  = factor(se_headache, 0:1, c("No", "Yes")),
    se_diarrhea_f  = factor(se_diarrhea, 0:1, c("No", "Yes")),
    se_dry_mouth_f = factor(se_dry_mouth, 0:1, c("No", "Yes"))
  )
# However, with across() function:
drug_trial %>% 
  mutate(
    across(
      .cols = -id,
      .fns = ~ factor(.x, 0:1, c("No", "Yes")),
      .names = "{col}_f"
    )
  )

# across() with filter()
# So, apparently as of dplyr's version 1.0.4, the usage of across() is discouraged with the filter() function. The arguments passed to the across() function are however
# used with the if_any() and if_all() functions of dplyr in order to properly substitute the across() function
df6 %>% 
  filter(
    if_all(
      .cols = c(x:v),
      .fns = ~ !is.na(.x)
    )
  )
# This tells the filter() function to keep the rows that do not have any missing value on all the columns passed to the .cols argument
df6 %>% 
  filter(
    if_any(
      .cols = x:v,
      .fns = ~!is.na(.x)
    )
  )
# This tells the filter() function to keep the rows that do not have a missing value on at least one column. All the rows 
# in the df6 have at least two values that are not NA so this code chunk returns all the rows