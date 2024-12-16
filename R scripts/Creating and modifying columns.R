# Creating and modifying columns
# Let me introduce a few more functions to simulate more complex data

Schistosoma_analysis <- tibble(
  ID = rep(1:20, each = 4),
  # I have created 20 participants, each with 4 observations
  Year = rep(0:3, times = 20),
  # I have created a variable showing the year of checkup from baseline to year 3 for all participants individually
  Age = sample(25:50, 20, TRUE) %>% rep(each = 4),
  # I have created a variable showing age for all 20 participants that has randomized values and told R it can repeat a value;
  # Because there are 4 observartions for each individual, I have told R to repaeat the randomized age value for each 4 times
  Drug = sample(c("Placebo", "Pzq"), 20, TRUE) %>% rep(each = 4),
  Blood_poo = if_else(
    Drug == "Placebo",
    sample(c("Yes", "No"), 80, TRUE, c(.5,.5)),
    sample(c("Yes", "No"), 80, TRUE, c(.5, .5))
  ),
  # Created a variable showing randomized yes/no, reported symptoms on a symptom of Schistosomiasis
  Blood_pee = if_else(
    Drug == "Pzq",
    sample(c("Yes", "No"), 80, TRUE),
    sample(c("Yes", "No"), 80, TRUE)
  ),
  Blood_cough = if_else(
    Drug == "Placebo",
    sample(c("Yes", "No"), 80, TRUE),
    sample(c("Yes", "No"), 80, TRUE)
  )
)

Sch.summary <- Schistosoma_analysis %>% 
  group_by(Blood_poo) %>% 
  summarise(
    Count = n()
  ) %>% 
  print()
# ----------------------------------------------------------------------------------------------------------------------------
# Recycling rule
# Recycling rule for tidyverse functions

Schistosoma_analysis %>% 
  mutate(
    Complete = 1
  )

# This is the recycling rule applied in tidyverse functions- that it can only accept a vector having the same size as the number of rows, or of size 1
# As demonstrated below, the recycling rule doesn't work if the values provided in the new variable are not matching the row sizes or 1

Schistosoma_analysis %>% 
  mutate(
    Complete.1 = c(1, 2)
  )
# or:
Schistosoma_analysis$Complete.1 <- c(1, 2)

# However, base R functions like data.frame has different rules for recycling as it can allow usage of vector sizes that are multiples of the 
# size of the existing number of observations
# So let me create a dataframe using base R function from the tibble we created

Schistosoma_analysis.1 <- as.data.frame(Schistosoma_analysis)
# Now lemme exploit recycling rules using vector sizes that are larger than 1 or that doesnt match the size of existing observations

Schistosoma_analysis.1$Complete.1 <- c(0, 1)
# But this only worked because the vector size 2 that I have assigned to the new columnn is a multiple of 80 (the total number of observations)- ie: 80/2 = 40
# It however does not work if the vector size I assign a new column to is not a multiple of 80:

Schistosoma_analysis.1$Complete.2 <- c(0, 1, 2)
# This is so because 80/3 = 26.67

# Let me explore vectorized functions

Values <- 1:10
Values + 1
# This utilizes the recycling rule to exert vectorized functions on all the values of the vector
# But the same concept of multiples of the vector size apply:
Values + c(1,3)
# Works because 10/2=8
Values + c(1, 2, 3)
# Doesn't work because 10/3 = 3.3