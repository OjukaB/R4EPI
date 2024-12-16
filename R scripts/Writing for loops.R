# =====================================================================================================================
# Writing for loops
# =====================================================================================================================
# So, this one took a second to understand but lets begin:
# I am going to use the previous dataframe I created - the df6 to run write a for loop
# For loops operations basically run in iterations (operations run in loops one event at a time)
# Lets begin with a simple for loop
for(i in 1) {
  print(i)
}
# A for loop contains an object and a for() function and a for loop body. The for loop body is going to contain the operation to be run in loops and the object is going to store the results of the 
# operations run in the body; updating its values at each iteration/loop
# The above code chunk is just the for() function and the forloop body of a for loop so it is not a complete for loop.
# The for loop body is enclosed in curly brackets 
# The for() function tells R that you want to create a for loop
# The arguments of a for() that are important to us right now are the index variable (the 'i'), the keyword 'in' and an object or a vector
# The object or the vector to the right of the keyword 'in' is going to specify the number of iterations to be made by the looping operation
# So the code above contains an index variable named 'i' and operations are going to be run on it
# The code above specifies that 1 is assigned to 'i' and the occurence of 1 is  so the iteration is going to run once and the function run on it is printing it. 
# So its result is going to be 1

for(i in c(1, 2, 3, 4)) {
  print(i)
}
# Now, for loops printed the combination of the values in the vector in iterations equal to the length of the vector.
# However, each iteration is returned as a result in different result vectors
for(i in df6) {
  print(i)
} 
# This prints all the values of df6 in 6 different result vectors because df6 has 6 variables

# The results of the for loops however need to be saved in a vector that is not traditionally assigned using the assigning operator in this case
# A separate object needs to be created inside the for loop. The object is created by creating an emppty vector as usually analysis in real case data are run on arbitrary events if the observations
# are multiple making it difficult to count manually
df6_mean <- vector("numeric", length(df6))
# What this does is it creates a vector with spots for values that match the number of variables in df6. (confirm in the global environment)
# The vector is now ready for use and update with each loop:

df6_mean <- vector("numeric", length(df6))

for(i in seq_along(df6)) {
  df6_mean[[i]] <- mean(df6[[i]])
}
df6_mean
# Thats correct however the df6 contains a lot of missing values so most of my results are NA. Let me fix that:
df7 <- df6 %>% 
  filter(
    if_all(
      .cols = everything(),
      .fns = ~ !is.na(.x)
    )
  )
# Let me try the for loops with the values of the df7 then:
df7_mean <- vector("numeric", length(df7))
# The vector() is considered a logical vector by default so thats why the first argument, the 'mode ='  specifies numeric above
for(i in seq_along(df7)) {
  df7_mean[[i]] <- mean(df7[[i]])
}
df7_mean
# Voila! Thats a fully functional for loop

# Lets take it that I want to use this for loop in other R programs; I can embed the functional for loop into a function:
multi.means <- function(data) {
  result <- vector("numeric", length(data))
  
  for(i in seq_along(data)) {
    result[[i]] <- mean(data[[i]])
  }
  result
}
# Let me test out the new function with the newly created for loop embedded in it and see if it works with another data frame:
multi.means(demographics.4)
# Yes it does!

# If I want my returned mean results to have more info similar to descriptions returned by across():\
df7_mean <- vector("numeric", length(df7))
for(i in seq_along(df7)) {
  df7_mean[[i]] <- mean(df7[[i]])
  names(df7_mean[[i]]) <- paste0(names(df7)[[i]], "_mean") # I have told the for loop to name the results of each iteration by pasting the name of the column names of the df7 variables followed by "_mean"
}
df7_mean
# I do not understand why It does not label the results of each iteration even after specifying what I want it to return
# Anyway more description for the iterated results can be achieved by:
df7_mean %>% 
  as.list() %>% 
  as_tibble()
# It doesnt work because the previous code to name each iteration apparently doesnt seem to have gone through

# Functions and for loops do come in handy when using multiple data frames because analysis using across() and summarise() doesnt work with multpile data frames
# Lets say I want to import different sheets from an excel workbook. Without functions or for loops, this is how it is done:
library(readxl)
Houston <- read_excel(
  "C:/Users/ADMIN/Downloads/city_ses.xlsx",
  sheet = "Houston") %>% 
  print()
# And the same done for all the other sheets in the workbook. Now imagine there were 20 sheets to import. I dont need all of that repetition
# Hence, for loops and functions:
import_cities <- function(sheet) {
  df <- read_excel(
    "C:/Users/ADMIN/Downloads/city_ses.xlsx",
    sheet = sheet)
}
import_cities("Atlanta") %>% print()
# That reduces the amount of repetition if I am to import numerous sheets

# However, I find for loops to be even more helpful in this situation:
path <- "C:/Users/ADMIN/Downloads/city_ses.xlsx" # This creates a vector that houses the file path of the workbook to avoid typing it over and over again

sheets <- excel_sheets(path) # This creates a vector with the names of all the sheets in the workbook
# Now lemme create the for loop:
for(i in seq_along(sheets)) {
  new <- tolower(sheets[[i]]) # change the characters to all lower because thats apparently easier to call later
  assign(new, read_excel(path, sheet = sheets[[i]]))
}
# The assign() function assigns objects to the global environment. So, I wanted to assign the imported sheets as separate objects to the environment and the assign() function did exactly that
# Its first argument is 'x =' and is passed with the name to be given to the object being created. 
# The second argument is 'value =' that is meant to assign values to the name(s) created
# So above, I wanted the new objects to be names the names of the sheets that was called by the code above it and then assigned to it their specific values
# So now to call a data frame created from importation of the workbook is just going to need typing of the name of the sheet:
charlotte

# Okay, lemme try for loops in data management. The operations I want to do are similar to the one I already did when writing functions to highlight matching columns of the joined data frame people
# Only now I want to embed the function into a for loop that is going to make it easier to work across multiple data sets
# I will first begin with writing a for loop that just names columns:
cols <- c("name_first", "name_last", "street")

for(i in seq_along(cols)) {
  col1 <- paste0(cols[[i]], "_1")
  col2 <- paste0(cols[[i]], "_2")
  colmatch <- paste0(cols[[i]], "_match")
}
# The above codes creates "name_first_1"..."2" and "match" for each iteration.
# So now, to introduce values of the people data frame into it, this is how I do it:
for(i in seq_along(cols)) {
  col1 <- paste0(cols[[i]], "_1")
  col2 <- paste0(cols[[i]], "_2")
  colmatch <- paste0(cols[[i]], "_match")
  print(people[[col1]])
}
# And the same for the '_2's. The '_match' is not yet in the data frame people and so its inclusion in the above for loop is just to create the new '_match' columns 
# So, to create a full comparison, I will use my function is_matching() like so:
for(i in seq_along(cols)) {
  col1 <- paste0(cols[[i]], "_1")
  col2 <- paste0(cols[[i]], "_2")
  colmatch <- paste0(cols[[i]], "_match")
  people[[colmatch]] <- is_matching(people[[col1]], people[[col2]])
}
# And this does exactly the same matching that my is_matching() function did before only now in a for loop that makes it more robust  as it is flexible enough to be used across different programs

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Using for loops for analysis
library(dplyr)
library(tidyr)
# I would want to create a data frame that has the count of all the categories in all the categorical variables in the town.3 data frame
# Using across() doesnt work because the different categorical variables in town.3 (Age.group, Sex, Health) have different numbers of rows hence the mismatch would return an error
# I can use for loops to create an empty data frame however I want then fill in the values after to counter that

# Lemme create a data frame:
cat_table <- tibble(
  variable = vector("character"), 
  category = vector("character"), 
  n        = vector("numeric")
) 

# Then I shal create  the for loop:
for(i in c("Age.group", "Sex", "Health")) {
  cat_stat <- town.3 %>% 
    count(.data[[i]]) %>%  # So, I cannot key in 'i' like usual because it will not work in this case as it will look for the values passed to it as objects in the global environment
    mutate(variable = names(.)[1]) %>% 
    rename(category = 1)
  # Then I just have to merge the results of the for loop function above with the empty data frame I created
  cat_table <- bind_rows(cat_table, cat_stat)
}
# '.data' is a special key that tells the for loop that the values passed to i are not objects but rather variables in the object passed in the pipeline being created
# The (.) refers to the results that immediately preceed the code line it has been written on
cat_table
# And voila! I shall however expound later as I dont fully understand it at this moment

# So now, to make this usable across different data sets, the for loop can be embedded into a function:
cat_stat <- function(data, ...) {
  cat_table <- tibble(
    variable = vector("character"), 
    category = vector("character"), 
    n        = vector("numeric")
  ) 
  for(i in c(...)) {
    stats <- data %>% 
      count(.data[[i]]) %>% # Use .data to refer to the current data frame.
      mutate(variable = names(.)[1]) %>% # Use . to refer to the current data frame.
      rename(category = 1)
    
    # Here is where we update cat_table with the results for each column
    cat_table <- bind_rows(cat_table, stats)
  }
  # Return results
  cat_table
}

# Let me see if the function will return similar results
cat_stat(town.3, "Age.group", "Sex", "Health")
# Yes it does! 
cat_stat(Snail_field.2, "Species", "Location")
# It works. So the function above can be used to get the count of observations of values (categories) in multiple categorical variables in a data frame regardless of
# mismatching numbers of observations for the different categories 
