# =====================================================================================================================================================================
# purrr package
# =====================================================================================================================================================================
library(purrr)
# I shall focus on reducing repetition using a purrr function called map()
# Most R users prefer it to for loops as it does they do the same tasks whilst the map functions additionally appear less complex relatively
# So, the map functions together with base R's apply functions are commonly referred to as the functional approaches to iterative functions
# I shall use the df6 data frame to compare the mean functions that were previously carried out with the for loops and across functions
df6_means <- map_dbl(
  .x = df6, # Argument passed with a list, data frame or a vector
  .f = mean # Second argument is passed with the fuction to be run across the dataset(s) mentioned above
)
# It returns an error, I dont know why. Myabe my data? I'll look into it later. But you get the point yeah?

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Using purrr for data transfer:
library(readxl)

path <- "C:/Users/ADMIN/Downloads/city_ses.xlsx"
# I shall use purrr's walk() function that essentially doesnt do anything to the data set
walk(
  .x = excel_sheets(path),
  .f = function(x) {
    new_nm <- tolower(x)
    assign(new_nm, read_excel(path, sheet = x), envir = .GlobalEnv)
  }
)
charlotte
houston
atlanta
# Notice the 'envir = .GlobalEnv' argument. Apparently functions usually create their own little enclosed environments, but this argument tells R to assign the imported
# data frames into the global environment instead

# Now, this is not the only way that those data sets could have been imported by the other map functions. Lets take a look:
list_of_df <- map(
  .x = excel_sheets(path),
  .f = ~ read_excel(path, sheet = .x)
)
str(list_of_df)
# It is a bit unconventional how the general map() has imported the data, and it would definitely require management:
houston1 <- list_of_df[[1]]
# And the same for the other sheets. Thats a lot of repetition though

# The map_dfr() can also be used. However this function returns a data frame that is as a result of row binding of results:
bind_rows(list_of_df)
# Lets see if the results returned will be similar:
cities <- map_dfr(
  .x = excel_sheets(path),
  .f = ~ read_excel(path, sheet = .x)
)
cities
# Yep! But it ofcourse will require cleaning up and management of the imported merged data frame into different objects in the global environment.
# Thats a lot of repetiton/added coding of course and so in this case, the walk() function would be easier

# So, the walk() function returns exactly what is passed to the .x argument unchanged:
return_by_walk <- walk(
  .x = excel_sheets(path),
  .f = function(x) {
    new_nm <- tolower(x)
    assign(new_nm, read_excel(path, sheet = x), envir = .GlobalEnv)
  }
)
return_by_walk
# # This returns the names of the sheets alone because the function excel_sheets() returns only the names of the different sheets in an excel workbook
# # It doesnt matter that I have assigned the values of the different sheets to the names and saved them as different data frames in the global environment in the .f argument
# # Do not be mistaken however, cause the data frames are actually being imported by the anonymous functions passed to the .f argument (for some reason, here functions
# # passed to the .f argument are called anonymous functions. Donno why) but as a "side-effect" of the operations occuring in the walk() function. Yeah, I also dont totally get it!
# 
# # The code could be made more concise by using the purrr-lambda style syntax:
# walk(
#   .x = excel_sheets(path),
#   .f = ~ assign(tolower(.), read_excel(path, sheet = .), envir = .GlobalEnv)
# )
# houston
# charlotte
# atlanta
# 
# # --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Using purrr for data management
# 
# # I'll begin with addition of NA at multiple positions across a data frame
# That can be done through:
df6$x[4] <- NA_real_
df6$y[8] <- NA_real_
df6$w[2] <- NA_real_
df6$v[10] <- NA_real_

# However, this code chunk includes a lot of repetition that we do not want and so I shall introduce NA using functions:
add_na <- function(vect, pos) {
  vect[[pos]] <- NA
  vect
}
add_na(df6$z, 1)
# Works perfectly!
# I can apply it in the data frame above in the following way:
df6 %>% 
  mutate(
    x = add_na(x, 4),
    y = add_na(y, 8),
    w = add_na(w, 2),
    v = add_na(v, 10),
    z = add_na(z, 1)
  )
# Works perfectly, however, there is still a lot of repetition in the code chunk
# Thats where purrr comes in. Purrr's map functions to be specific. So, I know that map_dfr() and map_dfc() functions return a data frame. 
# However, when troubleshooting, it is safer to use map() function as it is generic enough to return a list that can be a lot of things in case I do not know 
# the exact format of data is going to be returned. If I use the specific map functions and the result is not what R expects to be returned by that function, it is going to return an error
# Hence I shall use a map() function to solve that:
map(
  .x = df6,
  .f = add_na, 2
)
# I could also use purrr lambda style:
map(
  .x = df6,
  .f = ~ add_na(.x, 3)
)
# The results brought back by the plain map() is always going to be a list, and so upon glancing at the results, I can see that the list is composed of elements that are
# vectors of numbers that are all of the same length. And so from that I am able to tell that my dataset is data frame and so I can convert the list into it as such:
map(
  .x = df6,
  .f = ~ add_na(.x, 3),
) %>% 
  as.data.frame()
# That does it, but it can also be done using a specific data frame result returning map function - map_dfc():
map_dfc(
  .x = df6,
  .f = ~ add_na(.x, 2)
)
# It is important to note that map_dfr() also returns a data frame that we wanted but it not applicable in this case as I wanted to combine the elements of the list returned
# by the plain map() function into columns to create a data frame that is done specifically by map_dfc(). map_dfr() on the other hand creates a data frame through combining rows

# The code above works however it is not what I want exactly as the function I passed to the purrr function was being iterated over a single position across all columns
# I want to add NA across the data frame at different positions. That is where the map2 variants of the map functions come in:
map2_dfc(
  .x = df6,
  .y = c(2, 4, 8, 1, 2, 10),
  .f = ~ add_na(.x, .y)
)
# The map2_dfc(), takes two values to reiterate over instead of one - the specific data frame column and the specific position on the column being worked on 
# Thats exactly what I wanted. However, in real case data sets that have tens, hundreds, thousands of variables and/or observations, it might be difficult to type out each position
# and so, I can use a randomizing code instead:
set.seed(141)
map2_dfc(
  .x = df6,
  .y = sample(1:10, 6, FALSE),
  .f = ~ add_na(.x, .y)
)
# Thats good!
# However, in a case that the addition of the NA task wasnt something that I wanted to do over and over, then there would have not been any need to create a named function ahead of time:
map2_dfc(
  .x = df6,
  .y = c(2, 4, 8, 1, 2, 10),
  .f = ~ function(vect, pos) {
    vect[[pos]] <- NA
    vect
  }
)
# I could have just create an anonymous (unnamed) function directly into the map2 function. However this code returns an error. I do not understand why.
# The purrr-style lambda can also be used. Let me see if that works:
map2_dfc(
  .x = df6,
  .y = c(2, 4, 8, 1, 2, 9),
  .f = ~ {
    .x[[.y]] <- NA
    .x
  }
)
# AAh! This formation works perfectly!

# The second example I'd like to use is the matching of values across a data frame.
# I am going to use the example I used in the previous chapters that merged the people 1 & 2 data frame then created dummy variables to indicate values that were matching
# I initially used a for loop to do that task. Using purrr in one way actually just requires copying the same codes I used to create the for loop to the anonymous function I am going 
# to pass to the .f argument of the map2 function:
map_dfc(
  .x = c("name_first", "name_last", "street"),
  .f = function(col, data = people) {
    col_1 <- paste0(col, "_1")
    col_2 <- paste0(col, "_2")
    new_nm <- paste0(col, "_match")
    data[[new_nm]] <- data[[col_1]] == data[[col_2]]
    data[[new_nm]] <- if_else(is.na(data[[new_nm]]), FALSE, data[[new_nm]])
    data[c(col_1, col_2, new_nm)]
  }
)
# Honestly, this one I havent understood

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Using purrr for analysis

# Analysis of continuous variables
# Remember this function I wrote to return stats about town.3?
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
# I shall use it to return the stat results of all the columns in one data frame using a purrr function:
map_dfr(
  .x = quos(Age, Weight, Height),
  .f = Summary_stats
)
# The quos() function tells R to look for the column(s) highlighted in it in a data frame that is previously highlighted in the dplyr verb used instead of looking for it in the global environment
# I would however like to see the name of the column aside the results:
Summary_stats <- function(var) {
  town.3 %>%
    summarise(
      variable = quo_name(var), # Captures the variable being worked on and adds it onto the output
      n.miss = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      std = sd({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE)
    )
}
map_dfr(
  .x = quos(Age, Weight, Height),
  .f = Summary_stats
)
# Perfect!
# But I want to generalise the function so that it can analyse other data frames aside from town.3:
Summary_stats <- function(data, var) {
  data %>%
    summarise(
      variable = quo_name(var), # Captures the variable being worked on and adds it onto the output
      n.miss = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      std = sd({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE)
    )
}
# Now it can be used to analyse any continuous variable from any data frame:
map_dfr(
  .x = quos(Age, Weight, Height),
  .f = Summary_stats, data = town.3
)
map_dfr(
  .x = quos(x, y, z, w, v),
  .f = Summary_stats, data = df6
)
# Voila!

# Analysis of categorical variables
# Again, I will just copy most of the code that I had used to analyse categorical varibales using for loops befor into my purrr analysis of the town.3 data set


cat_table <- tibble(
  variable = vector("character"), 
  category = vector("character"), 
  n        = vector("numeric")
) 
for(i in c("Age.group", "Sex", "Health")) {
  cat_stat <- town.3 %>% 
    count(.data[[i]]) %>%  
    mutate(variable = names(.)[1]) %>% 
    rename(category = 1)
 
  cat_table <- bind_rows(cat_table, cat_stat)
}
cat_table
# This was the code. I will use it as such:
map_dfr(
  .x = c("Age.group", "Sex", "Health"),
  .f = function(x) {
    town.3 %>% 
      count(.data[[x]]) %>% 
      mutate(variable = names(.)[1]) %>% 
      rename(category = 1) %>% 
      select(variable, category, n)
  }
)
# Also using tidy evaluation's quo()
map_dfr(
  .x = quos(Age.group, Sex, Health), # Change c() to quos()
  .f = function(x) {
    town.3 %>% 
      count({{ x }}) %>% # Change .data[[x]] to {{ x }}
      mutate(variable = names(.)[1]) %>% 
      rename(category = 1) %>% 
      select(variable, category, n)
  }
)

# But now I want to generalise it so thaat I can use the purrr function in other data frames:
map_dfr(
  .x = quos(Age.group, Sex, Health),
  .f = function(x, data = town.3) {
    data %>% 
      count({{ x }}) %>% 
      mutate(variable = names(.)[1]) %>% 
      rename(category = 1) %>% 
      select(variable, category, n)
  }
)
map_dfr(
  .x = quos(Species, Location),
  .f = function(x, data = Snail_field.2) {
    data %>% 
      count({{ x }}) %>% 
      mutate(variable = names(.)[1]) %>% 
      rename(category = 1) %>% 
      select(variable, category, n)
  } 
)
# Voila!