# ================================================================================================================================
# Working with multiple dataframes
# ==============================================================================================================================
library(dplyr)

# Combining data frames vertically (adding rows)
Diabetes.study <- tibble(
  Date = c(as.Date("2024-10-21"), as.Date("2024-11-02"), as.Date("2024-11-08"), as.Date("2024-11-11")),
  FBG = c(3.2, 3.5, 8.4, 3.8)
)
# Lets take it that a dataframe with additional FBG records of the rats in the following dates was created separately
records <- tibble(
  Date = c(as.Date("2024-11-15"), as.Date("2024-11-18")),
  FBG = c(3.3, 7.8)
)
# Lemme combine the additional data
Diabetes.study <- Diabetes.study %>% 
  bind_rows(records)
# The bind_rows() however allows combining of as many dataframes as possible:
records.1 <- tibble(
  Date = c(as.Date("2024-11-21"), as.Date("2024-11-26")),
  FBG = c(8.1, 8.4)
)
records.2 <- tibble(
  Date = c(as.Date("2024-11-30"), as.Date("2024-12-05"), as.Date("2024-12-08")),
  FBG = c(8.0, 7.3, 6.1)
)
Diabetes.study <- Diabetes.study %>% 
  bind_rows(records.1, records.2)
# How about combining dataframes with differing columns?:
# Lets say that treatment began to be administered later and began being recorded during records.1:
records.1 <- tibble(
  Date = c(as.Date("2024-11-21"), as.Date("2024-11-26")),
  FBG = c(8.1, 8.4),
  Treatment.gms_weight = c(10, 20)
)
records.2 <- tibble(
  Date = c(as.Date("2024-11-30"), as.Date("2024-12-05"), as.Date("2024-12-08")),
  FBG = c(8.0, 7.3, 6.1),
  Treatment.gms_weight = c(30, 40, 50)
)
Diabetes.study <- Diabetes.study %>% 
  bind_rows(records.1, records.2)
# The additional column is not present for all dataframes; so R keys in 'NA' for the values of the dataframe lacking the new column
# Lets assume that the person keying in the records did not use the arrangement followed by the other dataframes:
records.1 <- tibble(
  Date = c(as.Date("2024-11-21"), as.Date("2024-11-26")),
  Treatment.gms_weight = c(10, 20),
  FBG = c(8.1, 8.4)
)
Diabetes.study %>% 
  bind_rows(records.1)
# It will not matter as the bind_rows() doesnt bind dataframes according to the arrangement but according to the column names
# Let us assume that the columns were renamed in the subsequent records would the binding function infuse it into the dataframe correctly:
records.2 <- tibble(
  Date = c(as.Date("2024-11-30"), as.Date("2024-12-05"), as.Date("2024-12-08")),
  FBG.mmol.l = c(8.0, 7.3, 6.1),
  Treatment.gms.weight = c(30, 40, 50)
)
Diabetes.study %>% 
  bind_rows(records.2)
# Nope, it creates separate columns for all columns with different names
# Fixing it is through renaming though to match the original column names
Diabetes.study %>% 
  bind_rows(
    records.2 %>% 
      rename(
        FBG = FBG.mmol.l,
        Treatment.gms_weight = Treatment.gms.weight
      )
  )
# Easy fix!

# --------------------------------------------------------------------------------------------------------------------------------------
# Combining data frames horizontally (adding variables)
# Combining data frames horizontally by position (matching rows of the different dataframes)
df1 <- tibble(
  Sport = c("football", "swimming", "tennis"),
  Level = c("pro", "beginner", "intermediate")
)
df2 <- tibble(
  Grade = c(5, 8, 4),
  Age = c(11, 14, 10)
)
df2 %>% 
  bind_cols(df1)
# The bind_cols() function only works if the number of rows of the dataframes to be joined are similar

# Combining data frames by key values
# One-to-one relationship
set.seed(11)
df3 <- tibble(
  ID = 100:110,
  Sex = c(sample(1:2, 10, TRUE), NA),
  Age = sample(20:35, 11, FALSE)
)
# Lets say I want to combine this with a data frame that shows their treatments from baseline year:
set.seed(12)
df4 <- tibble(
  ID = c(101, 100, 102, 110, 109, 103, 104, 105, 106, 107, 108),
  Medication = sample(c("placebo", "pzq"), 11, TRUE),
  Treat.date = as.Date(c(16380, 16745, 17110, 17475, 17840, 18205, 18570, 18935, 19300, 19665, 20030))
)
# To combine the two data frames horizontally is technically possible using the bind_cols function:
df3 %>% 
  bind_cols(df4)
# But this is not what I would want because it just pastes the df4 data values next to the df3 values when in real sense I just want to merge the two
# Also, as much as the participants in both data frames are same, their values are mismatched in the combined data frame because of the different arrangement of the participants in df4

# Say hello to matching key values!: (Key values are matching values of a similar column across different data frames)
# I'll do that using the dplyr's join types:
df3 %>% 
  left_join(df4, by = "ID")
# And that my friends is a perfect correctly matched merge of the two data frames! and It is similar for all the other 3 types of dplyr's join types for my datasets here:
df3 %>% 
  right_join(df4, by = "ID")
df4 %>% 
  full_join(df3, by = "ID")
df3 %>% 
  inner_join(df4, by = "ID")
# Well, it wasnt identically merged for all, but the point is that they were all correctly matched in all
# Lets take it that there were other participants who just never made it to the treatments
set.seed(11)
df3 <- tibble(
  ID = 100:112,
  Sex = c(sample(1:2, 12, TRUE), NA),
  Age = sample(20:35, 13, FALSE)
)
df3 %>% 
  left_join(df4, by = "ID")
# The above code joins it correctly as it mintains all observvations on the 'x =' argument (df3).
# Same for full_join() function:
df3 %>% 
  full_join(df4, by = "ID")
# But for the other two, not so much:
df3 %>% 
  right_join(df4, by = "ID")
# The above code maintains only the observations in the 'y =' data frame that has incomplete records.
df3 %>% 
  inner_join(df4, by = "ID")
# The above code only maintains observations with matching key values and excludes every other observation

# Lets assume that the team recording the treatment records misnamed the key column:
set.seed(12)
df4 <- tibble(
  Participant = c(101, 100, 102, 110, 109, 103, 104, 105, 106, 107, 108),
  Medication = sample(c("placebo", "pzq"), 11, TRUE),
  Treat.date = as.Date(c(16380, 16745, 17110, 17475, 17840, 18205, 18570, 18935, 19300, 19665, 20030))
)
# Merging now would return an error because the key columns in both data frames do not have mattchinng column names
df3 %>% 
  left_join(df4, by = "ID")
# But I can solve this by specifying to the left_join() function what the key column is:
df3 %>% 
  left_join(df4, by = c("ID" = "Participant"))

# Let me make the df4 longitudinal data more complex by making it that each participant came back for another dose after 30 days:
set.seed(12)
df4 <- tibble(
  ID = rep(c(101, 100, 102, 110, 109, 103, 104, 105, 106, 107, 108), each = 2),
  Medication = rep(sample(c("placebo", "pzq"), 11, TRUE), each = 2),
  Treat.date = as.Date(c(16380, 16410, 16745, 16775, 17110, 17140, 17475, 17505, 17840, 17870, 18205, 18235, 18570, 18600, 18935, 18965, 19300, 19330, 19665, 19695, 20030, 20060))
)
# Joining this two data frames now changes the relationship of the join from a one-to-one relationship to a one-to-many:
df3 %>% 
  left_join(df4, by = "ID")

# Lets say that there was also an additional dataframe that was recording weight of our participants
# This is going to make it into a many-to-many relationship:
set.seed(12)
df5 <- tibble(
  ID = rep(c(101, 100, 102, 110, 109, 103, 104, 105, 106, 107, 108), each = 2),
  Medication = rep(sample(c("placebo", "pzq"), 11, TRUE), each = 2),
  weight = c(77, 77, 65, 63, 90, 95, 83, 84, 45, 46, 71, 75, 78, 79, 66, 65, 59, 59, 53, 57, 94, 94)
)
# If the two data frames are joined sequentially without any edition to the joining functions:
df3 %>% 
  left_join(df4, by = "ID") %>% 
  left_join(df5, by = "ID")
# It brings back 4 observations for each participants because it adds data onto each other sequentially (merged 2 observations of df3/df4 to the two observations each of df5):
# How to do it correctly:
df3 %>% 
  left_join(df4, by = "ID") %>% 
  left_join(df5, by = c("ID", "Medication"))
# I dont know why it is still showing 4 observations for each when it should have shown two after the correct merging. Nitauliza DOc.