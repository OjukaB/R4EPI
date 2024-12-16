#Importation of data
#Importation of plain text data. 
#First begin with the installation of a package used to import data called 'readr'

install.packages("readr")
library(readr)

#=============================================================================
#Import a space delimited text file
?read_delim

Field_isolates <- read_delim(
  file = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates.txt",
  delim = " ",
  na = "."
)
Field_isolates

#Notice that R parsed the Av_shell_cm column as a character vector which is incorrect
#This can be corrected by adding the na argument in the read_delim() function and specifying what missing data should be represented with
#===================================================================================================================================================

#Importation of tab limited plain text files
#We can use the same iteration used in the previous read_delim() function only with the specification that the separating character is "\t"
#Like so: delim = "\t"
#But here is a shortcut that can be used
library(readr)
Field_isolates.1 <- read_tsv("C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates tab delimited.txt")
Field_isolates.1

#===================================================================================================================================================
#Importation of fixed width delimited plain texts

Field_isolates.2 <- read_table("C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates Fixed width delimited.txt")
Field_isolates.2

#===================================================================================================================================================
#I want to import a plain text file that is still considered a fixed width delimited file but there is no character delimiting the variables
#Importing a dataset like that can lead to numerous parsing errors by R as R will see your dataset as just one column with all the values belonging to it
#Here is how we can specify how R should parse the data set

?read_fwf
Field_isolates.3 <- read_fwf(
  file = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates without characters delimiting values.txt",
  col_positions = fwf_widths(
    widths = c(3, 11, 8, 2),
    col_names = c("ID", "Species", "Location", "Size")
  ),
  skip = 1
)
Field_isolates.3

#=========================================================================================================================================================
#Another way of specifying how R can parse the same data set is through specifying the column in which each variable starts and ends

Field_isolates.4 <- read_fwf(
  file = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates without characters delimiting values.1.txt",
  col_positions = fwf_positions(
    start = c(1, 4, 15, 23),
    end = c(3, 14, 21, 24),
    col_names = c("ID", "Species", "Location", "Size")
  ),
  skip = 1
)
Field_isolates.4

#=========================================================================================================================================================
#A shortcut is the combination of the first method above specifying the width of each variable into one argument for each variable like so:

Field_isolates.5 <- read_fwf(
  file = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates without characters delimiting values.2.txt",
  col_positions = fwf_cols(
    ID = 3,
    Species = 11,
    Location = 8,
    Size = 2
  ),
  skip = 1
)
Field_isolates.5

#=========================================================================================================================================================
#Another shortcut is to combine the second method of parse specification for R into an argument with values of variable namr and the start and end values for each

Field_isolates.6 <- read_fwf(
  file = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates without characters delimiting values.3.txt",
  col_positions = fwf_cols(
    ID = c(1, 3),
    Species = c(4, 14),
    Location = c(15, 21),
    Size = c(23, 24)
  ),
  skip = 1
)
Field_isolates.6

#=========================================================================================================================================================
#Importation of comma delimited files- .csv files
#There is a dedicated function for importation of csv files

Field_isolates.7 <- read_csv("C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates comma delimited.csv")
Field_isolates.7

#==========================================================================================================================================================
#So far we have used simulated data that is 'well behaved'- to mean that they have been exactly how R would expect them to be structured
#But real life/study data doesn't always conform to this. So let us try and simulate data that is somewhat not well behaved
#I will import that data as is without altering the raw data using text editor or excel 
#That way, I can track the alterations I have made without having to change anything in the raw data that most of the time usually comes with undesired consequences

Field_isolates.8 <- read_csv("C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates comma delimited.1.csv")
Field_isolates.8

#The data frame is obviously not ready for analysis as it is not sturctured in a manner ready for analysis. So lets begin cleaning and eliminating those errors

Field_isolates.8 <- read_csv(
  file = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Snail field isolates comma delimited.1.csv",
  col_names = c("ID", "Species", "Location", "Size"),
  #This introduces the variable names that we want to use for our data frame
  col_types = cols(
    col_character(),
    col_character(),
    col_character(),
    col_integer(),
    col_skip()
  ),
  #These codes help me specify the vector type of each variable for R as it got most wrong. The codes follow the arrangement of the variables we have specified from left to right respectively
  #And that is why the last function is col_skip() because the last column of the raw dataset was the column for 'Notes' that aren't really helpful for our analysis
  skip = 3
  #This code will tell R to ignore the first three raws of the raw data set as they are not useful for our analysis and because we have also rephrased the variable names
)
Field_isolates.8
#The dataset is now cleaned and ready for analysis by R.
#If there were missing data. Then we would have included the argument 'na = ' and specify to R what it should substitute for 'NA' in the dataset


#It is also possible to import raw datasets through the 'import' option provided by RStudio in the environment pane
#It can allow usage of both base-R packages for importation and those that aren't too
#The options provided by this avenue allows you some form of allowance to manage the data set

