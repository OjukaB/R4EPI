#Install packages for importing excel, SAS and Stata data sets

install.packages("readxl")
install.packages("haven")

#==============================================================================================================================================
#Import an excel worksheet

library(readxl)
Snail_field <- read_excel("C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Excel snail field isolates.xlsx")
Snail_field

Snail_field.1 <- read_excel("C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Excel snail field isolates.xlsx")
Snail_field.1

#Let us begin organizing the excel workbook

Snail_field.2 <- read_excel(
  path = "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Excel snail field isolates.xlsx",
  sheet = "Field data",
  #This code specifies the the exact sheet you want to import if you are dealing with an excel workbook
  col_names = c("ID", "Species", "Location", "Receipt date", "Sample size", "Average shell (cm)"),
  col_types = c(
    "text",
    "text",
    "text",
    "date",
    "numeric",
    "numeric"
  ),
  #If it is unclear what the vector type of a variable is, R allows you to key in "guess" on the specific unclear variable so that it can help estimate the type of vectors in that variable
  skip = 1
)
Snail_field.2

#===========================================================================================================================================================================================
#Importation of SAS and Stata data sets.
library(haven)

Height_weight_sas <- read_sas("C:/Users/ADMIN/Downloads/height_and_weight.sas7bdat")
Height_weight_sas

#Importation of SAS transport files

CDC_trasport_file <- read_xpt("C:/Users/ADMIN/Downloads/LLCP2018XPT.zip")

#We can also import SAS files straight from the internet using a web link
CDC_SAS_file <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT")
head(CDC_SAS_file)
#The head() function trims the dataset to only display 6 rows instead of the entire dataset

#Importation of Stata data sets

Height_weight_stata <- read_stata("C:/Users/ADMIN/Downloads/height_and_weight.dta")
Height_weight_stata
