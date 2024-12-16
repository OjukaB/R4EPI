#Exportation of files from R into various formats
#Exportation of dataframes into .csv files
library(readr)

readr::write_csv(Height_weight_sas, "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/Height and Weight SAS.csv")

#Exporting files as a binary file allows retention of metadata if deemed useful
#R can allow exportation of data in two binary file formats that are the .Rdata or .rds format

readr::write_rds(CDC_SAS_file, "C:/Users/ADMIN/Documents/R4EPI Brad Cannel/R4EPI/CDC SAS data file.rds")
