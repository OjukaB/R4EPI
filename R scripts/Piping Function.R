#load the dplyr packages as tibble is not a base-R function
library(dplyr)
#====================================================================

#Simulate data using a tibble

schistosoma_analysis <- tibble(
  ID = c("001", "002", "003", "004"),
  Species = c("B.sudanica", "B.pfeiferri", "Bulinus", "B.pfeiferri"),
  Size = c(2, 4, 1, 5),
  Infectivity = c("No", "Yes", "Yes", "No")
)
schistosoma_analysis

#Introduction into the filter function
#This function contains two values, the first of which is the name of the object and the second is the condition used to subset a group

filter(schistosoma_analysis, Species == "B.pfeiferri")

#Introduction into piping function
#You can pass the schistosoma_analysis data frame to the filter() function using the piping function

schistosoma_analysis %>% filter(Species == "B.pfeiferri")

#Styling of pipes
Bulinus.schistosoma.analysis <- schistosoma_analysis %>% 
  filter(Species == "Bulinus")

#===============================================================================================
 