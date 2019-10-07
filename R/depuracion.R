# Load needed libraries
library(readxl)
library(tidyverse)


# Describe variables type
var_descrip <- read_excel("data/raw/test_depuracion.xlsx", sheet = 2)
var_type <- descrip_var$type  %>% replace(descrip_var$type  == "dt", "date") %>% 
                                  replace(descrip_var$type  == "c", "numeric") %>%
                                  replace(descrip_var$type  == "f" | var_type == "txt", "text") %>% 
                                  replace(descrip_var$type  == "n", "logical")

# Load purified data from .xlsx file and save into .RData file. "col_types" 
# allows the variables to be loaded in R successfully in the correct format.

purified_data <- read_excel("data/raw/test_depuracion.xlsx", sheet = 1, col_types = var_type)
save(purified_data, file="data/RData/data_depurada.RData")

