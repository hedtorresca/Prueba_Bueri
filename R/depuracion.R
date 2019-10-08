# Load needed libraries
library(readxl)
library(tidyverse)
library(here)

# Describe variables type
descrip_var <- read_excel(here("data/raw/test_depuracion.xlsx"), sheet = 2)
var_type <- descrip_var$type  %>% replace(descrip_var$type  == "dt", "date") %>% 
                                  replace(descrip_var$type  == "c", "numeric") %>%
                                  replace(descrip_var$type  == "f" | descrip_var$type == "txt" | descrip_var$type  == "n",  "text")

# Load purified data from .xlsx file and save into .RData file. Thus "col_types" 
# allows the variables to be loaded in R successfully in the correct format.

purified_data <- read_excel(here("data/raw/test_depuracion.xlsx"), sheet = 1, col_types = var_type)
save(purified_data, file=here("data/RData/data_depurada.RData"))
save(descrip_var, file=here("data/RData/descrip_var.RData"))

