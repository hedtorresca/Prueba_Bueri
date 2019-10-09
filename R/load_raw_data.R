# Load needed libraries
list.of.packages <- c("readxl", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(readxl)
library(here)

# Load data from .xlsx file and save into .RData file

test_data <- read_excel(here("data/raw/test_depuracion.xlsx"), sheet = 1)
save(test_data, file=here("data/RData/test_depuracion.RData"))

