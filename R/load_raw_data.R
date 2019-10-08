# Load needed libraries

library(readxl)
library(here)

# Load data from .xlsx file and save into .RData file

test_data <- read_excel(here("data/raw/test_depuracion.xlsx"), sheet = 1)
save(test_data, file=here("data/RData/test_depuracion.RData"))

