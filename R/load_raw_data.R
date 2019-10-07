# Load needed libraries

library(readxl)


# Load data from .xlsx file and save into .RData file

test_data <- read_excel("data/raw/test_depuracion.xlsx", sheet = 1)
save(test_data, file="data/RData/test_depuracion.RData")

