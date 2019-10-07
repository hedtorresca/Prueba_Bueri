# Load needed libraries

library(tidyverse)
library(here)
library(plyr)

# Load data precharged from a .RData file in the project's environment

load(here("data/RData/data_depurada.RData"))
head(purified_data)

# Select only numeric variables

numeric_var <- dplyr::select_if(purified_data, is.numeric)
stats <- t(summary(numeric_var))
colnames(stats) <- c("p0", "p0.25","p0.5","mean", "p0.75", "p1", "NAs")
stats_df <- as.data.frame.matrix((stats)) 


for(i in colnames(stats)){
stats_df <- stats_df %>% separate(i ,c("name",i), ":", convert = T)
}

stats_df$NAs[is.na(stats_df$NAs)] <- 0

stats_table <- stats_df %>% mutate(cont = rownames(stats_df),  
                                   sd = sapply(numeric_var, sd, na.rm=TRUE), 
                                   n = colSums(!is.na(numeric_var))) %>%  
                            select(cont, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n)

 
# Select only categorical variables

categoric_var <- dplyr::select_if(purified_data, is.character)
categoric_var[is.na(categoric_var)] <- "NA"

k =1 
count_attr <- NULL
for(i in colnames(categoric_var) ){
  count_attr[k] <- nrow(unique(categoric_var[i]))
  k =k+1
}

h=1
resumen <- matrix(NA, nrow= ncol(categoric_var)*4, ncol= max(count_attr)+3)
 for(i in colnames(categoric_var)){
     nrona <- max(count_attr)+2 - length(c(i, "categories", rownames(table(categoric_var[i]))))
     resumen[h,] <-  c(i, "categories", rownames(table(categoric_var[i])) , rep(NA, nrona), "n")
     h=h+1
     resumen[h,] <-  c(i, "frecabs", table(categoric_var[i]), rep(NA, nrona), nrow(categoric_var[i]))
     h=h+1
     resumen[h,] <-  c(i, "frecrel", prop.table(table(categoric_var[i])), rep(NA, nrona), nrow(categoric_var[i]))
     h=h+1
     nrona <- max(count_attr)+2 - length(c(i, "categories", rownames(table(categoric_var[categoric_var[i] != "NA",][i]))))
     resumen[h,] <-  c(i, "frecrel", prop.table(table(categoric_var[categoric_var[i] != "NA",][i])), rep(NA, nrona), nrow(categoric_var[categoric_var[i] != "NA",][i]))
     h=h+1
   }


  

 


