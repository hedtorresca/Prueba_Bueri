### Load needed libraries

library(tidyverse)
library(here)

### Load data precharged from a .RData file in the project's environment
load(here("./data/RData/data_depurada.RData"))
load(here("./data/RData/descrip_var.RData"))
head(purified_data)

### Select only numeric variables

numeric_var <- dplyr::select_if(purified_data, is.numeric)
stats <- t(summary(numeric_var))
colnames(stats) <- c("p0", "p0.25","p0.5","mean", "p0.75", "p1", "NAs")
stats_df <- as.data.frame.matrix(stats)


for(i in colnames(stats)){
stats_df <- stats_df %>% separate(i ,c("name",i), ":", convert = T)
}

stats_df$NAs[is.na(stats_df$NAs)] <- 0

stats_table <- stats_df %>% mutate(cont = rownames(stats_df),  
                                   sd = sapply(numeric_var, sd, na.rm=TRUE), 
                                   n = colSums(!is.na(numeric_var))) %>%  
                            select(cont, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n)

 
### Select only categorical variables

categoric_var_na <- dplyr::select_if(purified_data, is.character)
categoric_var <- categoric_var_na 
categoric_var[is.na(categoric_var)] <- "NA"

k =1 
count_attr <- NULL
for(i in colnames(categoric_var) ){
  count_attr[k] <- nrow(unique(categoric_var[i]))
  k =k+1
}


resumen <- matrix(NA, nrow= ncol(categoric_var)*4, ncol= max(count_attr)+3)

h=1
for(i in colnames(categoric_var)){
     nrona <- max(count_attr)+2 - length(c(i, "categories", rownames(table(categoric_var[i]))))
     resumen[h,] <-  c(i, "categories", rownames(table(categoric_var[i])) , rep("", nrona), "n")
     resumen[h+1,] <-  c(i, "frecabs", table(categoric_var[i]), rep("", nrona), nrow(categoric_var[i]))
     resumen[h+2,] <-  c(i, "frecrel", prop.table(table(categoric_var[i])), rep("", nrona), nrow(categoric_var[i]))
     nrona2 <- max(count_attr)+2 - length(c(i, "categories", rownames(table(categoric_var[categoric_var[i] != "NA",][i]))))
     resumen[h+3,] <-  c(i, "frecrel", prop.table(table(categoric_var[categoric_var[i] != "NA",][i])), rep("", nrona2), nrow(categoric_var[categoric_var[i] != "NA",][i]))
     h=h+4
   }

### Bivariate

# Categorical variables with numerical variables

outcome_cat <- c(na.omit(descrip_var[descrip_var$outcome == "1" & descrip_var$type == "f",]$var))
outcome_cat # [1] "type"    "distrib"

levels_type <- NULL
levels_distrib <- NULL

for(i in outcome_cat){
for(k in colnames(numeric_var)){
  for(l in levels(as.factor(purified_data[[i]]))){
level_var <- purified_data[categoric_var_na[i] == l,][k]
stats <- t(summary(level_var))
if (ncol(stats) == 7){
stats_df <- as.data.frame.matrix(stats)
}else{
  stats_df <- as.data.frame.matrix(cbind(stats, NA))
}
colnames(stats_df) <- c("p0", "p0.25","p0.5","mean", "p0.75", "p1", "NAs")
for(h in colnames(stats_df)){
  stats_df <- stats_df %>% separate(h ,c("name",h), ":", convert = T)
}
stats_df$NAs[is.na(stats_df$NAs)] <- 0
stats_table <- stats_df %>% mutate(cont= k, fact = i ,level = l,  
                                   sd = sapply(level_var, sd, na.rm=TRUE), 
                                   n = nrow(!is.na(level_var)),
                                   p.valor =  kruskal.test(purified_data[[k]] ~ purified_data[[i]])$p.value) %>%  
  select(cont, fact, level, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n, p.valor)

rownames(stats_table) <- NULL

assign(paste0("levels_", i), rbind(eval(as.name(paste0("levels_",i))), stats_table))
  }
}
}
  

# Numerical variables with categorical variables

outcome_num <- c(na.omit(descrip_var[descrip_var$outcome == "1" & descrip_var$type == "c",]$var))
outcome_num #"l_poplit_angle" "standar_value" 

levels_l_poplit_angle <- NULL
levels_standar_value <- NULL


for(i in colnames(categoric_var_na)){
  for(k in outcome_num){
    for(l in levels(as.factor(purified_data[[i]]))){
      level_var <- purified_data[categoric_var_na[i] == l,][k]
      stats <- t(summary(level_var))
      if (ncol(stats) == 7){
        stats_df <- as.data.frame.matrix(stats)
      }else{
        stats_df <- as.data.frame.matrix(cbind(stats, NA))
      }
      colnames(stats_df) <- c("p0", "p0.25","p0.5","mean", "p0.75", "p1", "NAs")
      for(h in colnames(stats_df)){
        stats_df <- stats_df %>% separate(h ,c("name",h), ":", convert = T)
      }
      stats_df$NAs[is.na(stats_df$NAs)] <- 0
      stats_table <- stats_df %>% mutate(cont= k, fact = i ,level = l,  
                                         sd = sapply(level_var, sd, na.rm=TRUE), 
                                         n = nrow(!is.na(level_var)),
                                         p.valor =  kruskal.test(purified_data[[k]] ~ purified_data[[i]])$p.value) %>%  
        select(cont, fact, level, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n, p.valor)
      
      rownames(stats_table) <- NULL
      
      assign(paste0("levels_", k), rbind(eval(as.name(paste0("levels_",k))), stats_table))
    }
  }
}


# Categorical variables with categorical variables

categoric_var_na <- categoric_var_na[ ,colnames(categoric_var_na) != "cx_tox" & colnames(categoric_var_na) != "Q22_1609m_walk" ]
table_categ_type <- NULL
table_categ_distrib <- NULL

  for(k in colnames(categoric_var_na)){
    for(i in outcome_cat){
    contingency <- table(categoric_var_na[[k]],categoric_var_na[[i]])
    proporrp <- prop.table(contingency, 1)
    proporcp <- prop.table(contingency, 2)
    table_var <- cbind(k, rownames(contingency),contingency, proporrp, proporcp, chisq.test(contingency)$p.value)
    rownames(table_var) <- NULL
    # colnames(table_var) <- c("var", "Xlevels", paste0(i, "1"), paste0(i, "2"), paste0(i, "1_rp"), paste0(i, "2_rp"), paste0(i, "1_cp"), paste0(i, "2_cp"), "p-valor" )
    
    assign(paste0("table_categ_", i), rbind(eval(as.name(paste0("table_categ_",i))), table_var))
    }
}

### Numerical variables with numerical variables

for(i in outcome_num){
  for(k in colnames(numeric_var[ , colnames(numeric_var_na) != i ] )){
  cbind(i, cor.test(numeric_var[[k]], numeric_var[[i]])$estimate, cor.test(numeric_var[[k]], numeric_var[[i]])$p.value)
  }
}
