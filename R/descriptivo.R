# Install and load needed libraries ---------------------------------------------------------

list.of.packages <- c("ggplot2", "tidyverse","xlsx" , "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(xlsx)
library(here)
library(ggplot2)

# Load data precharged from a .RData file in the project's environment ----------

load(here("./data/RData/data_depurada.RData"))
load(here("./data/RData/descrip_var.RData"))

### Univariate ------------------------------------------------------------------

## Select only numeric variables -------------------------------------------------

numeric_var <- dplyr::select_if(purified_data, is.numeric)
stats <- t(summary(numeric_var))
colnames(stats) <- c("p0", "p0.25", "p0.5", "mean", "p0.75", "p1", "NAs")
stats_df <- as.data.frame.matrix(stats)

# Sort data frame and calculate statistics

for (i in colnames(stats)) {
  stats_df <- stats_df %>% separate(i, c("name", i), ":", convert = T)
}

stats_df$NAs[is.na(stats_df$NAs)] <- 0

stats_table <- stats_df %>% mutate(cont = rownames(stats_df), 
                                   sd = sapply(numeric_var, sd, na.rm = TRUE), 
                                   n = colSums(!is.na(numeric_var))) %>% 
  select(cont, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n)

# Export on an excel sheet

write.xlsx(stats_table, row.names = F, showNA = F, file = "outputs/descriptivo.xlsx", 
           sheetName = "univar_numeric")


# Select only categorical variables ---------------------------------------------

categoric_var_na <- dplyr::select_if(purified_data, is.character)
categoric_var <- categoric_var_na
categoric_var[is.na(categoric_var)] <- "NA"

# Sort data frame and calculate statistics

k = 1 # Index initialized for loop
count_attr <- NULL
for (i in colnames(categoric_var)) {
  count_attr[k] <- nrow(unique(categoric_var[i]))
  k = k + 1
}


resumen <- matrix(NA, nrow = ncol(categoric_var) * 4, ncol = max(count_attr) + 3)

h = 1 # Initialized index for loop
for (i in colnames(categoric_var)) {
  nrona <- max(count_attr) + 2 - length(c(i, "categories", 
                                          rownames(table(categoric_var[i]))))
  resumen[h, ] <- c(i, "categories", rownames(table(categoric_var[i])), 
                    rep("", nrona), "n")
  resumen[h + 1, ] <- c(i, "frecabs", table(categoric_var[i]), rep("", nrona), 
                        nrow(categoric_var[i]))
  resumen[h + 2, ] <- c(i, "frecrel", prop.table(table(categoric_var[i])), 
                        rep("",nrona), 
                        nrow(categoric_var[i]))
  nrona2 <- max(count_attr) + 2 - length(c(i, "categories", 
                                           rownames(table(categoric_var[categoric_var[i] != "NA", ][i]))))
  resumen[h + 3, ] <- c(i, "frecrel", 
                        prop.table(table(categoric_var[categoric_var[i] != "NA", ][i])), 
                        rep("", nrona2), nrow(categoric_var[categoric_var[i] != "NA", ][i]))
  h = h + 4
}

# Export on an excel sheet

write.xlsx(resumen, row.names = F, col.names = F, showNA = F, 
           file = "outputs/descriptivo.xlsx", 
           sheetName = "univar_categoric", append = T)


### Bivariate -------------------------------------------------------------------

## Categorical variables with numerical variables --------------------------------

outcome_cat <- c(na.omit(descrip_var[descrip_var$outcome == "1" & 
                                       descrip_var$type == "f", ]$var))
outcome_cat  # Variables 'type' and 'distrib' are categorical

# Sort data frame and calculate statistics using an empirical loop

for (i in outcome_cat) {
  assign(paste0("levels_", i), NULL)
  for (k in colnames(numeric_var)) {
    for (l in levels(as.factor(purified_data[[i]]))) {
      level_var <- purified_data[categoric_var_na[i] == l, ][k]
      stats <- t(summary(level_var))
      if (ncol(stats) == 7) {
        stats_df <- as.data.frame.matrix(stats)
      } else {
        stats_df <- as.data.frame.matrix(cbind(stats, NA))
      }
      colnames(stats_df) <- c("p0", "p0.25", "p0.5", "mean", "p0.75", "p1", 
                              "NAs")
      for (h in colnames(stats_df)) {
        stats_df <- stats_df %>% separate(h, c("name", h), ":", convert = T)
      }
      stats_df$NAs[is.na(stats_df$NAs)] <- 0
      stats_table <- stats_df %>% mutate(cont = k, fact = i, level = l, 
                                         sd = sapply(level_var, sd, na.rm = TRUE), 
                                         n = nrow(na.omit(level_var)), 
                                         # Kruskal test assess for significant differences 
                                         # on a continuous dependent variable by a 
                                         # categorical independent one.
                                         p.valor = kruskal.test(purified_data[[k]] ~  
                                                                  purified_data[[i]])$p.value) %>% 
        select(cont, fact, level, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n, 
               p.valor)
      rownames(stats_table) <- NULL
      assign(paste0("levels_", i), rbind(eval(as.name(paste0("levels_", i))), 
                                         stats_table))
    }
  }
}

# Export on an excel sheet

for (i in outcome_cat) {
  write.xlsx(eval(as.name(paste0("levels_", i))), row.names = F, showNA = F, 
             file = "outputs/descriptivo.xlsx", 
             sheetName = paste0("bivar_", i, "_num"), append = T) 
}

# Create plots (boxplots) and export on a pdf file

m = 0 # Initialized index for loop
pdf("outputs/selected categorical vs. numerical.pdf")
for (i in outcome_cat) {
  signif_df <- eval(as.name(paste0("levels_", i)))
  signif_graph <- distinct(signif_df[signif_df$p.valor < 0.05, ][ ,c(1,2,13)])
for (h in 1:nrow(signif_graph) ) {
 data_plot <- tibble(categorical = categoric_var_na[[signif_graph[h,]$fact]], 
                     numerical = numeric_var[[signif_graph[h, ]$cont]] )
  p <- ggplot(data_plot, aes(x = categorical , y = numerical, fill = categorical)) + 
    geom_boxplot(outlier.colour = "red", outlier.shape = 8,
                 outlier.size = 4) + theme(legend.title = element_blank()) +
    labs(title=paste0("Gráfico de ", signif_graph[h,]$cont, " según ", 
                      signif_graph[h, ]$fact, " donde p-valor = ", 
                      round(signif_graph[h, ]$p.valor, 7)), x= signif_graph[h, ]$fact, 
         y = signif_graph[h, ]$cont)
print(assign(paste0("plot", m + 1), p))
}
}
dev.off()


# Numerical variables with categorical variables --------------------------------

outcome_num <- c(na.omit(descrip_var[descrip_var$outcome == "1" & 
                                       descrip_var$type == "c", ]$var))
outcome_num  # Variables 'l_poplit_angle' and 'standar_value' are numerical

# Sort data frame and calculate statistics using an empirical loop

for (k in outcome_num) {
    assign(paste0("levels_", k), NULL)
  for (i in colnames(categoric_var_na)) {
    for (l in levels(as.factor(purified_data[[i]]))) {
      level_var <- purified_data[categoric_var_na[i] == l, ][k]
      stats <- t(summary(level_var))
      if (ncol(stats) == 7) {
        stats_df <- as.data.frame.matrix(stats)
      } else {
        stats_df <- as.data.frame.matrix(cbind(stats, NA))
      }
      colnames(stats_df) <- c("p0", "p0.25", "p0.5", "mean", "p0.75", "p1", "NAs")
      for (h in colnames(stats_df)) {
        stats_df <- stats_df %>% separate(h, c("name", h), ":", convert = T)
      }
      stats_df$NAs[is.na(stats_df$NAs)] <- 0
      stats_table <- stats_df %>% mutate(cont = k, fact = i, level = l, 
                                         sd = sapply(level_var, sd, na.rm = TRUE), 
                                         n = nrow(!is.na(level_var)), 
                                         p.valor = kruskal.test(purified_data[[k]] ~ 
                                                                  purified_data[[i]])$p.value) %>% 
        select(cont, fact, level, p0, p0.25, p0.5, p0.75, p1, mean, sd, NAs, n, p.valor)
      rownames(stats_table) <- NULL
      assign(paste0("levels_", k), rbind(eval(as.name(paste0("levels_", k))), 
                                         stats_table))
    }
  }
}

# Export on an excel sheet

for (k in outcome_num) {
write.xlsx(eval(as.name(paste0("levels_", k))), row.names = F, showNA = F, 
           file = "outputs/descriptivo.xlsx", 
           sheetName = paste0("bivar_", k, "_cat"), append = T)
}

# Create plots (boxplots) and export on a pdf file

m = 0 # Initialized index for loop
h=10
pdf("outputs/selected numerical vs. categorical.pdf")
for (k in outcome_num) {
  signif_df <- eval(as.name(paste0("levels_", k)))
  signif_graph <- distinct(signif_df[signif_df$p.valor < 0.05, ][ ,c(1,2,13)])
  for (h in 1:nrow(signif_graph) ) {
    data_plot <- tibble(categorical = categoric_var_na[[signif_graph[h,]$fact]], 
                        numerical = numeric_var[[signif_graph[h, ]$cont]])
    p <- ggplot(data_plot, aes(x = categorical , y = numerical, fill = categorical)) + 
      geom_boxplot(outlier.colour = "red", outlier.shape = 8,
                   outlier.size = 4) + theme(legend.title = element_blank()) +
      labs(title=paste0("Gráfico de ", signif_graph[h, ]$cont, " según ", 
                        signif_graph[h, ]$fact, " donde p-valor = ", 
                        round(signif_graph[h, ]$p.valor,7)), x= signif_graph[h, ]$fact, 
           y = signif_graph[h, ]$cont)
    print(assign(paste0("plot", m + 1), p))
  }
}
dev.off()

# Categorical variables with categorical variables ------------------------------

categoric_var_na <- categoric_var_na[, colnames(categoric_var_na) != "cx_tox" & 
                                       colnames(categoric_var_na) != "Q22_1609m_walk"]

for (i in outcome_cat) {
  assign(paste0("table_categ_", i), NULL)
  for (k in colnames(categoric_var_na[, colnames(categoric_var_na) != i])) {
    contingency <- table(categoric_var_na[[k]], categoric_var_na[[i]])
    proporrp <- prop.table(contingency, 1)
    proporcp <- prop.table(contingency, 2)
    table_var <- cbind(k, rownames(contingency), contingency, proporrp, proporcp, 
                       chisq.test(contingency)$p.value)
    rownames(table_var) <- NULL
    assign(paste0("table_categ_", i), rbind(eval(as.name(paste0("table_categ_", 
                                                                i))), table_var))
  }
  if (i == "distrib") { # Assign names for columns
    colnames(table_categ_distrib) <- c("var", "Xlevels", paste0(i, "1"), 
                                       paste0(i, "2"), paste0(i, "1_rp"), 
                                       paste0(i, "2_rp"), paste0(i, "1_cp"), 
                                       paste0(i, "2_cp"), "p-valor")
  } else {
    colnames(table_categ_type) <- c("var", "Xlevels", paste0(i, "1"), paste0(i, "2"), 
                                    paste0(i, "3"), paste0(i, "4"), paste0(i, "1_rp"), 
                                    paste0(i, "2_rp"), paste0(i, "3_rp"), paste0(i, "4_rp"), 
                                    paste0(i, "1_cp"), paste0(i, "2_cp"), 
                                    paste0(i, "3_cp"), paste0(i, "4_cp"), "p-valor")
  }
}

# Export on an excel sheet

for (i in outcome_cat) {
write.xlsx(eval(as.name(paste0("table_categ_", i))), row.names = F, showNA = F, 
           file = "outputs/descriptivo.xlsx", sheetName = paste0("bivar_", i, "_cat"), 
           append = T)
}

# Create plots (stacked column charts) and export on a pdf file

m = 0 # Initialized index for loop
pdf("outputs/selected categorical vs. categorical.pdf")
for (i in outcome_cat) {
  signif_df <- as.data.frame(eval(as.name(paste0("table_categ_", i))))
  signif_df$`p-valor` <- as.numeric(as.character(signif_df$`p-valor`))
  signif_graph <- na.omit(distinct(signif_df[signif_df$`p-valor` < 0.05,][,c(1, ncol(signif_df))]))
  for (h in 1:nrow(signif_graph)) {
    data_plot <- data.frame(table(categoric_var_na[[i]], 
                                  categoric_var_na[[paste(signif_df[h, ]$var)]]))
    names(data_plot) <- c("categorical1", "categorical2", "Count")
   p <- ggplot(data = data_plot, aes(x = categorical1 , fill = categorical2, 
                                     y = Count)) + 
      geom_bar(stat = "identity") + labs(title=paste0("Gráfico de ", paste(i), 
                                                      " según ", signif_graph[h,]$var, 
                                                      " donde p-valor = ", 
                                                      round(signif_graph[h,]$`p-valor`,7)), 
                                         x= paste(i), y = signif_graph[h,]$var) +
     theme(legend.title = element_blank())
   print(assign(paste0("plot", m + 1 ), p))
  }
}
dev.off()


### Numerical variables with numerical variables --------------------------------

# Sort data frame and calculate statistics using an empirical loop

for (i in outcome_num) {
  assign(paste0("numeric_cor_", i), NULL)
  for (k in colnames(numeric_var[, colnames(numeric_var) != i])) {
    single_cor <- cbind(k, cor.test(numeric_var[[k]], numeric_var[[i]])$estimate, 
                        cor.test(numeric_var[[k]], numeric_var[[i]])$p.value)
    rownames(single_cor) <- NULL
    colnames(single_cor) <- c("  ", i, "p-value")
    assign(paste0("numeric_cor_", i), rbind(eval(as.name(paste0("numeric_cor_", i))), 
                                            single_cor))
  }
}

# Export on an excel sheet

for (i in outcome_num) {
write.xlsx(eval(as.name(paste0("numeric_cor_", i))), row.names = F, showNA = F, 
           file = "outputs/descriptivo.xlsx", sheetName = paste0("bivar_", i, "_num"), 
           append = T)
}

# Create plots (scatters graphs) and export on a pdf file

m = 0 # Initialized index for loop
pdf("outputs/selected numerical vs. numerical.pdf")
for (i in outcome_num) {
  signif_df <- as.data.frame(eval(as.name(paste0("numeric_cor_", i))))
  signif_df$`p-value` <- as.numeric(as.character(signif_df$`p-value`))
  signif_graph <- signif_df[signif_df$`p-value` < 0.05,]
  for (h in 1:nrow(signif_graph) ) {
    data_plot <- tibble(numeric1 = numeric_var[[i]], 
                                  numeric2 = numeric_var[[paste(signif_df[h,1])]])
    p <- ggplot(data_plot, aes(x = numeric1, y = numeric2)) + 
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue") + 
      labs(title=paste0("Gráfico de ", paste(i), " según ", signif_graph[h,]$var, 
                        " donde p-valor = ", round(signif_graph[h,]$`p-value`,7)), 
           x= paste(i), y = signif_graph[h,1]) +
      theme(legend.title = element_blank())
    print(assign(paste0("plot", m + 1 ), p))
  }
}
dev.off()

