library(RCA)
library(haven)
library(tidyverse)
library(dplyr)
library(poLCA)
library(data.table)
library(broom)
library(stats)

rm(list = ls())

path <- "~/Downloads/"
data <- 'BAM Dataset_w Merged 2012 ACS Census Tract Data.dta'
BAM2014 <- read_dta(paste0(path,data))

BAM2014_vars <- BAM2014 %>%
  dplyr::select(starts_with('v41', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('conxians', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('muslims', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('jews', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('atheists', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('none', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('sum', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('refused', ignore.case = TRUE)) %>%
  dplyr::select(-ends_with('imm', ignore.case = TRUE)) 
    
BAM2014_vars_names <-names(BAM2014_vars)
vars <- as.character(unlist(as.list(BAM2014_vars_names)))
BAM2014_vars <- BAM2014_vars[complete.cases(BAM2014_vars), ]
BAM2014_vars <- as.data.frame(map(BAM2014_vars[,BAM2014_vars_names], as.factor))

library("FactoMineR")
library("factoextra")
res.mca <- MCA(BAM2014_vars, graph = FALSE)
print(res.mca)
summary(res.mca)

eig.val <- res.mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

# Graphs of the individuals
plot(res.mca,invisible=c("var"),cex=.5,label="none",title="Graph of the individuals") 
# Graphs of the categories
plot(res.mca,invis=c("ind"),col.var=c(rep(c("black","red"),17),"black",rep("red",4)),title="Graph of the categories")
# Graphs of the variables
plot(res.mca,choix="var",title="Graph of the variables")
dimdesc(res.mca)
plotellipses(res.mca, cex=0.2, magnify=12, keepvar=1:4)

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
             col.var="cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             shape.var = 15,
             repel = TRUE,
             ggtheme = theme_minimal()
             )

fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_cos2(res.mca, choice = "var", axes = 1:2)
fviz_cos2(res.mca, choice = "var", axes = 2)

BAM2014_vars2 <- BAM2014_vars %>%
  dplyr::select(-ends_with('whites', ignore.case = TRUE))

res.mca <- MCA(BAM2014_vars2, graph = FALSE)
eig.val <- res.mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

fviz_cos2(res.mca, choice = "var", axes = 2)

fviz_mca_var(res.mca, 
             col.var="cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             shape.var = 15,
             repel = TRUE,
             ggtheme = theme_minimal()
)

library(FactoInvestigate)
Investigate(res.mca)