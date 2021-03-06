install.packages("Rcpp")
library("ggplot2")
setwd("Z:/personal folders/Emily/R")
drug <- read.csv("drug_graph_combined.csv", header = TRUE)

##Create graph

#reshape data
install.packages("stringi")
library("reshape2")
melted_drug <- melt(drug, id.vars="drug_Concentration")

#allows plots side by side
install.packages("gridExtra")
library("gridExtra")


##Problems:
#Cannot get title to work in a nested for loop
#Need standard error not standard deviation
library(matrixStats)
#install.packages("devtools")
#library(devtools)
#install_github("snandi/RFunctionsSN")

plot_mean_cell_line <- function(col_nos, titl){
  plotframe <- drug[,col_nos]
  subsetted_plotframe <- plotframe[,-1, drop = FALSE]
  plotframe$mean <- rowMeans(subsetted_plotframe, na.rm=TRUE)
  plotframe$sd <-  rowSds(as.matrix(subsetted_plotframe, na.rm = TRUE))
  print(ggplot(data=plotframe, aes(x=as.factor(drug_Concentration), y= mean, group = 1)) + 
    geom_line() +
    geom_point() +
    coord_cartesian(ylim =c(0, 150)) +
    labs(title = titl, x = "drug [ng/ml]", y = "Viability [%]") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)))
}


all_col_nos <- list(L1 = c(1, 2, 3, 9, 10, 11, 12),
                    L2 = c(1, 13, 14, 19, 20, 21, 22),
                    L3 = c(1, 23, 24, 25),
                    L4 = c(1, 26, 27, 28, 29),
                    L5 = c(1, 30:32),
                    L6 = c(1, 33:36),
                    L7 = c(1, 37:40),
                    L8 = c(1, 41:44),
                    L9 = c(1, 45:47))

all_titl <- list("", "", "", "", "", "", "", "", "")


#for(titl in all_titl){
  for(cell_line in all_col_nos){
  plot_mean_cell_line(cell_line, titl)
  }
#}  


