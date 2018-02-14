if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

setwd("Z:/personal folders/Emily/R/")

####Cell Lines

MyFPKMValues1 <- read.csv("SCcell_ord_unique_gene_max_20130630.csv", header = TRUE)
MyFPKMValues2 <- MyFPKMValues1[,-1]
rownames(MyFPKMValues2) <- MyFPKMValues1[,1]

#cell death pathway in SCLC lines
MyFPKMValuesGenesXY <- as.matrix(MyFPKMValues2[c("gene1", ), c("line1", )])

#log2 + 0.01

XYlineslog <- log2(MyFPKMValuesGenesXY + 0.01)
boxplot(XYlineslog)
hist(XYlineslog)

##Lung Cells

MyFPKMValuesLung1 <- read.csv("Normal-lung_ord_unique_gene_max_20130829.csv", header = TRUE)
MyFPKMValuesLung2 <- MyFPKMValuesLung1[,-1]
rownames(MyFPKMValuesLung2) <- make.names(MyFPKMValuesLung1$gene, unique = TRUE)

#cell dwath pathway in Lung Cells

MyFPKMValuesGenesXYLung <- as.matrix(MyFPKMValuesLung2[c( "gene1"),])

#log2 + 0.01

XYlunglog <- log2(MyFPKMValuesGenesXYLung + 0.01)
boxplot(XYlunglog)
hist(XYlunglog)

#CDP in Primary Untreated Tumours without neoadjuvant chemo

MyFPKMValuesPrimaryUntreatedNAnone1 <- read.csv("datareversedFPKMRNAuntreatedprimaryneoadjuvantnoneminusfirstrow.csv", header = TRUE)
MyFPKMValuesPrimaryUntreatedNAnone2 <- MyFPKMValuesPrimaryUntreatedNAnone1[,-1]
rownames(MyFPKMValuesPrimaryUntreatedNAnone2) <- make.names(MyFPKMValuesPrimaryUntreatedNAnone1$gene, unique = TRUE)

MyFPKMValuesGenesFerroptosisPrimaryUntreatedNAnone <- as.matrix(MyFPKMValuesPrimaryUntreatedNAnone2[c("gene1", ),])

XYprimaryuntreatedNAnonelog <- log2(MyFPKMValuesGenesXYPrimaryUntreatedNAnone + 0.01)
boxplot(XYprimaryuntreatedNAnonelog)
hist(XYprimaryuntreatedNAnonelog)

#CDP in Primary Untreated Tumours with neoadjuvant chemo

MyFPKMValuesPrimaryUntreatedNA1 <- read.csv("datareversedFPKMRNAuntreatedprimaryneoadjuvantminusfirstrow.csv", header = TRUE)
MyFPKMValuesPrimaryUntreatedNA2 <- MyFPKMValuesPrimaryUntreatedNA1[,-1]
rownames(MyFPKMValuesPrimaryUntreatedNA2) <- make.names(MyFPKMValuesPrimaryUntreatedNA1$gene, unique = TRUE)

MyFPKMValuesGenesXYPrimaryUntreatedNA <- as.matrix(MyFPKMValuesPrimaryUntreatedNA2[c("gene1", ),])

XYprimaryuntreatedNAlog <- log2(MyFPKMValuesGenesXYPrimaryUntreatedNA + 0.01)
boxplot(XYprimaryuntreatedNAlog)
hist(XYprimaryuntreatedNAlog)

#CDP in Primary Relapse Tumours (no Neoadjuvant Chemo)
#This is only one patient hence all the fiddling with matrices and dataframes to get it to like the dimensions

MyFPKMValuesPrimaryRelapse1 <- read.csv("datareversedFPKMRNArelapseprimaryneoadjuvantnone.csv", header = TRUE)
MyFPKMValuesPrimaryRelapse2 <- MyFPKMValuesPrimaryRelapse1
rownames(MyFPKMValuesPrimaryRelapse2) <- MyFPKMValuesPrimaryRelapse1[,1]
MyFPKMValuesGenesXYPrimaryRelapse <- as.matrix(MyFPKMValuesPrimaryRelapse2[c("gene1", ),])
MyFPKMValuesGenesXYPrimaryRelapse1 <- MyFPKMValuesGenesXYPrimaryRelapse[,-1]
MyFPKMValuesGenesXYPrimaryRelapseNumeric <- as.numeric(MyFPKMValuesGenesXYPrimaryRelapse1)
MyFPKMValuesGenesXYPrimaryRelapseNumericMatrix <- as.matrix(MyFPKMValuesGenesXYPrimaryRelapseNumeric)
rownames(MyFPKMValuesGenesXYPrimaryRelapseNumericMatrix) <- MyFPKMValuesGenesXYPrimaryRelapse[,1] 

#Metastases

MyFPKMValuesMetastasis1 <- read.csv("datareversedFPKMRNAmetastasispleurauntreatedneoadjuvantchemonone.csv", header = TRUE)
MyFPKMValuesMetastasis2 <- MyFPKMValuesMetastasis1
rownames(MyFPKMValuesMetastasis2) <- MyFPKMValuesMetastasis1[,1]
MyFPKMValuesGenesXYMetastasis <- as.matrix(MyFPKMValuesMetastasis2[c("gene1", ),])
MyFPKMValuesGenesXYMetastasis1 <- MyFPKMValuesGenesXYMetastasis[,-1]
MyFPKMValuesGenesXYMetastasisNumeric <- as.numeric(MyFPKMValuesGenesXYMetastasis1)
MyFPKMValuesGenesXYMetastasisNumericMatrix <- as.matrix(MyFPKMValuesGenesXYMetastasisNumeric)
rownames(MyFPKMValuesGenesXYMetastasisNumericMatrix) <- MyFPKMValuesGenesXYMetastasis[,1]




#Bind my data together 
XYlungsclcprimaryuntreatedrelapsemetastasis <- cbind(MyFPKMValuesGenesXYLung, MyFPKMValuesGenesXY, MyFPKMValuesGenesXYPrimaryUntreatedNAnone, MyFPKMValuesGenesXYPrimaryRelapseNumericMatrix, MyFPKMValuesGenesXYMetastasisNumericMatrix, MyFPKMValuesGenesXYPrimaryUntreatedNA)

XYlungsclcprimaryuntreatedrelapsemetastasislog <- log2(XYlungsclcprimaryuntreatedrelapsemetastasis + 0.01)

View(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe)

#Rename the log matrix to identify my relapsed patient, metastasis patient and NA patients with primary untreated tumour

XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe <- as.data.frame(XYlungsclcprimaryuntreatedrelapsemetastasislog)
colnames(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe)[colnames(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe)=="V90"] <- "S02382_relapsed"
colnames(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe)[colnames(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe)=="V91"] <- "S02194_metastasis"

install.packages("data.table")
library("data.table")

setnames(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe, old=c("S02120","S02139", "S02397"), new=c("S02120_NAchemo", "S02139_NAchemo", "S02397_NAchemo"))


XYlungsclcprimaryuntreatedrelapsemetastasislogdataframematrix <- as.matrix(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframe)

#Create heatmap

heatmap.2(XYlungsclcprimaryuntreatedrelapsemetastasislogdataframematrix,
          #cellnote = XYlog, 
          col = bluered, 
          density.info="none",  # turns off density plot inside color legend
          trace="none")
