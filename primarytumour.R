setwd("Z:/personal folders/Emily/R/")
tumourbg <- read.csv("tumour_background.csv", header = FALSE)
tumourbg1 <- tumourbg[-(1:3),]
colnames(tumourbg1) <- as.character(unlist(tumourbg1[1,])) # first row is a repeat of the heading but oh well

#Subsetting tumor bg data

str(tumourbg1) 

RNArelevanttumourbg <- tumourbg1[tumourbg1$RNA_seq == "yes" | tumourbg1$RNA_seq == "yes_peifer", ]
RNAprimarytumourbg <- subset(RNArelevanttumourbg, primary_tumor_metastasis=='primary')
RNAuntreatedprimarytumourbg <- subset(RNAprimarytumourbg, previous_therapeutic_treatment_for_SCLC=="untreated")
# 1 patient # RNArelapseprimarytumourbg <- subset(RNAprimarytumourbg, previous_therapeutic_treatment_for_SCLC=="relapse")
RNAuntreatedprimaryneoadjuvantchemotumourbg <- subset(RNAuntreatedprimarytumourbg, chemotherapy_NEOADJUVANT=="yes" )
RNAuntreatedprimaryneoadjuvantchemononetumourbg <-  subset(RNAuntreatedprimarytumourbg, chemotherapy_NEOADJUVANT=="no" )
#noneneoadjuvantsamples <- as.character(RNAuntreatedprimaryneoadjuvantchemononetumourbg$`Sample-ID`)


##FPKM tissue data

FPKMtissue <- read.csv("SCLC_tissue_FPKM.csv")
FPKMtissue1 <- FPKMtissue[-1,]
colnames(FPKMtissue1) <- as.character(unlist(FPKMtissue1[1,])) # first row is a repeat of the heading but oh well
FPKMtissue2 <- FPKMtissue1[,-1]
rownames(FPKMtissue2) <- make.names(FPKMtissue1$gene, unique = TRUE)

##Now I want to subset my FPKM data by only the samples which match that in my subsetted primary tumour bg data

reversedRNAuntreatedprimaryneoadjuvantchemononetumourbg <- t(RNAuntreatedprimaryneoadjuvantchemononetumourbg)

dataRNAuntreatedprimaryneoadjuvantchemononetumourbg <- as.data.frame(RNAuntreatedprimaryneoadjuvantchemononetumourbg)
datareversedRNAuntreatedprimaryneoadjuvantchemononetumourbg <- as.data.frame(reversedRNAuntreatedprimaryneoadjuvantchemononetumourbg)

dataFPKMtissue2 <- as.data.frame(FPKMtissue2)
reversedFPKM <- t(FPKMtissue2)
datareversedFPKM <- as.data.frame(reversedFPKM)

FPKMRNAuntreatedprimaryneoadjuvantnone <- datareversedFPKM[datareversedFPKM$gene %in% dataRNAuntreatedprimaryneoadjuvantchemononetumourbg$`Sample-ID`, ]
reversedFPKMRNAuntreatedprimaryneoadjuvantnone <- t(FPKMRNAuntreatedprimaryneoadjuvantnone)
datareversedFPKMRNAuntreatedprimaryneoadjuvantnone <- as.data.frame(reversedFPKMRNAuntreatedprimaryneoadjuvantnone)

datareversedFPKMRNAuntreatedprimaryneoadjuvantnoneminusfirstrow <- datareversedFPKMRNAuntreatedprimaryneoadjuvantnone[-1,] 

###WOOOO!!!! (This is my FPKM data of all the samples which meet the conditions specified)
write.csv(datareversedFPKMRNAuntreatedprimaryneoadjuvantnone, "datareversedFPKMRNAuntreatedprimaryneoadjuvantnone.csv")
write.csv(datareversedFPKMRNAuntreatedprimaryneoadjuvantnoneminusfirstrow, "datareversedFPKMRNAuntreatedprimaryneoadjuvantnoneminusfirstrow.csv")



