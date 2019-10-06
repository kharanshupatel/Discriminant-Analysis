---
title: "Kharanshu-Patel-DiCA"
author: "Kharanshu Patel"
date: "11/10/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ExPosition)
library(corrplot)
library(ggplot2)
library(InPosition)
library(dplyr)
library(PTCA4CATA)
library(data4PCCAR)
library(classInt)
library(Hmisc)
library(psych)
library(TInPosition)
library(RColorBrewer)
library(plotly)
library(heatmaply)
library(TExPosition)
```
# Method : Discriminant Correspondence Analysis

Discriminant correspondence analysis (DCA) is an extension of discriminant analysis (DA) and correspondence analysis (CA). Like discriminant analysis, the goal of DCA is to categorize observations in pre-defined groups, and like correspondence analysis, it is used with nominal variables. The main idea behind DCA is to represent each group by the sum of its observations and to perform a simple CA on the groups by variables matrix. The original observations are then projected as supplementary elements and each observation is assigned to the closest group. The comparison between the a priori and the a posteriori classifications can be used to assess the quality of the discrimination. A similar procedure can be used to assign new observations to categories. The stability of the analysis can be evaluated
using cross-validation techniques such as jackknifing or bootstrapping.

# Dataset : oasis_cross-sectional

The Open Access Series of Imaging Studies (OASIS) is a project aimed at making MRI data sets of the brain freely available to the scientific community.
It is a cross-sectional MRI Data in Young, Middle Aged, Nondemented and Demented Older Adults.
This set consists of a cross-sectional collection of 216 subjects (rows) aged 33 to 96. The subjects include both men and women. 
It measures 216 subjects (rows) on 4 quanitiative variables (columns). The 5 qualitative variables have been used to interpret the data. 

Measures included in the study:

Age: Age (years)

Gender: Male or Female

Education: Years of Education - Recoded into categories from 1 (highest) to 5 (lowest)

SES: Socioeconomic status classified into categories from 1 (highest) to 5 (lowest)

MMSE: Mini-Mental State Examination score – range from 0 (worst) to 30 (best)

CDR: Clinical Dementia Rating - (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2 = moderate AD)

ASF : Atlas scaling factor (unitless). Computed scaling factor that transforms native-space brain and skull to the atlas target (i.e. the determinant of the transform matrix)

eTIV: Estimated total intracranial volume (cm3)

nWBV: Normalized whole brain volume, expressed as a percent of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process 

```{r data_set}

library(readxl)
oasis_cross_sectional <- read_excel("oasis_cross_sectional.xlsx")

head(oasis_cross_sectional)

oasis.dummy <- oasis_cross_sectional[c(3,6,8,9)]
head(oasis.dummy)

# Plot histogram

### Age
qts.age <- quantile(as.numeric(unlist(oasis_cross_sectional[,3])))[2:4]

hist(oasis_cross_sectional[,3], breaks = 10, main="Age", xlab = "Values") 
abline(v = qts.age, col = "red", lwd =2)
dica.a2.hist.age <- recordPlot()

# cut Age
Age_recode <- cut(as.numeric(unlist(oasis_cross_sectional[,3])), breaks= c(min(oasis_cross_sectional[,3])-1,qts.age, max(oasis_cross_sectional[,3])+1),labels=c(1,2,3,4))

# check correlation
cor(as.numeric(Age_recode), as.numeric(unlist(oasis_cross_sectional[,3])), method = "spearman")

table(Age_recode)

### MMSE
hist(oasis.dummy[,2], breaks = 1, main="MMSE", xlab = "Values") 
abline(v = c(27,29), col = "red", lwd =2)
dica.a3.hist.mmse <- recordPlot()

# Cut MMSE
MMSE_recode <- cut(as.numeric(unlist(oasis.dummy[,2])), breaks= c(min(oasis.dummy[,2])-1,27, 29, max(oasis.dummy[,2])+1),labels=c(1,2,3))

table(MMSE_recode)                   
# check correlation
cor(as.numeric(MMSE_recode), as.numeric(unlist(oasis.dummy[,2])), method = "spearman")

### eTIV
min(oasis.dummy[,3])
max(oasis.dummy[,3])

qts.e <- quantile(as.numeric(unlist(oasis.dummy[,3])))[2:4]

hist(oasis.dummy[,3], breaks = 10, main = "eTIV", xlab = "values")
abline(v = qts.e, col = "red", lwd =2)
dica.a4.hist.etiv <- recordPlot()

# cut eTIV
eTIV_recode <- cut(as.numeric(unlist(oasis.dummy[,3])), breaks= c(min(oasis.dummy[,3])-1,qts.e, max(oasis.dummy[,3])+1),labels=c(1,2,3,4))

# check correlation
cor(as.numeric(eTIV_recode), as.numeric(unlist(oasis.dummy[,3])), method = "spearman")

table(eTIV_recode)

### nWBV
min(oasis.dummy[,4])
max(oasis.dummy[,4])

qts.n <- quantile(as.numeric(unlist(oasis.dummy[,4])))[2:4]

hist(oasis.dummy[,4], breaks = 10, main = "nWBV", xlab = "values")
abline(v = qts.n, col = "red", lwd =2)
dica.a5.hist.nwbv <- recordPlot()

# cut nWBV

nWBV_recode <- cut(as.numeric(unlist(oasis.dummy[,4])), breaks= c(min(oasis.dummy[,4])-1,qts.n, max(oasis.dummy[,4])+1),labels=c(1,2,3,4))

# check correlation
cor(as.numeric(nWBV_recode), as.numeric(unlist(oasis.dummy[,4])), method = "spearman")

table(nWBV_recode)


## DiCA - Bind Columns 

Col.bind <- cbind(Age_recode, MMSE_recode, eTIV_recode, nWBV_recode)

colnames(Col.bind) <- c("Age", "MMSE", "eTIV", "nWBV")

head(Col.bind)

Col.bind <- makeNominalData(as.matrix(Col.bind))

Col.bind <- Col.bind[,order(colnames(Col.bind))]

oasis.design <- oasis_cross_sectional$SES

oasis.design <- makeNominalData(as.matrix(oasis.design))

oasis.design <- oasis.design[,order(colnames(oasis.design))]

colnames(oasis.design) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

# DiCA - Design Colors
COLPAL <- list()

COLPAL$rows$CDR$vec <- oasis_cross_sectional$CDR
COLPAL$rows$MF$vec <- oasis_cross_sectional$Gender
COLPAL$rows$Educ$vec <- oasis_cross_sectional$Educ
COLPAL$rows$SES$vec <- oasis_cross_sectional$SES
COLPAL$rows$Age$vec <- Age_recode

COLPAL$rows$CDR$mat <- makeNominalData(as.matrix(COLPAL$rows$CDR$vec))
COLPAL$rows$MF$mat <- makeNominalData(as.matrix(COLPAL$rows$MF$vec))
COLPAL$rows$Educ$mat <- makeNominalData(as.matrix(COLPAL$rows$Educ$vec))
COLPAL$rows$SES$mat <- makeNominalData(as.matrix(COLPAL$rows$SES$vec))
COLPAL$rows$Age$mat <- makeNominalData(as.matrix(COLPAL$rows$Age$vec))

# Reorder the columns in ascending order
COLPAL$rows$CDR$mat <- COLPAL$rows$CDR$mat[,order(colnames(COLPAL$rows$CDR$mat))]
COLPAL$rows$Educ$mat <- COLPAL$rows$Educ$mat[,order(colnames(COLPAL$rows$Educ$mat))]
COLPAL$rows$SES$mat <- COLPAL$rows$SES$mat[,order(colnames(COLPAL$rows$SES$mat))]
COLPAL$rows$MF$mat <- COLPAL$rows$MF$mat[,order(colnames(COLPAL$rows$MF$mat))]

#The group names are labeled:
colnames(COLPAL$rows$CDR$mat) <- c("CDR 0", "CDR 0.5", "CDR 1", "CDR 2")
COLPAL$rows$CDR$labels <- colnames(COLPAL$rows$CDR$mat)

colnames(COLPAL$rows$Educ$mat) <- c("Class 1", "Class 2", "Class3", "Class 4", "Class 5")
COLPAL$rows$Educ$labels <- colnames(COLPAL$rows$Educ$mat)

colnames(COLPAL$rows$SES$mat) <- c("Class 1", "Class 2", "Class3", "Class 4", "Class 5")
COLPAL$rows$SES$labels <- colnames(COLPAL$rows$SES$mat)

colnames(COLPAL$rows$MF$mat) <- c("Female", "Male")
COLPAL$rows$MF$labels <- unique(oasis_cross_sectional$Gender)

colnames(COLPAL$rows$Age$mat) <- c("30-66", "67-73", "74-82", "83-100")

COLPAL$rows$Age$labels <- colnames(COLPAL$rows$Age$mat)
COLPAL$rows$CDR$labels <- colnames(COLPAL$rows$CDR$mat)
COLPAL$rows$Educ$labels <- colnames(COLPAL$rows$Educ$mat)
COLPAL$rows$SES$labels <- colnames(COLPAL$rows$SES$mat)
COLPAL$rows$MF$labels <- unique(oasis_cross_sectional$Gender)

# Colors for Groups
COLPAL$rows$CDR$color_groups <- c("#e197ef","#acd12b","#687cef","#750000")
COLPAL$rows$Educ$color_groups <-c("#03022f","#03a6a8","#84026a","#f802ab","#d40108")
COLPAL$rows$SES$color_groups <- c("#cb01cc","#cbcc00","#01cccd","#7f3332","#0100fb")
COLPAL$rows$MF$color_groups <- c("#fc33e2", "#282be2")

#Next we need to create a vector of the group colors.
#We'll take one group at a time and trade the group names for the color names...

#First, copy the group names
COLPAL$rows$CDR$color_obs <- as.matrix(COLPAL$rows$CDR$vec)
COLPAL$rows$Educ$color_obs <- as.matrix(COLPAL$rows$Educ$vec)
COLPAL$rows$SES$color_obs <- as.matrix(COLPAL$rows$SES$vec)
COLPAL$rows$MF$color_obs <- as.matrix(COLPAL$rows$MF$vec)

#Then, for each group, replace the group name with the respective group color
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "0")] <- 
  COLPAL$rows$CDR$color_groups[1]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "0.5")] <- COLPAL$rows$CDR$color_groups[2]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "1")] <-
  COLPAL$rows$CDR$color_groups[3]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "2")] <-
  COLPAL$rows$CDR$color_groups[4]

COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "1")] <- 
  COLPAL$rows$Educ$color_groups[1]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "2")] <- 
  COLPAL$rows$Educ$color_groups[2]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "3")] <- 
  COLPAL$rows$Educ$color_groups[3]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "4")] <- 
  COLPAL$rows$Educ$color_groups[4]
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "5")] <- 
  COLPAL$rows$Educ$color_groups[5]

COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "1")] <- 
  COLPAL$rows$SES$color_groups[1] 
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "2")] <- 
  COLPAL$rows$SES$color_groups[2] 
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "3")] <- 
  COLPAL$rows$SES$color_groups[3] 
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "4")] <- 
  COLPAL$rows$SES$color_groups[4] 
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "5")] <- 
  COLPAL$rows$SES$color_groups[5] 

COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "F")] <- 
  COLPAL$rows$MF$color_groups[1]
COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "M")] <- 
  COLPAL$rows$MF$color_groups[2]

col4dica <- c("orange","orange","orange","orange","darkblue","darkblue","darkblue",
              "darkblue","darkred","darkred","darkred","darkgreen","darkgreen","darkgreen","darkgreen")

# DiCA
resDICA <- tepDICA(DATA = Col.bind,
                   DESIGN = oasis_cross_sectional$SES,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL, graphs =  FALSE,
                   k = 0)

Fk <- resDICA$TExPosition.Data$fi
Fi <- resDICA$TExPosition.Data$fii
Fj <- resDICA$TExPosition.Data$fj

# DiCA - Inferences
resDICA.inf <- tepDICA.inference.battery(DATA = Col.bind,
                                         DESIGN = oasis_cross_sectional$SES,
                                         make_design_nominal = TRUE,
                                         group.masses = NULL,
                                         weights = NULL,
                                         graphs = FALSE,
                                         k = 2,
                                         test.iters = 100,
                                         critical.value = 2)

# DiCA - PLOTTING GRAPHS

### DiCA - Heat Map

ses <- cor(Col.bind, oasis.design)
t.ses <- t(ses)

heatMapIJ.WE <- makeggHeatMap4CT(t.ses,
                                 colorAttributes = col4dica,
                                 colorProducts = COLPAL$rows$SES$color_groups,
                                 fontSize.x = 15
) + ggtitle('DiCA Heat Map')

print(heatMapIJ.WE)
dica.a1.heat <- recordPlot()

### DiCA - Scree Plot DiCA + Inference

# Plot the Scree
PlotScreeFix(ev = resDICA$TExPosition.Data$eigs,
             p.ev = resDICA.inf$Inference.Data$components$p.vals, 
             max.ev = NULL, alpha = 0.05,
             col.ns = "#006D2C", col.sig = "#54278F",
             title = "DiCA Scree Plot with Inference",
             plotKaiser = TRUE)
dica.b1.scree <- recordPlot()

### DiCA - Graphs for the J set
baseMap.j <- PTCA4CATA::createFactorMap(Fj,
                                        col.points   = col4dica,
                                        alpha.points =  1,
                                        cex = 4,
                                        display.points = TRUE, pos = 2,
                                        display.names = TRUE,
                                        display.labels = TRUE,
                                        col.labels   = col4dica,
                                        title = "OASIS DiCA Column Factor Scores"
)

# Labels for Inertia
label4Map.oasis <- createxyLabels.gen(1,2,
                                      lambda = resDICA$TExPosition.Data$eigs,
                                      tau = resDICA$TExPosition.Data$t)

# A graph for the J-set
aggMap.j <- baseMap.j$zeMap_background + baseMap.j$zeMap_dots + baseMap.j$zeMap_text + label4Map.oasis
print(aggMap.j)
dica.c1.j1 <- recordPlot()

colnames(Fj) <- paste0('Dimension ',1:ncol(Fj))
seg.1 <- as.data.frame(Fj[c(1,2,3,4),])   
seg.2 <- as.data.frame(Fj[c(5,6,7,8),])   
seg.3 <- as.data.frame(Fj[c(9,10,11),])
seg.4 <- as.data.frame(Fj[c(12,13,14,15),])

axis1 = 1
axis2 = 2

colnames(seg.1) <- paste0('Dimension ',1:ncol(Fj))
colnames(seg.2) <- paste0('Dimension ',1:ncol(Fj))
colnames(seg.3) <- paste0('Dimension ',1:ncol(Fj))
colnames(seg.4) <- paste0('Dimension ',1:ncol(Fj))

mca.line.map <- aggMap.j +
  geom_path(data = seg.1, col = col4dica[c(1)], alpha =1) +
  geom_path(data = seg.2, col = col4dica[c(5)], alpha =1) + 
  geom_path(data = seg.3, col = col4dica[c(9)], alpha =1) +
  geom_path(data = seg.4, col = col4dica[c(12)], alpha =1)
print(mca.line.map)
dica.c2.j2 <- recordPlot()

### DiCA - Graphs for the I set

# Create the base map
baseMap.i <- PTCA4CATA::createFactorMap(Fi,
                                        col.points   = COLPAL$rows$SES$color_obs,
                                        alpha.points = 1,
                                        cex = 2.2)

# Plain map with color for the I-set
aggMap.i <- baseMap.i$zeMap_background + baseMap.i$zeMap_dots + label4Map.oasis +
  ggtitle("OASIS DiCA Row Factor Scores")

print(aggMap.i)
dica.d1.i1 <- recordPlot()

Fk <- Fk[order(rownames(Fk)),]
rownames(Fk) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

# color for the means
col4Means <- COLPAL$rows$SES$color_groups

# create the map for the means
MapGroup    <- PTCA4CATA::createFactorMap(Fk,
                                          axis1 = 1, axis2 = 2,
                                          constraints = baseMap.i$constraints,
                                          title = NULL,
                                          col.points = col4Means,
                                          alpha.points = 1,
                                          alpha.labels = 1,
                                          display.points = TRUE,
                                          pch = 19, cex = 6,
                                          display.labels = TRUE,
                                          col.labels = col4Means,
                                          text.cex = 4,
                                          font.face = "bold",
                                          font.family = "sans",
                                          col.axes = "darkorchid",
                                          alpha.axes = 0.2,
                                          width.axes = 1.1,
                                          col.background = adjustcolor("lavender",
                                                                       alpha.f = 0.4),
                                          force = 1, segment.size = 0)

# The map with observations and group means
aggMap.i.withMeans <- aggMap.i+
  MapGroup$zeMap_dots + MapGroup$zeMap_text + label4Map.oasis +
  ggtitle("OASIS DiCA Row Factor Scores with Means")

print(aggMap.i.withMeans)
dica.d2.i2 <- recordPlot()

## DiCA - Confusion matrices - Confidence Interval Plots

# To be saved as table
fixedCM   <-   resDICA.inf$Inference.Data$loo.data$fixed.confuse
looedCM   <- resDICA.inf$Inference.Data$loo.data$loo.confuse

# Create Confidence Interval Plots
BootCube <- resDICA.inf$Inference.Data$boot.data$fi.boot.data$boots
dimnames(BootCube)[[2]] <- c("Dimension 1","Dimension 2")

# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- MakeCIEllipses(BootCube[,1:2,],
                            names.of.factors = c("Dimension 1","Dimension 2"),
                            col = c("#01cccd", "#cb01cc", "#cbcc00", "#7f3332", "#0100fb"),
                            p.level = .95
)

# create the I-map with Observations, means and confidence intervals
aggMap.i.withCI <-  aggMap.i +  GraphElli + MapGroup$zeMap_text + MapGroup$zeMap_dots + label4Map.oasis +
  ggtitle("OASIS DiCA Row Factor Scores with Confidence Intervals")

print(aggMap.i.withCI)
dica.d3.i3 <- recordPlot()

# Create 75% Tolerance interval polygons
GraphTI.Hull.90 <- MakeToleranceIntervals(Fi,
                                          as.factor(oasis_cross_sectional$SES),
                                          names.of.factors = c("Dim1","Dim2"),
                                          col = col4Means,
                                          line.size = .5, line.type = 3,
                                          alpha.ellipse = .2,
                                          alpha.line = .4,
                                          p.level = .75, 
                                          type = 'hull' 
)

aggMap.i.withHull <- aggMap.i +
  GraphTI.Hull.90 + MapGroup$zeMap_dots +
  MapGroup$zeMap_text +  MapGroup$zeMap_dots + label4Map.oasis +
  ggtitle("OASIS DiCA Row Factor Scores with Tolerance Intervals")

print(aggMap.i.withHull)
dica.d4.i4 <- recordPlot()

## DiCA - Contribution Bars I-set
## Signed Contributions
signed.ctrI <- resDICA$TExPosition.Data$ci * sign(resDICA$TExPosition.Data$fi)
signed.ctrI <- signed.ctrI[order(rownames(signed.ctrI)), ]  
rownames(signed.ctrI) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

## Contribution 1
ctrI.s.1 <- PrettyBarPlot2(signed.ctrI[,1],
                           threshold = 1 / NROW(signed.ctrI),
                           font.size = 5,
                           color4bar = COLPAL$rows$SES$color_groups, 
                           main = 'OASIS DiCA:  I set - Variable Contribution 1 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrI), 1.2*max(signed.ctrI))
)
print(ctrI.s.1)
dica.e1.c1.i1 <- recordPlot()

## Contribution 2 
ctrI.s.2 <- PrettyBarPlot2(signed.ctrI[,2],
                           threshold = 1 / NROW(signed.ctrI),
                           font.size = 5,
                           color4bar = COLPAL$rows$SES$color_groups, 
                           main = 'OASIS DiCA: I set - Variable Contribution 2 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrI), 1.2*max(signed.ctrI))
)
print(ctrI.s.2)
dica.e2.c2.i2 <- recordPlot()

## Contribution 3 
ctrI.s.3 <- PrettyBarPlot2(signed.ctrI[,3],
                           threshold = 1 / NROW(signed.ctrI),
                           font.size = 5,
                           color4bar = COLPAL$rows$SES$color_groups, 
                           main = 'OASIS DiCA: I set - Variable Contribution 3 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrI), 1.2*max(signed.ctrI))
)
print(ctrI.s.3)
dica.e3.c3.i3 <- recordPlot()

## DiCA - Contribution Bars J-set

## Signed Contributions
signed.ctrJ <- resDICA$TExPosition.Data$cj * sign(resDICA$TExPosition.Data$fj)

## Contribution 1
ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4dica), 
                           main = 'OASIS DiCA:  J set - Variable Contribution 1 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ)),
                           horizontal = FALSE
)
print(ctrJ.s.1)
dica.f1.c1.j1 <- recordPlot()

## Contribution 2
ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4dica), 
                           main = 'OASIS DiCA:  J set - Variable Contribution 2 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ)),
                           horizontal = FALSE
)
print(ctrJ.s.2)
dica.f2.c2.j2 <- recordPlot()

## Contribution 3
ctrJ.s.3 <- PrettyBarPlot2(signed.ctrJ[,3],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4dica), 
                           main = 'OASIS DiCA:  J set - Variable Contribution 3 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ)),
                           horizontal = FALSE
)
print(ctrJ.s.3)
dica.f3.c3.j3 <- recordPlot()

## DiCA - Bootstrap Ratios I-set

BR.i <- resDICA.inf$Inference.Data$boot.data$fi.boot.data$tests$boot.ratios
BR.i <- BR.i[order(rownames(BR.i)), ]
rownames(BR.i) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

laDim = 1
i.BR1 <- PrettyBarPlot2(BR.i[,laDim],
                        threshold = 2,
                        font.size = 5,
                        color4bar = COLPAL$rows$SES$color_groups,
                        main = paste0(
                          'DiCA on the Oasis: I-set Bootstrap ratio Dimension ',laDim),
                        ylab = 'Bootstrap ratios'
)
print(i.BR1)
dica.g1.br1.i1 <- recordPlot()

laDim = 2
i.BR2 <- PrettyBarPlot2(BR.i[,laDim],
                        threshold = 2,
                        font.size = 5,
                        color4bar = COLPAL$rows$SES$color_groups,
                        main = paste0(
                          'DiCA on the Oasis: I-set Bootstrap ratio Dimension ',laDim),
                        ylab = 'Bootstrap ratios'
)
print(i.BR2)
dica.g2.br2.i2 <- recordPlot()

## DiCA - Bootstrap Ratios J-set
BR.j <- resDICA.inf$Inference.Data$boot.data$fj.boot.data$tests$boot.ratios

laDim = 1
j.BR1 <- PrettyBarPlot2(BR.j[,laDim],
                        threshold = 2,
                        font.size = 5,
                        color4bar = gplots::col2hex(col4dica),
                        main = paste0(
                          'DiCA on the Oasis: J-set Bootstrap ratio Dimension ',laDim),
                        ylab = 'Bootstrap ratios'
)
print(j.BR1)
dica.h1.br1.j1 <- recordPlot()

laDim = 2
j.BR2 <- PrettyBarPlot2(BR.j[,laDim],
                        threshold = 2,
                        font.size = 5,
                        color4bar = gplots::col2hex(col4dica),
                        main = paste0(
                          'DiCA on the Oasis: J-set Bootstrap ratio Dimension ',laDim),
                        ylab = 'Bootstrap ratios'
)
print(j.BR2)
dica.h2.br2.j2 <- recordPlot()

saveGraph2pptx('kp.bada-dicappt.pptx',addGraphNames = TRUE)
