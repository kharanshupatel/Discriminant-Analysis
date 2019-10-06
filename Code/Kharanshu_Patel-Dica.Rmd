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
# Recode Variable Educ
# Used dummy values 50 and 100 for the sake of recode

table(oasis_cross_sectional$Educ)
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 1] <- "50" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 5] <- "1" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 50] <- "5" 

oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 2] <- "100" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 4] <- "2" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 100] <- "4" 

```
### Recode Columns for DiCA

```{r Recode Columns for DiCA}

# Plot histogram

### Age
qts.age <- quantile(as.numeric(unlist(oasis_cross_sectional[,3])))[2:4]

hist(oasis_cross_sectional[,3], breaks = 10, main="Age", xlab = "Values") 
abline(v = qts.age, col = "red", lwd =2)
A1 = recordPlot()

# cut Age
Age_recode <- cut(as.numeric(unlist(oasis_cross_sectional[,3])), breaks= c(min(oasis_cross_sectional[,3])-1,qts.age, max(oasis_cross_sectional[,3])+1),labels=c(1,2,3,4))

# check correlation
cor(as.numeric(Age_recode), as.numeric(unlist(oasis_cross_sectional[,3])), method = "spearman")

table(Age_recode)

### MMSE
hist(oasis.dummy[,2], breaks = 1, main="MMSE", xlab = "Values") 
abline(v = c(27,29), col = "red", lwd =2)
A2 = recordPlot()

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
A3 = recordPlot()

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
A4 = recordPlot()

# cut nWBV

nWBV_recode <- cut(as.numeric(unlist(oasis.dummy[,4])), breaks= c(min(oasis.dummy[,4])-1,qts.n, max(oasis.dummy[,4])+1),labels=c(1,2,3,4))

# check correlation
cor(as.numeric(nWBV_recode), as.numeric(unlist(oasis.dummy[,4])), method = "spearman")

table(nWBV_recode)

```
## Bind Columns 

```{r Bind Columns}

Col.bind <- cbind(Age_recode, MMSE_recode, eTIV_recode, nWBV_recode)

colnames(Col.bind) <- c("Age", "MMSE", "eTIV", "nWBV")

head(Col.bind)

Col.bind <- makeNominalData(as.matrix(Col.bind))

Col.bind <- Col.bind[,order(colnames(Col.bind))]

oasis.design <- oasis_cross_sectional$SES

oasis.design <- makeNominalData(as.matrix(oasis.design))

oasis.design <- oasis.design[,order(colnames(oasis.design))]

colnames(oasis.design) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

```

# Design Colors

```{r Colors}

COLPAL <- list()

COLPAL$rows$CDR$vec <- oasis_cross_sectional$CDR
COLPAL$rows$MF$vec <- oasis_cross_sectional$`M/F`
COLPAL$rows$Educ$vec <- oasis_cross_sectional$Educ
COLPAL$rows$SES$vec <- oasis_cross_sectional$SES

COLPAL$rows$CDR$mat <- makeNominalData(as.matrix(COLPAL$rows$CDR$vec))
COLPAL$rows$MF$mat <- makeNominalData(as.matrix(COLPAL$rows$MF$vec))
COLPAL$rows$Educ$mat <- makeNominalData(as.matrix(COLPAL$rows$Educ$vec))
COLPAL$rows$SES$mat <- makeNominalData(as.matrix(COLPAL$rows$SES$vec))


# Reorder the columns in ascending order
COLPAL$rows$CDR$mat <- COLPAL$rows$CDR$mat[,order(colnames(COLPAL$rows$CDR$mat))]
COLPAL$rows$Educ$mat <- COLPAL$rows$Educ$mat[,order(colnames(COLPAL$rows$Educ$mat))]
COLPAL$rows$SES$mat <- COLPAL$rows$SES$mat[,order(colnames(COLPAL$rows$SES$mat))]
COLPAL$rows$MF$mat <- COLPAL$rows$MF$mat[,order(colnames(COLPAL$rows$MF$mat))]

COLPAL$rows$CDR$labels <- colnames(COLPAL$rows$CDR$mat)
COLPAL$rows$Educ$labels <- colnames(COLPAL$rows$Educ$mat)
COLPAL$rows$SES$labels <- colnames(COLPAL$rows$SES$mat)
COLPAL$rows$MF$labels <- unique(oasis_cross_sectional$`M/F`)

## CDR original --> 
COLPAL$rows$CDR$color_groups <- c("#037520","#282be2","#f08300","#780000")
COLPAL$rows$Educ$color_groups <-c("#03022f","#03a6a8","#84026a","#f802ab","#d40108")
COLPAL$rows$SES$color_groups <- c("#013600","#f08300","#282be2","#f802ab","#ff0200")
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
COLPAL$rows$Educ$color_groups[1] #darkblue
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "2")] <- 
COLPAL$rows$Educ$color_groups[2] #cyan
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "3")] <- 
COLPAL$rows$Educ$color_groups[3] #magenta
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "4")] <- 
COLPAL$rows$Educ$color_groups[4] #pink
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "5")] <- 
COLPAL$rows$Educ$color_groups[5] #red

COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "1")] <- 
COLPAL$rows$SES$color_groups[1] #darkgreen
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "2")] <- 
COLPAL$rows$SES$color_groups[2] #olivegreen
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "3")] <- 
COLPAL$rows$SES$color_groups[3] #yellow
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "4")] <- 
COLPAL$rows$SES$color_groups[4] #lightpink
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "5")] <- 
COLPAL$rows$SES$color_groups[5] #red

COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "F")] <- 
COLPAL$rows$MF$color_groups[1]
COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "M")] <- 
COLPAL$rows$MF$color_groups[2]

col4dica <- c("orange","orange","orange","orange","darkgreen","darkgreen","darkgreen",
             "darkgreen","darkblue","darkblue","darkblue","darkred","darkred","darkred","darkred")


```
# DiCA

```{r DiCA}

resDICA <- tepDICA(DATA = Col.bind,
                   DESIGN = oasis_cross_sectional$SES,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL, graphs =  FALSE,
                   k = 0)

Fk <- resDICA$TExPosition.Data$fi
Fi <- resDICA$TExPosition.Data$fii
Fj <- resDICA$TExPosition.Data$fj

```

# Inferences

```{r Inferences}
resDICA.inf <- tepDICA.inference.battery(DATA = Col.bind,
                   DESIGN = oasis_cross_sectional$SES,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL,
                   graphs = FALSE,
                   k = 2,
                   test.iters = 100,
                   critical.value = 2)

```

#---------------------------------------------------------------------

# PLOTTING GRAPHS

### Heat Map

```{r Heat Map}

col4heat <- c("red","green")

ses <- cor(Col.bind, oasis.design)
t.ses <- t(ses)

heatMapIJ.WE <- makeggHeatMap4CT(t.ses,
colorAttributes = col4dica,
colorProducts = col4heat,
fontSize.x = 15
) + ggtitle('Heat Map')

print(heatMapIJ.WE)
A5 = recordPlot()

```

### Scree Plot DiCA + Inference

```{r Scree Plot DiCA}


# Create Function for Scree Plot
PlotScreeFix <- function (ev, p.ev = NULL, max.ev = NULL, alpha = 0.05, col.ns = "#006D2C", 
          col.sig = "#54278F", title = "Explained Variance per Dimension", 
          plotKaiser = FALSE, color4Kaiser = "darkorchid4", lwd4Kaiser = 2.5) 
{
  val.tau = (100 * ev/sum(ev))
  Top.y = ceiling(max(val.tau) * 0.1) * 10
  if (!is.null(max.ev)) {
    ev = ev * (max.ev/ev[1])
  }
  par(mar = c(5, 6, 4, 4))
  plot(x = seq(1, length(val.tau)), y = val.tau, xlab = "Dimensions", 
       ylab = "Percentage of Explained Variance", main = title, 
       type = "l", col = col.ns, lwd = 1, xlim = c(1, length(val.tau)), 
       ylim = c(0, Top.y))
  points(x = seq(1, length(val.tau)), y = val.tau, pch = 16, 
         cex = 1, col = col.ns, lwd = 2.5)
  if (!is.null(p.ev)) {
    signi.vp = which(p.ev < alpha)
    ##### These are the lines I changed #####
    lines(x = seq(1, max(signi.vp)), y = val.tau[1:max(signi.vp)], 
          type = "l", col = col.sig, lwd = 1.5)
    points(x = signi.vp, y = val.tau[signi.vp], 
           pch = 16, cex = 1.5, col = col.sig, lwd = 3.5)
    #########################################
  }
  par(new = TRUE)
  par(mar = c(5, 6, 4, 4) + 0.5)
  le.max.vp = Top.y * (ev[1]/val.tau[1])
  plot(ev, ann = FALSE, axes = FALSE, type = "n", ylim = c(0, 
                                                           le.max.vp))
  if (plotKaiser) {
    abline(h = sum(ev)/length(ev), col = color4Kaiser, lwd = lwd4Kaiser)
  }
  mtext("Inertia Extracted by the Components", side = 4, line = 3)
  axis(4)
}
resDICA.inf$Fixed.Data$TExPosition.Data$eigs
resDICA.inf$Inference.Data$components$p.vals
resDICA.inf$Fixed.Data$TExPosition.Data$t

# Plot the Scree
PlotScreeFix(ev = resDICA$TExPosition.Data$eigs,
          p.ev = resDICA.inf$Inference.Data$components$p.vals, 
          max.ev = NULL, alpha = 0.05,
          col.ns = "#006D2C", col.sig = "#54278F",
          title = "Explained Variance per Dimension",
          plotKaiser = TRUE)
A6 = recordPlot()
       
```

### Graphs for the J set

```{r J set Graphs}

baseMap.j <- PTCA4CATA::createFactorMap(Fj,
                            col.points   = col4dica,
                            alpha.points =  1,
                            cex = 4,
                            display.points = TRUE, pos = 2,
                            display.names = TRUE,
                            display.labels = TRUE,
                            col.labels   = col4dica,
                            title = "Column Factor Scores"
                            )

# Labels for Inertia
label4Map.oasis <- createxyLabels.gen(1,2,
lambda = resDICA$TExPosition.Data$eigs,
tau = resDICA$TExPosition.Data$t)

# A graph for the J-set
aggMap.j <- baseMap.j$zeMap_background + baseMap.j$zeMap_dots + baseMap.j$zeMap_text + label4Map.oasis
print(aggMap.j)

rownames(Fj)
colnames(Fj) <- paste0('Dimension ',1:ncol(Fj))
seg.1 <- as.data.frame(Fj[c(1,2,3,4),])   #1,4,2,3
seg.2 <- as.data.frame(Fj[c(5,6,7,8),])   #4,1,2,3
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
A7 = recordPlot()

```
### Graphs for the I set

```{r I set Graphs}

# Create the base map
baseMap.i <- PTCA4CATA::createFactorMap(Fi,
                                        col.points   = COLPAL$rows$SES$color_obs,
                                        alpha.points = 0.5,
                                        cex = 2.2)

#---------------------------------------------------------------------
# Plain map with color for the I-set
aggMap.i <- baseMap.i$zeMap_background + baseMap.i$zeMap_dots + label4Map.oasis
#---------------------------------------------------------------------
# print this Map
print(aggMap.i)
A8 = recordPlot()
#---------------------------------------------------------------------

# a vector of color for the means
col4Means <- COLPAL$rows$SES$color_groups
#---------------------------------------------------------------------

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
  MapGroup$zeMap_dots + MapGroup$zeMap_text
#---------------------------------------------------------------------
# plot it!
print(aggMap.i.withMeans)
A9 = recordPlot()
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Create 75% Tolerance interval polygons
#
GraphTI.Hull.90 <- MakeToleranceIntervals(Fi,
                                  as.factor(oasis_cross_sectional$SES),
                                  names.of.factors = c("Dim1","Dim2"),
                                  col = col4Means,
                                  line.size = .5, line.type = 3,
                                  alpha.ellipse = .2,
                                  alpha.line = .4,
                                  p.level = .75, # 75% TI
                                  type = 'hull' #
                                          # use 'hull' for convex hull
)
#---------------------------------------------------------------------
# Create the map
aggMap.i.withHull <- aggMap.i +
  GraphTI.Hull.90 + MapGroup$zeMap_dots +
  MapGroup$zeMap_text +  MapGroup$zeMap_dots
#---------------------------------------------------------------------
# Plot it!
print(aggMap.i.withHull)
A10 = recordPlot()
```

## Confusion matrices - Confidence Interval Plots

```{r Confusion Matrices - Confidence Interval Plots}

# To be saved as table
fixedCM   <-   resDICA.inf$Inference.Data$loo.data$fixed.confuse
looedCM   <- resDICA.inf$Inference.Data$loo.data$loo.confuse

#---------------------------------------------------------------------
# Create Confidence Interval Plots
BootCube <- resDICA.inf$Inference.Data$boot.data$fi.boot.data$boots
dimnames(BootCube)[[2]] <- c("Dimension 1","Dimension 2")
# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- MakeCIEllipses(BootCube[,1:2,],
                  names.of.factors = c("Dimension 1","Dimension 2"),
                  col = col4Means,
                  p.level = .95
)
#---------------------------------------------------------------------
# create the I-map with Observations, means and confidence intervals
#
aggMap.i.withCI <-  aggMap.i +  GraphElli + MapGroup$zeMap_text + MapGroup$zeMap_dots
#---------------------------------------------------------------------
# plot it!
print(aggMap.i.withCI)
```

## Contribution Bars I-set

```{r DiCA - Contribution Bars I-set}

## Signed Contributions
signed.ctrI <- resDICA$TExPosition.Data$ci
signed.ctrI <- signed.ctrI[order(rownames(signed.ctrI)), ]  

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
A11 = recordPlot()

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
A12 = recordPlot()

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
A13 = recordPlot()

```
## Contribution Bars J-set

```{r DiCA - Contribution Bars J-set}

## Signed Contributions
signed.ctrJ <- resDICA$TExPosition.Data$cj
## Contribution 1
ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4dica), 
                         main = 'OASIS DiCA:  J set - Variable Contribution 1 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)

print(ctrJ.s.1)
A14 = recordPlot() 

## Contribution 2
ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4dica), 
                         main = 'OASIS DiCA:  J set - Variable Contribution 2 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)

print(ctrJ.s.2)
A15 = recordPlot()

## Contribution 3
ctrJ.s.3 <- PrettyBarPlot2(signed.ctrJ[,3],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4dica), 
                         main = 'OASIS DiCA:  J set - Variable Contribution 3 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)

print(ctrJ.s.3)
A16 = recordPlot()

```


## Bootstrap Ratios I-set

```{r Bootstrap Ratios I-set}

BR.i <- resDICA.inf$Inference.Data$boot.data$fi.boot.data$tests$boot.ratios
BR.i<- BR.i[order(rownames(BR.i)), ]

laDim = 1
i.BR1 <- PrettyBarPlot2(BR.i[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = COLPAL$rows$SES$color_groups,
                   main = paste0(
                     'DiCA on the Oasis: I-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(i.BR1)
A17 = recordPlot()

laDim = 2
i.BR2 <- PrettyBarPlot2(BR.i[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = COLPAL$rows$SES$color_groups,
                   main = paste0(
                     'DiCA on the Oasis: I-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(i.BR2)
A18 = recordPlot()
```

## Bootstrap Ratios J-set

```{r Bootstrap Ratios J-set}

BR.j <- resDICA.inf$Inference.Data$boot.data$fj.boot.data$tests$boot.ratios

laDim = 1
j.BR1 <- PrettyBarPlot2(BR.j[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = gplots::col2hex(col4dica),
                   main = paste0(
                     'DiCA on the Oasis: J-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(j.BR1)
A19 = recordPlot()

laDim = 2
j.BR2 <- PrettyBarPlot2(BR.j[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = gplots::col2hex(col4dica),
                   main = paste0(
                     'DiCA on the Oasis: J-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(j.BR2)
A20 = recordPlot()
```


